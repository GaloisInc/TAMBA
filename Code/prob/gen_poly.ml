open Lang
open Util
open Ppl_ocaml
open Gmp

type gen_poly =
  { bounds      : (int * int) VarIDMap.t;
    constraints : (int VarIDMap.t * int) list list
  }

let poly_of_gen_poly (p : gen_poly) : polyhedron list =
  let varid_eq (x : varid) (y : varid) : bool =
    OrderedVarID.compare x y = 0
  in

  let fvs = List.map fst (VarIDMap.bindings p.bounds) in
  let ret_sz = List.length p.constraints in

  let ret = list_replicate ret_sz (ppl_new_C_Polyhedron_from_space_dimension (List.length fvs) Universe) in

  let append_bound (p : polyhedron) (var : varid) (b : int * int) : unit =
    let (l, u) = b in
    let idx = list_idx (varid_eq var) fvs in
    ppl_Polyhedron_add_constraint p (Less_Or_Equal (Coefficient (Z.from_int l), Variable idx));
    ppl_Polyhedron_add_constraint p (Less_Or_Equal (Variable idx, Coefficient (Z.from_int u)))
  in

  List.iter (fun disj -> VarIDMap.iter (append_bound disj) p.bounds) ret;

  let append_constraint (p : polyhedron) (lhs : int VarIDMap.t) (rhs : int) : unit =
    let to_linear_expression (lhs_cpy : int VarIDMap.t) : linear_expression =
      VarIDMap.fold (fun var coeff acc ->
          let idx = list_idx (varid_eq var) fvs in
          Plus (Times (Z.from_int coeff, Variable idx), acc)) lhs_cpy (Coefficient Z.zero)
    in
    
    ppl_Polyhedron_add_constraint p (Less_Or_Equal (to_linear_expression lhs, Coefficient (Z.from_int rhs)))
  in

  let tmp = list_zip ret p.constraints in

  List.iter (fun (ret_poly, disj) -> List.iter (fun (lhs, rhs) -> append_constraint ret_poly lhs rhs) disj) tmp;

  List.map fst tmp

let latte_of_gen_poly (p : gen_poly) = List.map Latte.latte_of_poly (poly_of_gen_poly p)

let volcomp_of_gen_poly (p : gen_poly) : Volcomp.volcomp list =
  let decls = VarIDMap.bindings p.bounds in
  let fvs = List.map fst decls in
  let ret = List.map (fun c ->
                { Volcomp.decls = decls;
                  leqs = (List.map (fun (lhs, rhs) ->
                             (List.map (fun var -> try VarIDMap.find var lhs with | Not_found -> 0) fvs), rhs) c)
                }) p.constraints in
  ret

let string_of_gen_poly (p : gen_poly) : string =
  let constrs = p.constraints in

  let terms_to_string (ts : int VarIDMap.t) : string =
    String.concat " + " (List.map (fun ((_, name), coeff) -> (string_of_int coeff) ^ "*" ^ name) (VarIDMap.bindings ts))
  in

  let ineq_to_string (ineq : int VarIDMap.t * int) : string =
    let (ts, lim) = ineq in
    let terms_s = terms_to_string ts in
    terms_s ^ " <= " ^ (string_of_int lim)
  in

  let ineqs_to_string (ineqs : (int VarIDMap.t * int) list) : string =
    String.concat "\n" (List.map ineq_to_string ineqs)
  in

  String.concat "\n" (List.map ineqs_to_string constrs)
