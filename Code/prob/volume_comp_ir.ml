open Lang
open Util
open Ppl_ocaml
open Gmp

type fvs = varid list
type bounds = (int * int) VarIDMap.t
type constrs = (int VarIDMap.t * int) list list

type vc_ir = (fvs * bounds * constrs)

let poly_of_vc_ir (ir : vc_ir) : polyhedron =
  let (fvs, bounds, constrs) = ir in
  let ret = ppl_new_C_Polyhedron_from_space_dimension (List.length fvs) Universe in

  let add_bound (var : varid) (b : int * int) : unit =
    let (l, u) = b in
    let varidx = list_idx (fun x -> OrderedVarID.compare var x = 0) fvs in
    ppl_Polyhedron_add_constraint ret (Less_Or_Equal (Coefficient (Z.from_int l), Variable varidx));
    ppl_Polyhedron_add_constraint ret (Less_Or_Equal (Variable varidx, Coefficient (Z.from_int u)))
  in

  let terms_to_ppl (ts : int VarIDMap.t) : linear_expression =
    VarIDMap.fold (fun var coeff acc ->
        let varidx = list_idx (fun x -> OrderedVarID.compare var x = 0) fvs in
        Plus (Times (Z.from_int coeff, Variable varidx), acc)) ts (Coefficient Z.zero)
  in

  VarIDMap.iter add_bound bounds;

  (match constrs with
  | [] -> ()
  | h :: _ -> List.iter (fun (terms, lim) -> ppl_Polyhedron_add_constraint ret (Less_Or_Equal (terms_to_ppl terms, Coefficient (Z.from_int lim)))) h);

  ret

let volcomp_of_vc_ir (ir : vc_ir) : Volcomp.volcomp =
  ([], [])

let vc_ir_to_string (ir : vc_ir) : string =
  let (_, _, constrs) = ir in

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

(*
let rec linear_expression_of_asym (vmap : (string * int) list) (a : asym) : linear_expression =
  match a with
  | SymInt n -> Coefficient (Z.from_int n)
  | SymAtom (name) -> let (_, x) = name in Variable (List.assoc x vmap)
  | SymAdd (a1, a2) -> Plus ((linear_expression_of_asym vmap a1), (linear_expression_of_asym vmap a2))
  | SymSub (a1, a2) -> Minus ((linear_expression_of_asym vmap a1), (linear_expression_of_asym vmap a2))
  | SymMul (a1, SymInt n) -> Times ((Z.from_int n), (linear_expression_of_asym vmap a1))
  | SymMul (SymInt n, a2) -> Times ((Z.from_int n), (linear_expression_of_asym vmap a2))
  | _ -> raise (Invalid_argument "nonlinear asym passed to linear_expression_of_asym")
                       
let linear_constraint_of_lsym (vmap : (string * int) list) (l : lsym) : linear_constraint =
  match l with
  | SymLt (a1, a2) -> Less_Or_Equal (Plus ((linear_expression_of_asym vmap a1), (Coefficient (Z.from_int 1))), (linear_expression_of_asym vmap a2))
  | SymLeq (a1, a2) -> Less_Or_Equal ((linear_expression_of_asym vmap a1), (linear_expression_of_asym vmap a2))
  | SymEq (a1, a2) -> Equal ((linear_expression_of_asym vmap a1), (linear_expression_of_asym vmap a2))
  | SymGt (a1, a2) -> Greater_Or_Equal ((linear_expression_of_asym vmap a1), Plus ((linear_expression_of_asym vmap a2), (Coefficient (Z.from_int 1))))
  | SymGeq (a1, a2) -> Greater_Or_Equal ((linear_expression_of_asym vmap a1), (linear_expression_of_asym vmap a2))
  | _ -> raise (Invalid_argument "constraint_of_lsym on logical operator or true or false")
                  
let poly_of_lsym (l : lsym) : polyhedron =
  let vmap = lsym_fvs l in
  (* List.iter (fun (s, i) -> print_string "("; print_string s; print_string ", "; print_string (string_of_int i); print_string ")"; print_newline()) vmap; *)
  (* print_endline (lsym_to_string (simplify_lsym (dnf_of_dnfi (dnfi_of_nnf (nnf_of_lsym l))))); *)
  let num_vars = List.length vmap in
  let simple = dnfi_of_nnf (simplify_lsym (dnf_of_dnfi (dnfi_of_nnf (nnf_of_lsym l)))) in
  (* List.iter (fun sym -> print_endline (lsym_to_string sym)) (List.hd simple); *)
  let to_poly (ls : lsym list) : polyhedron =
    let ret = ppl_new_C_Polyhedron_from_space_dimension num_vars Universe in
    List.iter (fun c -> ppl_Polyhedron_add_constraint ret (linear_constraint_of_lsym vmap c)) ls;
    ret
  in
  let polyhedra = List.map to_poly simple in
  let ret = ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension num_vars Universe in
  List.iter (fun p -> ppl_Pointset_Powerset_C_Polyhedron_add_disjunct ret p) polyhedra;
  if (List.length polyhedra = 1) then
    List.hd polyhedra
  else
    raise (Failure "poly_of_lsym doesn't know how to handle disjunctions yet")
          (* ret *)

 *)
