open Util
open Lang
open Ppl_ocaml
open Gmp

(* == Symbol Definition == *)
type asym =
  | SymInt  of int
  | SymAtom of Lang.varid
  | SymAdd  of asym * asym
  | SymSub  of asym * asym
  | SymMul  of asym * asym
  | SymDiv  of asym * asym

type lsym =
  | SymTrue
  | SymFalse
  | SymLt  of asym * asym
  | SymLeq of asym * asym
  | SymEq  of asym * asym
  | SymGt  of asym * asym
  | SymGeq of asym * asym
  | SymAnd of lsym * lsym
  | SymOr  of lsym * lsym
  | SymNot of lsym

(* == Stringify == *)
let add_parens str parent curr =
  if parent = 0 then
    str
  else if parent < curr then
    "(" ^ str ^ ")"
  else
    str
      
let asym_to_string (a : asym) : string =
  let rec asym_to_string_h ast prec =
    match ast with
    | SymInt  n        -> string_of_int n
    | SymAtom name     -> let (_, x) = name in x
    | SymAdd  (a1, a2) -> add_parens (asym_to_string_h a1 4 ^ " + " ^ asym_to_string_h a2 4) prec 4
    | SymSub  (a1, a2) -> add_parens (asym_to_string_h a1 4 ^ " - " ^ asym_to_string_h a2 4) prec 4
    | SymMul  (a1, a2) -> add_parens (asym_to_string_h a1 3 ^ " × " ^ asym_to_string_h a2 3) prec 3
    | SymDiv  (a1, a2) -> add_parens (asym_to_string_h a1 3 ^ " / " ^ asym_to_string_h a2 3) prec 3 in
  asym_to_string_h a 0
      
let lsym_to_string (l : lsym) : string =
  let rec lsym_to_string_h ast prec =
    match ast with
    | SymTrue         -> "T"
    | SymFalse        -> "F"
    | SymLt  (a1, a2) -> add_parens (asym_to_string a1      ^ " < " ^   asym_to_string a2) prec 6
    | SymLeq (a1, a2) -> add_parens (asym_to_string a1      ^ " ≤ " ^   asym_to_string a2) prec 6
    | SymEq  (a1, a2) -> add_parens (asym_to_string a1      ^ " = " ^   asym_to_string a2) prec 7
    | SymGt  (a1, a2) -> add_parens (asym_to_string a1      ^ " > " ^   asym_to_string a2) prec 6
    | SymGeq (a1, a2) -> add_parens (asym_to_string a1      ^ " ≥ " ^   asym_to_string a2) prec 6
    | SymAnd (l1, l2) -> add_parens (lsym_to_string_h l1 11 ^ " ∧ " ^ lsym_to_string_h l2 11) prec 11
    | SymOr  (l1, l2) -> add_parens (lsym_to_string_h l1 12 ^ " ∨ " ^ lsym_to_string_h l2 12) prec 12
    | SymNot l'       -> "¬" ^ add_parens (lsym_to_string_h l' 2) prec 2
  in
  lsym_to_string_h l 0

(* == Helpers == *)
                   
(* Given an asym that looks like ax + by + cz + d, returns (ax + by + cz, d) *)
let rec break_linear_aexp (a : asym) : asym * asym =
  match a with
  | SymAdd (a1, SymInt n) -> (a1, SymInt n)
  | SymAdd (a1, a2) ->
     let (aterms, cterm) = break_linear_aexp a2 in
     (SymAdd (a1, aterms), cterm)
  | _ -> raise (General_error ("Tried to break constant or non-linear asym: " ^ (asym_to_string a)))
                   
(* Union function over map for convenience, see Map.union in 4.03.0 docs *)
let union f m1 m2 =
  let do_union k v1 v2 =
    match (v1, v2) with
    | (Some x, Some y) -> f k x y
    | (Some x, None)   -> Some x
    | (None, Some y)   -> Some y
    | _                -> None
  in
  VarIDMap.merge do_union m1 m2

(* Map a function over all the logical atoms of DNF (as produced by dnf_of_lsym) *)
let rec map_dnf (f : lsym -> lsym) (dnf : lsym) : lsym =
  let rec map_conjs (f' : lsym -> lsym) (cs : lsym) : lsym =
    match cs with
    | SymAnd (l, rest) -> SymAnd (f' l, map_conjs f' rest)
    | _                -> f' cs
  in

  let rec map_disjs (f' : lsym -> lsym) (ds : lsym) : lsym =
    match ds with
    | SymOr (cs, rest) -> SymOr (map_conjs f' cs, map_dnf f' rest)
    | _                -> map_conjs f' ds
  in

  map_disjs f dnf

(* Looks for occurences of T or F in DNF, and eliminates them *)
let simple_dnf (dnf : lsym) : lsym =
  let rec simple_conjs (cs : lsym) : lsym =
    match cs with
    | SymAnd (l, rest) ->
       let rest' = simple_conjs rest in
       (match (l, rest') with
        | (SymFalse, _) -> SymFalse
        | (_, SymFalse) -> SymFalse
        | (SymTrue, _)  -> rest'
        | (_, SymTrue)  -> l
        | _             -> SymAnd (l, rest'))
    | _                -> cs
  in
  
  let rec simple_disjs (ds : lsym) : lsym =
    match ds with
    | SymOr (cs, rest) ->
       let cs' = simple_conjs cs in
       let rest' = simple_disjs rest in
       (match (cs', rest') with
       | (SymTrue, _)  -> SymTrue
       | (_, SymTrue)  -> SymTrue
       | (SymFalse, _) -> rest'
       | (_, SymFalse) -> cs' 
       | _             -> SymOr (cs', rest'))
    | _               -> simple_conjs ds
  in

  simple_disjs dnf

let collapse_dnf (dnf : lsym) : lsym list list =
  let rec collapse_conjs (cs : lsym) : lsym list =
    match cs with
    | SymAnd (l, rest) ->
       let rest' = collapse_conjs rest in
       l :: rest'
    | _                -> [cs]
  in
  
  let rec collapse_disjs (ds : lsym) : lsym list list =
    match ds with
    | SymOr (cs, rest) ->
       let cs' = collapse_conjs cs in
       let rest' = collapse_disjs rest in
       cs' :: rest'
    | _                -> [collapse_conjs ds]
  in

  collapse_disjs dnf
               
(* == Negation Normal Form == *)
let rec negation_free (l : lsym) : bool =
  match l with
  | SymAnd (l1, l2) -> (negation_free l1) && (negation_free l2)
  | SymOr  (l1, l2) -> (negation_free l1) && (negation_free l2)
  | SymNot _        -> false
  | _               -> true

let nnf_of_lsym (l : lsym) : lsym =
  let rec nnf_of_lsym' (l' : lsym) : lsym =
    match l' with
    | SymAnd (p, q)             -> SymAnd (nnf_of_lsym' p, nnf_of_lsym' q)
    | SymOr  (p, q)             -> SymOr  (nnf_of_lsym' p, nnf_of_lsym' q)
    | SymNot (SymTrue)          -> SymFalse
    | SymNot (SymFalse)         -> SymTrue
    | SymNot (SymLt  (a1, a2))  -> SymGeq (a1, a2) 
    | SymNot (SymLeq (a1, a2))  -> SymGt  (a1, a2)
    | SymNot (SymEq  (a1, a2))  -> SymOr  (SymLt (a1, a2), SymGt (a1, a2))
    | SymNot (SymGt  (a1, a2))  -> SymLeq (a1, a2)
    | SymNot (SymGeq (a1, a2))  -> SymLt  (a1, a2)
    | SymNot (SymAnd (p, q))    -> SymOr  (nnf_of_lsym' (SymNot p), nnf_of_lsym' (SymNot q))
    | SymNot (SymOr  (p, q))    -> SymAnd (nnf_of_lsym' (SymNot p), nnf_of_lsym' (SymNot q))
    | SymNot (SymNot p)         -> nnf_of_lsym' p
    | _                         -> l'
  in
  let ret = nnf_of_lsym' l in
  assert (negation_free ret);
  ret

(* == Equals Normal Form == *)
let rec equals_free (l : lsym) : bool =
  match l with
  | SymEq  (_, _)    -> false
  | SymAnd (l1, l2)  -> (equals_free l1) && (equals_free l2)
  | SymOr  (l1, l2)  -> (equals_free l1) && (equals_free l2)
  | SymNot l'        -> equals_free l'
  | _                -> true
   
let eqnf_of_lsym (l : lsym) : lsym =
  let rec eqnf_of_lsym' (nnf : lsym) : lsym =
    match nnf with
    | SymAnd (l1, l2) -> SymAnd (eqnf_of_lsym' l1, eqnf_of_lsym' l2)
    | SymOr  (l1, l2) -> SymOr  (eqnf_of_lsym' l1, eqnf_of_lsym' l2)
    | SymEq  (a1, a2) -> SymAnd (SymLeq (a1, a2), SymLeq (a2, a1))
    | _ -> nnf
  in
  let ret = eqnf_of_lsym' (nnf_of_lsym l) in
  assert (equals_free ret);
  ret
                
(* == Disjunctive Normal Form == *)
let rec is_dnf (l : lsym) : bool =
  let rec and_or_free (l' : lsym) : bool =
    match l' with
    | SymAnd (_, _) -> false
    | SymOr  (_, _) -> false
    | SymNot p -> and_or_free p
    | _ -> true
  in
    
  let rec is_conjs (cs : lsym) : bool =
    match cs with
    | SymAnd (p, SymTrue) -> and_or_free p
    | SymAnd (p, rest) -> (and_or_free p) && (is_conjs rest)
    | _ -> false
  in

  let rec is_disjs (ds : lsym) : bool =
    match ds with
    | SymOr (c, SymFalse) -> is_conjs c
    | SymOr (c, rest)     -> (is_conjs c) && (is_disjs rest)
    | _ -> false
  in

  is_disjs l

let dnf_of_lsym (l : lsym) : lsym =
  let rec dnfi_of_lsym (eqnf : lsym) : lsym list list =
    match eqnf with
    | SymAnd (p, q) ->
       let p' = dnfi_of_lsym p in
       let q' = dnfi_of_lsym q in
       List.fold_left (fun ret pi -> (List.fold_left (fun disji qi -> (pi @ qi) :: disji) [] q') @ ret) [] p'
    | SymOr (p, q) ->
       let p' = dnfi_of_lsym p in
       let q' = dnfi_of_lsym q in
       p' @ q'
    | _ -> [[eqnf]]
  in

  let dnf_of_dnfi (dnfi : lsym list list) : lsym =
    let conjs = List.map (fun clst -> List.fold_left (fun cs p -> SymAnd (p, cs)) SymTrue clst) dnfi in
    let disjs = List.fold_left (fun ds cs -> SymOr (cs, ds)) SymFalse conjs in
    disjs
  in

  let ret = dnf_of_dnfi (dnfi_of_lsym (eqnf_of_lsym l)) in
  assert (is_dnf ret);
  ret

(* == Linear Normal Form == *)
let rec is_linear_asym (a : asym) : bool =
   match a with
   | SymAdd (SymMul (SymInt _, SymAtom _), rest) -> is_linear_asym rest
   | SymMul (SymInt _, SymAtom _) -> true
   | SymInt _ -> true
   | _ -> false

let is_linear_lsym (l : lsym) : bool =
   match l with
   | SymLeq (a, SymInt _) -> is_linear_asym a
   | _ -> false
 
let lnf_of_lsym (l : lsym) : lsym =
  let linearize_asym (a : asym) : asym =
    let rec lineari_of_asym (a' : asym) : int VarIDMap.t * int =
      match a' with
      | SymInt n        -> (VarIDMap.empty, n)
      | SymAtom x       -> (VarIDMap.singleton x 1, 0)
      | SymAdd (a1, a2) ->
         let (aterms1, cterm1) = lineari_of_asym a1 in
         let (aterms2, cterm2) = lineari_of_asym a2 in
         (union (fun _ coeff1 coeff2 -> Some (coeff1 + coeff2)) aterms1 aterms2, cterm1 + cterm2)
      | SymSub (a1, a2) ->
         let (aterms1, cterm1) = lineari_of_asym a1 in
         let (aterms2, cterm2) = lineari_of_asym a2 in
         let do_sub k v1 v2 =
           match (v1, v2) with
           | (Some x, Some y) -> Some (x - y)
           | (Some x, None) -> Some x
           | (None, Some y) -> Some (0 - y)
           | _ -> None
         in
         (VarIDMap.merge do_sub aterms1 aterms2, cterm1 - cterm2)
      | SymMul (a1, a2) ->
         let (aterms1, cterm1) = lineari_of_asym a1 in
         let (aterms2, cterm2) = lineari_of_asym a2 in
         (match (VarIDMap.is_empty aterms1, VarIDMap.is_empty aterms2) with
          | (true , true)  -> (VarIDMap.empty, cterm1 * cterm2)
          | (true , false) -> (VarIDMap.map (fun n -> cterm1 * n) aterms2, cterm1 * cterm2)
          | (false, true)  -> (VarIDMap.map (fun n -> n * cterm2) aterms1, cterm1 * cterm2)
          | (false, false) -> raise (General_error ("Tried to linearize non-linear asym: " ^ (asym_to_string a))))
      | SymDiv (_, _)   ->
         raise (General_error ("Tried to linearize non-linear asym: " ^ (asym_to_string a)))
    in

    let linear_of_lineari (lineari : int VarIDMap.t * int) : asym =
      let (aterms, cterm) = lineari in
      VarIDMap.fold (fun atom coeff lin -> SymAdd (SymMul (SymInt coeff, SymAtom atom), lin)) aterms (SymInt cterm)
    in

    let ret = linear_of_lineari (lineari_of_asym a) in
    assert (is_linear_asym ret);
    ret
  in

  let linearize_lsym (l : lsym) : lsym =
    let lhs =
      (match l with
       | SymLt  (a1, a2) -> SymAdd (SymSub (a1, a2), SymInt 1)                          (* a1 <  a2 ~>   a1 - a2  + 1 <= 0 *)
       | SymLeq (a1, a2) -> SymSub (a1, a2)                                             (* a1 <= a2 ~>   a1 - a2      <= 0 *)
       | SymGt  (a1, a2) -> SymAdd (SymMul (SymInt (0 - 1), SymSub (a1, a2)), SymInt 1) (* a1 >  a2 ~> -(a1 - a2) + 1 <= 0 *)
       | SymGeq (a1, a2) -> SymMul (SymInt (0 - 1), SymSub (a1, a2))                    (* a1 >= a2 ~> -(a1 - a2)     <= 0 *)
       | _ -> raise (General_error ("Tried to linearize non-linear lsym: " ^ (lsym_to_string l))))
    in

    let lhs_linear = linearize_asym lhs in
    let ret =
      (match lhs_linear with
       | SymInt n -> SymLeq (SymInt n, SymInt 0)
       | _        -> let (aterms, SymInt n) = break_linear_aexp lhs_linear in SymLeq (aterms, SymInt (0 - n)))
    in

    assert (is_linear_lsym ret);
    ret
  in

  let elim_const_leq (l : lsym) : lsym =
    match l with
    | SymLeq (SymInt n, SymInt m) -> if n <= m then SymTrue else SymFalse
    | _                           -> l
  in

  let dnf = simple_dnf (dnf_of_lsym l) in
  let linear_dnf = map_dnf linearize_lsym dnf in
  let minimal_linear_dnf = simple_dnf (map_dnf elim_const_leq linear_dnf) in
  minimal_linear_dnf


let linear_system_of_lsym (l : lsym) : (int VarIDMap.t * int) list list =
  let lnf = lnf_of_lsym l in
  let lnf_lists = collapse_dnf lnf in
  let ir_of_linear_lsym (l : lsym) : int VarIDMap.t * int =
    let rec ir_of_linear_asym (a : asym) : int VarIDMap.t =
      match a with
      | SymAdd (SymMul (SymInt n, SymAtom x), rest) -> VarIDMap.add x n (ir_of_linear_asym rest)
      | SymMul (SymInt n, SymAtom x)                -> VarIDMap.singleton x n
      | _                                           -> raise (General_error ("Tried to get IR for constant or non-linear asym: " ^ (asym_to_string a)))
    in
    
    match l with
    | SymLeq (aterms, SymInt n) -> (ir_of_linear_asym aterms, n)
    | _                         -> raise (General_error ("Tried to get IR for non-linear lsym: " ^ (lsym_to_string l)))
  in
    
  let ret = List.map (fun c -> List.map ir_of_linear_lsym c) lnf_lists in
  (* List.iter (fun cs -> List.iter (fun (a, c) -> VarIDMap.iter (fun (_, name) coeff -> print_string ((string_of_int coeff) ^ name ^ " ")) a; print_endline ("<= " ^ (string_of_int c))) cs; print_newline ()) ret; *)
  ret



    
(*












   let sdnf_tmp    = List.map (fun conjs -> List.map simplify_lsym conjs) (dnf_of_lsym l) in
  let sdnf_conjs  = List.map (fun conjs -> List.filter (fun c -> c <> SymTrue) conjs) sdnf_tmp in (* q /\ T = q *)
  let sdnf_conjs' = List.map (fun conjs -> if List.exists (fun c -> c = SymFalse) then [SymFalse] else conjs) sdnf_conjs in (* q /\ F = F *)

  let sdnf_disjs  = List.filter (fun conjs -> conjs <> [SymFalse]) sdnf_conjs' in (* p \/ F = p *)
  let sdnf_disjs' = if List.exists (fun conjs -> conjs = [SymTrue]) sdnf_disjs then [[SymTrue]] else sdnf_disjs in (* p \/ T = T *)

  sdnf_disjs'

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
