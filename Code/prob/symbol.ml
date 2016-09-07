open Ppl_ocaml
open Gmp

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
    | SymInt  (n)       -> string_of_int n
    | SymAtom (name)   -> let (_, x) = name in x
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
    | SymNot (l')     -> "¬" ^ add_parens (lsym_to_string_h l' 2) prec 2
  in
  lsym_to_string_h l 0

let lsym_fvs (l : lsym) : (string * int) list =
  let rec asym_fvs_h (a : asym) : string list =
    match a with
    | SymAtom (name) -> let (_, x) = name in [x]
    | SymAdd (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymSub (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymMul (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymDiv (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | _ -> []
  in
  let rec lsym_fvs_h (l : lsym) : string list =
    match l with
    | SymLt (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymLeq (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymEq (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymGt (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymGeq (a1, a2) -> (asym_fvs_h a1) @ (asym_fvs_h a2)
    | SymAnd (l1, l2) -> (lsym_fvs_h l1) @ (lsym_fvs_h l2)
    | SymOr (l1, l2) -> (lsym_fvs_h l1) @ (lsym_fvs_h l2)
    | SymNot (l) -> lsym_fvs_h l
    | _ -> []
  in
  List.mapi (fun idx curr -> (curr, idx)) (List.sort_uniq String.compare (lsym_fvs_h l))
                   
let rec nnf_of_lsym (l : lsym) : lsym =
  match l with
  | SymAnd (p, q) -> SymAnd (nnf_of_lsym p, nnf_of_lsym q)
  | SymOr  (p, q) -> SymOr  (nnf_of_lsym p, nnf_of_lsym q)
  | SymNot (SymTrue) -> SymFalse
  | SymNot (SymFalse) -> SymTrue
  | SymNot (SymLt (a1, a2)) -> SymGeq (a1, a2) 
  | SymNot (SymLeq (a1, a2)) -> SymGt (a1, a2)
  | SymNot (SymEq (a1, a2)) -> SymOr (SymLt (a1, a2), SymGt (a1, a2))
  | SymNot (SymGt (a1, a2)) -> SymLeq (a1, a2)
  | SymNot (SymGeq (a1, a2)) -> SymLt (a1, a2)
  | SymNot (SymAnd (p, q)) -> SymOr (nnf_of_lsym (SymNot p), nnf_of_lsym (SymNot q))
  | SymNot (SymOr (p, q)) -> SymAnd (nnf_of_lsym (SymNot p), nnf_of_lsym (SymNot q))
  | SymNot (SymNot p) -> nnf_of_lsym p
  | _ -> l

let rec dnfi_of_nnf (nnf : lsym) : lsym list list =
  match nnf with
  | SymAnd (p, q) ->
     let p_new = dnfi_of_nnf p in
     let q_new = dnfi_of_nnf q in
     List.fold_left (fun outer pi -> (List.fold_left (fun inner qi -> (pi @ qi) :: inner) [] q_new) @ outer) [] p_new
  | SymOr (p, q) ->
     let p_new = dnfi_of_nnf p in
     let q_new = dnfi_of_nnf q in
     p_new @ q_new
  | _ -> [[nnf]]

let dnf_of_dnfi (dnfi : lsym list list) : lsym =
  List.fold_left (fun disj c -> SymOr (List.fold_left (fun conj a -> SymAnd (a, conj)) SymTrue c, disj)) SymFalse dnfi

let rec simplify_asym (a : asym) : asym =
  match a with
  | SymAdd (a1, a2) ->
     let a1' = simplify_asym a1 in
     let a2' = simplify_asym a2 in
     (match (a1', a2') with
      | (SymInt n1, SymInt n2) -> SymInt (n1 + n2)
      | _ -> SymAdd (a1', a2'))
  | SymSub (a1, a2) ->
     let a1' = simplify_asym a1 in
     let a2' = simplify_asym a2 in
     (match (a1', a2') with
      | (SymInt n1, SymInt n2) -> SymInt (n1 - n2)
      | _ -> SymSub (a1', a2'))
  | SymMul (a1, a2) ->
     let a1' = simplify_asym a1 in
     let a2' = simplify_asym a2 in
     (match (a1', a2') with
      | (SymInt n1, SymInt n2) -> SymInt (n1 * n2)
      | _ -> SymMul (a1', a2'))
  | SymDiv (a1, a2) ->
     let a1' = simplify_asym a1 in
     let a2' = simplify_asym a2 in
     (match (a1', a2') with
      | (SymInt n1, SymInt n2) -> SymInt (n1 / n2)
      | _ -> SymDiv (a1', a2'))
  | _ -> a

let simplify_lsym (l : lsym) : lsym =
  let rec simplify_lsym_h (ast : lsym) : lsym =
    match ast with
    | SymLt (a1, a2) ->
       let a1' = simplify_asym a1 in
       let a2' = simplify_asym a2 in
       (match (a1', a2') with
        | (SymInt n1, SymInt n2) -> if n1 < n2 then SymTrue else SymFalse
        | _ -> SymLt (a1', a2'))
    | SymLeq (a1, a2) ->
       let a1' = simplify_asym a1 in
       let a2' = simplify_asym a2 in
       (match (a1', a2') with
        | (SymInt n1, SymInt n2) -> if n1 <= n2 then SymTrue else SymFalse
        | _ -> SymLeq (a1', a2'))
    | SymEq (a1, a2) ->
       let a1' = simplify_asym a1 in
       let a2' = simplify_asym a2 in
       (match (a1', a2') with
        | (SymInt n1, SymInt n2) -> if n1 = n2 then SymTrue else SymFalse
        | _ -> SymEq (a1', a2'))
    | SymGt (a1, a2) ->
       let a1' = simplify_asym a1 in
       let a2' = simplify_asym a2 in
       (match (a1', a2') with
        | (SymInt n1, SymInt n2) -> if n1 > n2 then SymTrue else SymFalse
        | _ -> SymGt (a1', a2'))
    | SymGeq (a1, a2) ->
       let a1' = simplify_asym a1 in
       let a2' = simplify_asym a2 in
       (match (a1', a2') with
        | (SymInt n1, SymInt n2) -> if n1 >= n2 then SymTrue else SymFalse
        | _ -> SymGeq (a1', a2'))
    | SymAnd (l1, l2) ->
       let l1' = simplify_lsym_h l1 in
       let l2' = simplify_lsym_h l2 in
       (match (l1', l2') with
        | (SymTrue, _) -> l2'
        | (_, SymTrue) -> l1'
        | (SymFalse, _) -> SymFalse
        | (_, SymFalse) -> SymFalse
        | _ -> SymAnd (l1', l2'))
    | SymOr (l1, l2) ->
       let l1' = simplify_lsym_h l1 in
       let l2' = simplify_lsym_h l2 in
       (match (l1', l2') with
        | (SymTrue, _) -> SymTrue
        | (_, SymTrue) -> SymTrue
        | (SymFalse, _) -> l2'
        | (_, SymFalse) -> l1'
        | _ -> SymOr (l1', l2'))
    | _ -> ast
  in
  simplify_lsym_h l

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

