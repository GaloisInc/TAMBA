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

let rec eliminate_negations (l : lsym) : lsym =
  match l with
  | SymAnd (p, q) -> SymAnd (eliminate_negations p, eliminate_negations q)
  | SymOr  (p, q) -> SymOr  (eliminate_negations p, eliminate_negations q)
  | SymNot (SymTrue) -> SymFalse
  | SymNot (SymFalse) -> SymTrue
  | SymNot (SymLt (a1, a2)) -> SymGeq (a1, a2) 
  | SymNot (SymLeq (a1, a2)) -> SymGt (a1, a2)
  | SymNot (SymEq (a1, a2)) -> SymOr (SymLt (a1, a2), SymGt (a1, a2))
  | SymNot (SymGt (a1, a2)) -> SymLeq (a1, a2)
  | SymNot (SymGeq (a1, a2)) -> SymLt (a1, a2)
  | SymNot (SymAnd (p, q)) -> SymOr (eliminate_negations (SymNot p), eliminate_negations (SymNot q))
  | SymNot (SymOr (p, q)) -> SymAnd (eliminate_negations (SymNot p), eliminate_negations (SymNot q))
  | SymNot (SymNot p) -> eliminate_negations p
  | _ -> l

(* looks like disjunctive normal form with no negations.
    - # of disjunctions = number of polyhedra
    - # of conjunctions per disjunctions = # of constraints on that polyhedron *)
let linearize (l : lsym) : lsym = l (* TODO *)

(* TODO: this is a dummy function *)
let poly_of_lsym (l : lsym) : polyhedron =
  let ret = ppl_new_C_Polyhedron_from_space_dimension 2 Universe in
  ppl_Polyhedron_add_constraint ret (Greater_Or_Equal (Variable 0, Coefficient Z.zero));
  ppl_Polyhedron_add_constraint ret (Less_Or_Equal (Variable 0, Coefficient (Z.from_int 9)));
  ppl_Polyhedron_add_constraint ret (Greater_Or_Equal (Variable 1, Coefficient Z.zero));
  ppl_Polyhedron_add_constraint ret (Less_Or_Equal (Variable 1, Coefficient (Z.from_int 9)));
  ret

