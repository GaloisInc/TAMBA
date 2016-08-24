open Ppl_ocaml

type t =
  | SymTrue
  | SymAtom of Lang.varid
  | SymInt of int
  | SymAdd of t * t
  | SymSub of t * t
  | SymMul of t * t
  | SymDiv of t * t
  | SymEq of t * t
  | SymLt of t * t
  | SymAnd of t * t
  | SymOr of t * t
  | SymNot of t

let to_string (s : t) : string =
  let parens str parent curr =
    if parent = 0 then
      str
    else if parent < curr then
      "(" ^ str ^ ")"
    else
      str
  in
  let rec to_string' exp prec =
    match exp with
    | SymTrue -> "T"
    | SymAtom (name) -> let (_, x) = name in x
    | SymInt (n) -> string_of_int n
    | SymAdd (c1, c2) -> parens (to_string' c1 4 ^ " + " ^ to_string' c2 4) prec 4
    | SymSub (c1, c2) -> parens (to_string' c1 4 ^ " - " ^ to_string' c2 4) prec 4
    | SymMul (c1, c2) -> parens (to_string' c1 3 ^ " * " ^ to_string' c2 3) prec 3
    | SymDiv (c1, c2) -> parens (to_string' c1 3 ^ " / " ^ to_string' c2 3) prec 3
    | SymEq (c1, c2) -> parens (to_string' c1 7 ^ " == " ^ to_string' c2 7) prec 7
    | SymLt (c1, c2) -> parens (to_string' c1 6 ^ " < " ^ to_string' c2 6) prec 6
    | SymAnd (c1, c2) -> parens (to_string' c1 11 ^ " ^ " ^ to_string' c2 11) prec 11
    | SymOr (c1, c2) -> parens (to_string' c1 12 ^ " v " ^ to_string' c2 12) prec 12
    | SymNot (c) -> "~" ^ parens (to_string' c 2) prec 2
  in
  to_string' s 0

(* TODO: this is a dummy function *)
let poly_of_symbol (s : t) : polyhedron = ppl_new_C_Polyhedron_from_space_dimension 2 Universe
