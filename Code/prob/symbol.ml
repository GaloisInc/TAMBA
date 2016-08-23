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


let rec to_string (s : t) : string =
  match s with
  | SymTrue -> "T"
  | SymAtom (name) -> let (_, x) = name in x
  | SymInt (n) -> string_of_int n
  | SymAdd (c1, c2) -> "(" ^ to_string c1 ^ ") + (" ^ to_string c2 ^ ")"
  | SymSub (c1, c2) -> "(" ^ to_string c1 ^ ") - (" ^ to_string c2 ^ ")"
  | SymMul (c1, c2) -> "(" ^ to_string c1 ^ ") * (" ^ to_string c2 ^ ")"
  | SymDiv (c1, c2) -> "(" ^ to_string c1 ^ ") / (" ^ to_string c2 ^ ")"
  | SymEq (c1, c2) -> "(" ^ to_string c1 ^ ") = (" ^ to_string c2 ^ ")"
  | SymLt (c1, c2) -> "(" ^ to_string c1 ^ ") < (" ^ to_string c2 ^ ")"
  | SymAnd (c1, c2) -> "(" ^ to_string c1 ^ ") ^ (" ^ to_string c2 ^ ")"
  | SymOr (c1, c2) -> "(" ^ to_string c1 ^ ") v (" ^ to_string c2 ^ ")"
  | SymNot c -> "~(" ^ to_string c ^ ")"

(* TODO: this is a dummy function *)
let poly_of_symbol (s : t) : polyhedron = ppl_new_C_Polyhedron_from_space_dimension 2 Universe
