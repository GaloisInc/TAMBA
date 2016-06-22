open Util
open Lang
open List
open Printf

type valueStatus =
  | Static
  | Dynamic

let value_status_to_string vs =
  match vs with
    | Static  -> "Static"
    | Dynamic -> "Dynamic"

module Varid =
  struct
    type t = varid
    let compare s1 s2 =
      let (_, s1_name) = s1 in
      let (_, s2_name) = s2 in
      String.compare s1_name s2_name
    let equal s1 s2 =
      let (_, s1_name) = s1 in
      let (_, s2_name) = s2 in
      s1_name = s2_name
  end

module AbsMap = Map.Make(Varid)

type abs_env = valueStatus AbsMap.t

let print_abs_env env =
  let elems = AbsMap.bindings env in
  let print_binding (vid, vs) =
    printf "(%s, %s); " (varid_to_string vid) (value_status_to_string vs) in
  List.iter print_binding elems

let rec map_from_list (xs : (varid * valueStatus) list) =
  match xs with
    | []               -> AbsMap.empty
    | ((v_id, st)::ys) -> AbsMap.add v_id st (map_from_list ys)

let merge_two key a b =
  match (a,b) with
    | (None,    None)    -> None
    | (Some v,  None)    -> Some v
    | (None,    Some v)  -> Some v
    | (Some v1, Some v2) -> match (v1, v2) with
                              | (Static, Static) -> Some Static
                              | _                -> Some Dynamic

(* any_in_map : varid list -> abs_env -> bool *)
let rec any_in_map_dynamic vs m = if AbsMap.is_empty m then false else
  match vs with
    | []      -> false
    | (v::ys) -> (try
                   let status = AbsMap.find v m in
                   status = Dynamic
                 with
                   _ -> true) || any_in_map_dynamic ys m
