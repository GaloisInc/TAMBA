open Printf
open Util
open Lang
open Gmp
open Value_status

type context = bool

(* static_check : stmt -> abs_env -> context -> (int * abs_env) *)
let rec static_check cstmt env cntx : abs_env = 
  match cstmt with
    | SSkip -> env
    | SDefine (name, datatype) ->
        AbsMap.add name Static env

    | SUniform (name, _, _) ->
        AbsMap.add name Dynamic env

    | SSeq (s1,s2) ->
        let map1 = static_check s1 env cntx in
          static_check s2 map1 cntx

    | SAssign (name, varaexp) ->
        let has_dyn = any_in_map_dynamic (aexp_vars varaexp) env in
        let dyn     = if has_dyn || cntx then Dynamic else Static in
          AbsMap.add name dyn env

    | SPSeq (s1, s2, _, _, _) ->
        let map1 = static_check s1 env cntx in
        let map2 = static_check s2 env cntx in
          AbsMap.merge merge_two map1 map2

    (* Gather all of the variables used in the guard. If any of them are
       dynamic then you must factor that in when processing the branches *)
    | SIf (guard, stmt1, stmt2) ->

        (* Check if any of the guard variables are dynamic *)
        let is_dyn = any_in_map_dynamic (lexp_vars guard) env in
  
        let map1 = static_check stmt1 env (is_dyn || cntx) in
        let map2 = static_check stmt2 env (is_dyn || cntx) in
          AbsMap.merge merge_two map1 map2

    | s -> failwith "Output and While loops not supported in analysis"
