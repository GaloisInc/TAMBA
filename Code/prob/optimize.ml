open Lang
open Util
open Printf
open Gmp
open Value_status
open State
open Evalstate

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

let rec sub_aexp raexp env stck =
  match raexp with
    | AEVar vid            -> (match safe_assoc vid env with
                                | Some (Some v) -> v
                                | None   -> (match safe_assoc vid stck with
                                                | Some v -> v
                                                | None   -> AEVar vid))
    | AEInt i              -> AEInt i
    | AEBinop (op, a1, a2) -> AEBinop (op, sub_aexp a1 env stck, sub_aexp a2 env stck)
    | AERecord rc          -> failwith "Records not yet implemented in analysis"

let rec sub_lexp rlexp env stck : Lang.lexp =
  match rlexp with
    | LEBool b             -> LEBool b
    | LEBinop (op, l1, l2) -> LEBinop (op, sub_lexp l1 env stck, sub_lexp l2 env stck)
    | LEReln (rel, a1, a2) -> LEReln (rel, sub_aexp a1 env stck, sub_aexp a2 env stck)

let rec rewrite_stmt cstmt env assign_stack : ((varid * aexp option) list * (varid * aexp) list * stmt) =
  match cstmt with
    | SSkip -> (env, assign_stack, SSkip)
    | SDefine (n, v)       -> (env, assign_stack, SDefine (n, v))
    | SUniform (n, v1, v2) -> (env, assign_stack, SUniform (n, v1, v2))
    | SSeq (s1, s2) ->
        let (env1, stk1, s11) = rewrite_stmt s1 env assign_stack in
        let (env2, stk2, s22) = rewrite_stmt s2 env1 stk1 in
        (env2, stk2, SSeq (s11, s22))

    (* For assignment we check to see if the lhs is known to be static.
       If it is, then we either know its value and can replace the statement
                                               with a skip (unlikely)
                                or we don't know its value which means the
                                                substituted rhs is its value.
      
       If it is not, we are still going to try then we rewrite the rhs by using
          the assignment stack.
     *)
    | SAssign (name, varaexp) ->
        let lkup = safe_assoc name env in
        let rhs1 = sub_aexp varaexp env assign_stack in
        (match lkup with
           | Some v -> (match v with
                          | Some y -> (env, assign_stack, SSkip)
                          | None   -> (((name, Some rhs1)::env), assign_stack, SSkip))
           | None   -> (env, (name, rhs1)::assign_stack, SSkip))


    | SIf (p, st, sf) ->
        let p1 : Lang.lexp = sub_lexp p env assign_stack in
        let vs = lexp_vars p1 in
        (match vs with
           | (x::xs) -> failwith "Not possible to collapse If-statement"
           | []      -> let res = Evalstate.eval_lexp p1 (new State.state_empty) in
                          (match res with
                            | 0 -> rewrite_stmt sf env assign_stack
                            | 1 -> rewrite_stmt st env assign_stack
                            | i -> failwith "Collapsing If-statement failed in code transformation"))


let rec flip_seq cstmt =
  match cstmt with
     | SSeq (s1, s2)           -> SSeq (flip_seq s2, flip_seq s1)
     | SPSeq (s1, s2, a, b, c) -> SPSeq (flip_seq s2, flip_seq s1, a, b, c)
     | SIf      (l1, s1, s2)   -> SIf (l1, flip_seq s1, flip_seq s2)
     | SWhile   (l1, s1)       -> SWhile (l1, flip_seq s1)
     | s                       -> s

let rec ann_use_def cstmt =
  match cstmt with
   | SSkip -> SLivenessAnnot (([],[],[],[]), SSkip)
   | SSeq  (s1, s2) -> SSeq (ann_use_def s1, ann_use_def s2)
   | SPSeq (s1, s2, q, i1, i2) -> SPSeq (ann_use_def s1, ann_use_def s2, q, i1, i2)
   | SAssign  (name, aex) -> let used = aexp_vars aex in
                             let defd = [name] in
                             SLivenessAnnot ((used, defd, [],[]), SAssign (name, aex))
   | SDefine  (name, d_type) -> SLivenessAnnot (([], [name], [], []), SDefine (name, d_type))
   | SIf      (lex, s1, s2)  -> let used = lexp_vars lex in
                                let (s12, s22) = (ann_use_def s1, ann_use_def s2) in
                                SLivenessAnnot ((used, [], [], []), SIf (lex, s12, s22))
   | SLivenessAnnot (i, s)   -> print_stmt_type s; failwith "\nAnnotation node found earlier than expected"
   | s -> print_stmt_type s; failwith " is not yet supported in liveness analysis\n"

let rec succ_ins_stmt cstmt =
  match cstmt with
   | SSkip -> []
   | SSeq  (s1, s2) -> List.append (succ_ins_stmt s1) (succ_ins_stmt s2)
   | SPSeq (s1, s2, q, i1, i2) -> List.append (succ_ins_stmt s1) (succ_ins_stmt s2)
   | SAssign  (name, aex) -> []
   | SDefine  (name, d_type) -> []
   | SIf      (lex, s1, s2)  -> List.append (succ_ins_stmt s1) (succ_ins_stmt s2)
   | SLivenessAnnot ((u, d, o, i), s)   -> List.append i (succ_ins_stmt s)
   | s -> print_stmt_type s; failwith " is not yet supported in liveness analysis\n"

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let rec liveness_analysis cstmt vids =
  match cstmt with
    | SSkip -> SSkip
    | SAssign (name, rhs) -> SAssign (name, rhs)
    | SDefine  (name, d_type) -> SDefine (name, d_type)
    | SLivenessAnnot ((u, d, o, i), stmt) ->
        let stmt2 = liveness_analysis stmt vids in
        let out1 = succ_ins_stmt stmt2 in
        let in1 = List.append u (diff out1 d) in
        SLivenessAnnot ((u, d, out1, in1), stmt2)
    | SSeq (s1, s2) -> SSeq (liveness_analysis s1 vids, liveness_analysis s2 vids)
    | SIf (lex, s1, s2) -> SIf (lex, liveness_analysis s1 vids, liveness_analysis s2 vids)
    | s -> print_stmt_type s; failwith " is not yet supported in liveness analysis\n"

let rec get_succ_ins cstmt =
    match cstmt with
      | SLivenessAnnot ((u, d, o, i), s1) -> i
      | SSeq (s1, s2) -> get_succ_ins s1
      
let rec liveness_analysis_rev cstmt vids =
  match cstmt with
    | SSkip -> SSkip
    | SAssign (name, rhs) -> SAssign (name, rhs)
    | SDefine  (name, d_type) -> SDefine (name, d_type)
    | SLivenessAnnot ((u, d, o, i), stmt) ->
        let in1 = list_unique (List.append u (diff vids d)) in
        let stmt2 = liveness_analysis_rev stmt vids in
        SLivenessAnnot ((u, d, vids, in1), stmt2)
    | SSeq (s1, s2) -> let s1p = liveness_analysis_rev s1 vids in
                       let outs = get_succ_ins s1p in
                       SSeq (s1p, liveness_analysis_rev s2 outs)
    | SIf (lex, s1, s2) -> SIf (lex, liveness_analysis_rev s1 vids, liveness_analysis_rev s2 vids)
    | s -> print_stmt_type s; failwith " is not yet supported in liveness analysis\n"
