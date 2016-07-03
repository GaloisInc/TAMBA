open Lang
open Util
open Printf
open Gmp
open Value_status
open State
open Evalstate

type context = bool

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let rec sub_member (vid, aexp) ps =
  match ps with
    | [] -> false
    | ((vid1, aexp1)::pss) -> (vid = vid1 && equal_aexp aexp aexp1) || sub_member (vid, aexp) pss

let intersect l1 l2 = List.filter (fun x -> (sub_member x l2)) l1

let extend (vid, aexp) a_stack =
  printf "\n!!!!! Extending a_stack with: %s\n" (varid_to_string vid);
  (vid,aexp) :: List.filter (fun (vid1, _) -> not (vid = vid1)) a_stack

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

let rec sub_aexp raexp stck =
  match raexp with
    | AEVar vid            -> (match safe_assoc vid stck with
                                  | Some v -> v
                                  | None   -> AEVar vid)
    | AEInt i              -> AEInt i
    | AEBinop (op, a1, a2) -> AEBinop (op, sub_aexp a1 stck, sub_aexp a2 stck)
    | AERecord rc          -> failwith "Records should be desugared before analysis!"

let rec sub_lexp rlexp stck : Lang.lexp =
  match rlexp with
    | LEBool b             -> LEBool b
    | LEBinop (op, l1, l2) -> LEBinop (op, sub_lexp l1 stck, sub_lexp l2 stck)
    | LEReln (rel, a1, a2) -> LEReln (rel, sub_aexp a1 stck, sub_aexp a2 stck)

(* take a variable and an assignment stack and create the explicit assignment
   odd order of arguments is to facilitate mapping
 *)
let write_assign ass_stack varid =
  match safe_assoc varid ass_stack with
    | None -> failwith (String.concat " " ["Trying to write out an assignment"
                                          ;"that doesn't exist in assignment"
                                          ;"stack: "
                                          ;(varid_to_string varid)
                                          ])
    | Some aexp -> SAssign (varid, aexp)

let rec rewrite_stmt cstmt s_vars assign_stack : ((varid * aexp) list * stmt) =
  match cstmt with
    | SSeq (s1, s2) ->
        let (stk1, s11) = rewrite_stmt s1 s_vars assign_stack in
        let (stk2, s22) = rewrite_stmt s2 s_vars stk1 in
        (stk2, SSeq (s11, s22))
    | SSkip -> (assign_stack, SSkip)
    | SLivenessAnnot ((u,d,o,i), SSkip) -> (assign_stack, SSkip)
    | SLivenessAnnot (info,SDefine (v,t))    -> (assign_stack, SDefine (v,t))
    | SLivenessAnnot (info,SUniform (v,i,j)) -> (assign_stack, SUniform (v,i,j))

    (* For assignment we check to see if the lhs is known to be static.
       If it is, then we either know its value and can replace the statement
                                               with a skip (unlikely)
                                or we don't know its value which means the
                                                substituted rhs is its value.
      
       If it is not, we are still going to try then we rewrite the rhs by using
          the assignment stack.
     *)
    | SLivenessAnnot (info, SAssign (name, varaexp)) ->
        let rhs1 = sub_aexp varaexp assign_stack in
        printf "\nAssignment firing\n";
        (extend (name, rhs1) assign_stack, SSkip)

    | SLivenessAnnot ((u,d,o,i),SPSeq (s1,s2,q,i1,i2)) ->
        let (a_stack1, new_assigns, s12, s22) = manage_branch o s_vars assign_stack s1 s2 in
        let stmt2 = SPSeq (s12, s22, q, i1, i2) in
        (a_stack1, List.fold_right (fun s1 s2 -> SSeq (s1,s2)) new_assigns stmt2)
    | SLivenessAnnot ((u,d,o,i),SIf (p, st, sf)) ->
        printf "\PIF firing\n";
        let p1 = sub_lexp p assign_stack in
        let vs = lexp_vars p1 in
        (match vs with
           | (_::_) ->
             let (a_stack1, new_assigns, st2, sf2) = manage_branch o s_vars assign_stack st sf in
             let stmt2 = SIf (p, st2, sf2) in
             (a_stack1, List.fold_right (fun s1 s2 -> SSeq (s1, s2)) new_assigns stmt2)
           | []      -> let res = Evalstate.eval_lexp p1 (new State.state_empty) in
                          (match res with
                            | 0 -> rewrite_stmt sf s_vars assign_stack
                            | 1 -> rewrite_stmt st s_vars assign_stack
                            | _ -> failwith "the impossible happened, predicate returned non-bool"))
    | p -> print_stmt p; failwith "Need to implement"

and manage_branch out_live s_vars a_stack s1 s2 =
  let needed = diff out_live s_vars in
  let new_assigns = List.map (write_assign a_stack) needed in
  let (a_stack1,s12) = rewrite_stmt s1 s_vars a_stack in
  let (a_stack2,s22) = rewrite_stmt s2 s_vars a_stack in
  (intersect a_stack1 a_stack2, new_assigns, s12, s22)



let rec flip_seq cstmt =
  match cstmt with
     | SSeq (s1, s2)           -> SSeq (flip_seq s2, flip_seq s1)
     | SPSeq (s1, s2, a, b, c) -> SPSeq (flip_seq s2, flip_seq s1, a, b, c)
     | SIf      (l1, s1, s2)   -> SIf (l1, flip_seq s1, flip_seq s2)
     | SWhile   (l1, s1)       -> SWhile (l1, flip_seq s1)
     | SLivenessAnnot (i, s1)  -> SLivenessAnnot (i, flip_seq s1)
     | s                       -> s

let rec ann_use_def cstmt =
  match cstmt with
   | SSkip -> SLivenessAnnot (empty_info, SSkip)
   | SSeq  (s1, s2) -> SSeq (ann_use_def s1, ann_use_def s2)
   | SPSeq (s1, s2, q, i1, i2) -> let s12 = ann_use_def s1 in
                                  let s22 = ann_use_def s2 in
                                  SLivenessAnnot (empty_info, SPSeq (s12, s22, q, i1, i2))
   | SAssign  (name, aex) -> let used = aexp_vars aex in
                             let defd = [name] in
                             SLivenessAnnot ((used, defd, [],[]), SAssign (name, aex))
   | SUniform (v, i1, i2) -> let defd = [v] in
                             SLivenessAnnot (([], defd, [],[]), SUniform (v, i1, i2))
   | SDefine  (name, d_type) -> SLivenessAnnot (([], [name], [], []), SDefine (name, d_type))
   | SIf      (lex, s1, s2)  -> let used = lexp_vars lex in
                                let (s12, s22) = (ann_use_def s1, ann_use_def s2) in
                                SLivenessAnnot ((used, [], [], []), SIf (lex, s12, s22))
   | SLivenessAnnot (i, s)   -> print_stmt_type s; failwith "\nAnnotation node found earlier than expected"
   | s -> print_stmt_type s; failwith " is not yet supported in liveness analysis\n"

(* TODO: Delete if possible
let rec succ_ins_stmt cstmt =
  match cstmt with
   | SSkip -> []
   | SSeq  (s1, s2) -> List.append (succ_ins_stmt s1) (succ_ins_stmt s2)
   | SPSeq (s1, s2, q, i1, i2) -> List.append (succ_ins_stmt s1) (succ_ins_stmt s2)
   | SAssign  (name, aex) -> []
   | SDefine  (name, d_type) -> []
   | SIf      (lex, s1, s2)  -> List.append (succ_ins_stmt s1) (succ_ins_stmt s2)
   | SPSeq (s1, s2, q, i1, i2) -> List.append (succ_ins_stmt s1) (succ_ins_stmt s2)
   | SLivenessAnnot ((u, d, o, i), s)   -> List.append i (succ_ins_stmt s)
   | s -> print_stmt_type s; failwith " is not yet supported in liveness analysis\n"
*)


(* TODO: Delete if possible
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
*)

let rec clear_liveness cstmt =
  match cstmt with
    | SLivenessAnnot ((u,d,_,_), s1) ->
        SLivenessAnnot ((u,d,[],[]), clear_liveness s1)
    | SSeq (s1, s2)                  ->
        SSeq (clear_liveness s1, clear_liveness s2)
    | SIf (lex, s1, s2)              ->
        SIf (lex, clear_liveness s1, clear_liveness s2)
    | SPSeq (s1, s2, q, i1, i2)      ->
        SPSeq (clear_liveness s1, clear_liveness s2, q, i1, i2)
    | s -> s

let rec get_succ_ins cstmt =
  match cstmt with
      | SLivenessAnnot ((u, d, o, i), s1) -> i
      | SSeq (s1, s2) -> get_succ_ins s1

let rec get_succ_ins_if cstmt =
  match cstmt with
      | SLivenessAnnot ((u, d, o, i), s1) -> i
      | SSeq (s1, s2) -> get_succ_ins_if s2
      
let rec liveness_analysis_rev cstmt vids =
  match cstmt with
    | SSkip -> SSkip
    | SAssign (name, rhs) -> SAssign (name, rhs)
    | SUniform (v, i1, i2) -> SUniform (v, i1, i2)
    | SDefine  (name, d_type) -> SDefine (name, d_type)
    | SLivenessAnnot ((u, d, o, i), SIf (lex, s1,s2)) ->
        let s12 = liveness_analysis_rev s1 vids in
        let s22 = liveness_analysis_rev s2 vids in
        let succ_ins = list_unique (List.append (get_succ_ins_if s12) (get_succ_ins_if s22)) in
        let in1 = list_unique (List.append u (diff succ_ins d)) in
        SLivenessAnnot ((u,d,succ_ins,in1),SIf (lex, s12, s22))
    | SLivenessAnnot ((u, d, o, i), stmt) ->
        let in1 = list_unique (List.append u (diff vids d)) in
        let stmt2 = liveness_analysis_rev stmt vids in
        SLivenessAnnot ((u, d, vids, in1), stmt2)
    | SSeq (s1, s2) -> let s1p = liveness_analysis_rev s1 vids in
                       let succ_ins = get_succ_ins s1p in
                       SSeq (s1p, liveness_analysis_rev s2 succ_ins)
    | SPSeq (s1, s2, q, i1, i2) -> let s12 = liveness_analysis_rev s1 vids in
                                   let s22 = liveness_analysis_rev s2 vids in
                                    SPSeq (s12, s22, q, i1, i2)
    | SIf (lex, s1, s2) -> failwith "Every SIf should have an annotation node"
    | s -> print_stmt_type s; failwith " is not yet supported in liveness analysis\n"
