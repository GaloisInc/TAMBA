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

let rec stack_rem vids stck =
  match stck with
    | [] -> []
    | ((vid1, aexp1)::mems) -> if List.mem vid1 vids
                               then stack_rem vids mems
                               else (vid1, aexp1) :: stack_rem vids mems

let intersection l1 l2 = diff l1 (diff l1 l2)

let intersect l1 l2 = List.filter (fun x -> (sub_member x l2)) l1

let extend (vid, aexp) a_stack =
  ifdebug (printf "\n!!!!! Extending a_stack with: %s\n" (varid_to_string vid));
  (vid,aexp) :: List.filter (fun (vid1, _) -> not (vid = vid1)) a_stack

(* static_check : stmt -> abs_env -> context -> (int * abs_env)
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
*)

(* determine if a set of variables are only used, as opposed to defined,
 * in a sequence of statements.
 *
 * the returned list contains only the elements that are only used in the
 * sequence
 *)
let rec only_use vids cstmt =
  match cstmt with
    | SSeq (s1, s2) -> List.append (only_use vids s1) (only_use vids s2)
    | SLivenessAnnot (info, SIf(p, s1, s2)) ->
        intersection (only_use vids s1) (only_use vids s2)
    | SLivenessAnnot (info, SPSeq (s1, s2, _, _, _)) ->
        intersection (only_use vids s1) (only_use vids s2)
    | SLivenessAnnot ((u,d,o,i), stmt) ->
        let used_below = (only_use vids stmt) in
        diff used_below d
    | SAssign (v, _)     -> List.filter (fun x -> not (x = v)) vids
    | SUniform (v, _, _) -> List.filter (fun x -> not (x = v)) vids
    | SSkip              -> vids
    | SHalt              -> vids
    | SIf (_, s1, s2) ->
        let used_s1 = only_use vids s1 in
        let used_s2 = only_use vids s2 in
        intersection used_s1 used_s2
    | SPSeq (s1, s2, _, _, _) ->
        let used_s1 = only_use vids s1 in
        let used_s2 = only_use vids s2 in
        intersection used_s1 used_s2
    | s -> print_stmt_type_no_ann s; failwith " is not yet implemented in 'only_use'"

let all_assigns cstmt =
  let rec all_assigns_prime acc st =
    match st with
      | SLivenessAnnot (info, s1) -> all_assigns_prime acc s1
      | SSeq (s1, s2)           -> merge_two acc s1 s2
      | SPSeq (s1, s2, _, _, _) -> merge_two acc s1 s2
      | SAssign (v, _)          -> [v]
      | SUniform (v, _, _)      -> [v]
      | SIf (_, s1, s2)         -> merge_two acc s1 s2
      | s -> []
  and merge_two acc s1 s2 =
    let s1_ass = all_assigns_prime acc s1 in
    let s2_ass = all_assigns_prime acc s2 in
    list_unique (List.append s1_ass s2_ass)
  in all_assigns_prime [] cstmt

let all_decls cstmt =
  let rec all_decls_prime acc st =
    match st with
      | SLivenessAnnot (info, s1) -> all_decls_prime acc s1
      | SSeq (s1, s2)           -> merge_two acc s1 s2
      | SPSeq (s1, s2, _, _, _) -> merge_two acc s1 s2
      | SDefine (v, _)          -> [v]
      | SUniform (v, _, _)      -> [v] (* TODO: is this right? *)
      | SIf (_, s1, s2)         -> merge_two acc s1 s2
      | s -> []
  and merge_two acc s1 s2 =
    let s1_ass = all_decls_prime acc s1 in
    let s2_ass = all_decls_prime acc s2 in
    list_unique (List.append s1_ass s2_ass)
  in all_decls_prime [] cstmt

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
    | None -> SSkip
        (* failwith (String.concat " " ["Trying to write out an assignment"
                                          ;"that doesn't exist in assignment"
                                          ;"stack: "
                                          ;(varid_to_string varid)
                                          ])
         *)
    | Some aexp -> SAssign (varid, aexp)

let get_new_assigns out_live s_vars a_stack =
  let needed = diff out_live s_vars in
  (needed, List.map (write_assign a_stack) needed)

let partial_op name v =
  match name with
  | "and" -> if v then None else Some false
  | "or"  -> if v then Some true else None

(* Try to short-circuit lexp so that we can avoid having a SIf statement *)
let rec try_eval_lexp ex =
  match ex with
  | LEBool (v)           -> Some (v = 1)
  | LEBinop (op, l1, l2) -> try_eval_binop op l1 l2
  | rel                  -> match lexp_vars rel with
                            | (_::_) -> None
                            | []     -> try_eval_relop rel
and try_eval_binop (name, f) l1 l2 =
  match (try_eval_lexp l1, try_eval_lexp l2) with
  | (None, None)       -> None
  | (Some v, None)     -> partial_op name v
  | (None, Some v)     -> partial_op name v
  | (Some v1, Some v2) -> let v11 = if v1 then 1 else 0 in
                          let v21 = if v2 then 1 else 0 in
                          Some (f v11 v21 = 1)
and try_eval_relop rel =
  ifdebug (printf "The relation: "; print_lexp rel; printf " ");
  let res = Evalstate.eval_lexp rel (new State.state_empty) in
  ifdebug (printf "The result: %d" res; printf "\n");
  Some (res = 1)


(* We return a tuple of:
 *  [a list of variables],
 *  the new assignment stack,
 *  and the rewritten statment
 *)
let rec rewrite_stmt cstmt s_vars assign_stack : (varid list * (varid * aexp) list * stmt) =
  match cstmt with
    | SSeq (s1, s2) ->
        let (a_vars, stk1, s11)  = rewrite_stmt s1 s_vars assign_stack in
        let (a_vars1, stk2, s22) =
        (* printf "adding %s to s_vars argument\n" (varid_list_to_string a_vars); *)
        rewrite_stmt s2 (list_unique (List.append a_vars s_vars)) stk1 in
        (a_vars, stk2, SSeq (s11, s22))
    | SSkip -> (s_vars, assign_stack, SSkip)
    | SLivenessAnnot ((u,d,o,i), SSkip) -> (s_vars, assign_stack, SSkip)
    | SLivenessAnnot ((u,d,o,i), SHalt) ->
        let (_, new_assigns) = get_new_assigns o s_vars assign_stack in
        (s_vars, assign_stack, List.fold_right (fun s1 s2 -> SSeq (s1, s2)) new_assigns SHalt)
    | SLivenessAnnot (info,SDefine (v,t))    -> (s_vars, assign_stack, SDefine (v,t))
    | SLivenessAnnot (info,SUniform (v,i,j)) -> (s_vars, assign_stack, SUniform (v,i,j))

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
        if List.mem name s_vars
        then (s_vars, assign_stack, SAssign (name, rhs1))
        else (s_vars, extend (name, rhs1) assign_stack, SSkip)

    | SLivenessAnnot ((u,d,o,i),SPSeq (s1,s2,q,i1,i2)) ->
        let (a_vars, a_stack1, new_assigns, s12, s22) =
              manage_branch o s_vars assign_stack s1 s2 in
        let stmt2 = SPSeq (s12, s22, q, i1, i2) in
        (a_vars, a_stack1, List.fold_right (fun s1 s2 -> SSeq (s1,s2)) new_assigns stmt2)
    | SLivenessAnnot ((u,d,o,i),SIf (p, st, sf)) ->
        let p1 = sub_lexp p assign_stack in
        let vs = try_eval_lexp p1 in
        (match vs with
           | None ->
             let (a_vars, a_stack1, new_assigns, st2, sf2) =
                    manage_branch o s_vars assign_stack st sf in
             let stmt2 = SIf (p1, st2, sf2) in
             (a_vars, a_stack1, List.fold_right (fun s1 s2 -> SSeq (s1, s2)) new_assigns stmt2)
           | Some (v) -> (match v with
                          | false -> rewrite_stmt sf s_vars assign_stack
                          | true  -> rewrite_stmt st s_vars assign_stack
                          | _ -> failwith "the impossible happened, predicate returned non-bool"))
    | p -> print_stmt p; failwith "Need to implement"

and manage_branch out_live s_vars a_stack s1 s2 =
  let assmnt = list_unique (List.append (all_assigns s1) (all_assigns s2)) in
  (* printf "Assignments: %s\n" (varid_list_to_string assmnt); *)
  let just_used = intersection (only_use out_live s1) (only_use out_live s2) in
  let poss_needed = diff out_live just_used in
  let (needed, new_assigns) = get_new_assigns poss_needed s_vars a_stack in
  (* printf "s_vars in manage_branch: %s\n" (varid_list_to_string s_vars); *)
  let dont_remove =
    (* printf "adding %s to avars\n" (varid_list_to_string needed); *)
    list_unique (List.append assmnt (List.append needed s_vars)) in
  let (av1,a_stack1,s12) = rewrite_stmt s1 dont_remove a_stack in
  let (av2,a_stack2,s22) = rewrite_stmt s2 dont_remove a_stack in
  (* printf "av1: %s\n" (varid_list_to_string av1); *)
  (* printf "av2: %s\n" (varid_list_to_string av2); *)
  let new_stack = stack_rem assmnt (intersect a_stack1 a_stack2) in
  (dont_remove, new_stack, new_assigns, s12, s22)
(* above we are assuming a well formed program!
   If a nested if flushed out an assignment we ignore it since it should have
   been flushed out at the top-level if (liveness analysis should tell us this
 *)

let rem_redundant_decl cstmt =
  let decls = all_decls cstmt in
  (* printf "Declarations in prog: %s\n" (varid_list_to_string decls); *)
  let all_vars = stmt_vars cstmt in
  (* printf "All vars in prog: %s\n" (varid_list_to_string all_vars); *)
  let to_remove = diff decls all_vars in
  (* printf "To remove in prog: %s\n" (varid_list_to_string to_remove); *)
  let rec rem_decls vids cstmt1 =
    match cstmt1 with
      | SSeq (s1, s2) ->
          let s11 = rem_decls vids s1 in
          let s22 = rem_decls vids s2 in
          SSeq (s11, s22)
      | SLivenessAnnot (info, stmt) ->
          SLivenessAnnot (info, rem_decls vids stmt)
      | SAssign (v, e) -> SAssign (v, e)
      | SDefine (v, t) -> if List.mem v vids
                          then SSkip
                          else SDefine (v, t)
      | SUniform (v, e, f) -> if List.mem v vids
                          then SSkip
                          else SUniform (v, e, f)
      | SIf (p, s1, s2) ->
          let s11 = rem_decls vids s1 in
          let s22 = rem_decls vids s2 in
          SIf (p, s11, s22)
      | SPSeq (s1, s2, q, i1, i2) ->
          let s11 = rem_decls vids s1 in
          let s22 = rem_decls vids s2 in
          SPSeq (s11, s22, q, i1, i2)
      | SSkip -> SSkip
      | SHalt -> SHalt
      | s -> print_stmt_type_no_ann s;
             failwith " is not yet implemented in 'rem_redundant_decl'"
  in rem_decls to_remove cstmt


let rec flip_seq cstmt =
  match cstmt with
     | SSeq (s1, s2)           -> SSeq (flip_seq s2, flip_seq s1)
     | SPSeq (s1, s2, a, b, c) -> SPSeq (flip_seq s2, flip_seq s1, a, b, c)
     | SIf      (l1, s1, s2)   -> SIf (l1, flip_seq s1, flip_seq s2)
     | SWhile   (l1, s1)       -> SWhile (l1, flip_seq s1)
     | SLivenessAnnot (i, s1)  -> SLivenessAnnot (i, flip_seq s1)
     | s                       -> s

let rec add_halt cstmt =
  match cstmt with
     | SSeq (s1, s2) -> SSeq (s1, add_halt s2)
     | s             -> SSeq (s, SHalt)

let rec add_skip cstmt =
  match cstmt with
     | SSeq (s1, s2) -> SSeq (s1, add_halt s2)
     | s             -> SSeq (s, SSkip)

let rec ann_use_def cstmt =
  match cstmt with
   | SSkip -> SLivenessAnnot (empty_info, SSkip)
   | SHalt -> SLivenessAnnot (empty_info, SHalt)
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
      
(*
 * The liveness analysis returns a pair of the `live-in` variables of the
 * successor nodes and the analysed successors. Important to remember that the
 * nodes are in reverse sequential order.
 *
 * The data-flow equations we're using are as follows ("|_|" is set union):
 *
 *    in(n)  = use(n) |_| (out(n) \ def(n))
 *
 *    out(n) = { in(s) | s <- succ(n) }
 *
 * The above can be read as:
 *
 * In:
 *  For each node `n` the live-in variables are the variables used at `n` along
 *  with all of the live-out variables, except for the variables that are
 *  defined at `n`.
 *
 * Out:
 *  The live-out variables at node `n` is the union of all of the live-in
 *  variables of each successor to `n`.
 *)
let rec liveness_analysis_rev_prime cstmt vids =
  match cstmt with
    (* The first few cases are the ones that have no `live-in` variables *)
    | SSkip -> ([], SSkip)
    | SHalt -> ([], SHalt)
    | SAssign (name, rhs) -> ([], SAssign (name, rhs))
    | SUniform (v, i1, i2) -> ([], SUniform (v, i1, i2))
    | SDefine  (name, d_type) -> ([], SDefine (name, d_type))

    (* Remembering that each liveness annotations contains the following:
     *   (u, d, o, i) -> (variables used, variables defined, live-out,  live-in)
     *
     * For IF statements we have to ensure that the live-in for the IF
     * statement as a whole is the _union_ of the live-in for each branch
     * in order to ensure that we 'discharge' all of the necessary assignments
     * before we evaluate the IF.
     *)
    | SLivenessAnnot ((u, d, o, i), SIf (lex, s1,s2)) ->
        let (i1, s12) = liveness_analysis_rev_prime s1 vids in
        let (i2, s22) = liveness_analysis_rev_prime s2 vids in
        let succ_ins = list_unique (List.append i1 i2) in
        let in1 = list_unique (List.append u (diff succ_ins d)) in
        (in1, SLivenessAnnot ((u,d,succ_ins,in1),SIf (lex, s12, s22)))
    | SLivenessAnnot ((u, d, o, i), stmt) ->
        let (in1, stmt2) = liveness_analysis_rev_prime stmt vids in
        let in2 = list_unique (List.append u (diff vids d)) in
        (in2, SLivenessAnnot ((u, d, vids, in2), stmt2))
    | SSeq (s1, s2) ->
        let (i1, s1p) = liveness_analysis_rev_prime s1 vids in
        (*printf "s1 type: "; print_stmt_type_no_ann s1; printf "\n";
        printf "succ_ins: %s\n" (varid_list_to_string i1); *)
        let (i2, s2p) = liveness_analysis_rev_prime s2 i1 in
        (i2, SSeq (s1p, s2p))
    | SPSeq (s1, s2, q, i1, i2) ->
        let (in1, s12) = liveness_analysis_rev_prime s1 vids in
        let (in2, s22) = liveness_analysis_rev_prime s2 vids in
        let succ_ins = list_unique (List.append in1 in2) in
        (succ_ins, SPSeq (s12, s22, q, i1, i2))
    | SIf (lex, s1, s2) -> failwith "Every SIf should have an annotation node"
    | s -> print_stmt_type s;
           failwith " is not yet supported in liveness analysis\n"

let liveness_analysis_rev cstmt vids =
  let (_, res) = liveness_analysis_rev_prime cstmt vids in
  res
