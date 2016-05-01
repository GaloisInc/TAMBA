open Eval
open State
open Lang
open Gmp
open Printf
open Util

exception Eval_error of string;;

  (* eval_aexp: aexp -> state -> int *)
  (* evaluates aexp caexp on given state cstate *)
  let rec eval_aexp caexp (cstate: state) : int =
    match caexp with
      | AEVar (id) -> cstate#get id
      | AEBinop (op, aexp1, aexp2) ->
          let (bname, beval) = op in
            beval (eval_aexp aexp1 cstate) (eval_aexp aexp2 cstate)
      | AEInt (v) -> v
      | AERecord _ -> failwith "Cannot evaluate records to ints"

  (* eval_lexp: lexp -> state -> int *)
  (* evaluates lexp clexp on given state cstate *)
  let rec eval_lexp clexp (cstate: state) : int =
    match clexp with
      | LEBinop (op, lexp1, lexp2) ->
          let (bname, beval) = op in
            beval (eval_lexp lexp1 cstate) (eval_lexp lexp2 cstate)
      | LEReln (op, aexp1, aexp2) ->
          let (bname, beval) = op in
            beval (eval_aexp aexp1 cstate) (eval_aexp aexp2 cstate)
      | LEBool (v) -> v

  let rec eval_aexp_assign (caexp : aexp) (name : Lang.varid)
      (cstate : state) : (int * state) =
    match caexp with
    | AERecord record -> let (_, namestr) = name in
      (cstate#set_record name record) ;
            (0, cstate)
      | _ -> let varval = eval_aexp caexp cstate in
        (varval, (cstate#set name varval; cstate))

  (* eval: stmt -> state -> (int * state)
     Evaluate stmt cstmt within state cstate, raise Eval_error if expression cannot be evaluated
     to a value. *)
  let rec eval_enum_uniform cstmt (cstate: state) =
    match cstmt with
    | SDefine (name, datatype) ->
      SDefine (name, datatype)
    | SAssign (name, varaexp) ->
      SAssign (name, varaexp)
    | SPSeq (s1, s2, p, n1, n2) ->
      SPSeq ( eval_enum_uniform s1 cstate, eval_enum_uniform s2 cstate, p, n1, n2)
    | SSeq (stmt1, stmt2) ->
      let s1 = eval_enum_uniform stmt1 cstate in
      let s2 = eval_enum_uniform stmt2 state1 in
      SSeq(s1, s2)
    | SIf (guardlexp, stmt1, stmt2) ->
      SIf (guardlexp, eval_enum_uniform stmt1 cstate, eval_enum_uniform stmt2 cstate)
    | SWhile (guardlexp, stmtbody) ->
      let s = eval_enum_uniform stmtbody cstate
      SWhile (guardlexp, s)
    | SSkip -> SSkip
    | SUniform (varid, blower, bupper) ->
      SUniform (varid, blower, bupper)
    | SEnumUniform (varid, blower_id, bupper_id) ->
      let blower = cstate#get blower_id in
      let bupper = cstate#get bupper_id in
      SUniform (varid, blower, bupper)
    | SOutput (varid, toagent) ->
      SOutput (varid, toagent)


let rec eval cstmt (cstate: state) : (int * state) =
    let cstate = cstate#copy in
      match cstmt with
        | SDefine (name, datatype) ->
          let (_, name_str) = name in
            (0, (cstate#addvar name; cstate))
        | SAssign (name, varaexp) ->
          eval_aexp_assign varaexp name cstate
        | SPSeq (s1, s2, p, n1, n2) ->
            if (Random.float 1.0) < (Q.to_float p) then
              eval s1 cstate
            else
              eval s2 cstate
        | SSeq (stmt1, stmt2) ->
            let (val1, state1) = eval stmt1 cstate in
            let (val2, state2) = eval stmt2 state1 in
              (val2, state2)
        | SIf (guardlexp, stmt1, stmt2) ->
            (match (eval_lexp guardlexp cstate) with
               | 0 -> eval stmt2 cstate
               | 1 -> eval stmt1 cstate
               | _ -> raise (Eval_error ("guard expression did not evaluate to 0 or 1")))
        | SWhile (guardlexp, stmtbody) ->
            (match (eval_lexp guardlexp cstate) with
               | 0 -> (0, cstate)
               | 1 ->
                   let (val1, state1) = eval stmtbody cstate in
                     eval cstmt state1
               | _ -> raise (Eval_error ("guard expression did not evaluate to 0 or 1"))
            ) (* !!! infinite loop potential *)
        | SSkip -> (0, cstate)
        | SUniform (varid, blower, bupper) ->
            let a = (Random.int (bupper - blower + 1)) + blower in
              (a, (cstate#set varid a; cstate))
        | SEnumUniform (varid, blower_id, bupper_id) ->
          let blower = cstate#get blower_id in
          let bupper = cstate#get bupper_id in
          let a = (Random.int (bupper - blower + 1)) + blower in
          (a, (cstate#set varid a; cstate))
        | SOutput (varid, toagent) ->
            (0, cstate)
