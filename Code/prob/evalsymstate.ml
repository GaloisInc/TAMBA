open Lang
open Symstate

let rec eval_aexp (aexp : aexp) (state : symstate) : int =
  match aexp with
  | AEVar (name) -> getvar name state
  | AEBinop (op, aexp1, aexp2) ->
     let (_, beval) = op in
     beval (eval_aexp aexp1 state) (eval_aexp aexp2 state)
  | AEInt (n) -> n
  | AERecord _ -> failwith "eval_aexp (sym) on record"

let rec eval_lexp (lexp : lexp) (state : symstate) : int =
  match lexp with
  | LEBinop (op, lexp1, lexp2) ->
     let (_, beval) = op in
     beval (eval_lexp lexp1 state) (eval_lexp lexp2 state)
  | LEReln (op, aexp1, aexp2) ->
     let (_, beval) = op in
     beval (eval_aexp aexp1 state) (eval_aexp aexp2 state)
  | LEBool (b) -> b

let rec sym_aexp (aexp : aexp) (state : symstate) : Symbol.t =
  match aexp with
  | AEVar (name) -> getsym name state
  | AEBinop (op, aexp1, aexp2) ->
     let s1 = sym_aexp aexp1 state in
     let s2 = sym_aexp aexp2 state in
     let (op_name, _) = op in
     (match op_name with
     | "+" -> Symbol.SymAdd (s1, s2)
     | "-" -> Symbol.SymSub (s1, s2)
     | "*" -> Symbol.SymMul (s1, s2)
     | "/" -> Symbol.SymDiv (s1, s2)
     | _   -> raise (Evalstate.Eval_error ("sym_aexp (sym) unrecognized op: " ^ op_name)))
  | AEInt (n) -> Symbol.SymInt n
  | AERecord _ -> failwith "sym_aexp (sym) on record"

let rec sym_lexp (lexp : lexp) (state : symstate) : Symbol.t =
  match lexp with
  | LEBinop (op, lexp1, lexp2) ->
     let s1 = sym_lexp lexp1 state in
     let s2 = sym_lexp lexp2 state in
     let (op_name, _) = op in
     (match op_name with
      | "and" -> Symbol.SymAnd (s1, s2)
      | "or"  -> Symbol.SymOr (s1, s2)
      | _     -> raise (Evalstate.Eval_error ("sym_lexp (sym) unrecognized lebinop: " ^ op_name)))
  | LEReln (op, aexp1, aexp2) ->
     let s1 = sym_aexp aexp1 state in
     let s2 = sym_aexp aexp2 state in
     let (op_name, _) = op in
     (match op_name with
      | "<"  -> Symbol.SymLt (s1, s2)
      | "<=" -> Symbol.SymOr (Symbol.SymLt (s1, s2), Symbol.SymEq (s1, s2))
      | ">"  -> Symbol.SymLt (s2, s1)
      | ">=" -> Symbol.SymOr (Symbol.SymLt (s2, s1), Symbol.SymEq (s1, s2))
      | "==" -> Symbol.SymEq (s1, s2)
      | _    -> raise (Evalstate.Eval_error ("sym_lexp (sym) unrecognized lereln: " ^ op_name)))
  | LEBool (b) ->
     match b with
     | 0 -> Symbol.SymNot Symbol.SymTrue
     | 1 -> Symbol.SymTrue
     | _ -> raise (Evalstate.Eval_error ("sym_lexp (sym) LEBool not 0 or 1"))
     
let rec eval (stmt : stmt) (state : symstate) : (int * symstate) =
  match stmt with
  | SDefine (name, _) ->
     (0, addvar name state)
  | SAssign (name, aexp) ->
     let res = eval_aexp aexp state in
     let sym = sym_aexp aexp state in
     let st1 = setvar name res state in
     let st2 = setsym name sym st1 in
     (res, st2)
  | SPSeq (s1, s2, p, n1, n2) ->
     failwith "eval (sym) on prob. if stmt"
  | SSeq (stmt1, stmt2) ->
     let (_, st1) = eval stmt1 state in
     let (res, st2) = eval stmt2 st1 in
     (res, st2)
  | SIf (lexp, stmt1, stmt2) ->
     let guard = eval_lexp lexp state in
     let symguard = sym_lexp lexp state in
     (match guard with
     | 0 ->
        let (res, st1) = eval stmt2 (appendpc (Symbol.SymNot symguard) state) in
        (res, st1)
     | 1 ->
        let (res, st2) = eval stmt1 (appendpc symguard state) in
        (res, st2)
     | _ -> raise (Evalstate.Eval_error ("guard expression did not evaluate to 0 or 1")))
  | SWhile (lexp, stmtb) ->
     let guard = eval_lexp lexp state in
     let symguard = sym_lexp lexp state in
     (match guard with
      | 0 ->
         (0, appendpc (Symbol.SymNot symguard) state)
      | 1 ->
         let (_, st) = eval stmtb state in
         eval stmt (appendpc symguard st)
      | _ -> raise (Evalstate.Eval_error ("guard expression did not evaluate to 0 or 1")))
  | SSkip -> (0, state)
  | SHalt -> (0, state)
  | SUniform (name, lower, upper) ->
     let res = (Random.int (upper - lower + 1)) + lower in
     let sym = Symbol.SymInt res in
     let st1 = setvar name res state in
     let st2 = setsym name sym st1 in
     (res, st2)
  | SOutput (_, _) -> (0, state)
         
     
     
