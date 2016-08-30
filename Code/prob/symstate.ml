open State

module OrderedVarID =
  struct
    type t = Lang.varid
    let compare (a1, var1) (a2, var2) =
      match String.compare a1 a2 with
        0 -> String.compare var1 var2
      | c -> c
  end

module VarIDMap = Map.Make(OrderedVarID)
    
type symstate =
  { cstore : int         VarIDMap.t ;
    sstore : Symbol.asym VarIDMap.t ;
    pc     : Symbol.lsym
  }

let empty : symstate =
  { cstore = VarIDMap.empty ;
    sstore = VarIDMap.empty ;
    pc     = Symbol.SymTrue
  }

let state_to_symstate (st : state) : symstate =
  let cstore_f (l : (Lang.varid * int) list) : int VarIDMap.t =
    List.fold_left (fun map (k, v) -> VarIDMap.add k v map) VarIDMap.empty l
  in
  let sstore_f (l : (Lang.varid * int) list ) : Symbol.asym VarIDMap.t =
    List.fold_left (fun map (k, _) -> VarIDMap.add k (Symbol.SymAtom k) map) VarIDMap.empty l
  in
  let st_canon = st#canon in
  { cstore = cstore_f st_canon ;
    sstore = sstore_f st_canon ; 
    pc     = Symbol.SymTrue
  }

let addvar (x : Lang.varid) (st : symstate) : symstate =
  { st with cstore = VarIDMap.add x 0 st.cstore }

let getvar (x : Lang.varid) (st : symstate) : int =
  VarIDMap.find x st.cstore

let setvar (x : Lang.varid) (v : int) (st : symstate) : symstate =
  { st with cstore = VarIDMap.add x v st.cstore }

let getsym (x : Lang.varid) (st : symstate) : Symbol.asym =
  VarIDMap.find x st.sstore

let setsym (x : Lang.varid) (v : Symbol.asym) (st : symstate) : symstate =
  { st with sstore = VarIDMap.add x v st.sstore }
                                                
let appendpc (s : Symbol.lsym) (st : symstate) : symstate =
  { st with pc = Symbol.SymAnd (s, st.pc) }
