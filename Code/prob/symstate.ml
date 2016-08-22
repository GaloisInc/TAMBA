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
  { cstore : int VarIDMap.t      ;
    sstore : Symbol.t VarIDMap.t ;
    pc     : Symbol.t
  }

let state_to_symstate (st : state) : symstate =
  let to_map (l : (Lang.varid * 'a) list) : 'a VarIDMap.t =
    List.fold_left (fun map (k, v) -> VarIDMap.add k v map) VarIDMap.empty l
  in
  { cstore = to_map st#canon ;
    sstore = VarIDMap.empty  ;
    pc     = Symbol.SymTrue
  }

let addvar (x : Lang.varid) (st : symstate) : symstate =
  { st with cstore = VarIDMap.add x 0 st.cstore }

let getvar (x : Lang.varid) (st : symstate) : int =
  VarIDMap.find x st.cstore

let setvar (x : Lang.varid) (v : int) (st : symstate) : symstate =
  { st with cstore = VarIDMap.add x v st.cstore }

let addsym (x : Lang.varid) (st : symstate) : symstate =
  { st with sstore = VarIDMap.add x Symbol.SymTrue st.sstore }

let getsym (x : Lang.varid) (st : symstate) : Symbol.t =
  VarIDMap.find x st.sstore

let setsym (x : Lang.varid) (v : Symbol.t) (st : symstate) : symstate =
  { st with sstore = VarIDMap.add x v st.sstore }
                                                
let appendpc (s : Symbol.t) (st : symstate) : symstate =
  { st with pc = Symbol.SymAnd (s, st.pc) }
