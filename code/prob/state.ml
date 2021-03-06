open Util

(* Records stored in state's "records" hashtbl that maps record variable ids
 * to records (lists of field_name*datatype). Record fields implemented as
 * ordinary variables with "<record name>." prefix. Storing records separately
 * in state allows records passed in as query parameters to be expanded into
 * their field variables.
 * *)

class type state = object
  method canon: (Lang.varid * int) list
  method addvar: Lang.varid -> unit
  method set: Lang.varid -> int -> unit
  method set_record: Lang.varid -> Lang.record -> unit
  method get: Lang.varid -> int
  method get_list: Lang.varid list -> (Lang.varid * int) list
  method vars: Lang.varid list
  method iter: (Lang.varid -> int -> unit) -> unit
  method to_string: string
  method as_args: string
  method print: unit
  method print_as_args: unit
  method set_list: (Lang.varid * int) list -> unit
  method copy: state
  method eq: state -> bool
  method eq_on: state -> bool
  method merge: state -> unit
  method remove: Lang.varid -> unit
  method project: Lang.varid list -> unit
  method set_vals: (Lang.varid, int) Hashtbl.t -> unit
  method is_record: (Lang.varid) -> bool
  method get_vals: (Lang.varid) -> Lang.varid list
  method print_records: unit -> unit
  method print_vals: unit -> unit
end;;

class state_hashed hv hr : state = object (self)
  val mutable vals: (Lang.varid, int) Hashtbl.t = hv
  val mutable records: (Lang.varid, Lang.record) Hashtbl.t = hr

  method print_records unit : unit =
    print_endline "++++++++++++++++++++++++++++++++";
    Hashtbl.iter  (fun (_,name) _ -> print_endline name) records;
    print_endline "++++++++++++++++++++++++++++++++";

  method print_vals unit : unit =
    print_endline "+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
    Hashtbl.iter  (fun (_,name) _ -> print_endline name) vals;
    print_endline "-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

  method is_record (r:Lang.varid) : bool  =
    Hashtbl.mem records r

  method get_vals (r:Lang.varid) : Lang.varid list =
    List.map (fun name -> ("", name)) (Hashtbl.find records r)

  method set_vals h =
    ignore(vals <- h)

  method canon: (Lang.varid * int) list =
    Hashtbl.fold (fun k v accum -> (k, v) :: accum) vals []

  method addvar varname =
    Hashtbl.replace vals varname 0

  method set varname varval =
    if Hashtbl.mem vals varname then
      Hashtbl.replace vals varname varval
    else
      raise (General_error ("undefined variable " ^ (Lang.varid_to_string varname)))

  method set_record (varname : Lang.varid) (r : Lang.record) : unit =
    if Hashtbl.mem vals varname && ((Hashtbl.find vals varname) = 0) then
       Hashtbl.replace records varname r
    else
      (
      raise (General_error ("undefined record " ^ (Lang.varid_to_string varname))))

  method get (varname : Lang.varid) : int =
    try
      Hashtbl.find vals varname
    with
    | Not_found -> raise (General_error ("undefined variable " ^ (Lang.varid_to_string varname)))

  method get_list vs = List.map (fun v -> (v, self#get v)) vs

  method vars: (Lang.varid list) = Hashtbl.fold (fun k v accum -> k :: accum ) vals []

  method iter f = Hashtbl.iter f vals

  method to_string =
    "[" ^
      (String.concat ", "
         (List.map
            (fun (id, v) -> (Lang.varid_to_string id) ^ "=" ^ string_of_int(v))
            self#canon)) ^
      "]"

  method as_args =
    "(" ^
      (String.concat ", "
         (List.map
            (fun (id, v) -> (Lang.varid_to_string id) ^ "=" ^ string_of_int(v))
            self#canon)) ^
      ")"

  method print = print_string self#to_string

  method print_as_args = print_string self#as_args

  method set_list sl =
    List.iter (fun (vname, vval) -> ignore (self#addvar vname; self#set vname vval)) sl

  method copy = new state_hashed (Hashtbl.copy vals) (Hashtbl.copy records)

  method eq (s: state) =
    let s1 = self#canon in
    let s2 = s#canon in
      (List.length
         (List.filter
            (fun e1 -> let (id1, val1) = e1 in
               ((List.mem_assoc id1 s2) && (List.assoc id1 s2) = val1))
            s1))
      =
        (List.length s2)

  method eq_on (agreeon: state) =
    List.for_all (fun (varname, varval) -> self#get varname = varval)
      agreeon#canon

  method merge (s: state) =
    s#iter (fun varname varval -> ignore (self#addvar varname; self#set varname varval))

  method remove var =
    Hashtbl.remove vals var

  method project vars =
    let varsremove = list_subtract (self#vars) vars in
      List.iter (fun vname -> ignore(self#remove vname)) varsremove
end;;

class state_empty = object
  inherit state_hashed (Hashtbl.create 4) (Hashtbl.create 4)
end;;

class state_default vars =
  let h = (
    let h = Hashtbl.create (List.length vars) in
      (List.iter (fun varid -> ignore (Hashtbl.replace h varid 0)) vars);
       h) in
object
  inherit state_hashed h (Hashtbl.create 4)
end;;

let rec states_merge sl1 sl2 =
  match sl1 with
    | [] -> sl2
    | sh :: st ->
        if not (List.exists (fun s -> s#eq sh) sl2) then
          sh :: states_merge st sl2
        else
          states_merge st sl2

module TEMP: Hashtbl.HashedType = struct
  type t = state
  let equal (s1: t) (s2: t) = s1#eq s2
  let hash (s: t) = Hashtbl.hash s#canon
end;;

module STATE_HASHABLE = Hashtbl.Make (TEMP);;
