open Printf
open ExtList
open ExtHashtbl

let float_of_bool b = if b then 1.0 else 0.0
let string_of_bool b = if b then "true" else "false"

let max_float fl =
  List.fold_left (fun a v -> if v > a then v else a) neg_infinity fl
;;

let map_max_float fl =
  Pmap.fold (fun a v -> if v > a then v else a) fl neg_infinity
;;

let max_arg_float arg_list f =
  List.fold_left (fun (best_arg, best_val) v ->
    let temp = f v in
    if temp > best_val then (Some v, temp) else (best_arg, best_val)) (None, neg_infinity) arg_list
;;

let log2 a = (log a) /. (log 2.0)
;;

let last_msg = ref (Unix.gettimeofday ());;

let logerr (form: ('a, unit, string, unit) format4) =
  let now = Unix.gettimeofday () in
  let tdelta = now -. !last_msg in
  last_msg := now;
  ksprintf
    (fun s -> fprintf stderr "(+ %f): %s\n%!" tdelta s)
    form
;;

let debug = ref false;;

let logdebug (form: ('a, unit, string, unit) format4) =
  let now = Unix.gettimeofday () in
  let tdelta = now -. !last_msg in
  last_msg := now;
  ksprintf
    (fun s -> (if !debug then fprintf stdout "(+ %f): %s\n%!" tdelta s))
    form
;;

let maps_merge m1 m2 mf =
  Pmap.foldi
    (fun v2 p2 m ->
      Pmap.insert_with mf v2 p2 m)
    m2 m1
;;

let map_filteri f m =
  let temp = ref Pmap.empty in
  Pmap.iter (fun k v -> if f k v then temp := Pmap.add k v !temp else ())
    m;
  !temp

let maps_flatten ml mf =
  match ml with
    | [] -> Pmap.empty
    | h :: rest -> List.fold_left (fun a m -> maps_merge a m mf) h rest

let list_of_map m =
  Pmap.foldi (fun v p a -> (v, p) :: a) m []

let map_of_list l =
  List.fold_left (fun a (p, v) -> Pmap.add v p a) Pmap.empty l

let map_length m =
  Pmap.fold (fun _ a -> a + 1) m 0

let rec list_range a b =
  if a = b then [a]
  else a :: (list_range (a+1) b);;

let rec list_insert l pos e =
  if pos = 0 then e :: l
  else match l with
    | lhead :: lrest -> lhead :: (list_insert lrest (pos - 1) e)
    | _ -> failwith "list not long enough to insert"

let gen_inserts_of l e =
  let poss = list_range 0 ((List.length l)+1) in
  List.map (fun pos -> list_insert l pos e) poss

let rec _gen_perms l = match l with
  | [] -> [[]]
  | l ->
    List.flatten 
      (List.map (fun e ->
        let rest = _gen_perms (List.remove l e) in
        List.map (fun r -> e :: r) rest) l)
         
let gen_perms i =
  match i with
    | 0 -> [[]]
    | 1 -> [[0]]
    | i -> List.map (fun p -> 0 :: p) (_gen_perms (list_range 1 i))

let gen_perms_of l = _gen_perms l

let gen_orders i = _gen_perms (list_range 0 i)

let rec factorial i = if i = 1 then 1 else i * (factorial (i - 1))

let rec lists_prod l1 l2 = match l1 with
  | [] -> []
  | i1 :: t1 ->
      List.append
	(List.map (fun x -> (i1,x)) l2)
	(lists_prod t1 l2)
;;

let rec dump_hash h =
  String.concat "\n"
    (List.map 
       (fun (k,v) -> sprintf "%s\t -> %s" (Std.dump k) (Std.dump v))
       (List.of_enum (Hashtbl.enum h)))

let rec dump_parpmap h =
  String.concat "\n"
    (List.map 
       (fun (k,v) -> sprintf "%s\t -> %s" (Std.dump k) (Std.dump v))
       (List.of_enum (Pmap.enum h)))
