open Lang
open Core.Std
open Core.Std.List
open Core_extended.Readline

let assoc_vid tups str =
  let vid_eq (_, vid1) (_, vid2) = vid1 = vid2 in
  Assoc.find_exn tups ~equal:vid_eq ("", str)

let safe_parse_int str_opt =
 try Option.map str_opt int_of_string
 with _ -> None

let get_query_params str querydefs =
  let (inlist, outlist, progstmt) = Assoc.find_exn querydefs str in
  printf "Input values:\n";
  let get_in_vals vid =
        (vid,
         safe_parse_int (input_line ~prompt:(varid_to_string vid ^ " = ") ())
        ) in
  map inlist get_in_vals

let all_safe xs =
  List.for_all xs (fun (_,x) -> Option.is_some x)
