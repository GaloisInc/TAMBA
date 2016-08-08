open Sys
open Benchmark
open Printf
open Filename
open Unix
open Util



let currently_parsing = ref "";;

let latte_count = ref 0;;

let inc_latte_count =
  latte_count := !latte_count + 1;;

let renderlatex = ref false;;

let simplify_steps = ref 0;;

let output_bench   = ref false;;
let output_bench_latte = ref false;;
let output_bench_bakeoff = ref false;;

let current_executable = Sys.argv.(0);;
let current_dir = Filename.dirname current_executable;;
let original_dir = Unix.getcwd ();;

let file_relative f = (* relative to starting envionment *)
  if Filename.is_relative f then
    original_dir ^ Filename.dir_sep ^ f
  else
    f
;;

let file_abs f = (* assuming f is specified relative to new environment *)
  if Filename.is_relative f then
    (Unix.getcwd ()) ^ Filename.dir_sep ^ f
  else
    f

let do_ifverbose f =
  if !Cmd.opt_verbose = 2 then f ();;
let do_ifverbose1 f =
  if !Cmd.opt_verbose = 1 then f ();;
let do_if_not_verbose f =
  if !Cmd.opt_verbose = 0 then f ();;
let do_ifdebug f =
  if !Cmd.opt_debug then f ();;
let do_ifbench f =
  if !output_bench then f ();;
let do_ifbench_latte f =
  if !output_bench_latte then f ();;
let do_ifbench_bakeoff f =
  if !output_bench_bakeoff then f ();;
let do_ifsampling f =
  if !Cmd.opt_samples > 0 then f ();;

(* Initial max belief which is used to calculate cumulative leakage *)
let init_max_belief = ref 0.0;;

(* latte timing recording *)
let chan_bench_latte = ref Pervasives.stdout;;
let set_bench_latte s =
  chan_bench_latte := (open_out (file_relative s));;
let timer_bench_latte = new timer;;
let bench_latte_out_header () =
  do_ifbench_latte (fun () ->
                      fprintf !chan_bench_latte "type,dimensions,constraints,time (s),time (real s)\n";
                      flush !chan_bench_latte);;
let bench_latte_start () =
  do_ifbench_latte (fun () -> ignore (timer_bench_latte#mark));;
let bench_latte_end k d c =
  do_ifbench_latte (fun () ->
                      let (t, tr) = timer_bench_latte#mark in
                        fprintf !chan_bench_latte "%s,%d,%d,%f,%f\n" k d c t tr;
                        flush !chan_bench_latte);;
let bench_latte_close () =
  do_ifbench_latte (fun () -> close_out !chan_bench_latte);;
(* end of latte timing recording *)

(* bakeoff timing recording *)
let chan_bench_bakeoff = ref Pervasives.stdout;;
let set_bench_bakeoff s =
  chan_bench_bakeoff := (open_out (file_relative s));;
let timer_bench_bakeoff = new timer;;
let bench_bakeoff_out_header () =
  do_ifbench_bakeoff (fun () ->
                      fprintf !chan_bench_bakeoff "type,dimensions,constraints,time (s),time (real s)\n";
                      flush !chan_bench_bakeoff);;
let bench_bakeoff_start () =
  do_ifbench_bakeoff (fun () -> ignore (timer_bench_bakeoff#mark));;
let bench_bakeoff_end k d c =
  do_ifbench_bakeoff (fun () ->
                      let (t, tr) = timer_bench_bakeoff#mark in
                        fprintf !chan_bench_bakeoff "%s,%d,%d,%f,%f\n" k d c t tr;
                        flush !chan_bench_bakeoff);;
let bench_bakeoff_close () =
  do_ifbench_bakeoff (fun () -> close_out !chan_bench_bakeoff);;
(* end of bakeoff timing recording *)

let max_complexity = ref 0;;
let seen_complexity a = if a > !max_complexity then max_complexity := a;;

let b = new bench;;

let new_record = b#new_record;;
let inc_record = b#inc_record;;
let inc_val_record = b#inc_val_record;;
let max_record = b#max_record;;
let set_record = b#set_record;;
let new_timer = b#new_timer;;
let start_timer = b#start_timer;;
let stop_timer = b#stop_timer;;
let mark_epoch = fun() -> b#mark_epoch;;
let next_epoch = fun() -> b#next_epoch;;
let print_header = fun () -> b#print_header;;
let print_epoch = fun () -> b#print_epoch;;
let set_bench s = b#set_bench (file_relative s);;
let close_bench () = b#close;;

let timer_count     = "latte count";;
let timer_barvinok  = "barvinok count";;
let timer_maximize  = "latte maximize";;
let timer_simplify  = "simplify";;
let timer_query     = "query";;
let record_vertices = "vertices";;

new_timer timer_count;;
new_timer timer_barvinok;;
new_timer timer_maximize;;
new_timer timer_simplify;;
new_timer timer_query;;
new_record record_vertices;;

let _memoize_common name =
  let record_hit = "memoize " ^ name ^ " hit" in
  let record_fault = "memoize " ^ name ^ " fault" in
  let h = Hashtbl.create 256 in
    new_record record_hit "0";
    new_record record_fault "0";
    ((fun () -> inc_record record_hit),
     (fun () -> inc_record record_fault),
     h)
;;

let memoize_named1 name f =
  let bench_hit, bench_fault, h = _memoize_common name in
    fun k ->
    try
      let temp = Hashtbl.find h k in
        bench_hit ();
        temp
    with
        Not_found ->
          (let temp = f k
           in
             Hashtbl.replace h k temp;
             bench_fault ();
             temp)
;;

let memoize_named2 name f =
  curry2 (memoize_named1 name (uncurry2 f))
;;

Unix.chdir current_dir;;
