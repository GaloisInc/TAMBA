open Util
open Ppl_ocaml
open Gmp
open Lang

open Gmp.Q.Infixes

let volcomp_bin = "volComp"
let volcomp_env = Unix.environment ()
let volcomp_tmp = Globals.file_relative "volcomp_tmp"
let volcomp_workdir = (volcomp_tmp ^ "/" ^ (string_of_int (Unix.getpid ())));;

if Sys.file_exists volcomp_workdir then
  ignore (Unix.system ("rm -Rf " ^ volcomp_workdir));;
Unix.mkdir volcomp_workdir 0o700;;

let rand_tmp_name () = "model_" ^ (string_of_int (Unix.getpid ()))

type volcomp =
  { decls : (varid * (int * int)) list;
    leqs  : (int list * int) list list
  }
    
let string_of_volcomp (v : volcomp) : string =
  let decls_str = (String.concat "\n" (List.map (fun ((_, name), (l, u)) -> Printf.sprintf "V %s I %d %d" name l u) v.decls)) ^ "\n" in

  let leqs_str =
    (match v.leqs with
     | [] -> ""
     | h :: _ -> (String.concat "\n" (List.map (fun (coeffs, lim) -> Printf.sprintf "C %s %d" (String.concat " " (List.map string_of_int coeffs)) lim) h)) ^ "\n")
  in
  
  let ret = decls_str ^ leqs_str ^ "E" in
  print_endline ret;
  ret
                                                
let total_volume (v : volcomp) : Z.t =
  let sizes = List.map (fun (_, (l, u)) -> Z.add (Z.sub (Z.from_int u) (Z.from_int l)) Z.one) v.decls in
  List.fold_left Z.mul Z.one sizes

let count_models (v : volcomp) : Z.t =
  let rexp = Str.regexp "^ Probability Lower Bound : \\([0-9]+\\.?[0-9]+\\)$" in
  let filename = volcomp_workdir ^ "/" ^ (rand_tmp_name ()) in
  write_file filename (string_of_volcomp v);
  let (stdin, stderr) = exec_and_read_all (volcomp_bin ^ " " ^ filename) volcomp_env in
  let out_lines = string_split stdin "\n" in
  match (List.rev out_lines) with
  | _ :: lower_bound_str :: _ ->
     if (string_search rexp stdin) then
       let pr_str = Str.matched_group 1 stdin in
       print_endline pr_str;
       let res = from_string_Q pr_str in
       print_endline (Q.to_string res);
       let count_exact = Q.from_z (total_volume v) */ res in
       print_endline (Z.to_string (total_volume v));
       Z.fdiv_q (Q.get_num count_exact) (Q.get_den count_exact)
     else
       raise (Failure "count_models: volcomp output malformed (regexp failed to retrieve prob)")
  | _ -> raise (Failure "count_models: volcomp output malformed (no second-to-last line)")
