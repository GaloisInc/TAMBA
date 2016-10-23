open State
open Lang
open Printf
open Pdefs
open Globals
open Esys
open Policy
open Util
open Parser_util
open Value_status
open Optimize


module type EXP_SYSTEM = sig
  val run: Pdefs.tpmocksetup -> unit
end;;

module MAKE_EVALS (ESYS: EVAL_SYSTEM) = struct
  module PSYS = MAKE_PSYSTEM(ESYS)

  let assoc_with_vid ass vid =
    let (_, vid_name) = vid in
    printf "vid_name: %s\n%!" vid_name;
    Some (List.assoc vid_name ass)

  let get_query_params_list args inlist =
    let get_in_vals vid = (vid, assoc_with_vid args vid) in
    List.map get_in_vals inlist

  let rec run_query queryname args (querydefs, s_vars, s_state) =
    let querytuple = List.assoc queryname querydefs  in
    let (inlist, outlist, progstmt) = querytuple in

    (* get the input values from `args` using the keys in `inlist`
     * and make the initial state *)
    let ins = get_query_params_list args inlist in
    let qstmt = make_int_assignments ins in

    let (ignored, inputstate) = Evalstate.eval qstmt (new state_empty) in


    (*-------------------------------------------------------------------*)
    (*---------------------------- Original -----------------------------*)
    (*-------------------------------------------------------------------*)


    let flipped = flip_seq (add_halt progstmt) in
    let fannotated = ann_use_def flipped in

    (*-------------------------------------------------------------------*)
    (*---------------------------- Liveness -----------------------------*)
    (*-------------------------------------------------------------------*)
    let rec fix_live i vs stmts =
      let lived = liveness_analysis_rev stmts vs in
      let res = if equal_stmts stmts lived
                then lived
                else fix_live (i + 1) vs lived in
      res in
    let analed = flip_seq (fix_live 0 outlist fannotated) in

    (*-------------------------------------------------------------------*)
    (*---------------------------- Inlining -----------------------------*)
    (*-------------------------------------------------------------------*)
    let ignored_vids = List.concat [s_vars;inlist] in
    let inlinable    = List.map (fun x -> (x, AEInt (inputstate#get x))) inlist in
    let (_, _,rewritten) = rewrite_stmt analed ignored_vids inlinable in
    let clean = rem_redundant_decl rewritten in

    let printputstate = inputstate#copy in
    printputstate#project inlist;

    printf "%s" queryname;
    printputstate#print_as_args;

    inputstate#merge s_state;
    let (_, outputstate) = Evalstate.eval rewritten inputstate in
    outputstate#project outlist;
    printf " = "; outputstate#print_as_args; printf "\n%!";

    match outputstate#get ("", "result") with
    | 1 -> true
    | 0 -> false
    | _ -> raise (General_error "result should be a boolean")


  let algo_separate meta_info =
    let t = !Cmd.alloc_eta in
    let mid (x,y) = (x + y) / 2 in
    let ship_ids = [1;2;3;4;5;6;7] in
    let check_solution bs = let sm = Util.list_sum (List.map fst bs) in
                            (* printf "sm: %d\n%!" sm; *)
                            sm >= !Cmd.alloc_berths in

    let eta_loop ships =
      let rec go accum ss =
        match ss with
        | []    -> accum
        (* "can ship [i] arrive within [T] time units?" *)
        | s::ts -> let args = [("ship", s); ("time", t)] in
                   let res = run_query "close_enough" args meta_info  in
                   if res
                   then go (List.append accum [s]) ts
                   else go accum ts in
      go [] ships in

    let close_ships = eta_loop ship_ids in
    printf "Length of `close_ships`: %d\n%!" (List.length close_ships);
    List.iter (fun x -> printf "%d, " x) close_ships;

    (* initialize array of lower and upper bounds on the number of berths, initially set to 0 and 1000 *)
    let berths = Array.make (List.length close_ships) (0,1000) in
    let solution = [] in

    let rec bounds_loop n =
      if n >= List.length close_ships
      then ()
      else let ask = mid berths.(n) in
           (* "does ship [ship_ids[i]] have at least [ask] berths?" *)
           let args = [("ship", n);("berths_needed", ask)] in
           let result = run_query "enough_berths" args meta_info in
           (if result
            then (* positive result improves lower bound *)
                berths.(n) <- (ask, snd (berths.(n)))
            else (* negative result improves upper bound *)
                berths.(n) <- (fst (berths.(n)), ask));
           bounds_loop (n+1) in

    let rec solve_loop () = 
      let () = bounds_loop 0 in

      let res = check_solution (Array.to_list berths) in
      (* printf "res: %s\n%!" (string_of_bool res); *)
      if res
      then ()
      else solve_loop () in

    printf "Before solve_loop\n%!";
    solve_loop ()

  let run asetup =
    Printexc.record_backtrace true;
    (* let vars = pmock_all_vars asetup in*)
      let secretstmt = Preeval.preeval asetup.secret in
      let beliefstmt = Preeval.preeval asetup.belief in

      let querydefs = List.map
        (fun (astring, (l1, l2, apstmt)) -> (astring, (l1, l2, Preeval.preeval apstmt)))
        asetup.querydefs in
      let queries   = List.map
        (fun (astring, apstmt) -> (astring, Preeval.preeval apstmt))
        asetup.queries in
      let policies  = asetup.policies in

      let (ignored, secretstate) = Evalstate.eval secretstmt (new state_empty) in

      let secretvars = Lang.all_vars beliefstmt in

      let sa_beliefstmt = (sa_of_stmt beliefstmt [] secretvars) in

      let startdist = ESYS.peval_start sa_beliefstmt in

      printf "Before algo\n%!";
      let () = algo_separate (querydefs, secretvars, secretstate) in
      printf "After algo\n%!";
      printf "Cmd.alloc_berths: %d\n%!" !Cmd.alloc_berths;

end
;;

module EVALS_PPSS_BOX  = MAKE_EVALS(ESYS_PPSS_BOX);;

let main () =
  let infile = ref "-" in
  Arg.parse [
    ("--prefix",
     Arg.String (fun s ->
                   Globals.alloc_out_pre := s;
                ),
     "The prefix for the output files for each algorithm, --prefix \"test\" results in test-1.prob and test-2.prob");
    ("--eta",
     Arg.Set_int Cmd.alloc_eta,
     "The desired eta");
    ("--berths",
     Arg.Set_int Cmd.alloc_berths,
     "The desired number of berths");
    ("--debug",
     Arg.Set Cmd.opt_debug,
     "Set debugging output");
  ] (function s -> infile := s) "";
  let aexperiment = parse !infile Parser.pmock in
  let module E =
    (val (module EVALS_PPSS_BOX: EXP_SYSTEM): EXP_SYSTEM) in
    E.run aexperiment

;;

main();;

