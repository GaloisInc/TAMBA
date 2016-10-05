open State
open Lang
open Printf
open Pdefs
open Esys
open Policy
open Util
open Preeval
open Globals
open Parser_util
open Pareto.Distributions
open Pareto.Distributions.Beta
open Value_status
open Optimize
open Core_extended.Readline
open Repl

open Maths
open Gmp

(* for forking the process when used in daemon mode *)
open Core.Std.Unix
open Core.Never_returns
open Core_kernel

let add_policy_records aexp =
  List.iter
    (fun p ->
       Globals.new_record (policy_record_label p) "")
    aexp.policies;;

module type EXP_SYSTEM = sig
  val run: Core.Std.Unix.File_descr.t option -> Pdefs.tpmocksetup -> unit
end;;

module MAKE_EVALS (ESYS: EVAL_SYSTEM) = struct
  module PSYS = MAKE_PSYSTEM(ESYS)

  let make_expected_pairs outlist outstate =
    List.map (fun output_id -> let expected = outstate#get output_id in
                              (output_id, expected)) outlist
  (* This function prepares the intermediate states for sampling. Ensuring
     that the following are accounted for:

      1) The inputstate for the query we are sampling from
      2) The a function to apply to our sample inputs
      3) the output list and their expected values

   *)
  let make_trip querydefs (ps : PSYS.policysystem) (queryname, querystmt) =
    let querytuple = List.assoc queryname querydefs in
    let (inlist, outlist, progstmt) = querytuple in

    (* set up the initial state for sampling and keep a copy to find the expected results *)
    let (ignored, inputstate_temp) = Evalstate.eval querystmt (new state_empty) in
    inputstate_temp#merge ps.valcache;
    let initial_state = inputstate_temp#copy in

    (* find the expected results *)
    let (ignored, outputstate) = Evalstate.eval progstmt inputstate_temp in
    let out_and_expected = make_expected_pairs outlist outputstate in


    (initial_state, (Evalstate.eval progstmt), out_and_expected)

  let sample_final queries querydefs ps =
    let enddist = ps.PSYS.belief in
      let trips = List.map (make_trip querydefs ps) queries in
      let enddist2 = try (ESYS.psrep_sample enddist !Cmd.opt_samples trips)
                     with e -> enddist in
      let (y,n) = ESYS.get_alpha_beta enddist2 in
      let b_dist = beta (float_of_int (y + 1)) (float_of_int (n + 1)) in
      let { beta_alpha; beta_beta } = b_dist in
      let m_belief = ESYS.psrep_max_belief enddist2 in
      ifsampling (
        printf "\n--- Sampling -------------------\n";
        printf "max-belief (post-sampling): %s\n" (Q.to_string m_belief);
      );
      ifverbose1 (
        printf "alpha: %f, beta: %f\n" beta_alpha beta_beta
      );
      printf "\n";
      let size_z = Z.to_float (ESYS.psrep_size enddist2) in

      let (pmi, pma) = let (i, a) = ESYS.psrep_pmin_pmax enddist2
                       in (Q.float_from i, Q.float_from a) in
      let (smi, sma) = ESYS.psrep_smin_smax enddist2 in
      let (mmi, mma) = let (i, a) = ESYS.psrep_mmin_mmax enddist2
                       in (Q.float_from i, Q.float_from a) in


      (* The following code is using GSL via Pareto, and sometimes
         fails to converge, so we don't rely on it but it can be
         useful to see.
      let _ = try let sminp = ((quantile b_dist 0.001) *. size_z) in (* convert  size_z and the result of quantile to rationals*)
                                                                     (* use floor for sminp *)
                  let smaxp = ((quantile b_dist 0.999) *. size_z) in
                  let mminp = sminp *. pmi in
                  let mmaxp = smaxp *. pma in
                  printf "\n\nsmin (gsl): %f\n" sminp;
                  printf "smax (gsl): %f\n" smaxp;
                  printf "mmin (sampling): %f\n" mminp;
                  printf "mmax (sampling): %f\n" mmaxp;
                  printf "post-sampling revised belief: %f\n" (pma /. mminp);
                  printf "post-sampling revised belief: %f\n" (pma /. mminp);
              with e -> printf "GSL computation did not converge: ERR\n" in
       *)

      (* Output to be processed by bench.hs *)
      ifverbose1 (
        printf "\n\nsize_z = %f\n" size_z;
        printf "pmin = %f\n" pmi;
        printf "pmax = %f\n" pma;
        printf "smin = %s\n" (Z.string_from smi);
        printf "smax = %s\n" (Z.string_from sma);
        printf "mmin = %f\n" mmi;
        printf "mmax = %f\n" mma;
        printf "sample_true = %d\nsample_false = %d\n" y n
      )

  let common_run (queryname, querystmt) querydefs ps_in =
        ifbench Globals.start_timer Globals.timer_query;

        let ps = ps_in in

        let querytuple = List.assoc queryname querydefs  in
        let (inlist, outlist, progstmt) = querytuple in

        let secretvars = ESYS.psrep_vars ps.PSYS.belief in

        (* TODO: Single assignment not working with records, maybe because
         * arguments not being expanded yet and inlist doesn't contain
         * them
         * *)

        ifverbose (
          printf "Outlist: ";
          printf "%s\n" (varid_list_to_string outlist);

          printf "Inlist: ";
          printf "%s\n" (varid_list_to_string inlist);

          (* TODO: Check whether this is necesary for us
          let sa_progstmt = (sa_of_stmt progstmt (List.append secretvars inlist) outlist) in
          let sa_progstmt = (if !Globals.use_dsa then sa_progstmt else progstmt) in
           *)
          (*let sa_progstmt = progstmt in [> Temporary <]*)

          printf "-------------------------------------------------\n";
          printf "query %s from %s to %s\n"
            queryname
            (String.concat " " (List.map Lang.varid_to_string inlist))
            (String.concat " " (List.map Lang.varid_to_string outlist));
          print_stmt progstmt; printf "\n";
          printf "-------------------------------------------------\n%!"
        );

        (* I think we can safely remove static_check 
        let ab_env = map_from_list (List.map (fun x -> x, Static) inlist) in
        let res_map = static_check progstmt ab_env false in

        ifverbose1 (
          printf "The status of arguments and locals:\n\t";
          print_abs_env res_map
        );
        *)

        ifverbose
          (printf "\nquery (single assignment):\n"; print_stmt progstmt; printf "\n");


        let ans = PSYS.policysystem_answer ps (queryname, querytuple) querystmt in
        let res = ans.PSYS.result in
        let ps = PSYS.policysystem_answered ps ans.PSYS.update in

          (match res with
             | RTrueValue (vals) ->
                 (ifverbose1 (printf "*** query was accepted\n"))
                   (* , "results: %s\n" (String.concat " " (List.map (fun (k,v) -> k ^ "=" ^ (string_of_int v)) vals))) *)
             | RReject (reason) ->
                 (printf "*** query was rejected due to: %s\n" reason;
                  printf "*** belief will not be updated as a result of this query\n"))
          ;


        ifbench (
          Globals.stop_timer Globals.timer_query;
          Globals.mark_epoch ();
          Globals.print_epoch ();
          Globals.next_epoch ()
        );

        if !Cmd.opt_blackbox
        then ps_in
        else ps


  let rec pmock_queries count queries querydefs ps_in = match queries with
    | [] -> ps_in
    | (queryname, querystmt) :: t ->
        printf "\n--- Query #%d ------------------\n" count;
        let ps_out = common_run (queryname, querystmt) querydefs ps_in in
        pmock_queries (count + 1) t querydefs ps_out

  let prob_line = Core_extended.Readline.input_line ~prompt:"prob >> "

  let test_pipe r_opt str =
    match r_opt with
      | None -> ()
      | Some r ->
        if str = "read_pipe"
        then let buff = Core_string.create 100 in
             Core.Std.Unix.read r buff;
             printf "%s" (Core_string.to_string buff)

  let interpreter reader_opt count querydefs ps_orig =
      let query_names = List.map (fun (qname, _) -> qname) querydefs in
      let rec interpreter_loop count user_in ps_in =
          match user_in with
            | None -> printf "We're done.\n"; ps_in
            | Some str -> test_pipe reader_opt str;
                if not (List.mem str query_names)
                then (printf "%s is not a valid query.\n" str;
                     printf "Queries Available: \n\n";
                     List.iter (printf "\t%s\n") query_names;
                     ps_in)
                else (let (inlist, outlist, progstmt) = List.assoc str querydefs in
                     let ins = get_query_params str querydefs in
                     let ps_out = if not (all_safe ins)
                                  (* TODO: Print out which inputs failed (snd of the tuple will be None) *)
                                  then (printf "Input(s) are not valid\n"; ps_in)
                                  else (let qstmt = make_int_assignments ins in
                                        common_run (str, qstmt) querydefs) ps_in in
                     interpreter_loop (count + 1)
                                 (prob_line ())
                                 ps_out)
      in
      printf "Queries Available: \n\n";
      List.iter (printf "\t%s\n") query_names;
      let user_in = prob_line () in
      interpreter_loop 0 user_in ps_orig

  let run reader_opt asetup =
    ifverbose1 (printf "Binary for counting: %s\n%!" !Cmd.opt_count_bin;);
    Printexc.record_backtrace true;
(*      let vars = pmock_all_vars asetup in*)
      let secretstmt = Preeval.preeval asetup.secret in
      let beliefstmt = Preeval.preeval asetup.belief in

      (* let (inlist, outlist, progstmt) = asetup.expprog in *)

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
      let sa_beliefstmt = (if !Cmd.opt_dsa then sa_beliefstmt else beliefstmt) in
      (*let sa_beliefstmt = beliefstmt in*)

      let startdist = ESYS.peval_start sa_beliefstmt in

      (*let secretdist = ESYS.psrep_point (ESYS.srep_point secretstate) in*)
      (*let startrelent = ESYS.psrep_relative_entropy startdist secretdist in*)

        printf "secret:\n\t"; secretstate#print; printf "\n";
        ifverbose (printf "initial belief generator:\n"; print_stmt beliefstmt; printf "\n");
        ifverbose (printf "initial belief generator (single assignment):\n"; print_stmt sa_beliefstmt; printf "\n");
        ifverbose1 (printf "initial belief:\n"; ESYS.print_psrep startdist);
        let init_max = ESYS.psrep_max_belief startdist in
        printf "\nInitial max-belief: %s\n" (Gmp.Q.to_string init_max);
        Globals.init_max_belief := Gmp.Q.to_float init_max;
        (*printf "relative entropy (initial -> secret): %f\n" startrelent; *)

        let ps = {PSYS.policies =
            (List.map
               (fun p -> policy_new (policy_record_label p) p.name p.varlist p.param) policies);
                  PSYS.belief = startdist;
                  PSYS.valcache = secretstate} in

          ifdebug (printf "\n\nBefore pmock_queries\n\n");
          let final_dist = if !Cmd.opt_interactive
                           then interpreter reader_opt 1 querydefs ps
                           else pmock_queries 1 queries querydefs ps in
          if !Cmd.opt_count_latte
          then printf "Number of calls to LattE: %d\n" !Globals.latte_count;
          sample_final queries querydefs final_dist

end
;;

(*module EVALS_S = MAKE_EVALS(ESYS_S);;*)
module EVALS_PPSS_POLY = MAKE_EVALS(ESYS_PPSS_POLY);;
module EVALS_PPSS_BOX  = MAKE_EVALS(ESYS_PPSS_BOX);;
module EVALS_PPSS_OCTA = MAKE_EVALS(ESYS_PPSS_OCTA);;
module EVALS_PPSS_OCTALATTE = MAKE_EVALS(ESYS_PPSS_OCTALATTE);;

let main () =
  Arg.parse [
    ("--latte-minmax",
     Arg.Set Cmd.opt_latte_minmax,
     "use latte for maximization, constant 1 for minimization");
    ("--interactive",
     Arg.Set Cmd.opt_interactive,
     "Use prop as a repl");
    ("--dsa",
     Arg.Set Cmd.opt_dsa,
     "convert to dynamic single assignment");
    ("--server",
     Arg.Int (fun i -> Cmd.opt_server_port := i; Cmd.opt_server := true),
     "run TAMBA web service (TM) on specified port");
    ("--precision",
     Arg.Set_int Cmd.opt_precision,
     "set the precision");
    ("--samples",
     Arg.Set_int Cmd.opt_samples,
     "set the number of samples to use");
    ("--blackbox",
     Arg.Set Cmd.opt_blackbox,
     "reset the belief to uniform between each query");
    ("--split-factor",
     Arg.Set_int Cmd.opt_split_factor,
     "set the uniforms split factor, default = 1");
    ("--domain",
     Arg.String Cmd.set_domain,
     "set the PPL domain for evaluation (\"list\", \"box\", \"octa\", \"octalatte\", or \"poly\"), default = \"poly\"");
    ("--barv",
     Arg.Unit (fun () -> Cmd.opt_count_bin := "barvinok_count"),
     "Use `barvinok_count` to count number of points in a polyhedron");
    ("--bench",
     Arg.String (fun s ->
                   if s <> "--" then Globals.set_bench s;
                   Globals.output_bench := true
                ),
     "write out timing information, use -- to designate stdout");
    ("--bakeoff",
     Arg.String (fun s ->
                   if s <> "--" then Globals.set_bench_bakeoff s;
                   Globals.output_bench_bakeoff := true;
                   if s <> "--" then Globals.set_bench s;
                   Globals.output_bench := true;
                   if s <> "--" then Globals.set_bench_latte s;
                   Globals.output_bench_latte := true
                ),
     "compare latte to barvinok -- to designate stdout");
    ("--count-latte",
     Arg.Set Cmd.opt_count_latte,
     "count number of calls to count");
    ("--bench-latte",
     Arg.String (fun s ->
                   if s <> "--" then Globals.set_bench s;
                   Globals.output_bench := true;
                   if s <> "--" then Globals.set_bench_latte s;
                   Globals.output_bench_latte := true
                ),
     "write out latte timing information, use -- to designate stdout");
    ("--verbose",
     Arg.Set_int Cmd.opt_verbose,
     "verbose output (0, 1, 2)");
    ("--debug",
     Arg.Set Cmd.opt_debug,
     "debug output");
    ("--inline",
     Arg.Set Cmd.opt_inline,
     "Perform an inlining transformation on the queries before execution");
    ("--simplify",
     Arg.String Cmd.set_simplify,
     "precision simplifier (\"halfs\", \"simple\", \"slack\", \"random\"), default = \"halfs\"");
    ("--seed",
     Arg.Set_int Cmd.opt_seed,
     "set the random seed, default 0")
    ] (function s -> Cmd.input_file := s) "";

  ifdebug (Printexc.record_backtrace true);
  Random.init(!Cmd.opt_seed);

  try

    let prob reader_opt () =
      (* note to self: why is this called pmock? What is pmock? *)
      let policy = parse !Cmd.input_file Parser.pmock in

      ifbench (add_policy_records policy;
               Globals.print_header ());
      Globals.bench_latte_out_header ();

      let module Runner =
        (val (match !Cmd.opt_domain with
              | 0 -> raise (General_error "list-based evaluation not implemented")
              | 1 -> (module EVALS_PPSS_BOX : EXP_SYSTEM)
              | 2 -> (module EVALS_PPSS_OCTA : EXP_SYSTEM)
              | 1 -> (module EVALS_PPSS_BOX : EXP_SYSTEM)
              | 2 -> (module EVALS_PPSS_OCTA : EXP_SYSTEM)
              | 3 -> (module EVALS_PPSS_OCTALATTE : EXP_SYSTEM)
              | 4 -> (module EVALS_PPSS_POLY : EXP_SYSTEM)
              | _ -> raise Not_expected) : EXP_SYSTEM) in

      ifdebug(printf "\n\nBefore evaluation\n\n");

      Runner.run reader_opt policy;

      ifdebug(printf "\n\nAfter evaluation\n\n";
              printf "maximum complexity encountered = %d\n" !Globals.max_complexity;
              Globals.close_bench ());
      Globals.bench_latte_close () in


    if !Cmd.opt_server
    then let (r,w) = pipe () in
         match fork () with
           | `In_the_parent pid -> Server.start_server pid !Cmd.opt_server_port w ();
           | `In_the_child -> prob (Some r) ();
    else prob None ();

  with
    | e ->
       Unix.chdir Globals.original_dir;
       ifdebug(printf "%s\n" (Core.Backtrace.Exn.most_recent ()););
       raise e
;;

main ()