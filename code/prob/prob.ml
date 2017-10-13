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
open Gen_poly
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
  val run: (Core.Std.Unix.File_descr.t * Core.Std.Unix.File_descr.t) option -> Pdefs.tpmocksetup -> unit
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
                     with e -> (print_endline "sampling exception! No update."); enddist in
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

  let common_run (queryname, querystmt) ids conc_res querydefs ps_in =
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


        let ans = PSYS.policysystem_answer ps (queryname, querytuple) ids conc_res querystmt in
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
        let ps_out = common_run (queryname, querystmt) [] RunConc querydefs ps_in in
        pmock_queries (count + 1) t querydefs ps_out

  (* Lower Bound additions <begin> *)

  let run_query query querydefs init =
    (* initial public state *)
    let (qname, qstmt) = query in
    let (_, instate) = Evalstate.eval qstmt (new state_empty) in
    instate#merge init;

    (* run query *)
    let (_, _, pstmt) = List.assoc qname querydefs in
    let (_, outstate) = Evalstate.eval pstmt instate in
    outstate#copy

  let run_queries queries querydefs init =
    let pair_up query =
      let (qname, _) = query in
      let (_, outs, _) = List.assoc qname querydefs in
      make_expected_pairs outs (run_query query querydefs init) in
    List.map pair_up queries

  let sym_query query querydefs init =
    (* initial public state *)
    let (qname, qstmt) = query in
    let (_, instate) = Evalsymstate.eval qstmt init in

    (* sym query *)
    let (ins, outs, pstmt) = List.assoc qname querydefs in
    let (_, outstate) = Evalsymstate.eval pstmt instate in
    outstate

  let sym_queries queries querydefs init =
    let final = List.fold_left (fun st q -> sym_query q querydefs { init with pc = st.Symstate.pc }) init queries in
    final.pc

  let check_sample queries querydefs expected st =
    let actual = run_queries queries querydefs st in
    expected = actual

  let underapproximate belief queries querydefs st =
    let init = Symstate.state_to_symstate st in
    let pc : Symbol.lsym = sym_queries queries querydefs init in

    let rec belief_bounds (belief : stmt) (acc : (int * int) VarIDMap.t) : (int * int) VarIDMap.t =
      match belief with
      | SSeq (s1, s2) ->
         let s1_bounds = belief_bounds s1 acc in
         let s2_bounds = belief_bounds s2 s1_bounds in
         s2_bounds
      | SUniform (name, lower, upper) ->
         VarIDMap.add name (lower, upper) acc
      | SAssign (name, AEInt b) ->
         VarIDMap.add name (b, b) acc
      | _ -> acc
    in

    let belief' = belief_bounds belief VarIDMap.empty in
    let linear_system = Symbol.linear_system_of_lsym pc in

    (* now that we have a Volume Computation IR, we can use it to solve on any backend that handles it *)
    let p : gen_poly = { bounds = belief'; constraints = linear_system } in

    let ret =
      (match !Cmd.opt_volume_computation with
       | 0 -> List.map Latte.count_models (latte_of_gen_poly p)
       | 1 -> List.map Volcomp.count_models (volcomp_of_gen_poly p)
       | _ -> raise (General_error ("opt_volume_computation not valid, shouldn't be possible... should be caught with arg parsing"))) in

    let (ret, max_idx, _) = List.fold_left (fun (max, max_idx, idx) curr ->
                                if Z.compare curr max >= 0 then
                                  (curr, idx, idx + 1)
                                else
                                  (max, max_idx, idx + 1)) (Z.zero, 0, 0) ret in

    ifverbose1 (print_endline ("count {\n" ^ (string_of_gen_poly p) ^ "\n} = " ^ (Z.to_string ret) ^ "\n"));
    (ret, List.nth (poly_of_gen_poly p) max_idx)

  (* Lower Bound additions <end> *)

  let prob_line = Core_extended.Readline.input_line ~prompt:"prob >> "

  let manage_models qn ids model ps_ins ps_orig =
      let ps_ins2 = List.remove_assoc model ps_ins in
      let exists = List.mem_assoc model ps_ins in
      match qn with
      | "init_model"   -> let ps_outs = (model, ps_orig) :: ps_ins2 in
                          let msg = if exists
                                    then "model reinitialized\n"
                                    else "model initialized\n" in
                          (msg, ps_outs)
      | "delete_model" -> let ps_outs = ps_ins2 in
                          let msg = if exists
                                    then "Model deleted.\n"
                                    else "Model does not exist.\n" in
                          (msg, ps_outs)
      | "get_leakage"  -> let msg = if exists
                                    then let model_ps = List.assoc model ps_ins in
                                         let var_ids = List.map (fun x -> ("", x)) ids in
                                         let ps_vars = ESYS.psrep_on_vars (model_ps.PSYS.belief) var_ids in
                                         let max_belief = ESYS.psrep_max_belief ps_vars in
                                         string_of_float (Gmp.Q.to_float max_belief) ^ "\n"
                                    else "Model does not exist.\n" in
                          (msg, ps_ins)

  let manage_query querydefs ps_ins ps_orig (qn, ins, model, ids, res) =
      let (inlist, outlist, progstmt) = try List.assoc qn querydefs
                                        with e -> raise (General_error qn) in
      ifdebug (printf "inlist: %s\n%!" (varid_list_to_string inlist));

      (* We get the actual model under test from our assoc list (ps_in)
       * and then create a list that does not have it as a member (ps_ins2)
       *
       * The reason we do that latter now is because we are a bit memory
       * constrained and we want to be able to garbage collect the old one
       * ASAP as we are a analysing the query.
       *)
      let ps_in  = try List.assoc model ps_ins
                   with e -> raise (General_error ("model number " ^
                                                   (string_of_int model) ^
                                                   " not in ps_ins." ^
                                                   " This should not happen\n")) in
      let ps_ins2 = List.remove_assoc model ps_ins in

      (* Here we create the preamble for the concrete execution and then run
       * the analysis *)
      let ins2 = List.map (fun (n,x) -> (n, Some x)) ins in
      let ps_out = let qstmt = make_int_assignments ins2 in
                   common_run (qn, qstmt) ids res querydefs ps_in in
      ifdebug (printf "Query has been run\n%!");

      (* Once we've run the analysis we produce our response string and
       * package up the new assoc list of models *)
      let rev_belief = ESYS.psrep_max_belief ps_out.belief in
      ifdebug (printf "Max belief: %s\n%!" (Q.to_string rev_belief));
      (* lg (U/V) == lg U - lg V *)
      (* TODO: This is the leakage computation, SRI doesn't care
       * about this right now
       *
       * let cuma_leakage = lg (Gmp.Q.to_float rev_belief) -. lg (!Globals.init_max_belief) in
       *)
      let msg = string_of_float (Gmp.Q.to_float rev_belief) ^ "\n" in

      let ps_out = match res with
                   | Static -> ps_in
                   | _      -> ps_out in
      let ps_outs = (model, ps_out) :: ps_ins2 in
      (msg, ps_outs)

  let server (p_read, p_write) querydefs ps_orig =
      let query_names = List.map (fun (qname, _) -> printf "qname: %s\n%!" qname; qname) querydefs in
      let rec server_loop ps_ins =
          ifdebug (printf "Top of server_loop\n%!");
          let cmd = Pervasives.input_line p_read in
          ifdebug (printf "The command: %s\n" cmd);
          let cmd_info = Json.parse_query_json cmd in
          let (qn, ins, model, ids, res) = cmd_info in
          let (msg, ps_outs) = if qn = "init_model" || qn = "delete_model" || qn = "get_leakage"
                               then manage_models qn ids model ps_ins ps_orig
                               else manage_query querydefs ps_ins ps_orig cmd_info in
          output_string p_write msg;
          ifdebug (printf "%s has been written to p_write\n%!" msg);
          flush p_write;
          ifdebug (printf "p_write has been flushed\n%!");
          server_loop ps_outs
      in
      server_loop []

  let interpreter count querydefs ps_orig =
      let query_names = List.map (fun (qname, _) -> qname) querydefs in
      let rec interpreter_loop count user_in ps_in =
          match user_in with
            | None -> printf "We're done.\n"; ps_in
            | Some str ->
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
                                        (* TODO(ins): integrate `ids` into interpreter *)
                                        common_run (str, qstmt) [] RunConc querydefs) ps_in in
                     interpreter_loop (count + 1)
                                 (prob_line ())
                                 ps_out)
      in
      printf "Queries Available: \n\n";
      List.iter (printf "\t%s\n") query_names;
      let user_in = prob_line () in
      interpreter_loop 0 user_in ps_orig

  let run pipe_opt asetup =
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

          let base_final_dist =
                if !Cmd.opt_interactive
                then interpreter 1 querydefs ps
                else if !Cmd.opt_server
                then (match pipe_opt with
                      | None       -> raise (General_error "server failed to allocate pipes")
                      | Some (r,w) -> let r_chan = in_channel_of_descr r in
                                      let w_chan = out_channel_of_descr w in
                                      server (r_chan, w_chan) querydefs ps)
                else pmock_queries 1 queries querydefs ps in

          (* Lower Bound work <begin> *)
          (if !Cmd.opt_improve_lower_bounds > 0 then
             printf "\n--- Improve Lower Bounds ------------------- \n"
           else
             ());
          let improved_final_dist =
            if !Cmd.opt_improve_lower_bounds > 0 then
              let expected = run_queries queries querydefs base_final_dist.PSYS.valcache in (* run queries with our secret values *)
              let checker = check_sample queries querydefs expected in (* takes sample, run queries with sample, compare against expected *)
              let runner = underapproximate beliefstmt queries querydefs in (* takes sample, symbolically run sample, perform counting *)
              let belief_new = ESYS.psrep_improve_lower_bounds
                                 checker
                                 runner
                                 base_final_dist.PSYS.valcache
                                 !Cmd.opt_improve_lower_bounds
                                 base_final_dist.PSYS.belief in
              { base_final_dist with belief = belief_new }
            else
              base_final_dist in
          (if !Cmd.opt_improve_lower_bounds > 0 then
            let m_belief = ESYS.psrep_max_belief improved_final_dist.belief in
            printf "max-belief (after improve lower bounds): %s\n" (Q.to_string m_belief)
          else
            ());

          (* Lower Bound work <end> *)

          if !Cmd.opt_count_latte
          then printf "Number of calls to LattE: %d\n" !Globals.latte_count;
          sample_final queries querydefs improved_final_dist
  (*with
      | e ->
          printf "%s\n" (Printexc.to_string e);
          Printexc.print_backtrace stdout*)

end
;;

(*module EVALS_S = MAKE_EVALS(ESYS_S);;*)
module EVALS_PPSS_POLY = MAKE_EVALS(ESYS_PPSS_POLY);;
module EVALS_PPSS_BOX  = MAKE_EVALS(ESYS_PPSS_BOX);;
module EVALS_PPSS_OCTA = MAKE_EVALS(ESYS_PPSS_OCTA);;
module EVALS_PPSS_OCTALATTE = MAKE_EVALS(ESYS_PPSS_OCTALATTE);;
module EVALS_PDPSS_POLY = MAKE_EVALS(ESYS_PDPSS_POLY);;
module EVALS_PDPSS_RBOX = MAKE_EVALS(ESYS_PDPSS_RBOX);;

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
     Arg.Int (fun i -> Cmd.opt_server_port := i;
                       Cmd.opt_server := true),
     "run TAMBA web service (TM) on specified port");
    ("--precision",
     Arg.Set_int Cmd.opt_precision,
     "set the precision");
    ("--precise-conditioning",
     Arg.Set Cmd.opt_precise_conditioning,
     "use polyhedral intersection for conditional statements, otherwise use default domain, default = false");
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
     "set the PPL domain for evaluation (\"list\", \"box\", \"octa\", \"octalatte\", \"decomposed-poly\", \"relbox\", or \"poly\"), default = \"poly\"");
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
    ("--improve-lower-bounds",
     Arg.Set_int Cmd.opt_improve_lower_bounds,
     "Use sampling w/ path conditions + LattE to improve s_min");
    ("--volume_computation",
     Arg.String Cmd.set_volume_computation,
     "Choose tool to perform volume computation during lower bound improvement (\"latte\", or \"volcomp\") default = \"latte\"");
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

    let policy = parse !Cmd.input_file Parser.pmock in
    let prob pipe_opt () =
      (* note to self: why is this called pmock? What is pmock? *)

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
              | 5 -> (module EVALS_PDPSS_POLY : EXP_SYSTEM)
              | 6 -> (module EVALS_PDPSS_RBOX : EXP_SYSTEM)
              | _ -> raise Not_expected) : EXP_SYSTEM) in

      ifdebug(printf "\n\nBefore evaluation\n\n");

      Runner.run pipe_opt policy;

      ifdebug(printf "\n\nAfter evaluation\n\n";
              printf "maximum complexity encountered = %d\n" !Globals.max_complexity;
              Globals.close_bench ());
      Globals.bench_latte_close () in


    if !Cmd.opt_server
    then let (prob_r, server_w) = pipe () in
         let (server_r, prob_w) = pipe () in
         match fork () with
           | `In_the_parent pid -> close prob_r;
                                   close prob_w;
                                   Server.start_server pid
                                                       !Cmd.opt_server_port
                                                       (server_r, server_w)
                                                       policy.querydefs;
           | `In_the_child -> close server_w;
                              close server_r;
                              prob (Some (prob_r, prob_w)) ();
    else prob None ();

  with
    | e ->
       Unix.chdir Globals.original_dir;
       ifdebug(printf "%s\n" (Core.Backtrace.Exn.most_recent ()););
       raise e
;;

main ()
