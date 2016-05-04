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

open Maths
open Gmp

let add_policy_records aexp =
  List.iter
    (fun p ->
       Globals.new_record (policy_record_label p) "")
    aexp.policies;;

module type EXP_SYSTEM = sig
  val pmock: Pdefs.tpmocksetup -> unit
end;;

module MAKE_EVALS (ESYS: EVAL_SYSTEM) = struct
  module PSYS = MAKE_PSYSTEM(ESYS)

  let make_trip querydefs (ps : PSYS.policysystem) (queryname, querystmt) =
    let querytuple = List.assoc queryname querydefs in
    let (inlist, outlist, progstmt) = querytuple in
    let (ignored, inputstate_temp) = Evalstate.eval querystmt (new state_empty) in
    inputstate_temp#merge ps.valcache;

    (*
    printf "\n\n---------------------------------\n";
    printf "State in triple: %s\n\n" inputstate_temp#to_string;
    printf "query in triple:\n"; print_stmt progstmt; printf "\n\n";
    printf "outlist in triple: %s\n\n" (varid_list_to_string outlist);
    printf "---------------------------------\n";
    (* printf "\n\n------------------------------\nState before sampling: %s\n" inputstate_temp#to_string; *)
    *)
    (inputstate_temp, (Evalstate.eval progstmt), (list_first outlist))

  let sample_final queries querydefs ps =
    let enddist = ps.PSYS.belief in
      let trips = List.map (make_trip querydefs ps) queries in
      let (y,n) = try (ESYS.get_alpha_beta
                         (ESYS.psrep_sample
                                 enddist
                                 !Globals.sample_count
                                 trips))
                  with e -> (0,0) in
      let b_dist = beta (float_of_int (y + 1)) (float_of_int (n + 1)) in
      let { beta_alpha; beta_beta } = b_dist in
      let m_belief = ESYS.psrep_max_belief enddist in
      printf "max belief: %f\n" (Q.float_from m_belief);
      printf "alpha: %f, beta: %f\n" beta_alpha beta_beta;
      let size_z = Z.to_float (ESYS.psrep_size enddist) in

      let (pmi, pma) = let (i, a) = ESYS.psrep_pmin_pmax enddist
                       in (Q.float_from i, Q.float_from a) in
      let (smi, sma) = ESYS.psrep_smin_smax enddist in
      let (mmi, mma) = let (i, a) = ESYS.psrep_mmin_mmax enddist
                       in (Q.float_from i, Q.float_from a) in

      let _ = try let sminp = ((quantile b_dist 0.001) *. size_z) in
                  let smaxp = ((quantile b_dist 0.999) *. size_z) in
                  let mminp = sminp *. pmi in
                  let mmaxp = smaxp *. pma in
                  printf "\n\nsmin (gsl): %f\n" sminp;
                  printf "smax (gsl): %f\n" smaxp;
                  printf "mmin (sampling): %f\n" mminp;
                  printf "mmax (sampling): %f\n" mmaxp;
                  printf "post-sampling revised belief: %f\n" (pma /. mminp);
              with e -> printf "GSL computation did not converge: ERR\n" in

      printf "\n\nsize_z = %f\n" size_z;
      printf "pmin = %f\n" pmi;
      printf "pmax = %f\n" pma;
      printf "smin = %s\n" (Z.string_from smi);
      printf "smax = %s\n" (Z.string_from sma);
      printf "mmin = %f\n" mmi;
      printf "mmax = %f\n" mma;
      printf "sample_true = %d\nsample_false = %d\n" y n


  let rec pmock_queries queries querydefs ps_in = match queries with
    | [] -> ps_in
    | (queryname, querystmt) :: t ->
        ifbench Globals.start_timer Globals.timer_query;

        let ps = ps_in in

        let querytuple = List.assoc queryname querydefs  in
        let (inlist, outlist, progstmt) = querytuple in
        printf "Outlist: ";
        printf "%s\n" (varid_list_to_string outlist);

        let secretvars = ESYS.psrep_vars ps.PSYS.belief in

        (* TODO: Single assignment not working with records, maybe because
         * arguments not being expanded yet and inlist doesn't contain
         * them
         * *)

        let sa_progstmt = (sa_of_stmt progstmt (List.append secretvars inlist) outlist) in
        let sa_progstmt = (if !Globals.use_dsa then sa_progstmt else progstmt) in
        (*let sa_progstmt = progstmt in [> Temporary <]*)

          printf "-------------------------------------------------\n";
          printf "query %s from %s to %s\n"
            queryname
            (String.concat " " (List.map Lang.varid_to_string inlist))
            (String.concat " " (List.map Lang.varid_to_string outlist));
          print_stmt progstmt; printf "\n";

          ifverbose
            (printf "query (single assignment):\n"; print_stmt sa_progstmt; printf "\n");

          let ans = PSYS.policysystem_answer ps querytuple querystmt in
          let res = ans.PSYS.result in
          let ps = PSYS.policysystem_answered ps ans.PSYS.update in

            (match res with
               | RTrueValue (vals) ->
                   (printf "*** query was accepted\n")
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

            flush stdout;

          let ps_out = if !Globals.black_box
                       then ps_in
                       else ps in

          pmock_queries t querydefs ps_out

  let pmock asetup =
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
      let sa_beliefstmt = (if !Globals.use_dsa then sa_beliefstmt else beliefstmt) in
      (*let sa_beliefstmt = beliefstmt in*)

      let startdist = ESYS.peval_start sa_beliefstmt in

      (*let secretdist = ESYS.psrep_point (ESYS.srep_point secretstate) in*)
      (*let startrelent = ESYS.psrep_relative_entropy startdist secretdist in*)

        printf "secret:\n\t"; secretstate#print; printf "\n";
        ifverbose (printf "initial belief generator:\n"; print_stmt beliefstmt; printf "\n");
        ifverbose (printf "initial belief generator (single assignment):\n"; print_stmt sa_beliefstmt; printf "\n");
        printf "initial belief:\n"; ESYS.print_psrep startdist;
        (*printf "relative entropy (initial -> secret): %f\n" startrelent; *)

        let ps = {PSYS.policies =
            (List.map
               (fun p -> policy_new (policy_record_label p) p.name p.varlist p.param) policies);
                  PSYS.belief = startdist;
                  PSYS.valcache = secretstate} in

          ifdebug (printf "\n\nBefore pmock_queries\n\n");
          let final_dist = pmock_queries queries querydefs ps in
          sample_final queries querydefs final_dist
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

let main () =
  let infile = ref "-" in
  let opt_esys = ref 3 in
  let opt_pmock = ref 1 in
  let seed = ref 0 in
    Arg.parse [
      ("--use-latte-minmax",
       Arg.Set Globals.use_latte_minmax,
       "use latte for maximization, constant 1 for minimization");
      ("--use-dsa",
       Arg.Set Globals.use_dsa,
       "convert to dynamic single assignment");
      ("--precision",
       Arg.Set_int Globals.precision,
       "set the precision");
      ("--samples",
       Arg.Set_int Globals.sample_count,
       "set the number of samples to use");
      ("--blackbox",
       Arg.Set Globals.black_box,
       "reset the belief to uniform between each query");
      ("--split-factor",
       Arg.Set_int Globals.split_uniforms_factor,
       "set the uniforms split factor, default = 1");
      ("--pmock",
       Arg.Unit (fun () -> opt_pmock := 1),
       "run the mock policy");
      ("--domain",
       Arg.String (fun (s) ->
                     opt_esys :=
                       (match s with
                          | "octalatte" -> 4
                          | "octa" -> 2
                          | "poly" -> 3
                          | "box"  -> 1
                          | "list" -> 0
                          | _ -> raise (General_error ("unknown domain: " ^ s)))),
       "base domain: list, box, octalatte, poly");
      ("--bench",
       Arg.String (fun s ->
                     if s <> "--" then Globals.set_bench s;
                     Globals.output_bench := true
                  ),
       "write out timing information, use -- to designate stdout");
      ("--bench-latte",
       Arg.String (fun s ->
                     if s <> "--" then Globals.set_bench_latte s;
                     Globals.output_bench_latte := true
                  ),
       "write out latte timing information, use -- to designate stdout");
      ("--verbose",
       Arg.Set Globals.output_verbose,
       "verbose output");
      ("--debug",
       Arg.Set Globals.output_debug,
       "debug output");
      ("--simplify",
       Arg.String (fun (s) ->
                     Globals.simplifier :=
                       (match s with
                          | "simple" -> 1
                          | "halfs"  -> 0
                          | "random" -> 3
                          | "slack"  -> 2
                          | _ -> raise (General_error ("unknown simplifier: " ^ s)))),
       "precision simplifier: simple, halfs, random, slack");
      ("--seed",
       Arg.Set_int seed,
       "set the random seed, default 0")
        ] (function s -> infile := s) "";
    (*if !opt_esys <> 3 then Globals.split_uniforms := true;*)
    Random.init(!seed);
    ifdebug Printexc.record_backtrace true;
    try
      let aexperiment = parse !infile Parser.pmock in
        ifbench (add_policy_records aexperiment;
                 Globals.print_header ());
        Globals.bench_latte_out_header ();
        let module E =
          (val (match !opt_esys with
                  | 0 -> (*printf "running with lists\n";
                      (module EVALS_S: EXP_SYSTEM)*)
                      raise (General_error "list-based peval not implemented")
                  | 1 -> (module EVALS_PPSS_BOX: EXP_SYSTEM)
                  | 2 -> (module EVALS_PPSS_OCTA: EXP_SYSTEM)
                  | 3 -> (module EVALS_PPSS_POLY: EXP_SYSTEM)
                  | 4 -> (module EVALS_PPSS_OCTALATTE: EXP_SYSTEM)
                  | _ -> raise Not_expected): EXP_SYSTEM) in
          ifdebug (printf "\n\nBefore pmock\n\n");
          E.pmock aexperiment;
          ifdebug (printf "\n\nAfter pmock\n\n");
          ifdebug (printf "maximum complexity encountered = %d\n" !Globals.max_complexity);
          ifbench (Globals.close_bench ());
          Globals.bench_latte_close ();
(*          printf "simplification steps: %d\n" !Globals.simplify_steps;
          printf "no errors\n"*)
    with
      | e ->
          Unix.chdir Globals.original_dir;
          ifdebug (printf "%s\n" (Printexc.to_string e);
                   Printexc.print_backtrace stdout);
          raise e
;;

main();;
