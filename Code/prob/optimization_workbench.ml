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
  val pmock: Pdefs.tpmocksetup -> unit
end;;

module MAKE_EVALS (ESYS: EVAL_SYSTEM) = struct
  module PSYS = MAKE_PSYSTEM(ESYS)

  let rec pmock_queries queries querydefs s_vars =
    match queries with
      | [] -> ()
      | (queryname, querystmt) :: t ->

        let querytuple = List.assoc queryname querydefs  in
        let (inlist, outlist, progstmt) = querytuple in
        printf "Outlist: ";
        printf "%s\n" (varid_list_to_string outlist);


        (* TODO: Single assignment not working with records, maybe because
         * arguments not being expanded yet and inlist doesn't contain
         * them
         * *)

        printf "Inlist: ";
        printf "%s\n" (varid_list_to_string inlist);

        printf "query %s from %s to %s\n"
          queryname
          (String.concat " " (List.map Lang.varid_to_string inlist))
          (String.concat " " (List.map Lang.varid_to_string outlist));
        print_stmt progstmt; printf "\n";
        printf "-------------------------------------------------\n";

        (*-------------------------------------------------------------------*)
        (*---------------------------- Original -----------------------------*)
        (*-------------------------------------------------------------------*)
        let ab_env = map_from_list (List.map (fun x -> x, Static) inlist) in
        let res_map = static_check progstmt ab_env false in
        printf "The status of arguments and locals:\n\t";
        print_abs_env res_map;
        printf "\n\n";

        let flipped = flip_seq progstmt in
        let fannotated = ann_use_def flipped in
        print_stmt (fannotated); printf "\n--------------\n";

        (*-------------------------------------------------------------------*)
        (*---------------------------- Liveness -----------------------------*)
        (*-------------------------------------------------------------------*)
        let rec fix_live i vs stmts =
          let lived = liveness_analysis_rev stmts vs in
          let res = if equal_stmts stmts lived
                    then lived
                    else fix_live (i + 1) vs lived in
          res in
        let analed = fix_live 0 outlist fannotated in
        print_stmt (flip_seq analed); printf "\n--------------\n";

        (*-------------------------------------------------------------------*)
        (*---------------------------- Liveness -----------------------------*)
        (*-------------------------------------------------------------------*)
        let ignored_vids = List.concat [s_vars;inlist] in
        let (_,rewritten) = rewrite_stmt (flip_seq analed) ignored_vids [] in
        print_stmt rewritten; printf "\n--------------\n";

        pmock_queries t querydefs s_vars

  let pmock asetup =
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
      let sa_beliefstmt = (if !Globals.use_dsa then sa_beliefstmt else beliefstmt) in
      (*let sa_beliefstmt = beliefstmt in*)

      let startdist = ESYS.peval_start sa_beliefstmt in

      pmock_queries queries querydefs secretvars

end
;;

module EVALS_PPSS_BOX  = MAKE_EVALS(ESYS_PPSS_BOX);;

let main () =
  let infile = ref "-" in
  Arg.parse [] (function s -> infile := s) "";
  let aexperiment = parse !infile Parser.pmock in
  let module E =
    (val (module EVALS_PPSS_BOX: EXP_SYSTEM): EXP_SYSTEM) in
    E.pmock aexperiment

;;

main();;

