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

  let rec pmock_queries queries querydefs s_vars s_state =
    match queries with
      | [] -> ()
      | (queryname, querystmt) :: t ->

        let querytuple = List.assoc queryname querydefs  in
        let (inlist, outlist, progstmt) = querytuple in
        printf "Outlist: ";
        printf "%s\n" (varid_list_to_string outlist);

        let (ignored, inputstate) = Evalstate.eval querystmt (new state_empty) in

        let expanded_inlist =
          List.fold_left (fun a varname ->
              if (inputstate#is_record (varname)) then
                let (_, varname_str) = varname in
                (List.map (fun (agent, field) -> (agent, varname_str^"."^field)) (inputstate#get_vals (varname)))@a
              else varname::a) [] inlist in

        inputstate#project expanded_inlist;
        let inputstate_orig = inputstate#copy in

        (* TODO: Single assignment not working with records, maybe because
         * arguments not being expanded yet and inlist doesn't contain
         * them
         * *)

        printf "Inlist: ";
        printf "%s\n" (varid_list_to_string inlist);

        printf "Expanded Inlist: ";
        printf "%s\n" (varid_list_to_string inlist);

        (*-------------------------------------------------------------------*)
        (*---------------------------- Original -----------------------------*)
        (*-------------------------------------------------------------------*)

        printf "query %s from %s to %s\n"
          queryname
          (String.concat " " (List.map Lang.varid_to_string inlist))
          (String.concat " " (List.map Lang.varid_to_string outlist));
        print_stmt progstmt; printf "\n";
        printf "-------------------------------------------------\n";

        let flipped = flip_seq (add_halt progstmt) in
        let fannotated = ann_use_def flipped in
        (* only need to print when debugging *)
        print_stmt (fannotated); printf "\n----- ^annotated ---------\n";

        (*-------------------------------------------------------------------*)
        (*---------------------------- Liveness -----------------------------*)
        (*-------------------------------------------------------------------*)
        let rec fix_live i vs stmts =
          print_stmt (stmts); printf "\n----- ^input to fix_live %d ---------\n" i;
          let lived = liveness_analysis_rev stmts vs in
          let res = if equal_stmts stmts lived
                    then lived
                    else fix_live (i + 1) vs lived in
          res in
        let analed = flip_seq (fix_live 0 outlist fannotated) in
        (* only need to print when debugging *)
        print_stmt (analed); printf "\n----- ^analyzed ---------\n";

        (*-------------------------------------------------------------------*)
        (*---------------------------- Inlining -----------------------------*)
        (*-------------------------------------------------------------------*)
        let ignored_vids = List.concat [s_vars;expanded_inlist] in
        let inlinable    = List.map (fun x -> (x, AEInt (inputstate#get x))) expanded_inlist in
        let (_, _,rewritten) = rewrite_stmt analed ignored_vids inlinable in
        let clean = rem_redundant_decl rewritten in
        print_stmt clean; printf "\n----- ^rewritten ---------\n";

        printf "query (without inlining) %s" queryname; inputstate_orig#print_as_args;
        inputstate_orig#merge s_state;
        let (_, outputstate) = Evalstate.eval progstmt inputstate_orig in
        outputstate#project outlist;
        printf " = "; outputstate#print_as_args; printf "\n";

        printf "query (with inlining) %s" queryname; inputstate#print_as_args;
        inputstate#merge s_state;
        let (_, outputstate) = Evalstate.eval rewritten inputstate in
        outputstate#project outlist;
        printf " = "; outputstate#print_as_args; printf "\n";

        pmock_queries t querydefs s_vars s_state

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

      let startdist = ESYS.peval_start sa_beliefstmt in

      pmock_queries queries querydefs secretvars secretstate

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

