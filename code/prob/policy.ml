open State
open Util
open Esys
open Lang
open List
open Printf
open Gmp
open Gmp.Q.Infixes
open Pdefs
open Preeval
open Optimize

type policytype =
  | PMinRelEnt
  | PMaxProbOut
  | PMaxProbAll
  | PMaxProb
;;

type policy = {plabel: string;
               ptype: policytype;
               pvars: Lang.varid list;
               pparam: Q.t};;

let policytype_of_string pname =
  match pname with
  | "max_prob_output" -> PMaxProbOut
(*  | "max_prob_all"    -> PMaxProbAll
    | "max_prob"        -> PMaxProb*)
  | "min_rel_entropy" -> PMinRelEnt
  | _ -> raise (General_error ("unknown policy type " ^ pname))
;;

let policy_new plabel pname vars param = {plabel = plabel;
                                          ptype = policytype_of_string pname;
                                          pvars = vars;
                                          pparam = param}


let string_of_policytype pt = match pt with
  | PMinRelEnt -> "min_rel_entropy (minimum relative entropy)"
  | PMaxProbOut -> "max_prob_output (output-based min-entropy)"
  | PMaxProbAll -> "max_prob_all (min-entropy)"
  | PMaxProb -> "max_prob"
;;

let string_of_policy p =
  let string_ptype = string_of_policytype p.ptype in
  let string_vars = String.concat " " (List.map Lang.varid_to_string p.pvars) in
  "policy type " ^ string_ptype ^ " of vars " ^ string_vars ^ " with param " ^ (Q.to_string p.pparam)
;;

type queryresult =
  | RTrueValue of (string * int) list
  | RReject of string

module MAKE_PSYSTEM (ESYS: EVAL_SYSTEM) = struct
  type policysystem =
    {policies: policy list;
     belief: ESYS.psrep;
     valcache: state}

  type policysystemupdate =
    {newbelief: ESYS.psrep}

  type policysystemresult =
    {result: queryresult;
     update: policysystemupdate}

  let policysystem_of_list plist (belief: ESYS.psrep) (secret: state) =
    let pols = List.map (fun p ->
        policy_new (policy_record_label p) p.name p.varlist p.param) plist in
    {policies = pols;
     belief = belief;
     valcache = secret}

  let find_max_belief distout outputs pvars =
    (* let onvars = ESYS.psrep_on_vars distout outputs in *)
    let outstates = ESYS.psrep_enum_on_vars distout outputs in
    list_max (List.map
                (fun s ->
                   ifverbose (printf "-- possible output = "; s#print; printf "\n";
                              flush stdout);
                   (let revised = (ESYS.psrep_on_vars (ESYS.psrep_given_state distout s) pvars) in
                    (let m = ESYS.psrep_max_belief revised in
                     ifverbose (
                       printf "revised belief for this output:\n";
                       ESYS.print_psrep revised;
                       printf "max belief for this output= %s\n" (Q.to_string m);
                       flush stdout);
                     m
                    )))
                outstates)

  let policy_eval
      (p: policy)
      (distout: ESYS.psrep)
      (distbelief: ESYS.psrep)
      (distactual: ESYS.psrep)
      (outputs: Lang.varid list)
    : bool =
    (* TODO: the below needs to be fixed to include variables that are
       not mentioned in the policy but are used to probabilistically
       build up the ones mentioned *)
    match p.ptype with
    | PMinRelEnt ->
      let bred = ESYS.psrep_on_vars distbelief p.pvars in
      let ared = ESYS.psrep_on_vars distactual p.pvars in
      let relent = ESYS.psrep_relative_entropy bred ared in
      if relent < (Q.to_float p.pparam) then
        (printf "*** relative entropy was %f but needed at least %s ***\n" relent (Q.to_string p.pparam); false)
      else true
    | PMaxProbOut ->
      let max_prob = find_max_belief distout outputs p.pvars in
      printf "-- overall max_belief = %s = %f\n" (Q.to_string max_prob) (Q.to_float max_prob);
      ifbench Globals.set_record p.plabel (string_of_float (Q.to_float max_prob));
      (max_prob < p.pparam)
            (*
            let onvars = ESYS.psrep_on_vars distout outputs in
            let outstates = ESYS.psrep_enum onvars in
              (try
                 (List.iter
                    (fun s ->
                       (let revised =
                          (ESYS.psrep_on_vars
                             (ESYS.psrep_given_state distout s)
                             p.pvars) in
                        let maxprob = (ESYS.psrep_max_belief revised) in
                          s#print; printf "\n";
                          ESYS.print_psrep revised;
                          printf "max belief: %f\n" maxprob;
                          if (maxprob > p.pparam) then raise Loop_exit else ()
                       ))
                    outstates);
                 true
               with
                 | Loop_exit -> false)
            *)
    | PMaxProbAll -> true (* find max prob over all states in belief *)
    | PMaxProb -> true (* sample the probability of the secret in the belief here *)

  let policysystem_answered (ps: policysystem) (up: policysystemupdate): policysystem =
    {policies = ps.policies;
     belief = up.newbelief;
     valcache = ps.valcache}

  let rec policysystem_check_policies
      (count: int)
      (policies: policy list)
      (distout:    ESYS.psrep) (* output distribution *)
      (distbelief: ESYS.psrep) (* revised distribution, distout conditioned on something *)
      (distactual: ESYS.psrep) (* point distribution describing the secret values *)
      (outputs: Lang.varid list)
    : string option =
    (* let max_prob = find_max_belief distout outputs policies.pvars in *)
    ifverbose1 (
      printf "\n----------------------\n";
      printf "Checking Policy #%d:\n" count;
      printf "Number of policies: %d\n" (List.length policies);
      printf "Max belief of distout: %s\n" (Gmp.Q.to_string (ESYS.psrep_max_belief distout));
      printf "Max belief of revised: %s\n" (Gmp.Q.to_string (ESYS.psrep_max_belief distbelief));
      printf "Max belief of distactual: %s\n" (Gmp.Q.to_string (ESYS.psrep_max_belief distactual))
    );
    match policies with
    | [] -> None
    | p :: r ->
      if not (policy_eval p distout distbelief distactual outputs) then
        let string_p = string_of_policy p in
        Some ("policy not satisfied: " ^ string_p)
      else
        policysystem_check_policies (count + 1) r distout distbelief distactual outputs


  let policysystem_answer (ps: policysystem) (querytup: (string * (Lang.varid list * Lang.varid list * Lang.stmt))) (queryinput_stmt: Lang.stmt) :  policysystemresult =
    (* todo: simplify some of this query preparation, factor out to someplace else, also done repeatedly in prob.ml *)

    ifverbose1 (
      printf "\nstart belief:\n"; ESYS.print_psrep ps.belief
    );

    let (queryname, query) = querytup in
    let (inlist, outlist, querystmt) = query in
    let secretvars = ESYS.psrep_vars ps.belief in

    let (ignored, inputstate_temp) = Evalstate.eval queryinput_stmt (new state_empty) in

    let expanded_inlist =
      List.fold_left (fun a varname ->
          if (inputstate_temp#is_record (varname)) then
            let (_, varname_str) = varname in
            (List.map (fun (agent, field) -> (agent, varname_str^"."^field)) (inputstate_temp#get_vals (varname)))@a
          else varname::a) [] inlist in

    let sa_querystmt = if !Cmd.opt_dsa then
        (sa_of_stmt querystmt (List.append secretvars expanded_inlist) outlist)
      else
        querystmt in


    let inputstate = inputstate_temp#copy in
    inputstate#project expanded_inlist;
    let inputstate_full = inputstate#copy in
    let sa_querystmt = Preeval.predefine_as_state sa_querystmt inputstate expanded_inlist in
    ifdebug (printf "predefined is \n";
             Lang.print_stmt_pretty sa_querystmt "";
             printf "--- end of predefined ---\n");

    (*-------------------------------------------------------------------*)
    (*---------------------------- Liveness -----------------------------*)
    (*-------------------------------------------------------------------*)
    let rec fix_live i vs stmts =
      let lived = liveness_analysis_rev stmts vs in
      let res = if equal_stmts stmts lived
                then lived
                else fix_live (i + 1) vs lived in
      res in
    let fannotated = ann_use_def (flip_seq (add_halt sa_querystmt)) in
    let analed = flip_seq (fix_live 0 outlist fannotated) in
    ifdebug (print_stmt analed; printf "\n--------------\n");

    (*-------------------------------------------------------------------*)
    (*---------------------------- Inlining -----------------------------*)
    (*-------------------------------------------------------------------*)
    let ignored_vids = List.concat [secretvars;expanded_inlist] in
    let inlinable    = List.map (fun x -> (x, AEInt (inputstate_full#get x))) expanded_inlist in

    let final_stmt = if !Cmd.opt_inline
                     then let (_,_,rewritten) = rewrite_stmt analed ignored_vids inlinable in
                          rewritten
                     else sa_querystmt in

    ifverbose1 (print_stmt sa_querystmt; printf "\n--------------\n");
    ifverbose1 (print_stmt final_stmt; printf "\n--------------\n");

    inputstate_full#merge ps.valcache;

    (*let inputdist = ESYS.psrep_set_all ps.belief inputstate in *) (* inputs are now substituted above using Preeval.predefine *)
    let inputdist = ps.belief in

    (*ISSUE: Maybe remove record from valcache *)
    let secretdist = ESYS.psrep_point (ESYS.srep_point ps.valcache) in

    ifverbose1 (
      printf "\ninput belief:\n"; ESYS.print_psrep inputdist
    );

    let outputdist = ESYS.peval final_stmt inputdist in

    ifverbose1 (
      printf "\nend belief:\n"; ESYS.print_psrep outputdist
    );

    let (ignored, outputstate_temp) = Evalstate.eval final_stmt
        inputstate_full in

    let outputstate = outputstate_temp#copy in
    outputstate#project outlist;

              (*
               let startrelent = ESYS.psrep_relative_entropy inputdist secretdist in *)

    let enddist =
      ESYS.psrep_on_vars
        (ESYS.psrep_given_state outputdist outputstate)
        secretvars in

    let ps_updater = {newbelief = enddist} in

    (* let ps_updater = {newbelief = outputdist} in *)
              (*
                let endrelent = ESYS.psrep_relative_entropy enddist secretdist in
              *)

    ifverbose1 (
      printf "\ninput state:\n\t"; inputstate_full#print; printf "\n";
    );

    printf "query %s" queryname; inputstate#print_as_args;

    printf " = "; outputstate#print_as_args; printf "\n";

    ifverbose1 (
      printf "\nrevised belief\n"; ESYS.print_psrep enddist;

      (* printf "relative entropy (start -> secret): %f\n" startrelent;
         printf "relative entropy (revised -> secret): %f\n" endrelent;
         printf "bits learned: %f\n" (startrelent -. endrelent); *)

      printf "\n### checking policies ###\n";
    );
    (*flush stdout;*)

    ifnotverbose (
      let rev_belief = ESYS.psrep_max_belief enddist in
      printf "Revised max-belief: %s\n" (Gmp.Q.to_string rev_belief);
      (* lg (U/V) == lg U - lg V *)
      let cuma_leakage = lg (Gmp.Q.to_float rev_belief) -. lg (!Globals.init_max_belief) in
      printf "Cumulative leakage: %s\n" (string_of_float cuma_leakage)
    );

    match policysystem_check_policies 0 ps.policies outputdist enddist secretdist outlist with
        (*match policysystem_check_policies 0 ps.policies outputdist outputdist secretdist outlist with*)
    (*        | None -> {result = RTrueValue (outputstate#canon);*)
    | None -> {result = RTrueValue ([]);
               update = ps_updater}
    | Some (s) -> {result = RReject (s);
                   update = {newbelief = ps.belief}}
end
;;

(*module PSYS_S = MAKE_PSYSTEM (ESYS_S);;*)
(*module PSYS_SSP = MAKE_PSYSTEM (ESYS_SSP);;*)