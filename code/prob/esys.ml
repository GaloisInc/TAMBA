open State
open Lang
open Util
open Gmp
open Ppl_ocaml
open Printf

(*module SS  = Statesetsimple.Stateset(ES) *)
(*module SSP = Statesetpoly.Stateset*)

module PPL_POLY = Ppldomainpoly.Ppldomainpoly
module PPL_BOX  = Ppldomainbox.Ppldomainbox
module PPL_OCTA = Ppldomainocta.Ppldomainocta
module PPL_OCTALATTE = Ppldomainoctalatte.Ppldomainoctalatte

module SS_POLY  = Pplstatesetmaker.MakeStateset(PPL_POLY)
module SS_BOX   = Pplstatesetmaker.MakeStateset(PPL_BOX)
module SS_OCTA  = Pplstatesetmaker.MakeStateset(PPL_OCTA)
module SS_OCTALATTE  = Pplstatesetmaker.MakeStateset(PPL_OCTALATTE)

module PSS_POLY = Pstatesetmaker.MakePStateset(SS_POLY)
module PSS_BOX  = Pstatesetmaker.MakePStateset(SS_BOX)
module PSS_OCTA = Pstatesetmaker.MakePStateset(SS_OCTA)
module PSS_OCTALATTE = Pstatesetmaker.MakePStateset(SS_OCTALATTE)

module DPSS_POLY = Dpstatesetmaker.MakeDPStateset(SS_POLY)(PSS_POLY)
module DPSS_RBOX = Dpstatesetmaker.MakeDPStateset(SS_BOX)(PSS_BOX)

module PPSS_POLY = Ppowersetmaker.MakePPowerset(SS_POLY)(PSS_POLY)
module PPSS_BOX  = Ppowersetmaker.MakePPowerset(SS_BOX) (PSS_BOX)
module PPSS_OCTA = Ppowersetmaker.MakePPowerset(SS_OCTA)(PSS_OCTA)
module PPSS_OCTALATTE = Ppowersetmaker.MakePPowerset(SS_OCTALATTE)(PSS_OCTALATTE)
module PDPSS_POLY = Ppowersetmaker.MakePPowerset(SS_POLY)(DPSS_POLY)
module PDPSS_RBOX = Ppowersetmaker.MakePPowerset(SS_BOX)(DPSS_RBOX)

module EPPSS_POLY = Evalpstateset.Eval(PPSS_POLY)
module EPPSS_BOX  = Evalpstateset.Eval(PPSS_BOX)
module EPPSS_OCTA = Evalpstateset.Eval(PPSS_OCTA)
module EPPSS_OCTALATTE = Evalpstateset.Eval(PPSS_OCTALATTE)
module EPDPSS_POLY = Evalpstateset.Eval(PDPSS_POLY)
module EPDPSS_RBOX = Evalpstateset.Eval(PDPSS_RBOX)

module type EVAL_SYSTEM = sig
  type srep
  type psrep

  val srep_empty: unit -> srep
  val peval_start: stmt -> psrep
  val peval: stmt -> psrep -> psrep

  val srep_point: state -> srep

  val srep_copy: srep -> srep
  val psrep_copy: psrep -> psrep

  val psrep_enum: psrep -> (state list)
  val psrep_enum_on_vars: psrep -> Lang.varid list -> (state list)

  val psrep_on_vars: psrep -> (Lang.varid list) -> psrep
  val psrep_set_all: psrep -> state -> psrep
  val psrep_point: srep -> psrep
  val psrep_relative_entropy: psrep -> psrep -> float
  val psrep_given_state: psrep -> state -> psrep
  val psrep_vars: psrep -> Lang.varid list

  val psrep_sample: psrep -> int -> (state * (state -> (int * state)) * (varid * int) list) list -> psrep

  val psrep_improve_lower_bounds: (state -> bool) -> (state -> Z.t * polyhedron) -> state -> int -> psrep -> psrep

  val get_alpha_beta: psrep -> (int * int)

  val psrep_rep_size: psrep -> int
  val psrep_size: psrep -> Z.t
  val psrep_smin_smax: psrep -> (Z.t * Z.t)
  val psrep_pmin_pmax: psrep -> (Q.t * Q.t)
  val psrep_mmin_mmax: psrep -> (Q.t * Q.t)

  val psrep_max_belief: psrep -> Q.t

  val print_srep: srep -> unit
  val print_psrep: psrep -> unit
end

module Make_esys_pss
  (S: Stateset.STATESET_TYPE)
  (M: Pstateset.PSTATESET_TYPE with type stateset = S.stateset)
  (ME: Eval.EVALPSTATESET_TYPE with type pstateset = M.pstateset):
  (EVAL_SYSTEM with type srep = M.stateset
               and type psrep = M.pstateset
               and type psrep = ME.pstateset
  ) = struct
  (* eval system for probabilistic statesets *)

  type srep = S.stateset

  type psrep = M.pstateset

  let srep_empty () = S.stateset_empty ()

  let peval_start astmt = ME.peval_start astmt

  let peval astmt adist = ME.peval astmt adist

  let srep_point astate = S.stateset_point astate

  let srep_copy s = S.stateset_copy s
  let psrep_copy ps = M.copy ps

  let psrep_enum d = M.enum d
  let psrep_enum_on_vars d vars = M.enum_on_vars d vars

  let psrep_point ass = M.make_point_of_stateset ass

  let psrep_set_all d s = M.set_all d (s#canon)
  let psrep_relative_entropy d1 d2 = M.relative_entropy d1 d2

  let psrep_on_vars d vars = printf "In psrep_on_vars\n%!";
                             let res = M.project d vars in
                             printf "After projection!\n%!";
                             res

  let psrep_given_state d s = M.intersect d (S.stateset_point s)

  let psrep_vars d = M.vars d

  let get_alpha_beta = M.get_alpha_beta
  let psrep_sample = M.sample_pstateset
  let psrep_improve_lower_bounds = M.improve_lower_bounds
  let psrep_rep_size = M.rep_size
  let psrep_size = M.size
  let psrep_smin_smax = M.prob_smin_smax
  let psrep_pmin_pmax = M.prob_pmin_pmax
  let psrep_mmin_mmax = M.prob_mmin_mmax

  let psrep_max_belief d = (M.max_belief d)

  let print_srep ass = S.print_stateset ass
  let print_psrep apss = M.print apss
end

module ESYS_PPSS_POLY: EVAL_SYSTEM = Make_esys_pss(SS_POLY)(PPSS_POLY)(EPPSS_POLY)
module ESYS_PPSS_BOX: EVAL_SYSTEM = Make_esys_pss(SS_BOX)(PPSS_BOX)(EPPSS_BOX)
module ESYS_PPSS_OCTA: EVAL_SYSTEM  = Make_esys_pss(SS_OCTA)(PPSS_OCTA)(EPPSS_OCTA)
module ESYS_PPSS_OCTALATTE: EVAL_SYSTEM  = Make_esys_pss(SS_OCTALATTE)(PPSS_OCTALATTE)(EPPSS_OCTALATTE)
module ESYS_PDPSS_POLY: EVAL_SYSTEM = Make_esys_pss(SS_POLY)(PDPSS_POLY)(EPDPSS_POLY)
module ESYS_PDPSS_RBOX: EVAL_SYSTEM = Make_esys_pss(SS_BOX)(PDPSS_RBOX)(EPDPSS_RBOX)
