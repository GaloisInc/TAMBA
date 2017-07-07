open State
open Printf
open Util
open Stateset
open Pstateset
open Gmp
open Gmp.Q.Infixes
open Gmp.Z.Infixes
open Globals

open Gmp_util

(** Make a decomposed probabilistic stateset out of a probabilistic stateset. *)
module MakeDPStateset
  (SSM: STATESET_TYPE)
  (PSSM: PSTATESET_TYPE with type stateset = SSM.stateset
                        and type splitter = SSM.splitter)
  : (PSTATESET_TYPE with type stateset = SSM.stateset
                    and type splitter = SSM.splitter)
  = struct
  module SS = SSM
  module PSS = PSSM

  type stateset = SS.stateset
  type base_pstateset = PSS.pstateset
  type pstateset = base_pstateset list
  type splitter = SS.splitter

  let recompose dpss = match dpss with
    | [] -> PSS.make_empty ()
    | h :: t -> List.fold_left PSS.prod h t

  let copy = List.map PSS.copy
  let make_empty () = []
  let make_point s = [PSS.make_point s]
  let make_point_of_stateset ss = [PSS.make_point_of_stateset ss]
  let make_new vs = [PSS.make_new vs]

  let addvar dpss v = make_new [v] @ dpss

  let print dpss = printf "Decomposition:\n"; List.iter PSS.print dpss

  let size dpss = PSS.size (recompose dpss)
  let slack dpss = PSS.slack (recompose dpss)
  let prod dpss1 dpss2 = [PSS.prod (recompose dpss1) (recompose dpss2)]
  let make_uniform v lo hi = [PSS.make_uniform v lo hi]
  let transform dpss stmt = [PSS.transform (recompose dpss) stmt]
  let intersect dpss ss = [PSS.intersect (recompose dpss) ss]
  let exclude dpss1 dpss2 = raise Not_implemented
  let is_empty = List.for_all PSS.is_empty

  let make_splitter dpss lexp = raise Not_implemented
  let split dpss lexp = raise Not_implemented
  let split_many dpss lexp = raise Not_implemented
  let split_many_with_splitter dpss vs splitters = raise Not_implemented

  let set_all dpss vsis = [PSS.set_all (recompose dpss) vsis]

  let project dpss vs = [PSS.project (recompose dpss) vs]

  let vars dpss = List.concat (List.map PSS.vars dpss)
  let enum dpss = raise Not_implemented
  let enum_on_vars dpss vs = raise Not_implemented

  let abstract_plus dpss1 dpss2 = [PSS.abstract_plus (recompose dpss1) (recompose dpss2)]
  let relative_entropy dpss1 dpss2 = PSS.relative_entropy (recompose dpss1) (recompose dpss2)

  let prob_scale dpss scalar = [PSS.prob_scale (recompose dpss) scalar]

  let prob_max_in_min_out dpss s = PSS.prob_max_in_min_out (recompose dpss) s
  let prob_max_norm dpss s = PSS.prob_max_norm (recompose dpss) s
  let prob_max_min dpss = PSS.prob_max_min (recompose dpss)
  let prob_smin_smax dpss = PSS.prob_smin_smax (recompose dpss)
  let prob_pmin_pmax dpss = PSS.prob_pmin_pmax (recompose dpss)
  let prob_mmin_mmax dpss = PSS.prob_mmin_mmax (recompose dpss)
  let min_mass dpss = PSS.min_mass (recompose dpss)
  let max_belief dpss = PSS.max_belief (recompose dpss)
  let is_possible dpss = PSS.is_possible (recompose dpss)
  let stateset_hull dpss = PSS.stateset_hull (recompose dpss)
  let get_alpha_beta dpss = PSS.get_alpha_beta (recompose dpss)
  let sample_pstateset dpss n es = [PSS.sample_pstateset (recompose dpss) n es]
  let improve_lower_bounds checker runner init lim dpss = [PSS.improve_lower_bounds checker runner init lim (recompose dpss)]
end;;
