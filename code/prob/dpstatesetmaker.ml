open State
open Printf
open Util
open Stateset
open Pstateset
open Gmp
open Gmp.Q.Infixes
open Gmp.Z.Infixes
open Globals
open List

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

  let ( +@ ) lst1 lst2 = sort_uniq compare (lst1 @ lst2)
  let ( *@ ) lst1 lst2 = filter (fun x -> mem x lst1 && mem x lst2) (lst1 @ lst2)

  (** Given a DPSS, return a mapping [K -> i], where for each component PSS, K is the set of
   * variables managed by that PSS, and i is the index of that PSS in the DPSS.
   *)
  let _factor_index_mapping = mapi (fun i p -> (PSS.vars p, i))

  (** Given a variable k and a factor mapping [K -> v], find the pair (K, v) such that k ∈ K, or
   * None. *)
  let rec _find_factor k m = match m with
    | [] -> None
    | (s, v) :: t when mem k s -> Some (s, v)
    | _ :: t -> _find_factor k t

  (** Factorization API:
   * 1. *)

  type _factor_t = Lang.varid list
  type _factorization_t = _factor_t list

  (** Extract the factorization from a decomposed polyhedron. *)
  let _factorization_of (dpss: pstateset) : _factorization_t = map PSS.vars dpss

  (** Return the factor in a decomposed polyhedron for a given variable, None if variable is
   * implicitly unconstrained. *)
  let rec _get_factor_for_var (fs: _factorization_t) (v: Lang.varid) : _factor_t option = match fs with
    | [] -> None
    | h :: t when mem v h -> Some h
    | h :: t -> _get_factor_for_var t v

  (** Return the polyhedral component for a given factor in a decomposed polyhedron, None if the
   * factor is not represented. *)
  let rec _get_poly_for_factor (dpss: pstateset) (f: _factor_t): base_pstateset option =
    let s = sort_uniq compare f in
    match dpss with
    | [] -> None
    | h :: t when s = sort_uniq compare (PSS.vars h) -> Some h
    | h :: t -> _get_poly_for_factor t f

  let _reconstitute_factor (dpss: pstateset) (f: _factor_t): base_pstateset =
    let rec catSomes opts = match opts with
      | [] -> []
      | Some x :: t -> x :: catSomes t
      | None :: t -> catSomes t in
    let base_factorization = _factorization_of dpss in
    let constituent_factors = sort_uniq compare (catSomes (map (_get_factor_for_var base_factorization) f)) in
    fold_left PSS.prod (PSS.make_new []) (catSomes (map (_get_poly_for_factor dpss) constituent_factors))

  let _pull_factor_to_front fs f =
    match  _get_factor_for_var fs (hd f) with
    | None -> raise (General_error ("Invariant failed while pulling factor to front."))
    | Some fp -> fp :: filter (fun fq -> fq != fp) fs

  (** Refactorize a decomposed polyhedron with respect to a given factorization by merging
   * polyhedra as necessary. *)
  let rec _refactorize (dpss: pstateset) (fs: _factorization_t) : pstateset = match fs with
    | [] -> []
    | f :: t -> _reconstitute_factor dpss f :: _refactorize dpss t

  (** Determine the best common factorization of a pair of decomposed polyhedra. *)
  let _best_common_factorization (fs1: _factorization_t) (fs2: _factorization_t) : _factorization_t =
    let common_vars = sort_uniq compare (concat (fs1 @ fs2)) in
    let default a ma = match ma with
      | None -> a
      | Some b -> b in
    let accommodate_var r a =
      let s1 = default [a] (_get_factor_for_var fs1 a) in
      let s2 = default [a] (_get_factor_for_var fs2 a) in
      try (a, assoc a r +@ s1 +@ s2) :: (remove_assoc a r) with Not_found -> (a, s1 +@ s2) :: r in
    sort_uniq compare (snd (split (fold_left accommodate_var [] common_vars)))

  let _best_common_factorization_for_dpsss (dpss1: pstateset) (dpss2: pstateset): _factorization_t =
    let vars_1 = map PSS.vars dpss1 in
    let vars_2 = map PSS.vars dpss2 in
    _best_common_factorization vars_1 vars_2

  let _normalize_factorization_with (fs: _factorization_t) (f: _factor_t): _factorization_t =
    _pull_factor_to_front (_best_common_factorization fs [f]) f

  let print dpss = printf "Decomposition:\n"; List.iter PSS.print dpss
  let _print_factorization fs =
    print_string "Factorization: ";
    print_string "[";
    print_string (String.concat ", " (map Lang.varid_list_to_string fs));
    print_string "]\n"

  let _pairwise_refactor_apply dpss1 dpss2 f =
    let common_factorization = _best_common_factorization_for_dpsss dpss1 dpss2 in
    let new_dpss1 = _refactorize dpss1 common_factorization in
    let new_dpss2 = _refactorize dpss2 common_factorization in
    map (fun (p1, p2) -> f p1 p2) (combine new_dpss1 new_dpss2)

  let _dispatch_on_head dpss f = match dpss with
    | h :: t -> f h :: t
    | [] -> raise (General_error("_dispatch_on_head invariant failure."))

  let copy = List.map PSS.copy
  let make_empty () = []
  let make_point s = [PSS.make_point s]
  let make_point_of_stateset ss = [PSS.make_point_of_stateset ss]
  let make_new vs = [PSS.make_new vs]

  let addvar dpss v = make_new [v] @ dpss

  let size dpss = fold_left ( *! ) zone (map PSS.size dpss)
  let slack dpss = raise Not_implemented
  let prod dpss1 dpss2 = _pairwise_refactor_apply dpss1 dpss2 PSS.prod
  let make_uniform v lo hi = [PSS.make_uniform v lo hi]
  let transform dpss stmt =
    let common_factorization = _normalize_factorization_with (_factorization_of dpss) (Lang.stmt_vars stmt) in
    let new_dpss = _refactorize dpss common_factorization in
    _dispatch_on_head new_dpss (fun dpss -> PSS.transform dpss stmt)
  let intersect dpss ss =
    let common_factorization = _normalize_factorization_with (_factorization_of dpss) (SS.stateset_vars ss) in
    let new_dpss = _refactorize dpss common_factorization in
    _dispatch_on_head new_dpss (fun dpss -> PSS.intersect dpss ss)
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

  let abstract_plus dpss1 dpss2 = _pairwise_refactor_apply dpss1 dpss2 PSS.abstract_plus
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
