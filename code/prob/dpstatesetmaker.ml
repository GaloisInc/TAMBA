open Lang
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
                        )
  : (PSTATESET_TYPE with type stateset = SSM.stateset
                    )
  = struct
  module SS = SSM
  module PSS = PSSM

  (** A decomposed pstateset (dpstateset) is represented as a mapping from factors (set of related
     variables) to the corresponding underlying pstatesets which relate and contrain the variables
     in their factor. The representation relies on the invariant that the factors are both mutually
     exclusive and exahustive; this invariant must be checked whenever a dpstateset is
     created/modified.

     Ultimately, the pstateset represented by a dpstateset is the product of each of its constituent
     factors' pstatesets; the result of applying any function over the dpstateset should be
     identical to the result of applying that same function over the product (defactorization) of
     the constituents. In cases where maintaining factorization is not necessary, such functions are
     implemented in exactly this way.
   *)
  type stateset = SS.stateset
  type factor = varid list
  type base_pstateset = factor * PSS.pstateset
  type pstateset = base_pstateset list

  let (%) f g x = f (g x)

  let factor_to_string f = String.concat "" ["["; Lang.varid_list_to_string f; "]"]
  let print_factor = print_string % factor_to_string

  let factorization_to_string fs =
    String.concat "" (["["] @ [String.concat " || " (map factor_to_string fs)] @ ["]"])

  let print_factorization = print_string % factorization_to_string

  let print_bp (f, p) = print_string (factor_to_string f); PSS.print p

  let print dpss =
    print_string "Decomposition:\n";
    iter (fun (f, p) ->
        print_string "Factor ";
        print_string (factor_to_string f);
        print_string ":\n";
        PSS.print p
    ) dpss;
    print_string "\n"

  (** An empty factor is useless, and may safely be removed. *)
  let purge_useless (dpss: pstateset) = filter (fun (f, p) -> f != []) dpss

  (* Poor man's set library. *)

  let set_union lst1 lst2 = sort_uniq compare (lst1 @ lst2)
  let ( +@ ) = set_union

  let set_intersect lst1 lst2 = filter (fun i -> mem i lst2) lst1
  let ( *@ ) = set_intersect

  let set_contained lst1 lst2 = lst1 = set_intersect lst1 lst2
  let ( <=@ ) = set_contained

  let set_eq s1 s2 = (sort_uniq compare s1) = (sort_uniq compare s2)
  let set_set_eq ss1 ss2 = (sort_uniq compare (map (sort_uniq compare) ss1)) = (sort_uniq compare (map (sort_uniq compare) ss2))

  let non_empty lst = lst != []
  let (<$>) f ma = match ma with
    | None -> None
    | Some a -> Some (f a)

  let rec catSomes opts = match opts with
    | [] -> []
    | Some x :: t -> x :: catSomes t
    | None :: t -> catSomes t

  (** Factorization Utilities *)

  let factorization (dpss: pstateset): factor list = map fst dpss

  (** Find the factor in a dpstateset satisfying the given property; none if no such dpstateset exists. *)
  let find_factor_by (fn: factor -> factor -> bool) (f: factor) (dpss: pstateset) : base_pstateset option
    = try Some (find (fun (f', p) -> fn f f') dpss) with Not_found -> None

  (** Merge just enough factors from `fs` such that the factor `f` is fully contained in one of them. *)
  let rec reconcile_factor (fs: factor list) (f: factor) : factor list =
    match fs with
    | [] -> [f]
    | _ -> let (haves, havenots) = partition (fun f' -> non_empty (set_intersect f f')) fs in
           [fold_left set_union f haves] @ havenots

  let prod_with_factor ((f, p): base_pstateset) ((f', p'): base_pstateset) : base_pstateset = (f +@ f', PSS.prod p p')

  (** The least common factorization of two factorizations fs1 and fs2 is the fs such that each
     factor in fs1 or fs2 is contained fully in a factor in fs, and no two unrelated variables end
     up in the same factor.
   *)
  let lcf (fs1: factor list) (fs2: factor list) = fold_left reconcile_factor [] (fs1 @ fs2)

  let is_compatible_with (fs1: factor list) (fs2: factor list)
    = for_all (fun f1 -> exists (fun f2 -> set_contained f1 f2) fs2) fs1

  let _assert msg inv = if inv then () else failwith msg
  let invariant b s = if b then () else failwith s

  (** Construct the pstateset corresponding to the factor f, by merging pstatesets corresponding to
     constituent factors from dpss *)
  let construct_factor_poly (dpss: pstateset) (f: factor) : base_pstateset =
    let get_base_or_new v = match find_factor_by set_contained [v] dpss with
      | None -> ([v], PSS.make_new [v])
      | Some bp -> bp in
    let constituents = sort_uniq (fun (f, p) (f', p') -> compare f f') (map get_base_or_new f) in
    match constituents with
    | [] -> ([], PSS.make_empty ())
    | h :: t -> fold_left prod_with_factor h t

  (** "Refactorize" a dpss according to a new factorization fs, by merging pstatesets as necessary. *)
  let normalize (dpss: pstateset) (fs: factor list) =
    _assert "Factorizations incompatible!" (is_compatible_with (factorization dpss) fs);
    map (construct_factor_poly dpss) (sort_uniq compare fs)

  (** "Refactorize" two dpss' according to their own least common factorization, by merging pstatesets as necessary. *)
  let pairwise_normalize (dpss1: pstateset) (dpss2: pstateset): (pstateset * pstateset) =
    let cfs = lcf (factorization dpss1) (factorization dpss2) in
    (normalize dpss1 cfs, normalize dpss2 cfs)

  (** Apply function fn to the pstateset corresponding to the factor containing f, and replace it with the result. *)
  let rec with_factor (f: factor) (fn: base_pstateset -> base_pstateset) (dpss: pstateset) : pstateset =
    match dpss with
    | [] -> []
    | (f', p) :: t when set_contained f f' -> fn (f', p) :: t
    | h :: t -> h :: with_factor f fn t

  (** Merge all constituent factor pstatesets, essentially reverting a decomposition to the underlying pstateset. *)
  let defactorize (dpss: pstateset) = snd (fold_left prod_with_factor ([], PSS.make_new []) dpss)

  (** PStateset API *)
  let copy (dpss: pstateset) = map (fun (f, p) -> (f, PSS.copy p)) dpss
  let make_empty (): pstateset = []
  let make_new (vs: varid list) = map (fun v -> ([v], PSS.make_new [v])) vs
  let make_point (s: state): pstateset = [(s#vars, PSS.make_point s)]
  let make_point_of_stateset (ss: stateset): pstateset = [(SS.stateset_vars ss, PSS.make_point_of_stateset ss)]
  let make_singleton (pss: PSS.pstateset): pstateset = [(PSS.vars pss, pss)]
  let vars (dpss: pstateset) = concat (factorization dpss)

  let addvar (dpss: pstateset) (v: varid): pstateset =
    _assert "Cannot add an existing var!" (not (mem v (vars dpss)));
    make_new [v] @ dpss

  let rep_size dpss = 1
  let size (dpss: pstateset) = fold_left (fun s (f, p) -> s *! PSS.size p) zone dpss

  let prod (dpss1: pstateset) (dpss2: pstateset) =
    _assert "Operands to product must be disjoint!" (set_intersect (vars dpss1) (vars dpss2) = []);
    dpss1 @ dpss2


  let slack dpss = raise Not_implemented
  let make_uniform v lo hi = make_singleton (PSS.make_uniform v lo hi)

  let transform dpss stmt =
    let ndpss = normalize dpss (lcf (factorization dpss) [Lang.stmt_vars stmt]) in
    with_factor (Lang.stmt_vars stmt) (fun (f, p) -> (f, PSS.transform p stmt)) ndpss

  let intersect dpss ss =
    let ndpss = normalize dpss (lcf (factorization dpss) [SS.stateset_vars ss]) in
    with_factor (SS.stateset_vars ss) (fun (f, p) -> (f, PSS.intersect p ss)) ndpss

  let exclude dpss1 dpss2 = raise Not_implemented
  let is_empty = for_all (fun (f, p) -> PSS.is_empty p)

  let make_splitter dpss lexp = raise Not_implemented
  let split dpss alexp =
    let split_vars = Lang.collect_vars_lexp alexp in
    let ndpss = normalize dpss (lcf (factorization dpss) [split_vars]) in
    match find_factor_by (<=@) split_vars ndpss with
    | None -> raise(General_error("Whoops."))
    | Some (normalized_split_factor, normalized_split_pss) ->
       let remaining = filter (fun (f, p) -> f != normalized_split_factor) ndpss in
       let (split_ins, split_outs) = PSS.split normalized_split_pss alexp in
       ( purge_useless ((normalized_split_factor, split_ins) :: remaining)
       , purge_useless ((normalized_split_factor, split_outs) :: remaining))

  let split_many dpss alexp =
    let split_vars = Lang.collect_vars_lexp alexp in
    let ndpss = normalize dpss (lcf (factorization dpss) [split_vars]) in
    match find_factor_by (<=@) split_vars ndpss with
    | None -> raise(General_error("Whoops."))
    | Some (normalized_split_factor, normalized_split_pss) ->
       let remaining = filter (fun (f, p) -> f != normalized_split_factor) ndpss in
       let (split_inss, split_outss) = PSS.split_many normalized_split_pss alexp in
       let tack_on_remaining p = (normalized_split_factor, p) :: remaining in
       (map (purge_useless % tack_on_remaining) split_inss, map (purge_useless % tack_on_remaining) split_outss)

  let set_all (dpss: pstateset) (vsis: (Lang.varid * int) list)
    = fold_left (fun d (v, i) -> with_factor [v] (fun (f, p) -> (f, PSS.set_all p [(v, i)])) d) dpss vsis

  let project dpss vs =
    let partitions_by_factor = map (fun (f, _) -> filter (fun v -> mem v f) vs) dpss in
    filter (fun (f, p) -> f != []) (map2 (fun (f, p) part -> (part, PSS.project p part)) dpss partitions_by_factor)

  let enum dpss = raise Not_implemented
  let enum_on_vars dpss vs = raise Not_implemented

  let abstract_plus dpss1 dpss2 =
    let (ndpss1, ndpss2) = pairwise_normalize dpss1 dpss2 in
    map2 (fun (f1, p1) (f2, p2) ->
        _assert "Pairwise normalizations must produce identical factors!" (f1 = f2);
        (f1, PSS.abstract_plus p1 p2)
    ) ndpss1 ndpss2

  let relative_entropy dpss1 dpss2 = PSS.relative_entropy (defactorize dpss1) (defactorize dpss2)

  (* TODO: Check correctness of these probability calculations *)

  let prob_scale dpss scalar = make_singleton (PSS.prob_scale (defactorize dpss) scalar)

  let fold_map_q f c = fold_left ( */ ) qone (map f c)
  let fold_map_z f c = fold_left ( *! ) zone (map f c)
  let bifold_map_q f c = fold_left (fun (a, b) (c, d) -> (a */ b, c */ d)) (qone, qone) (map f c)
  let bifold_map_z f c = fold_left (fun (a, b) (c, d) -> (a *! b, c *! d)) (zone, zone) (map f c)

  let prob_max_in_min_out dpss s = PSS.prob_max_in_min_out (defactorize dpss) s
  let prob_max_norm dpss s = fold_map_q (fun (_, p) -> PSS.prob_max_norm p s) dpss
  let prob_max_min dpss = PSS.prob_max_min (defactorize dpss) (* TODO: This one doesn't follow expected decomposition. *)
  let prob_smin_smax dpss = bifold_map_z (PSS.prob_smin_smax % snd) dpss
  let prob_pmin_pmax dpss = bifold_map_q (PSS.prob_pmin_pmax % snd) dpss
  let prob_mmin_mmax dpss = bifold_map_q (PSS.prob_mmin_mmax % snd) dpss
  let min_mass dpss = fold_map_q (PSS.min_mass % snd) dpss
  let max_belief dpss = PSS.max_belief (defactorize dpss)
  let is_possible dpss = for_all (fun (f, p) -> PSS.is_possible p) dpss
  let stateset_hull dpss = PSS.stateset_hull (defactorize dpss)
  let get_alpha_beta dpss = PSS.get_alpha_beta (defactorize dpss)
  let sample_pstateset dpss n es = make_singleton (PSS.sample_pstateset (defactorize dpss) n es)
  let improve_lower_bounds checker runner init lim dpss
    = make_singleton (PSS.improve_lower_bounds checker runner init lim (defactorize dpss))
end;;
