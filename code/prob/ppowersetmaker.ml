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

module MakePPowerset (* create a powerset pstateset from a pstateset *)
  (SSM: STATESET_TYPE)
  (PSSM: PSTATESET_TYPE with type stateset = SSM.stateset)
  : (PSTATESET_TYPE with type stateset = SSM.stateset) =
struct
  module SS = SSM
  module PSS = PSSM

  type stateset = SS.stateset

  type base_pstateset = PSSM.pstateset

  type pstateset = base_pstateset list

  let copy pss = List.map PSS.copy pss

  let make_empty () = []

  let make_point s = [PSS.make_point s]

  let make_new sl = [PSS.make_new sl]

  let print pss =
    printf "PPowerset\n";
    List.iter PSS.print pss

  let rep_size pss = List.length pss

  let size pss =
    List.fold_left
      (fun accum apss -> accum +! PSS.size apss)
      zzero
      pss

  let rec _make_uniform_to_precision vname lower upper prec share =
    let points = (upper -! lower) +! zone in

      if (points <! ztwo) || (prec <= 1) then
        [PSS.prob_scale (PSS.make_uniform vname lower upper) share]
      else
        let mid = (upper +! lower) /! ztwo in
        let temp1 = _make_uniform_to_precision vname lower mid (prec - 1)
          (share */ ((Q.from_z (mid -! lower +! zone)) // (Q.from_z points))) in
        let temp2 = _make_uniform_to_precision vname (mid +! zone) upper (prec - 1)
          (share */ ((Q.from_z (upper -! (mid +! zone) +! zone)) // (Q.from_z points))) in
          List.append temp1 temp2

  let make_uniform vname lower upper =
    if !Cmd.opt_split_factor > 1 then
      _make_uniform_to_precision vname lower upper !Cmd.opt_split_factor qone
    else
      [PSS.make_uniform vname lower upper]

  let _purge_useless pss =
    List.filter (fun apss -> PSS.is_possible apss) pss

  let addvar pss varid =
    List.map (fun apss -> PSS.addvar apss varid) pss

  let transform pss astmt =
    List.map (fun apss -> PSS.transform apss astmt) pss

  let intersect pss ss =
    _purge_useless (List.map (fun apss -> PSS.intersect apss ss) pss)

  let exclude pss1 pss2 = raise Not_implemented

  let is_empty pss = List.for_all PSSM.is_empty pss

  let slack pss = raise Not_implemented

  let _slack_of_plus apss1 apss2 =
    printf "checking slack\n";
    flush stdout;
    let apss = PSS.abstract_plus apss1 apss2 in
      (apss, PSS.slack apss)

  let _simplify_to_precision_simple pss maxpolies =
    if (maxpolies = 0) || (List.length pss <= maxpolies) then pss else
      (let temp = ref pss in
         while ((List.length !temp) > maxpolies) do
           let p1 = List.hd !temp in
           let p2 = List.hd (List.tl !temp) in
             temp := PSS.abstract_plus p1 p2 :: List.tl (List.tl !temp)
         done;
         !temp)

  let _simplify_to_precision_random pss maxpolies =
    if (maxpolies = 0) || (List.length pss <= maxpolies) then pss else
      let ret = ref pss in
        while ((List.length !ret) > maxpolies) do
          Globals.simplify_steps := !Globals.simplify_steps + 1;
          let temp = Array.to_list (array_permute (Array.of_list !ret)) in
          let p1 = List.nth temp 0 in
          let p2 = List.nth temp 1 in
          let p3 = PSS.abstract_plus p1 p2 in
            ret := p3 :: List.tl (List.tl temp)
        done;
        !ret

  let _simplify_to_precision_complex pss maxpolies =
    if (maxpolies = 0) || (List.length pss <= maxpolies) then pss else
      (let temp = ref pss in
         while ((List.length !temp) > maxpolies) do
           let opts = list_pairs_and_rest !temp in
             printf "slack options = %d\n" (List.length opts); flush stdout;
           let opts_goodness = List.map (fun (p1, p2, rest) -> (_slack_of_plus p1 p2, rest)) opts in
           let ((bapps, bslack), brest) = list_ultimate_nonempty
             (fun
                ((aapss1, aslack1), arest1)
                ((aapss2, aslack2), arest2) -> aslack1 </ aslack2)
             opts_goodness
           in
             temp := bapps :: brest
         done;
         !temp)

  let _simplify_to_precision_halfs pss maxpolies =
    let ret = (
      if maxpolies = 1 then
        _simplify_to_precision_simple pss maxpolies
      else
        let have = List.length pss in
          if (maxpolies = 0) || (have <= maxpolies) then pss else
            let split1 = have / 2 in
              (*let split2 = have - split1 in*)
            let make1 = maxpolies / 2 in
            let make2 = maxpolies - make1 in
            let (half1, half2) = list_split_into split1 pss in
              List.append
                (_simplify_to_precision_simple half1 make1)
                (_simplify_to_precision_simple half2 make2)
    ) in
      ret

  let _simplify_to_precision_select () =
    match !Cmd.opt_simplify with
      | 0 -> _simplify_to_precision_halfs
      | 1 -> _simplify_to_precision_simple
      | 2 -> _simplify_to_precision_complex
      | 3 -> _simplify_to_precision_random
      | _ -> raise (General_error "unknown simplifier selected")

  let _simplify_to_precision pss maxpolies =
    Globals.start_timer Globals.timer_simplify;
    let ret = (_simplify_to_precision_select ()) pss maxpolies in
      Globals.stop_timer Globals.timer_simplify;
      ret

  let _simplify pss =
    _simplify_to_precision pss !Cmd.opt_precision

  let split_many pss alexp =
    let append_by_pair (xss, yss) (xs, ys) = (xss @ xs, yss @ ys) in
    if List.length pss = 0 then ([], []) else
      let (ins, outs) = List.fold_left append_by_pair ([], []) (List.map (fun p -> PSS.split_many p alexp) pss) in
      ([_purge_useless ins], [_purge_useless outs])

  let split pss alexp =
    if (List.length pss) = 0 then ([], []) else
      let (ins, outs) = split_many pss alexp in
        (_simplify_to_precision (List.hd ins) !Cmd.opt_precision,
         _simplify_to_precision (List.hd outs) !Cmd.opt_precision)

  let set_all pss sil = List.map (fun apss -> PSS.set_all apss sil) pss

  let project pss vl = List.map (fun apss -> PSS.project apss vl) pss

  let vars pss = match pss with
    | [] -> []
    | apss :: rest -> PSS.vars apss

  let stateset_hull pss = SS.statesets_union_list_nocomp (List.map (fun apss -> PSS.stateset_hull apss) pss)
  let stateset_hull_on_vars pss vars = SS.statesets_union_on_vars_list_nocomp (List.map (fun apss -> PSS.stateset_hull apss) pss) vars

  let enum pss = SS.stateset_enum (stateset_hull pss)
  let enum_on_vars pss vars =
    SS.stateset_enum (stateset_hull_on_vars pss vars)

  let relative_entropy pss1 pss2 = 0.0

  let max_prob pss = raise (General_error "max_prob not implemented")

  let prob_smin_smax pss = List.fold_left (fun (imin, imax) (min,max) -> (imin +! min, imax +! max)) (zzero, zzero)
                                          (List.map PSS.prob_smin_smax pss)

  let prob_pmin_pmax pss = Util.fold_left1 (fun (imin, imax) (min,max) -> (min_q imin min, max_q imax max))
                                           (List.map PSS.prob_pmin_pmax pss)

  let prob_mmin_mmax pss = List.fold_left (fun (imin, imax) (min,max) -> (imin +/ min, imax +/ max)) (qzero, qzero)
                                          (List.map PSS.prob_mmin_mmax pss)

  let prob_scale pss s = List.map (fun apss -> PSS.prob_scale apss s) pss

  let make_point_of_stateset ss = [PSS.make_point_of_stateset ss]

  let abstract_plus pss1 pss2 =
    (* This function performs an abstract plus for the powerset domain
       by reducing the number of disjuncts in the 2 input
       subcomponents. The strange arithmetic in the main case is an
       attempt to allocate number of disjuncts to the two
       sub-invocations invocations in a good way. When both inputs
       have the same number of disjuncts, each is reduced to smaller
       sets with half of max disjuncts (precision input). There are
       various cases where the two inputs have differing number of
       inputs where we still want to allocate as many total resulting
       disjuncts as possible. *)
    let (pssSmaller, pssBigger) = if (List.length(pss1) <= List.length(pss2)) then (pss1, pss2) else (pss2, pss1) in
    let numSmaller = List.length pssSmaller in
    let numBigger  = List.length pssBigger in
    Globals.seen_complexity (numSmaller + numBigger);
    let maxpolies = !Cmd.opt_precision in
    if (maxpolies = 0) || (numSmaller + numBigger >= maxpolies) then List.append pssSmaller pssBigger else
      if (maxpolies = 1) then
        _simplify_to_precision (List.append pssSmaller pssBigger) maxpolies
      else if !Cmd.opt_simplify = 3 then
        _simplify_to_precision (List.append pssSmaller pssBigger) maxpolies
      else
        (
          let halfpolies = maxpolies / 2 in
          let removeFromSmaller = max 0 (numSmaller - halfpolies) in
          let removeFromBigger  = max 0 ((numSmaller - removeFromSmaller + numBigger) - maxpolies) in
          (*printf "splitting %d,%d into %d,%d and max=%d\n" numSmaller numBigger (numSmaller - removeFromSmaller) (numBigger - removeFromBigger) maxpolies;*)
          let ret = List.append
            (_simplify_to_precision pssSmaller (numSmaller - removeFromSmaller))
            (_simplify_to_precision pssBigger (numBigger  - removeFromBigger)) in
          (*printf "returned %d polies\n" (List.length ret);*)
          if (numSmaller + numBigger >= maxpolies && List.length ret < maxpolies) ||
            (numSmaller + numBigger < maxpolies && List.length ret < numSmaller + numBigger) then
            raise (General_error "not using up all polies");
          ret
        )

  let is_possible pss =
    List.exists PSS.is_possible pss

  let min_mass pss = list_sum_general qzero Q.add (List.map (fun apss -> PSS.min_mass apss) pss)

  let prob_max_min pss = raise Not_implemented

  let max_belief pss =
    if not (is_possible pss) then qzero else (
      let total_mass = min_mass pss in
        if total_mass =/ qzero then qone
        else (
          let probs = List.map (fun apss ->
                                  (PSS.stateset_hull apss,
                                   PSS.prob_max_min apss)) pss in

          (*let prob_lists = SS.statesets_approx_intersections probs in  *)

          (* don't need exact intersections here, just overlap *)
          (*let prob_lists = SS.statesets_exact_intersections probs in*)
          let prob_lists = SS.statesets_overlap probs in

          let prob_maxes = List.map
            (fun plist ->
               let (massin, massout) =
                 (List.fold_left (fun (accin, accout) (amassin, amassout) -> (accin +/ amassin, accout +/ amassout))
                    (Q.zero, Q.zero) plist) in
                 (massin // total_mass)) prob_lists in
          let ret = list_max_q prob_maxes in
            if ret >/ qone then qone else ret
        ))

        (*             (if massout =/ qzero then
                     (if massin >/ qzero then qone else qzero) *)

  let prod pss1 pss2 =
    _simplify
      (List.fold_left
         (fun accum apss1 ->
            List.append accum
              (List.map (fun apss2 -> PSS.prod apss1 apss2)
                 pss2))
         [] pss1)

  let sample_pstateset ps n fs =
    let sizes = List.map PSS.size ps in
    let total =
      List.fold_left
        (fun accum n -> accum +! n)
        zzero
        sizes in
    let ns = List.map (fun x -> int_of_float (x /. Gmp.Z.to_float total *. float_of_int n)) (List.map Gmp.Z.to_float sizes) in
    (* Sampling points divided into bins given by ns *)
    let sampleRegion p n = PSS.sample_pstateset p n fs in
    List.map2 sampleRegion ps ns

  let improve_lower_bounds checker runner init lim ps =
    ifdebug (print_endline ("Size of P(Prob. Poly): " ^ (string_of_int (List.length ps))));
    ifdebug (print_endline "Mapping improve_lower_bounds over all Prob. Poly...");
    let ret = List.map (PSS.improve_lower_bounds checker runner init lim) ps in
    ifdebug (print_endline "Done.");
    ret

  let get_alpha_beta ps = List.fold_left (fun (ay,an) (y,n) -> (ay + y, an + n)) (0,0) (List.map PSS.get_alpha_beta ps)

end;;
