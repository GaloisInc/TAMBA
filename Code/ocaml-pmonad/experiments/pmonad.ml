open ExtList
open Printf
open Option

type prob = float
let is_one p = (abs_float (1.0 -. p)) <= 0.00000001
let is_zero p = (abs_float p) <= 0.00000001

module MM = struct
  type 'a dist = ('a, prob) Pmap.t

  type ('a, 'b) bind = ('a -> 'b dist) -> 'b dist

  let prob_of d v =
    try Pmap.find v d
    with Not_found -> 0.0

  let scale p d = Pmap.map (fun p2 -> p *. p2) d;;

  let mass d = Pmap.foldi (fun v p total -> total +. p) d 0.0

  let normalize d = scale (1.0 /. (mass d)) d

  let length d = Util.map_length d

  let of_list (choices: ('a * prob) list): 'a dist =
    List.fold_left
      (fun a (v, p) -> Pmap.insert_with (+.) v p a)
      Pmap.empty choices
  ;;

  let bind d k =
    Pmap.foldi (fun v p a ->
      Util.maps_merge a (scale p (k v)) (+.))
      d Pmap.empty
  ;;

  let iter d f = Pmap.iter f d

  let bind_flip prob k =
    if is_one prob then bind (of_list [(true,1.0)]) k
    else if is_zero prob then bind (of_list [(false,1.0)]) k
    else
      bind (of_list [(true, prob);
                     (false, 1.0 -. prob)]) k
  ;;

  let bind_flipint prob k =
    if is_one prob then bind (of_list [(1,1.0)]) k
    else if is_zero prob then bind (of_list [(0,1.0)]) k
    else
      bind (of_list [(1,prob);
                     (0,1.0 -. prob)]) k
  ;;

  let bind_uniform_in c k =
    let p = 1.0 /. (float_of_int (List.length c)) in
    bind (of_list (List.map (fun v -> (v, p)) c)) k
  ;;

  let bind_uniform : int -> int -> (int -> 'a dist) -> 'a dist = 
    fun a b k ->
      let p = 1.0 /. (float_of_int (b - a + 1)) in
      bind (of_list (List.map (fun v -> (v, p)) (Util.list_range a b))) k
  ;;
  
  let to_string d =
    String.concat "\n" (List.map (fun (v, p) -> sprintf "%0.3f: %s" p (Std.dump v)) (Util.list_of_map d))

  let return v = Pmap.add v 1.0 (Pmap.empty);;

  let dist_map f d = Pmap.map f d;;

  let expect d =
    Pmap.foldi
      (fun x p acc -> acc +. p *. x) d 0.0

  let expect_map d f =
    Pmap.foldi
      (fun x p acc -> acc +. p *. (f x)) d 0.0

  let project d f = dist_map f d;;

  let project_bins d fbinkey finsidebin = 
    let m = ref Pmap.empty in

    Pmap.iter (fun v p ->
      let k = fbinkey v in
      let bink = finsidebin v in

      m := Pmap.insert_with
        (fun a b -> match (a, b) with
          | ((prob_total1, `Single (bink1, p1)),
             (prob_total2, `Single (bink2, p2))) ->
            let m = Pmap.empty in
            let m = Pmap.add bink1 p1 m in
            (prob_total1 +. prob_total2, `Map (Pmap.insert_with (+.) bink2 p2 m))
          | ((prob_total1, `Map binm1), (prob_total2, `Single (bink2, p2)))
          | ((prob_total2, `Single (bink2, p2)), (prob_total1, `Map binm1)) ->
            (prob_total1 +. prob_total2,
             `Map (Pmap.insert_with (+.) bink2 p2 binm1))
          | _ -> failwith "shouldn't happen"
        )
        k
        (p, `Single (bink, p))
        !m)
      d;

    Pmap.mapi (fun k v -> match v with
      | (ptotal, `Single (k2, p)) -> (ptotal, Pmap.add k2 p (Pmap.empty))
      | (ptotal, `Map m) -> (ptotal, m)) !m

  let condition ~dist: adist ~f: f =
    let temp = Util.map_filteri (fun v p -> f v) adist in
    normalize temp

  let condition_and_project ~dist: adist ~what: whatfun ~on: onfun =
    let temp = project_bins adist onfun whatfun in
    Pmap.mapi (fun k (ptotal, m) -> scale (1.0 /. ptotal) m) temp

  let vul ~dist: adist =
    Util.map_max_float adist

  let post_vul (prior: 'a dist) (query: 'a -> 'b): float =
    let (joint: ('a * 'b) dist) = bind prior (fun (input: 'a) ->
      let (output: 'b) = query input in
      return (input, output)
    ) in
    let (bins: ('b, float * ('a dist)) Pmap.t) = project_bins joint snd fst in
    Pmap.fold (fun ((prob: float), (unscaled_post: 'a dist)) (accum:float) ->
      let (post: 'a dist) = scale (1.0 /. prob) unscaled_post in
      accum +. prob *. (vul post)
    ) bins 0.0

  let post_vul_marginal (prior: 'a dist) (query: 'a -> 'b) (margin: 'a -> 'c): float =
    let (joint: ('c * 'b) dist) = bind prior (fun (input: 'a) ->
      let (output: 'b) = query input in
      return (margin input, output)
    ) in
    let (bins: ('b, float * ('c dist)) Pmap.t) = project_bins joint snd fst in
    Pmap.fold (fun ((prob: float), (unscaled_post: 'c dist)) (accum:float) ->
      let (post: 'c dist) = scale (1.0 /. prob) unscaled_post in
      accum +. prob *. (vul post)
    ) bins 0.0
      
      (*
  let post_vul (prior: 'a MM.dist) (query: 'a -> 'b): float =
    let (outputs: 'b MM.dist) =
      MM.bind prior (fun input -> MM.return (query input)) in
    let (outputs_vuls: float MM.dist) =
      MM.bind outputs (fun output ->
        let (post: 'a MM.dist) = MM.condition prior (fun (s:'a) -> (query s) = output) in
        MM.return (MM.vul post)
      ) in
    MM.expect outputs_vuls
      *)

          
  let condition_concrete ~dist: adist ~on: onfun =
    let (new_dist, prob_total) = Pmap.foldi
      (fun v p (new_dist, prob_total) ->
        if onfun v
        then (Pmap.add v p new_dist, prob_total +. p)
        else (new_dist, prob_total))
      adist
      (Pmap.empty, 0.0) in
    scale (1.0 /. prob_total) new_dist

  let support d =
    List.of_enum (Enum.map (fun (v, p) -> v) (Pmap.enum d))

  let to_map d = d
  let of_map d = d
end;;

module ME = struct
  type 'a dist = ('a * prob) Enum.t

  type ('a, 'b) bind = ('a -> 'b dist) -> 'b dist

  let to_map (d: 'a dist) : ('a MM.dist) =
    Enum.fold
      (fun (v,p) a -> Pmap.insert_with (+.) v p a)
      Pmap.empty
      (Enum.clone d)

  let of_map d = Pmap.enum d

  let of_list (choices: ('a * prob) list) : 'a dist =
    List.enum choices

  let length d = 0

  let bind (enum1: 'a dist) (k: 'a -> 'b dist) : 'b dist =
    let enum1 = Enum.clone enum1 in
    let maybe_e1 = ref None in
    let maybe_enum2 = ref None in

    let rec next = fun () ->
      match !maybe_e1 with
        | None ->
          maybe_e1 := Enum.get enum1;
          maybe_enum2 := None;
          begin match !maybe_e1 with
            | None -> raise Enum.No_more_elements
            | Some _ -> next () end
        | Some (v1, p1) ->
          match !maybe_enum2 with
            | None ->
              maybe_enum2 := Some (Enum.clone (k v1));
              next ()
            | Some enum2 -> 
              match Enum.get enum2 with
                | None ->
                  maybe_e1 := None;
                  maybe_enum2 := None;
                  next ()
                | Some (v2, p2) -> (v2, p1 *. p2) in
    Enum.from next

  let bind_flip prob k =
    if is_one prob then bind (of_list [(true,1.0)]) k
    else if is_zero prob then bind (of_list [(false,1.0)]) k
    else
      bind (of_list [(true,prob);
                     (false,1.0 -. prob)]) k
  ;;

  let bind_flipint prob k =
    if is_one prob then bind (of_list [(1,1.0)]) k
    else if is_zero prob then bind (of_list [(0,1.0)]) k
    else
      bind (of_list [(1,prob);
                     (0,1.0 -. prob)]) k
  ;;

  let bind_uniform_in c k =
    let p = 1.0 /. (float_of_int (List.length c)) in
    bind (of_list (List.map (fun v -> (v, p)) c)) k
  ;;

  let bind_uniform : int -> int -> (int -> 'a dist) -> 'a dist = 
    fun a b k ->
      let p = 1.0 /. (float_of_int (b - a + 1)) in
      bind (of_list (List.map (fun v -> (v, p)) (Util.list_range a b))) k
  ;;

  let return v =
    let once = ref true in
    Enum.from (fun () -> if !once
      then begin once := false; (v, 1.0) end
      else raise Enum.No_more_elements)
end;;
