open ExtList
open ExtArray
open Printf

module MM = Scenario.MM;;
module ME = Scenario.ME;;
module C = Common;;

let obs_penalty = ref 0.00;;

module P = struct
  include Scenario.MAKE_FTYPES (struct
    type dact = int option
    type high = int
    type low = int
    type obs = int
    type exp = int
  end)

  let all_dacts = ref [];;
  let all_highs = ref [];;
  let all_obss = ref [-1;0;1];;
  let all_lows = ref [];;
  let all_exps = ref [];;
  let all_acts_exp = ref [];;
  let all_acts_low = ref [];;

  let fix_params () =
    all_highs := Util.list_range 0 (!C.nlocs-1);
    all_exps := !all_highs;
    all_lows := (-1) :: !all_highs;
    all_acts_exp := List.map (fun e -> Exp e) !all_exps;
    all_acts_low := List.map (fun l -> Low l) !all_lows;
    all_dacts := None :: (List.map (fun h -> Some h) !all_highs)
              
  let all_dactstratfs = ref
    [|
      (fun (t: int) (tmax: int)
        (highs: high list)
        (acts: act list)
        (obss: obs list) -> 
          if (t mod !C.change_freq) = 0 then
            ME.bind_uniform_in (List.map (fun h -> Some h) !all_highs) ME.return
          else ME.return None)
    |]

  let dactstrat_span_func t tmax highs acts obss =
    if (t mod !C.change_freq) = 0 then (List.map (fun h -> Some h) !all_highs) else [None]

  let highgen_func t tmax highs acts obss dact =
    match dact with
      | Some h -> ME.return h
      | None -> begin
        match highs with
          | current_high :: _ -> ME.return current_high
          | [] -> failwith "highgen_func: not enough history"
      end
      
  let dactstrat_prior_func () = ME.return 0

  let system_func t tmax highs acts obss =
    match (highs, acts) with
      | (current_high :: _, (Low current_low) :: _) ->
        if current_low = -1 then ME.return (-1)
        else if current_high = current_low
        then ME.return 1
        else ME.return 0
      | _ -> failwith "system_func: unexpected"

  let actionstrat_span_func t tmax acts obss =
    if (not !C.adapt_wait) && (t < tmax) then
      [Low (-1); Low (t mod !C.nlocs)]
    else
      if t = tmax then (Low (-1)) :: !all_acts_exp
      else (Low (-1)) :: (Low (t mod !C.nlocs)) :: !all_acts_exp

  let _count_obss lows = 
    List.fold_left (fun a l -> a +
      (match l with
        | Low l when l >= 0 -> 1
        | _ -> 0)) 0 lows

  let gain_func t tmax highs acts obss =
    let obs_cost = 0.0 -. (!obs_penalty *. (float_of_int (_count_obss acts))) in
    match (acts, highs) with
      | ((Exp e) :: _,  current_high :: _) -> 
        ME.return (obs_cost +.
                     (if current_high = e
                      then 1.0
                      else 0.0))
      | ((Low l) :: _, _) -> ME.return obs_cost
      | _ -> failwith "gain_func: not enough history provided to evaluate gain"
          
  let loss_func t tmax highs acts obss =
    if !C.zerosum then
      gain_func t tmax highs acts obss
    else
      begin 
        match (acts, highs) with
          | ((Exp e) :: _,  current_high :: _) -> 
            ME.return
              (if current_high = e
               then 1.0
               else 0.0)
          | ((Low l) :: _, _) -> ME.return 0.0
          | _ -> failwith "loss_func: not enough history provided to evaluate loss"
      end
end;;

module S = Scenario.SCENARIO (P);;

let () =
  C.parse [("--obs-penalty",
            Arg.Set_float obs_penalty, "observation penalty")]
    (fun () -> sprintf "--obs-penalty %f" !obs_penalty);
  P.fix_params ();

  S.simulate !C.tmax

