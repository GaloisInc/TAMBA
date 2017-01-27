open ExtList
open ExtArray
open Printf

module MM = Pmonad.MM;;
module ME = Pmonad.MM;;

module C = Common;;

let minSpeed = ref 1;;
let maxSpeed = ref 10;;
let digits = ref 1;;
let margin = ref "ship";;

module P = struct
  type loc  = {lat: int; long: int}
  type ship = {loc: loc; speed: int}

  let fix_params () = ()
      
  let foi = float_of_int
      
  let distance l1 l2 =
    sqrt
      ((((foi l1.lat)  -. (foi l2.lat))  ** 2.0) +.
          (((foi l1.long) -. (foi l2.long)) ** 2.0))
   
  let prior =       MM.bind_uniform (-180) 180
      (fun long  -> MM.bind_uniform (-90) 90
      (fun lat   -> MM.bind_uniform 1 10
      (fun speed -> MM.return {loc = {lat = lat; long = long};
                               speed = speed}
      )));;

  let prec f = sprintf "%0.*f" !digits f
  
  let eta commLoc ship = (distance commLoc ship.loc) /. (foi ship.speed);;
  let query commLoc maxEta ship = (eta commLoc ship) <= maxEta;;

  let margin_ship  = (fun s -> s)
  let margin_loc   = (fun s -> s.loc)
  let margin_speed = (fun s -> s.speed)
  
  let run () =
    let tempComm = {lat = 0; long = 0} in
    
    let (vulPrior, vulPost) = match !margin with
      | "ship"  -> (MM.vul prior,
                    MM.post_vul_marginal prior (fun (s:ship) -> prec (eta tempComm s)) margin_ship)
      | "loc"   -> (MM.vul (MM.bind prior (fun s -> MM.return (margin_loc s))),
                    MM.post_vul_marginal prior (fun (s:ship) -> prec (eta tempComm s)) margin_loc)
      | "speed" -> (MM.vul (MM.bind prior (fun s -> MM.return (margin_speed s))),
                    MM.post_vul_marginal prior (fun (s:ship) -> prec (eta tempComm s)) margin_speed)
      | _ -> (0.0, 0.0)
    in

    let vulPriorBits = Util.log2 (1.0 /. vulPrior) in
    let vulPostBits  = Util.log2 (1.0 /. vulPost) in
    
    let leakage     = vulPost -. vulPrior in
    let leakageBits = vulPriorBits -. vulPostBits in
    
    printf "%s\t%d\t%d\t%d\t" !margin !minSpeed !maxSpeed !digits;
    printf "%0.10f\t%0.10f\t%0.10f\t%0.10f\t%0.10f\t%0.10f\n" vulPrior vulPriorBits vulPost vulPostBits leakage leakageBits;
  
end;;

let () =
  C.parse [("--min-speed",
            Arg.Set_int minSpeed, "min ship speed");
           ("--max-speed",
            Arg.Set_int maxSpeed, "max ship speed");
           ("--digits",
            Arg.Set_int digits, "digits in eta");
           ("--margin",
            Arg.Set_string margin, "measure vulnerability of (ship, loc, or speed)")
          ] (fun () -> "");
  P.fix_params ();
  P.run ()
