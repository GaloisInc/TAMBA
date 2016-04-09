open ExtList
open ExtArray
open Printf

module MM = Pmonad.MM;;
module ME = Pmonad.MM;;

(*module MM = Scenario.MM;;
  module ME = Scenario.ME;;*)
module C = Common;;

let minSpeed = ref 1;;
let maxSpeed = ref 10;;

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
      (fun lat   -> MM.bind_uniform 1 1
      (fun speed -> MM.return {loc = {lat = lat; long = long};
                               speed = speed}
      )));;

  let eta commLoc ship = (distance commLoc ship.loc) /. (foi ship.speed);;
  let query commLoc ship maxEta = (eta commLoc ship) <= maxEta;;

  let run () =
    let tempShip = {loc = {lat = 10; long = 20}; speed = 1} in
    let tempComm = {lat = 0; long = 0} in
    let tempEta  = eta tempComm tempShip in

      
    (*
    printf "tempEta = %f\n" tempEta;
    printf "dist = %f\n" (distance tempComm tempShip.loc);
    *)

    let vulPrior = MM.vul prior in
    
    let post = MM.condition prior (fun ship -> eta tempComm ship = tempEta) in
    
    let vulPost = MM.vul post in

    printf "prior vul = %f\n" vulPrior;
    printf "post  vul = %f\n" vulPost;
    printf "post = %s\n" (MM.to_string post);;

  
  
end;;

let () =
  C.parse [("--min-speed",
            Arg.Set_int minSpeed, "min ship speed");
           ("--max-speed",
            Arg.Set_int maxSpeed, "max ship speed")
          ] (fun () -> "");
  P.fix_params ();
  P.run ()
