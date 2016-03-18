open Ppl_ocaml
open Gmp
open Printf
open List
open Random

let num k = Coefficient (Z.of_int k);;
let num2 k = Z.of_int k;;
let neg m = Unary_Minus m;;
let x = Variable 0;;
let y = Variable 1;;
let z = Variable 2;;

(* Our Starting polyhedron.
   Defined by the following constraints:
      x +  y >=  2
      x - 2y >=  4
     -x + 2y >= -2
     -x -  y >= -8
 *)
let p1 = ppl_new_C_Polyhedron_from_space_dimension 2 Universe;;
ppl_Polyhedron_add_constraint p1 (Greater_Or_Equal (Plus (x, y) , num 2));;
ppl_Polyhedron_add_constraint p1 (Greater_Or_Equal (Minus (x, Times (num2 2, y)) , neg (num 4)));;
ppl_Polyhedron_add_constraint p1 (Greater_Or_Equal (Plus (neg x, Times (num2 2, y)) , neg (num 2)));;
ppl_Polyhedron_add_constraint p1 (Greater_Or_Equal (Minus (neg x, y) , neg (num 8)));;

(* Our starting point for Gibbs Sampling
    x = 2
    y = 1
 *) 
let start_point = ppl_new_C_Polyhedron_from_space_dimension 2 Universe;;
ppl_Polyhedron_add_constraint start_point (Equal (x , num 2));;
ppl_Polyhedron_add_constraint start_point (Equal (y , num 1));;

(* TODO: Ask Piotr is there is any reason that this shouldn't be space
         (and therefore affine)
 *)
let num_dimensions = ppl_Polyhedron_space_dimension p1;;

(* Get the min and max of that line within the polyhedron
let (b1, y_min1, y_min2, b2) = ppl_Polyhedron_minimize p2 y;;
let (b3, y_max1, y_max2, b4) = ppl_Polyhedron_maximize p2 y;;
let min_float = Q.to_float (Q.from_zs y_min1 y_min2);;
let max_float = Q.to_float (Q.from_zs y_max1 y_max2);;

(* Approximate min and max to line up with integer points *)
let (hi, lo) = (ceil max_float, ceil min_float);; 

(* get a random y value on that line *)
let our_val = int_of_float (floor (lo +. (Random.float (hi -. lo))));;

let c = ppl_Polyhedron_contains_Polyhedron p1 p_small2;;
*)

let p3 = ppl_new_C_Polyhedron_from_generators [Point (Plus (Times (num2 3, x), Plus (Times (num2 5, y), Times (num2 7, z))), num2 2)];;
let constraints = ppl_Polyhedron_get_constraints p3;;
(* let generators = ppl_Polyhedron_get_generators p_small2;; *)

(* let main = printf "Dimensions of p1: %d\n" num_dimensions;;*)
let main = printf "This is our polyhedron: \n\n%s\n"
              (ppl_Polyhedron_ascii_dump p1);;

(*
let p_small = ppl_new_C_Polyhedron_from_space_dimension 2 Universe;;
ppl_Polyhedron_add_constraint p_small (Equal (x , num 0));;
ppl_Polyhedron_add_constraint p_small (Equal (y , num 0));;

(* Add the constraint that forms a line at x == 2 *)
ppl_Polyhedron_add_constraint p2 (Equal (x , num 2));;


For copying:
let p2 = ppl_new_C_Polyhedron_from_C_Polyhedron p1;;

let main = printf "\nmin_float: %f\nmax_float: %f\n" min_float max_float;;

let main = printf "\ny_min1: %s\ny_min2: %s\n\ny_max1: %s\ny_max2: %s\n\nour_z: %s" (Z.to_string y_min1)
                                                                       (Z.to_string y_min2)
                                                                       (Z.to_string y_max1)
                                                                       (Z.to_string y_max2)
                                                                       (Q.to_string our_z);;


let main = printf "This is our polyhedron: \n\n%s\n\n%s\n"
              (ppl_Polyhedron_ascii_dump p1) (ppl_Polyhedron_ascii_dump p2);;
*)
