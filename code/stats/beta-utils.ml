#require "pareto";;
open Pareto.Distributions;;


let fl = float_of_int;;

let beta2normal n a b = 
  let sq x = x *. x in
  let mu = fl n *. a /. (a +. b) in 
  let s2 = (fl (n * n) *. a *. b) /. (sq (a +. b) *. (a +. b +. 1.0)) in
  (mu, s2);;

let normal2beta n mu s2 = 
  let s = mu *. (fl n -. mu) /. s2 -. 1.0 in
  let a = s *. mu /. fl n in
  let b = s -. a in
  (a,b);;

(* eps is the weight of the tail *)
let bounds2normal eps lo hi =
  (* normal distribution is symmetric *)
  let mu = (lo +. hi) /. 2.0 in
  let stdNormal = normal 0.0 1.0 in
  let z = Normal.quantile stdNormal (eps /. 2.0) in
  (* The scaling transformation that maps z to (hi-lo)/2 *
   * also maps 1.0 to the new standard deviation         *) 
  let s = (hi -. lo) /. (2.0 *. z) in
  let s2 = s *. s in
  (mu, s2);;

let bounds2beta n eps lo hi =
  let (mu,s2) = bounds2normal eps lo hi in
  let (a,b) = normal2beta n mu s2 in
  (n,a,b);;

let addBetas (n1,a1,b1) (n2,a2,b2) =
  let (mu1, s21) = beta2normal n1 a1 b1 in
  let (mu2, s22) = beta2normal n2 a2 b2 in
  let (a,b) = normal2beta (n1 + n2) (mu1 +. mu2) (s21 +. s22) in
  (n1 + n2, a, b);;