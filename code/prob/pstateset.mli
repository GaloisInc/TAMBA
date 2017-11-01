open State
open Stateset
open Lang
open Gmp
open Gmp_util
open Printf
open Util
(*open Precise*)
open Ppl_ocaml

module type PSTATESET_TYPE =
  sig
    type stateset
    type pstateset

    module SS: STATESET_TYPE

    (** Copy a pstateset. Would be id, except if the pstateset contains ref-cells, etc. *)
    val copy: pstateset -> pstateset

    (** Make an empty pstateset, which contains no points. *)
    val make_empty: unit -> pstateset

    (** Make a pstateset containing a single point. *)
    val make_point: state -> pstateset

    (** Make a pstateset containing the points in a given stateset. *)
    val make_point_of_stateset: stateset -> pstateset

    (** Make an unconstrained pstateset which models the given variables. *)
    val make_new: Lang.varid list -> pstateset

    (** Add an unconstrained variable to a given pstateset. *)
    val addvar: pstateset -> Lang.varid -> pstateset

    (** Print a pstateset to stdout. *)
    val print: pstateset -> unit

    (** Count the number of points inside a pstateset. *)
    val size: pstateset -> Z.t

    (** Return a measure of the size of the representation. *)
    val rep_size: pstateset -> int

    val slack: pstateset -> Q.t

    (** Combine two pstatesets representing mutually exclusive sets of dimensions into a single
     * pstateset. *)
    val prod: pstateset -> pstateset -> pstateset

    (** Make a pstateset modeling a single given variable, constrained to have values within the
     * given bounds, with each value uniformly possible. *)
    val make_uniform: Lang.varid -> Z.t -> Z.t -> pstateset

    (** Transform a pstateset according to a given statement. *)
    val transform: pstateset -> stmt -> pstateset

    (** Compute the intersection of a pstateset and a regular stateset, redistributing probability
     * mass as necessary. *)
    val intersect: pstateset -> stateset -> pstateset

    val exclude: pstateset -> pstateset -> pstateset list

    (** Check if a pstateset is empty. *)
    val is_empty: pstateset -> bool

    val split: pstateset -> lexp -> (pstateset * pstateset)
    val split_many: pstateset -> lexp -> ((pstateset list) * (pstateset list))

    (** Constrain all of the given variables to be equal to their corresponding values within a
     * given pstateset. *)
    val set_all: pstateset -> (Lang.varid * int) list -> pstateset

    (** From a given pstateset, project only the variables given, constructing a reduced
     * dimensionality resulting pstateset with probability mass redistributed as necessary. *)
    val project: pstateset -> Lang.varid list -> pstateset

    (*val revise: pstateset -> state -> pstateset *)

    (** Get a list of variables modeled by this pstateset. *)
    val vars: pstateset -> Lang.varid list

    (** Enumerate the list of states in this pstateset. *)
    val enum: pstateset -> state list
    val enum_on_vars: pstateset -> Lang.varid list -> state list

    val abstract_plus: pstateset -> pstateset -> pstateset

    val relative_entropy: pstateset -> pstateset -> float

    val prob_scale: pstateset -> Q.t -> pstateset

    val prob_max_min: pstateset -> (Q.t * Q.t)

    val prob_smin_smax: pstateset -> (Z.t * Z.t)
    val prob_pmin_pmax: pstateset -> (Q.t * Q.t)
    val prob_mmin_mmax: pstateset -> (Q.t * Q.t)

    val min_mass: pstateset -> Q.t

    val max_belief: pstateset -> Q.t (* returns the maximal normalized max prob over all points *)

    val is_possible: pstateset -> bool

    val stateset_hull: pstateset -> stateset

    val get_alpha_beta: pstateset -> (int * int)

    val sample_pstateset: pstateset -> int -> (state * (state -> (int * state)) * (varid * int) list) list -> pstateset

    val improve_lower_bounds: (state -> bool) -> (state -> Z.t * polyhedron) -> state -> int -> pstateset -> pstateset
  end;;
