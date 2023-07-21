module type RING =
sig
  type t
  val zero: t
  val one: t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( / ): t -> t -> t
  val compare: t -> t -> int
  val from_float: float -> t
  val to_float: t -> float
  val to_string: t -> string
end

module type VECTOR =
sig
  type elt
  type t = (*private*) elt array
  exception Bad_dim
  val t: int -> t
  val copy: t -> t
  val dim: t -> int
  val to_string: t -> string
  val map: (elt -> 'a) -> t -> 'a array
  val mapi: (int -> elt -> 'a) -> t -> 'a array
  val map2: (elt -> elt -> 'a) -> t -> t -> 'a array
  val iter: (elt -> unit) -> t -> unit
  val iteri: (int -> elt -> unit) -> t -> unit
  val iter2: (elt -> elt -> unit) -> t -> t -> unit
  val apply: (elt -> elt) -> t -> unit
  val applyi: (int -> elt -> elt) -> t -> unit
  val apply2: (elt -> elt -> elt) -> t -> t -> unit
  val compare: t -> t -> int
  val ( ~+ ): t -> t
  val ( ~- ): t -> t
  val ( ~* ): elt -> elt
  val ( ~/ ): elt -> elt
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): elt -> t -> t
  val ( / ): t -> elt -> t
  val ( += ): t -> t -> unit
  val ( *= ): t -> elt -> unit
  val ( -= ): t -> t -> unit
  val ( /= ): t -> elt -> unit
  val ( := ): t -> t -> unit
  val ( *. ): t -> t -> elt                (* dot product *)
  val ( ** ): t -> t -> elt array array    (* outer product *)
  val norm2: t -> elt
  val norm: t -> elt
  val unit: t -> t
  val normalize: t -> unit
  val angle2: t -> t -> elt
  val angle: t -> t -> elt
  val orthonormal: t list -> t -> t      (* Make the vector orthonormal to the given orthonormal set (can give the null vector) *)
  val gram_schmidt: t list -> t list    (* Gram-Schmidt process *)
end

module type MATRIX =
sig
  type elt
  type vector = (*private*) elt array
  type t = (*private*) elt array array
  exception Bad_dim
  exception Singular
  val t: int -> int -> t
  val copy: t -> t
  val id: int -> t
  val dim: t -> int * int
  val to_string: t -> string
  val from_vector: vector -> t
  val to_vector: t -> vector
  val row: t -> int -> vector
  val col: t -> int -> vector
  val map: (elt -> 'a) -> t -> 'a array array
  val mapi: ((int*int) -> elt -> 'a) -> t -> 'a array array
  val map2: (elt -> elt -> 'a) -> t -> t -> 'a array array
  val iter: (elt -> unit) -> t -> unit
  val iteri: ((int*int) -> elt -> unit) -> t -> unit
  val iter2: (elt -> elt -> unit) -> t -> t -> unit
  val apply: (elt -> elt) -> t -> unit
  val applyi: ((int*int) -> elt -> elt) -> t -> unit
  val apply2: (elt -> elt -> elt) -> t -> t -> unit
  val compare: t -> t -> int
  val ( ~+ ): t -> t
  val ( ~- ): t -> t
  val ( ~* ): elt -> elt
  val ( ~/ ): elt -> elt
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): elt -> t -> t
  val ( / ): t -> elt -> t
  val ( += ): t -> t -> unit
  val ( *= ): t -> elt -> unit
  val ( -= ): t -> t -> unit
  val ( /= ): t -> elt -> unit
  val ( := ): t -> t -> unit
  val trans: t -> t
  val ( ** ): t -> t -> t    (* matrix multiplication *)
  val ( *| ): t -> vector -> vector    (* right multiplication by a vector *)
  val ( |* ): vector -> t -> vector    (* left multiplication by a vector *)
  val ( |*| ): vector -> t -> vector -> elt    (* Left & right multiplication *)
  val det: t -> elt
  val inverse: t -> t
  val ( *. ): t -> t -> elt    (* dot product *)
  val norm2: t -> elt
  val norm: t -> elt
  val power_iteration: t -> elt -> vector -> (int * elt * vector)
  val inverse_iteration: t -> elt -> vector -> (int * elt * vector)
  val rayleigh_quotient_iteration: t -> elt -> vector -> (int * elt * vector)
end

module KVector: functor (K:RING) -> VECTOR with type elt = K.t

module KMatrix: functor (K:RING) -> MATRIX with type elt = K.t

module Real: RING with type t = float

module RVector: VECTOR with type elt = float

module RMatrix: MATRIX with type elt = float

module Stat :
sig
  type elt = Real.t
  type t = RVector.t
  type tt = RMatrix.t
  val mean: t list -> t
  val covariance: t list -> tt
  val mean_covariance: t list -> (t * tt)
end
