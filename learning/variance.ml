(* invariant (neither covariant nor contravariant) type: *)
module type Inv =
sig
  type 'a t
  val make : 'a -> 'a t
end

(* covariant (increasing) type: *)
module type Co =
sig
  type +'a t
  val make : 'a -> 'a t
end

(* contravariant (decreasing) type: *)
module type Contra =
sig
  type -'a t
  val make : 'a -> 'a t
end

(* bivariant (both covariant and contravariant) type:
 * there is no annotation for that in the language, you can only use it if you
 * know the type definition… *)
module type Bi =
sig
  type 'a t = int
  val make : 'a -> 'a t
end

module F (M : Co) =
struct
  let f :> _ -> [`A | `B] M.t = M.make
  (* val f : [< `A | `B ] -> [ `A | `B ] M.t *)
end

module G (M : Contra) =
struct
  let g :> _ -> [`A | `B] M.t = M.make
  (* val g : [> `A | `B ] -> [ `A | `B ] M.t *)
end

(* bivariant (both covariant and contravariant) type *)
module M : Bi =
struct
  type 'a t = int
  let make x = 42
end

module M' : Inv = M
module Mco : Co = M
module Mcontra : Contra = M

module FM = F (M)
module GM = G (M)

(* variance error: *)
(*module FM' = F (M')*)
(*module GM' = G (M')*)

module FMco = F (Mco)
module GMcontra = G (Mcontra)
