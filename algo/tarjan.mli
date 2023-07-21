(** Union-find algorithm (on keys) + mapping keys to values *)

(** Base type *)
type ('a, 'b) t

(** Union-find primitives on keys *)
val equiv     : ('a, 'b) t -> 'a -> 'a -> bool
val merge     : ('a, 'b) t -> 'a -> 'a -> 'b -> unit

(** Mapping keys to values *)
val add       : ('a, 'b) t -> 'a -> 'b -> unit
val replace   : ('a, 'b) t -> 'a -> 'b -> unit
val find      : ('a, 'b) t -> 'a -> 'b option

(** Other operations *)
val create : unit -> ('a, 'b) t
val iter_repr : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val to_list   : ('a, 'b) t -> ('a * 'b) list