(**
 ** Different implementations of classifiers
 **)

(* handy notations (from MyStdlib.ml): *)
let[@inline] ( %  ) g f x = g @@ f @@ x
let[@inline] ( %> ) f g x = x |> f |> g
let[@inline] ( ~~ ) caller ~(f : _ -> _) = caller f
let[@inline] ( ~~~ ) caller x ~(f : _ -> _) = caller x f
let[@inline] ( ~~~~ ) caller x y ~(f : _ -> _) = caller x y f
let[@inline] ( ~> ) caller ~(f : _ -> _) ~init s = caller f init s (* fold_left *)
let[@inline] ( ~< ) caller ~(f : _ -> _) s ~init = caller f s init (* fold_right *)

(*
 * Classify using hash tables:
 *
 * Keys are unordered and buckets are represented as lists which preserve (or
 * reverse) the ordering of the initial sequence.
 *)

(* DON’T USE: Contrary to one might expect, [Hashtbl.find_all] is not constant
 * time after the hash is computed: it takes time and space proportional to the
 * list returned, because it has to build the list by traversing the relevant
 * bucket. Even worse, it is not tail-rec, so it will crash when a given key has
 * too many bindings.
 * Purposely adding many bindings with the same key defeats the design of
 * [Hashtbl], whose efficiency relies on the fact that buckets remain small (few
 * hash collisions). *)
(*
let hashtbl_to_seq_grouped ?(nb_keys=64) ht =
  (* ugly *)
  let keys_seen = Hashtbl.create nb_keys in
  Hashtbl.to_seq_keys ht |> Seq.filter_map begin fun k ->
    if Hashtbl.mem keys_seen k then
      None
    else begin
      Hashtbl.add keys_seen k () ;
      Some (k, Hashtbl.find_all ht k)
    end
  end

let classify_into_rev_list_seq ?(nb_keys=64) ~f xs =
  let ht = Hashtbl.create nb_keys in
  xs
  |>  Seq.map (fun xs -> (f xs, xs))
  |>  Hashtbl.add_seq ht ; (* we cannot use [Hashtbl.of_seq] because
                             it uses [replace] instead of [add] :-( *)
  ht
  |>  hashtbl_to_seq_grouped ~nb_keys
*)

(* USE THIS INSTEAD *)
let hashtbl_update ht k ~f =
  begin match f (Hashtbl.find_opt ht k) with
  | None   -> Hashtbl.remove ht k
  | Some v -> Hashtbl.replace ht k v
  end

let classify_into_rev_list_hashtbl ?(nb_keys=64) ~f xs =
  let ht = Hashtbl.create nb_keys in
  ~~Seq.iter xs ~f:begin fun x ->
    hashtbl_update ht (f x) ~f:begin function
      | None     -> Some [x]
      | Some xs' -> Some (x :: xs')
    end
  end ;
  ht

let classify_into_list_hashtbl ?nb_keys ~f xs =
  let ht = classify_into_rev_list_hashtbl ?nb_keys ~f xs in
  Hashtbl.filter_map_inplace (Fun.const @@ Option.some % List.rev) ht ;
  ht

let classify_into_rev_list_seq ?nb_keys ~f xs =
  xs |> classify_into_rev_list_hashtbl ?nb_keys ~f |> Hashtbl.to_seq

let classify_into_list_seq ?nb_keys ~f xs =
  xs |> classify_into_list_hashtbl ?nb_keys ~f |> Hashtbl.to_seq

(*
 * Classify using maps and sets:
 *
 * Keys are ordered. Buckets are represented:
 *   - either as lists which preserve (or reverse) the ordering of the initial
 *     sequence;
 *   - or as sets; then, within buckets, elements are ordered.
 *
 * Functors make it annoying to use. To spare modules and functors to the user:
 *   - we must disguise the resulting map as a sequence;
 *   - we can take the set module as a first-class module;
 *     if no set appears in the interface, we can hide the set module altogether
 *     by rather taking the element comparison function, as for maps.
 *
 * We can’t make the map module first-class, because of a combination of two
 * limitations of the type system:
 *
 * (A) “The type constructor S.t would escape its scope”:
 *
      let classify_set_into_sets (type key elt)
          (module M : Map.S with type key = key)
          (module S : Set.S with type elt = elt)
          ~(f : elt -> key) (s : S.t) : S.t M.t = (* ERROR *)
        M.empty
 *
 * for sets, we can avoid this issue by quantifying over the type of sets as
 * a locally abstract type (see below), but we can’t do this for maps, because…
 *
 * (B) higher-order polymorphism (i.e quantifying over type constructors [m]) is
 * not supported, and (for now?) neither are constraints over parametrized types
 * in signatures of packed modules:
 *
      let classify_set_into_sets (type key elt m s)
          (module M : Map.S with type key = key and type 's t = m) (* ERROR *)
          (module S : Set.S with type elt = elt and type t = s)
          ~(f : elt -> key) (s : s) : m =
        M.empty
 *
 *)

module ListClassifier (M : Map.S)
: sig
  val classify_into_rev_lists : f:('elt -> M.key) -> 'elt Seq.t -> 'elt list M.t
  val classify_into_lists : f:('elt -> M.key) -> 'elt Seq.t -> 'elt list M.t
end
= struct
  let classify_into_rev_lists ~f s =
    ~>Seq.fold_left s ~init:M.empty ~f:begin fun m x ->
      ~~~M.update (f x) m ~f:begin function
        | None     -> Some [x]
        | Some xs' -> Some (x :: xs')
      end
    end
  let classify_into_lists ~f s =
    s |> classify_into_rev_lists ~f |> M.map List.rev
end

let classify_into_rev_list_seq (type key elt)
    ?(key_compare = Stdlib.compare)
    ~(f: elt -> key) (s : elt Seq.t)
  : (key * elt list) Seq.t =
  let module M = Map.Make (struct type t = key let compare = key_compare end) in
  let module C = ListClassifier (M) in
  s |> C.classify_into_rev_lists ~f |> M.to_seq

let classify_into_list_seq (type key elt)
    ?(key_compare = Stdlib.compare)
    ~(f: elt -> key) (s : elt Seq.t)
  : (key * elt list) Seq.t =
  (* NOTE: reusing [classify_into_rev_list_seq] would(?) keep the rev lists
   * alive longer than we’d like. *)
  let module M = Map.Make (struct type t = key let compare = key_compare end) in
  let module C = ListClassifier (M) in
  s |> C.classify_into_lists ~f |> M.to_seq

module SetClassifier (M : Map.S) (S : Set.S)
: sig
  val classify_into_sets : f:(S.elt -> M.key) -> S.elt Seq.t -> S.t M.t
end
= struct
  (*
  let classify_into_sets ~f s =
    ~>Seq.fold_left s ~init:M.empty ~f:begin fun m x ->
      ~~~M.update (f x) m ~f:begin function
        | None    -> Some (S.singleton x)
        | Some s' -> Some (S.add x s')
      end
    end
  *)
  (* faster? [S.of_list] is faster than repeated additions. *)
  module LC = ListClassifier (M)
  let classify_into_sets ~f s =
    s |> LC.classify_into_rev_lists ~f |> M.map S.of_list
end

let classify_into_set_seq (type key elt s)
    ?(key_compare = Stdlib.compare)
    (module S : Set.S with type elt = elt and type t = s)
    ~(f: elt -> key) (s : elt Seq.t)
  : (key * s) Seq.t =
  let module M = Map.Make (struct type t = key let compare = key_compare end) in
  let module C = SetClassifier (M) (S) in
  s |> C.classify_into_sets ~f |> M.to_seq

let classify_into_sorted_seqs (type key elt)
    ?key_compare
    ?(value_compare = Stdlib.compare)
    ~(f: elt -> key) (s : elt Seq.t)
  : (key * elt Seq.t) Seq.t =
  let module S = Set.Make (struct type t = elt let compare = value_compare end) in
  s
  |>  classify_into_set_seq ?key_compare (module S) ~f
  |>  Seq.map (fun (key, values) -> (key, S.to_seq values))
