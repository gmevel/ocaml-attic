(*
 * Here are several implementations of purely functional heaps. They exhibit the
 * following time complexities:
 *
 *               leftist  | pairing         | binomial        | skew binomial
 *               ---------+-----------------+-----------------+--------------
 *     find_min  O(1)     | O(1)            | O(1)            | O(1)
 *     take_min  O(log n) | O(log n) / O(n) | O(log n)        | O(log n)
 *     insert    O(log n) | O(1)            | O(1) / O(log n) | O(1)
 *     merge     O(log n) | O(1)            | O(log n) [*]    | O(log n) [*]
 *
 *         when 2 complexity are given, this is amortized / worst-case
 *
 *         [*] = Okasaki and Brodal show how to reduce the cost of merge to O(1)
 *
 * (The amortized complexities assume that the data structures are used
 * “linearly”, i.e. we do not reuse old versions of a data structure; to support
 * “persistent” usage, we can modify their implementation in order to use lazy
 * evaluation, as described by Okasaki.)
 *
 * They have been benchmarked with the usage scenario of [Euler.Primes.prime_seq],
 * (in which merging is not required, and an extraction is always followed by an
 * insertion). The leftist heap and the pairing heap appear to be the fastest,
 * on par with each other. The binomial and skew binomial heap are noticeably
 * slower. Here are very gross estimates of the running time:
 *
 *   - leftist:         from 3% faster to 5% slower than pairing
 *   - binomial (GADT)  35% / 70% slower than pairing (with/without memoizing the min)
 *   - binomial         25% / 75% slower than pairing (with/without memoizing the min)
 *   - skew binomial    40% / 75% slower than pairing (with/without memoizing the min)
 *
 * See `Heaps_bench.ml`
 *
 * The best known theoretical data structures are the Brodal heap and the
 * (strict) Fibonacci heap. However, they are complex to implement and exhibit
 * large constant factors that make them less efficient in practice than simpler
 * data structures. The pairing heap is considered one of the best heaps in
 * practice, both simple and fast, although the worst case of take_min is O(n).
 *
 * TODO: Implement skew heaps (≠ skew binomial heaps) and splay heaps (the
 * latter is a bit awkward in a purely functional setting, because accesses
 * modify the data structure).
 *
 * References:
 *   - "Purely Functional Data Structures", Chris Okasaki
 *   - Wikipedia: https://en.wikipedia.org/wiki/Template:Heap_Running_Times
 *
 *)

exception Empty

(* The signature of a type suitable for heap elements. *)
module type PARTIAL_ORD = CCHeap.PARTIAL_ORD

(* The signature of a heap implementation. It is a restriction of the signature
 * of heaps in C-Cube’s Containers library. *)
module type S =
sig
  type t
  type elt
  val empty : t
  val is_empty : t -> bool
  val merge : t -> t -> t
  val insert : elt -> t -> t
  val add : t -> elt -> t
  val find_min_exn : t -> elt
  val take_exn : t -> t * elt
  val take : t -> (t * elt) option
end

module type MAKE = functor (Elt : PARTIAL_ORD) -> S with type elt = Elt.t

(******************************************************************************)

(* This functor takes an implementation of heaps and produces a new
 * implementation where [find_min] is in constant time, and where other
 * operations retain their original complexity. *)
module Make_with_min_memoized (Make_Heap : MAKE) : MAKE =
  functor (Elt : PARTIAL_ORD) ->
struct

  module H = Make_Heap (Elt)

  type elt = Elt.t

  (* We store the minimum separately from the rest of the heap.
   *
   * This is arguably more elegant, and consistently faster (by 10 to 15% in the
   * [Prime.prime_seq] use case, according to benchmarks), than what Okasaki
   * suggests (i.e. storing the minimum along with the whole heap including the
   * minimum). *)
  type t =
    | EmptyHeap
    | NonEmptyHeap of elt * H.t

  let empty : t =
    EmptyHeap

  let is_empty (h : t) : bool =
    h = EmptyHeap

  let merge (h1 : t) (h2 : t) : t =
    begin match h1, h2 with
    | EmptyHeap, h
    | h, EmptyHeap ->
        h
    | NonEmptyHeap (m1, h1), NonEmptyHeap (m2, h2) ->
        if Elt.leq m1 m2 then
          NonEmptyHeap (m1, H.insert m2 (H.merge h1 h2))
        else
          NonEmptyHeap (m2, H.insert m1 (H.merge h1 h2))
    end

  let insert (x : elt) (h : t) : t =
    begin match h with
    | EmptyHeap           -> NonEmptyHeap (x, H.insert x H.empty)
    | NonEmptyHeap (m, h) ->
        if Elt.leq m x then
          NonEmptyHeap (m, H.insert x h)
        else
          NonEmptyHeap (x, H.insert m h)
    end

  let[@inline] add (h : t) (x : elt) : t =
    insert x h

  let find_min_exn (h : t) : elt =
    begin match h with
    | EmptyHeap           -> raise Empty
    | NonEmptyHeap (m, _) -> m
    end

  let take_exn (h : t) : t * elt =
    begin match h with
    | EmptyHeap           -> raise Empty
    | NonEmptyHeap (m, h) ->
        begin match H.take_exn h with
        | exception Empty -> (EmptyHeap, m)
        | (h', m')        -> (NonEmptyHeap (m', h'), m)
        end
    end

  let take (h : t) : (t * elt) option =
    begin try Some (take_exn h) with Empty -> None end

end (* module Make_with_min_memoized *)

(******************************************************************************)

(* LEFTIST HEAPS.
 *)
module Make_LeftistHeap : MAKE = CCHeap.Make (* implemented by the Containers library *)

(******************************************************************************)

(* PAIRING HEAPS.
 *)
module Make_PairingHeap : MAKE =
  functor (Elt : PARTIAL_ORD) ->
struct

  type elt = Elt.t

  (* Our data type is equivalent to the following one:
   *
   *     type tree =
   *       | Tree of elt * tree list
   *     type t = tree option
   *
   * but it avoids boxing the root of the heap. We achieve this with GADTs. *)
  type _ heap =
    | EmptyHeap : [`empty] heap
    | Tree : elt * [`nonempty] heap list -> [`nonempty] heap

  type tree = [`nonempty] heap

  (* the constructor Heap hides the type variable implied by [< ... ]
   * (existential quantification); it does not exist at run-time. *)
  type[@unboxed] t =
    | Heap : [<`empty | `nonempty] heap -> t

  let empty : t =
    Heap EmptyHeap

  let is_empty (h : t) : bool =
    h = Heap EmptyHeap

  let[@inline] singleton (x : elt) : t =
    Heap (Tree (x, []))

  let merge_trees (t1 : tree) (t2 : tree) : tree =
    let Tree (x1, subs1) = t1
    and Tree (x2, subs2) = t2 in
    if Elt.leq x1 x2 then
      Tree (x1, t2 :: subs1)
    else
      Tree (x2, t1 :: subs2)

  let merge (h1 : t) (h2 : t) : t =
    begin match h1, h2 with
    | Heap EmptyHeap, h
    | h, Heap EmptyHeap ->
        h
    | Heap (Tree _ as t1), Heap (Tree _ as t2) ->
        Heap (merge_trees t1 t2)
    end

  let[@inline] insert (x : elt) (h : t) : t =
    merge (singleton x) h

  let[@inline] add (h : t) (x : elt) : t =
    insert x h

  let find_min_exn (h : t) : elt =
    begin match h with
    | Heap EmptyHeap     -> raise Empty
    | Heap (Tree (x, _)) -> x
    end

  let rec merge_nonempty_tree_list (t1 : tree) (ts : tree list) : tree =
    begin match ts with
    | []              -> t1
    | t2 :: []        -> (merge_trees t1 t2)
    | t2 :: t3 :: ts' -> merge_trees (merge_trees t1 t2) (merge_nonempty_tree_list t3 ts')
    end

  let merge_tree_list (ts : tree list) : t =
    begin match ts with
    | []         -> Heap EmptyHeap
    | t1 :: ts'  -> Heap (merge_nonempty_tree_list t1 ts')
    end

  let take_exn (h : t) : t * elt =
    begin match h with
    | Heap EmptyHeap ->
        raise Empty
    | Heap (Tree (x, subs)) ->
        (merge_tree_list subs, x)
    end

  let take (h : t) : (t * elt) option =
    begin match h with
    | Heap EmptyHeap ->
        None
    | Heap (Tree (x, subs)) ->
        Some (merge_tree_list subs, x)
    end

end (* Make_PairingHeap *)

(******************************************************************************)

(* BINOMIAL HEAPS.
 *
 * This is a variant with funny phantom types, but it is less efficient than the
 * more classical implementation. given below. It has [find_min] in O(1).
 *)
module Make_BinomHeap_gadt : MAKE =
  Make_with_min_memoized (
    functor (Elt : PARTIAL_ORD) ->
struct

  type elt = Elt.t

  (* We use phantom types to encode ranks and enforce invariants about them.
   * This prevents many mistakes, and allows to leave the ranks implicit at
   * run-time, but it requires implementing lists-with-decreasing-ranks and
   * lists-with-increasing-ranks, and these lists cannot be scarce (i.e. there
   * must be explicit holes for when a rank is absent), so in the end it is
   * slower than a more classical implementation. *)
  type zero
  type +!'a succ

  type _ tree =
    | Tree : elt * ('rank, [`dense]) trees -> 'rank tree

  (* The type [('r, d') trees] is a list of trees of strictly decreasing ranks;
   * the rank ['r] of the list is one plus the rank of the highest-ranking
   * (i.e. first) position, i.e. the length of the list.
   * The density parameter ['d] is [ [`dense] ] if the list has a tree of every
   * rank up to ['r] excluded, or [ [`scarce] ] if it has holes. *)
  and (_,_) trees =
    | Tnil : (zero, [`dense]) trees
    | Tcons : 'rank tree * ('rank, 'density) trees -> ('rank succ, 'density) trees
    | Thole :              ('rank, 'density) trees -> ('rank succ, [`scarce]) trees

  (* A heap ['r heap] is a list of trees of strictly increasing ranks;
   * the rank ['r] is the rank of the lowest-ranking (i.e. first) position. *)
  type _ heap =
    | Hnil : 'rank heap
    | Hcons : 'rank tree * ('rank succ) heap -> 'rank heap
    | Hhole :              ('rank succ) heap -> 'rank heap

  (* Cons-cells and hole-cells correspond to the digits 1 and 0 (respectively)
   * in the binary representation of the cardinal of the heap. Then, inserting
   * is akin to incrementation and merging is akin to addition. *)

  type[@unboxed] t =
    | Heap of zero heap

  let empty : t =
    Heap Hnil

  let is_empty (h : t) : bool =
    h = Heap Hnil

  let[@inline] singleton_tree (x : elt) : zero tree =
    Tree (x, Tnil)

  let merge_trees (t1 : 'rank tree) (t2 : 'rank tree) : ('rank succ) tree =
    let Tree (x1, subs1) = t1
    and Tree (x2, subs2) = t2 in
    if Elt.leq x1 x2 then
      (Tree (x1, Tcons (t2, subs1)))
    else
      (Tree (x2, Tcons (t1, subs2)))

  let rec merge_tree_into_heap : type rank. rank tree -> rank heap -> rank heap =
  fun t h ->
    begin match h with
    | Hnil ->
        Hcons (t, Hnil)
    | Hhole h' ->
        Hcons (t, h')
    | Hcons (t2, h') ->
        Hhole (merge_tree_into_heap (merge_trees t t2) h')
    end

  let rec merge_heaps : type rank. rank tree option -> rank heap -> rank heap -> rank heap =
  fun ot h1 h2 ->
    begin match ot, h1, h2 with
    (* one of the two heaps is empty: *)
    | None, Hnil, h
    | None, h, Hnil ->
        h
    | Some t, Hnil, h
    | Some t, h, Hnil ->
        merge_tree_into_heap t h
    (* 0+0+0 *)
    | None, Hhole h1', Hhole h2' ->
        Hhole (merge_heaps None h1' h2')
    (* 0+0+1 *)
    | None, Hhole h1', Hcons (t, h2')
    | None, Hcons (t, h1'), Hhole h2'
    | Some t, Hhole h1', Hhole h2' ->
        Hcons (t, merge_heaps None h1' h2')
    (* 0+1+1 *)
    | None, Hcons (t1, h1'), Hcons (t2, h2')
    | Some t1, Hhole h1', Hcons (t2, h2')
    | Some t1, Hcons (t2, h1'), Hhole h2' ->
        Hhole (merge_heaps (Some (merge_trees t1 t2)) h1' h2')
    (* 1+1+1 *)
    | Some t0, Hcons (t1, h1'), Hcons (t2, h2') ->
        Hcons (t0, merge_heaps (Some (merge_trees t1 t2)) h1' h2')
    end

  let[@inline] merge (Heap h1 : t) (Heap h2 : t) : t =
    Heap (merge_heaps None h1 h2)

  let[@inline] insert (x : elt) (Heap h : t) : t =
    Heap (merge_tree_into_heap (singleton_tree x) h)

  let[@inline] add (h : t) (x : elt) : t =
    insert x h

  let rec find_heap_min_exn_aux : type rank. elt -> rank heap -> elt =
  fun m h ->
    begin match h with
    | Hnil                     -> m
    | Hhole h'                -> find_heap_min_exn_aux m h'
    | Hcons (Tree (x, _), h') -> find_heap_min_exn_aux (if Elt.leq m x then m else x) h'
    end

  let rec find_heap_min_exn : type rank. rank heap -> elt =
  fun h ->
    begin match h with
    | Hnil                     -> raise Empty
    | Hhole h'                -> find_heap_min_exn h'
    | Hcons (Tree (x, _), h') -> find_heap_min_exn_aux x h'
    end

  let[@inline] find_min_exn (Heap h : t) : elt =
    find_heap_min_exn h

  (* by careful usage of this smart constructor, we avoid building heaps with
   * trailing zeros… *)
  let[@inline] hhole : type rank. (rank succ) heap -> rank heap =
  fun h ->
    if h = Hnil then Hnil else Hhole h

  let rec rev_append_trees_to_heap : type rank density. (rank, density) trees -> rank heap -> zero heap =
  fun h1 h2 ->
    begin match h1 with
    | Tnil            -> h2
    | Thole h1'      -> rev_append_trees_to_heap h1' (hhole h2)
    | Tcons (t, h1') -> rev_append_trees_to_heap h1' (Hcons (t, h2))
    end

  let rec take_heap_exn_aux : type rank rankm density densitym.
    min_cursor:((rankm, densitym) trees * rankm tree * (rankm succ) heap) ->
    (rank, density) trees -> rank heap ->
    zero heap * elt =
  fun ~min_cursor:((before_m, Tree (m, subs_m), after_m) as min_cursor)
      before h ->
    begin match h with
    | Hnil ->
        let h1 = rev_append_trees_to_heap subs_m Hnil in
        let h2 = rev_append_trees_to_heap before_m (hhole after_m) in
        (merge_heaps None h1 h2, m)
    | Hhole h' ->
        take_heap_exn_aux ~min_cursor (Thole before) h'
    | Hcons ((Tree (x, _) as t), h') ->
        if Elt.leq m x then
          take_heap_exn_aux ~min_cursor (Tcons (t, before)) h'
        else
          take_heap_exn_aux ~min_cursor:(before, t, h') (Tcons (t, before)) h'
    end

  let rec take_heap_exn : type rank density. (rank, density) trees -> rank heap -> zero heap * elt =
  fun before h ->
    begin match h with
    | Hnil           -> raise Empty
    | Hhole h'      -> take_heap_exn (Thole before) h'
    | Hcons (t, h') -> take_heap_exn_aux ~min_cursor:(before, t, h') (Tcons (t, before)) h'
    end

  let[@inline] take_exn (Heap h : t) : t * elt =
    let (h, m) = take_heap_exn Tnil h in (Heap h, m)

  let take (h : t) : (t * elt) option =
    begin try Some (take_exn h) with Empty -> None end

end) (* Make_BinomHeap_gadt *)

(******************************************************************************)

(* BINOMIAL HEAPS.
 *
 * The classical implementation. It has [find_min] in O(1).
 *)
module Make_BinomHeap : MAKE =
  Make_with_min_memoized (
    functor (Elt : PARTIAL_ORD) ->
struct

  type elt = Elt.t

  (* A tree of rank r has r subtrees, of ranks r−1 down to 0, listed by
   * decreasing ranks. *)
  type tree =
    | Tree of int * elt * tree list

  (* A heap is a list of trees of strictly increasing ranks. Some ranks may be
   * absent. *)
  type[@unboxed] t =
    | Heap of tree list

  (* NOTE: there is no need to store the ranks of subtrees: explicit rank info
   * is only needed at the roots of the heap. Hence, an alternative definition
   * may be:
   *
   *     type tree =
   *     | Tree of elt * tree list
   *     type[@unboxed] t =
   *     | Heap of (int * tree) list
   *
   * but this type definition introduces more boxing (the pair type inside the
   * list type), and avoiding this boxing would lead to more verbose code (by
   * defining a custom type for list-of-pairs, for instance). *)

  let[@inline] rank (Tree (r, _, _) : tree) : int =
    r

  let empty : t =
    Heap []

  let is_empty (h : t) : bool =
    h = Heap []

  let[@inline] singleton_tree (x : elt) : tree =
    Tree (0, x, [])

  let merge_trees (t1 : tree) (t2 : tree) : tree =
    let Tree (r, x1, subs1) = t1
    and Tree (_, x2, subs2) = t2 in
    if Elt.leq x1 x2 then
      (Tree (r+1, x1, t2 :: subs1))
    else
      (Tree (r+1, x2, t1 :: subs2))

  let rec merge_tree_into_heap (t : tree) (h : tree list) : tree list =
    begin match h with
    | []       -> [ t ]
    | t1 :: h' ->
        if rank t < rank t1 then
          t :: h
        else
          merge_tree_into_heap (merge_trees t t1) h'
    end

  let rec merge_heaps (h1 : tree list) (h2 : tree list) : tree list =
    begin match h1, h2 with
    | [], h
    | h, [] ->
        h
    | t1::h1', t2::h2' ->
        if rank t1 < rank t2 then
          t1 :: merge_heaps h1' h2
        else if rank t1 > rank t2 then
          t2 :: merge_heaps h1 h2'
        else
          merge_heaps_with_carry (merge_trees t1 t2) h1' h2'
    end
  and merge_heaps_with_carry (t : tree) (h1 : tree list) (h2 : tree list) : tree list =
    begin match h1, h2 with
    | [], h
    | h, [] ->
        merge_tree_into_heap t h
    | t1::h1', t2::h2' ->
        if rank t < rank t1 then
          (* t < t1, t2 *)
          if rank t < rank t2 then
            t :: merge_heaps h1 h2
          (* t = t2 < t1 *)
          else
            merge_heaps_with_carry (merge_trees t t2) h1 h2'
        else
          (* t = t1 < t2 *)
          if rank t1 < rank t2 then
            merge_heaps_with_carry (merge_trees t t1) h1' h2
          (* t = t1 = t2 *)
          else
            t :: merge_heaps_with_carry (merge_trees t1 t2) h1' h2'
    end

  let[@inline] merge (Heap h1 : t) (Heap h2 : t) : t =
    Heap (merge_heaps h1 h2)

  let[@inline] insert (x : elt) (Heap h : t) : t =
    Heap (merge_tree_into_heap (singleton_tree x) h)

  let[@inline] add (h : t) (x : elt) : t =
    insert x h

  let rec find_min_exn_aux : elt -> tree list -> elt =
  fun m h ->
    begin match h with
    | []                   -> m
    | Tree (_, x, _) :: h' -> find_min_exn_aux (if Elt.leq m x then m else x) h'
    end

  let find_min_exn (Heap h : t) : elt =
    begin match h with
    | []                   -> raise Empty
    | Tree (_, x, _) :: h' -> find_min_exn_aux x h'
    end

  let rec take_exn_aux :
    min_cursor:(tree list * tree * tree list) ->
    before:tree list -> tree list ->
    t * elt =
  fun ~min_cursor:((before_m, Tree (_, m, subs_m), after_m) as min_cursor)
      ~before h ->
    begin match h with
    | [] ->
        let h1 = List.rev subs_m in
        let h2 = List.rev_append before_m after_m in
        (Heap (merge_heaps h1 h2), m)
    | (Tree (_, x, _) as t) :: h' ->
        if Elt.leq m x then
          take_exn_aux ~min_cursor ~before:(t :: before) h'
        else
          take_exn_aux ~min_cursor:(before, t, h') ~before:(t :: before) h'
    end

  let[@inline] take_exn (Heap h : t) : t * elt =
    begin match h with
    | []      -> raise Empty
    | t :: h' -> take_exn_aux ~min_cursor:([], t, h') ~before:[t] h'
    end

  let take (h : t) : (t * elt) option =
    begin try Some (take_exn h) with Empty -> None end

end) (* module Make_BinomHeap *)

(******************************************************************************)

(* SKEW BINOMIAL HEAPS.
 *
 * Skew binomial heaps are a variant of binomial heaps where [insert] is in O(1)
 * in the worst case, rather than O(1) amortized / O(log n) worst case.
 *
 * This implementation has [find_min] in O(1).
 *)
module Make_SkewBinomHeap : MAKE =
  Make_with_min_memoized (
    functor (Elt : PARTIAL_ORD) ->
struct

  type elt = Elt.t

  (* A skew tree of rank r has r subtrees, of ranks r−1 down to 0, listed by
   * decreasing ranks. It also has up to r elements as direct children. *)
  type tree =
    | Tree of int * elt * elt list * tree list

  (* A skew heap is a list of trees of strictly increasing ranks, except that
   * the first two trees may have an equal rank. Some ranks may be absent. *)
  type[@unboxed] t =
    | Heap of tree list

  let[@inline] rank (Tree (r, _, _, _) : tree) : int =
    r

  let empty : t =
    Heap []

  let is_empty (h : t) : bool =
    h = Heap []

  let[@inline] singleton_tree (x : elt) : tree =
    Tree (0, x, [], [])

  let merge_trees (t1 : tree) (t2 : tree) : tree =
    let Tree (r, x1, ys1, subs1) = t1
    and Tree (_, x2, ys2, subs2) = t2 in
    if Elt.leq x1 x2 then
      (Tree (r+1, x1, ys1, t2 :: subs1))
    else
      (Tree (r+1, x2, ys2, t1 :: subs2))

  let skew_merge_trees (x : elt) (t1 : tree) (t2 : tree) : tree =
    let Tree (r, x1, ys1, subs1) = t1
    and Tree (_, x2, ys2, subs2) = t2 in
    if Elt.leq x1 x2 then
      if Elt.leq x x1 then
        Tree (r+1, x, x1::ys1, t2::subs1)
      else
        Tree (r+1, x1, x::ys1, t2::subs1)
    else
      if Elt.leq x x2 then
        Tree (r+1, x, x2::ys2, t1::subs2)
      else
        Tree (r+1, x2, x::ys2, t1::subs2)

  (* This assumes the heap is not skew. *)
  let rec merge_tree_into_heap (t : tree) (h : tree list) : tree list =
    begin match h with
    | []       -> [ t ]
    | t1 :: h' ->
        if rank t < rank t1 then
          t :: h
        else
          merge_tree_into_heap (merge_trees t t1) h'
    end

  (* A skew heap is allowed to start with two trees of equal rank. The function
   * below turns a skew heap into a regular heap (that’s what Okasaki calls
   * normalization) *)
  let[@inline] unskew_heap (t1 : tree) (t2 : tree) (h : tree list) : tree list =
    (*! assert (rank t1 = rank t2) ; !*)
    merge_tree_into_heap (merge_trees t1 t2) h

  (* This is regular merging, and assumes the two input heaps are not skew. *)
  let rec merge_heaps_without_carry (h1 : tree list) (h2 : tree list) : tree list =
    begin match h1, h2 with
    | [], h
    | h, [] ->
        h
    | t1::h1', t2::h2' ->
        if rank t1 < rank t2 then
          t1 :: merge_heaps_without_carry h1' h2
        else if rank t1 > rank t2 then
          t2 :: merge_heaps_without_carry h1 h2'
        else
          merge_heaps_with_carry (merge_trees t1 t2) h1' h2'
    end
  and merge_heaps_with_carry (t : tree) (h1 : tree list) (h2 : tree list) : tree list =
    begin match h1, h2 with
    | [], h
    | h, [] ->
        merge_tree_into_heap t h
    | t1::h1', t2::h2' ->
        if rank t < rank t1 then
          (* t < t1, t2 *)
          if rank t < rank t2 then
            t :: merge_heaps_without_carry h1 h2
          (* t = t2 < t1 *)
          else
            merge_heaps_with_carry (merge_trees t t2) h1 h2'
        else
          (* t = t1 < t2 *)
          if rank t1 < rank t2 then
            merge_heaps_with_carry (merge_trees t t1) h1' h2
          (* t = t1 = t2 *)
          else
            t :: merge_heaps_with_carry (merge_trees t1 t2) h1' h2'
    end

  (* The merging function below is vastly more complex than Okasaki’s, because
   * it takes profit from the relaxation of skew heaps whenever possible,
   * whereas Okasaki’s simply un-skews the 2 input heaps before merging them. *)
  let merge_skew_heaps (h1 : tree list) (h2 : tree list) : tree list =
    begin match h1, h2 with
    | t1a::t1b::h1'', t2a::h2' when rank t1a = rank t1b ->
        begin match h2' with
        | t2b::h2'' when rank t2a = rank t2b ->
            (* both [h1] and [h2] have a double: *)
            if rank t1a < rank t2a then
              t1a :: t1b :: merge_heaps_without_carry h1'' (unskew_heap t2a t2b h2'')
            else
              t2a :: t2b :: merge_heaps_without_carry (unskew_heap t1a t1b h1'') h2''
        | _ ->
            (* only [h1] has a double: *)
            if rank t1a < rank t2a then
              t1a :: t1b :: merge_heaps_without_carry h1'' h2
            else
              t2a :: merge_heaps_without_carry (unskew_heap t1a t1b h1'') h2'
        end
    | t1a::h1', t2a::t2b::h2'' when rank t2a = rank t2b ->
            (* only [h2] has a double: *)
            if rank t1a > rank t2a then
              t2a :: t2b :: merge_heaps_without_carry h1 h2''
            else
              t1a :: merge_heaps_without_carry h1' (unskew_heap t2a t2b h2'')
    | t1a::h1', t2a::h2' when rank t1a = rank t2a ->
            (* neither [h1] or [h2] has a double, but together they have one: *)
              t1a :: t2a :: merge_heaps_without_carry h1' h2'
    | _ ->
            (* no double at all: *)
              merge_heaps_without_carry h1 h2
    end

  let[@inline] merge (Heap h1 : t) (Heap h2 : t) : t =
    Heap (merge_skew_heaps h1 h2)

  (* TODO: make insertion more parsimonious by adding the element to the list of
   * children of a tree when there is room for it?
   * (requires storing the length of the list of children) *)
  let insert (x : elt) (Heap h : t) : t =
    begin match h with
    | t1 :: t2 :: h' when rank t1 = rank t2 -> Heap (skew_merge_trees x t1 t2 :: h')
    | _                                     -> Heap (singleton_tree x :: h)
    end

  let[@inline] add (h : t) (x : elt) : t =
    insert x h

  let insert_all (xs : elt list) (h : t) : t =
    List.fold_left add h xs

  let rec find_min_exn_aux : elt -> tree list -> elt =
  fun m h ->
    begin match h with
    | []                      -> m
    | Tree (_, x, _, _) :: h' -> find_min_exn_aux (if Elt.leq m x then m else x) h'
    end

  let find_min_exn (Heap h : t) : elt =
    begin match h with
    | []                      -> raise Empty
    | Tree (_, x, _, _) :: h' -> find_min_exn_aux x h'
    end

  let rec take_exn_aux :
    min_cursor:(tree list * tree * tree list) ->
    before:tree list -> tree list ->
    t * elt =
  fun ~min_cursor:((before_m, Tree (_, m, kids_m, subs_m), after_m) as min_cursor)
      ~before h ->
    begin match h with
    | [] ->
        let h1 = List.rev subs_m in
        let h2 = List.rev_append before_m after_m in
        (insert_all kids_m (Heap (merge_skew_heaps h1 h2)), m)
    | (Tree (_, x, _, _) as t) :: h' ->
        if Elt.leq m x then
          take_exn_aux ~min_cursor ~before:(t :: before) h'
        else
          take_exn_aux ~min_cursor:(before, t, h') ~before:(t :: before) h'
    end

  let[@inline] take_exn (Heap h : t) : t * elt =
    begin match h with
    | []      -> raise Empty
    | t :: h' -> take_exn_aux ~min_cursor:([], t, h') ~before:[t] h'
    end

  let take (h : t) : (t * elt) option =
    begin try Some (take_exn h) with Empty -> None end

end) (* module Make_SkewBinomHeap *)

(******************************************************************************)

(* The best implementation: *)
module Make : MAKE = Make_PairingHeap
