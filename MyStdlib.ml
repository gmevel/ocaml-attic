(* last update: 2023-07-30 (OCaml 5.0) *)

(*
 * Some of the functions below are ones for which I felt the need several times
 * in practice and which IMHO are missing from the Stdlib. This includes mere
 * aliases to existing functions, because I struggled with their name (often
 * because of inconsistencies between modules).
 *
 * These functions are signaled with (* ! *) where the number of "!" increases
 * with the urge.
 *
 * Some other functions are there for consistency, even though I may not have
 * needed them several times yet. These are signaled with (* ? *).
 *)

(******************************************************************************)

(**
 **  Higher-order
 **)

(* nice: function composition, backward and forward
 *
 * [h % g % f] parsed as [(h % g) % f]
 * is equivalent to [h % (g % f)] (composition is associative)
 *
 * [g % f @@ x] parsed as [(g % f) @@ x]
 * is equivalent to [g @@ f @@ x] parsed as [g @@ (f @@ x)]
 * but typechecking is done in a different order
 *
 * [f %> g %> h] parsed as [(f %> g) %> h]
 * is equivalent to [f %> (g %> h)] (composition is associative)
 *
 * [x |> f %> g] parsed as [x |> (f %> g)]
 * is equivalent to [x |> f |> g] parsed as [(x |> f) |> g]
 * but typechecking is done in a different order
 *)
let[@inline] ( %  ) g f x = g @@ f @@ x (* !! *)
let[@inline] ( %> ) f g x = x |> f |> g

(* life-changing: naming as `~f` the 1st/2nd/3rd parameter of a higher-order
 * function, which allows invoking the higher-order function with the callback
 * function given in last position, after unnamed arguments.
 *
 * example:
 *
 *     ~~List.iter xs ~f:begin fun x ->
 *       (* ... *)
 *     end
 *)
let[@inline] ( ~~ ) caller ~(f : _ -> _) = caller f
let[@inline] ( ~~~ ) caller x ~(f : _ -> _) = caller x f
let[@inline] ( ~~~~ ) caller x y ~(f : _ -> _) = caller x y f
(*
 * for folding functions: naming one parameter `~f` and another one `~init`,
 * so that we don’t need to remember the actual order of parameters,
 * and we can give the callback function last.
 *
 * example:
 *
 *     ~>List.fold_left xs ~init:M.empty ~f:begin fun m x ->
 *       (* ... *)
 *     end
 *)
let[@inline] ( ~> ) caller ~(f : _ -> _) ~init s = caller f init s (* fold_left *)
let[@inline] ( ~< ) caller ~(f : _ -> _) s ~init = caller f s init (* fold_right *)

module Fun :
sig
  include module type of Fun
  val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) (* ! *)
end =
struct
  include Fun
  let compose = (%)
end (* module Fun *)

(******************************************************************************)

(**
 **  Options
 **)

module Option :
sig
  include module type of Option
  (* ! *)
  (* [get', fold', to_result'] are variants of [get, fold, to_result] where the
   * default value is computed only if needed.
   * TODO: find better naming.
   * Other people are interested in this function as well:
   * https://discuss.ocaml.org/t/option-fold-with-init-taking-a-thunk/11497
   * The Containers library has similar functions named [*_lazy] (but IMHO these
   * names are not great, because there is no proper lazy values). *)
  val get' : 'a option -> compute_default:(unit -> 'a) -> 'a (* !! *)
  val fold' : compute_none:(unit -> 'b) -> some:('a -> 'b) -> 'a option -> 'b
  val to_result' : compute_none:(unit -> 'e) -> 'a option -> ('a, 'e) result
  (* it took me a while to realize that [app] is the same as [fold]: *)
  val app : default:'b -> f:('a -> 'b) -> 'a option -> 'b
  module Infix :
  sig
    val ( or  ) : 'a option -> 'a -> 'a
    val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( |>= ) : 'a option -> ('a -> 'b) -> 'b option
  end
end =
struct
  include Option
  let get' o ~compute_default =
    begin match o with
    | Some x -> x
    | None   -> compute_default ()
    end
  let fold' ~compute_none ~some o =
    begin match o with
    | Some x -> some x
    | None   -> compute_none ()
    end
  let to_result' ~compute_none o =
    begin match o with
    | Some x -> Ok x
    | None   -> Error (compute_none ())
    end
  let app ~default ~f = fold ~none:default ~some:f
  module Infix = struct
    let ( or  ) o d = value ~default:d o
    let ( >>= ) o f = bind o f
    let ( |>= ) o f = map f o
  end
end

(******************************************************************************)

(**
 **  Iterators
 **)

type +!'a iter = ('a -> unit) -> unit

module Iter :
sig
  type +!'a t = 'a iter
  val flat_map : f:('a -> 'b iter) -> 'a iter -> 'b iter
  val empty : 'a iter
  val snoc : 'a iter -> 'a -> 'a iter
  val of_seq : 'a Seq.t -> 'a iter
  val of_list : 'a list -> 'a iter
  val of_array : 'a array -> 'a iter
end =
struct
  type +!'a t = 'a iter
  let iter ~f (it : _ iter) : unit = it f
  let flat_map (type a b) ~(f: a -> b iter) (it : a iter) : b iter =
    fun do_ -> it (fun x -> f x do_)
  let empty : type a. a iter =
    fun do_ -> ()
  let snoc (type a) (it : a iter) (x : a) : a iter =
    fun do_ -> it do_ ; do_ x
  let of_seq xs : _ iter =
    fun do_ -> Seq.iter do_ xs
  let of_list xs : _ iter =
    fun do_ -> List.iter do_ xs
  let of_array xs : _ iter =
    fun do_ -> Array.iter do_ xs
end

(******************************************************************************)

(**
 **  Sequences
 **)

module Seq =
struct

  include Seq

  (* ? *)
  (* alias [combine] to [zip] for consistency with module [List], and since
   * there already is [Seq.split] (however, beware that [List.combine] fails
   * when the lengths differ, whereas [Seq.zip] stops at the shortest length): *)
  let combine = zip

  (* ? *)
  module type TODO = sig
    (* It would make sense to throw all these variants in a submodule named
     * [Seq.Indexed]; this would also spare use the question of how to name them
     * (e.g. "filteri_map" or "filter_mapi" ?). Commented are those that already
     * exist as of OCaml 5.0. *)
    (*! val iteri : (int -> 'a -> unit) -> 'a t -> unit !*)
    (*! val fold_lefti : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b !*)
    val for_alli : (int -> 'a -> bool) -> 'a t -> bool
    val existsi : (int -> 'a -> bool) -> 'a t -> bool
    val findi : (int -> 'a -> bool) -> 'a t -> 'a option
    val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
    val iter2i : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    val fold_left2i : ('a -> int -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
    val for_all2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val unfoldi : ('b -> int -> ('a * 'b) option) -> 'b -> 'a t
    val iteratei : (int -> 'a -> 'a) -> 'a -> 'a t
    val existsi : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (*! val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t !*)
    val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
    val filter_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b t
    val scani : (int -> 'b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
    val groupi : (int -> 'a -> 'a -> bool) -> 'a t -> 'a t t
    val flat_mapi : (int -> 'a -> 'b t) -> 'a t -> 'b t
    val concat_mapi : (int -> 'a -> 'b t) -> 'a t -> 'b t
    val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val map_producti : (int -> 'a -> int -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val partitioni : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val partition_mapi : (int -> 'a -> ('b, 'c) Either.t) -> 'a t -> 'b t * 'c t
  end

  (* ? *)
  let take_whilei : 'a. (int -> 'a -> bool) -> 'a t -> 'a t =
    let rec take_whilei_aux cond i s () =
      begin match s () with
      | Cons (x, s') when cond i x -> Cons (x, take_whilei_aux cond (i+1) s')
      | node                       -> Nil
      end
    in
    fun cond s () -> take_whilei_aux cond 0 s ()

  (* ? *)
  let drop_whilei : 'a. (int -> 'a -> bool) -> 'a t -> 'a t =
    let rec drop_whilei_aux cond i s () =
      begin match s () with
      | Cons (x, s') when cond i x -> drop_whilei_aux cond (i+1) s' ()
      | node                       -> node
      end
    in
    fun cond s () -> drop_whilei_aux cond 0 s ()

  (* ! *)
  (* interruptible version of [fold_left]
   * TODO: the same for [iter], [find], [exists], etc.*)
  let fold_left_whilei : 'a 'b 'c.
    ('b -> int -> 'a -> ('c, 'b) Either.t) ->
    'b -> 'a t -> ('c * 'a t, 'b) Either.t =
    let rec aux f a i s =
      begin match s () with
      | Cons (x, s') ->
          begin match f a i x with
          | Either.Left z  -> Either.Left (z, s')
          | Either.Right y -> aux f y (i+1) s'
          end
      | Nil -> Either.Right a
      end
    in
    fun f a s -> aux f a 0 s

  (* ! *)
  let fold_left_while : 'a 'b 'c.
    ('b -> 'a -> ('c, 'b) Either.t) ->
    'b -> 'a t -> ('c * 'a t, 'b) Either.t =
    fun f a s -> fold_left_whilei (fun a i x -> f a x) a s

  (* BUG REPORT: doc: document that the order in [Seq.product] is compatible
   * with the pointwise order *)

end (* module Sval mapi : (int -> 'a -> 'b) -> 'a t -> 'b teq *)


(******************************************************************************)

(**
 **  Arrays
 **)

module Array =
struct

  include Array

  (* ? *)
  (* IMHO [zip/unzip] are better names than [combine/split], and they are also
   * consistent with module [Seq] and with Haskell (however, beware that
   * [Array.combine] fails when the lengths differ, whereas [Seq.zip] stops at
   * the shortest length): *)
  let zip = combine
  let unzip = split

  (* ? *)
  module type TODO = sig
    (* It would make sense to throw all these variants in a submodule named
     * [Array.Indexed]; this would also spare use the question of how to name
     * them (e.g. "filteri_map" or "filter_mapi" ?). Commented are those that
     * already exist as of OCaml 5.0. *)
    (*! val iteri : (int -> 'a -> unit) -> 'a t -> unit !*)
    (*! val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t !*)
    val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a (* !! *)
    val fold_left_mapi : (int -> 'a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val fold_righti : (int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
    val iter2i : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit (* ! *)
    val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val for_alli : (int -> 'a -> bool) -> 'a t -> bool
    val existsi : (int -> 'a -> bool) -> 'a t -> bool
    val for_all2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val exists2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val findi_opt : (int -> 'a -> bool) -> 'a t -> 'a option (* ! *) (* done *)
    val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option (* done *)
  end

  (* !! *)
  let init_matrix : 'a. int -> int -> (int -> int -> 'a) -> 'a array array =
    fun m n f ->
      Array.init m (fun i -> Array.init n (fun j -> f i j))

  (* ? *)
  let find : 'a. ('a -> bool) -> 'a array -> 'a =
    fun f ar ->
      begin match Array.find_opt f ar with
      | Some x -> x
      | None   -> raise Not_found
      end

  (* ? *)
  let find_mapi : 'a 'b. (int -> 'a -> 'b option) -> 'a array -> 'b option =
    let rec find_mapi f ar i n =
      if i < n then
        let o = f i ar.(i) in
        if o <> None then o
        else find_mapi f ar (i+1) n
      else None
    in
    fun f ar -> find_mapi f ar 0 (Array.length ar)

  (* ? *)
  let findi_idx : 'a. (int -> 'a -> bool) -> 'a array -> int =
    (*! fun f ar -> find_mapi_exn (fun i x -> if f i x then Some i else None) ar !*)
    let rec findi_idx f ar i n =
      if i < n then
        if f i ar.(i) then i
        else findi_idx f ar (i+1) n
      else raise Not_found
    in
    fun f ar -> findi_idx f ar 0 (Array.length ar)

  (* !! *)
  let findi_idx_opt : 'a. (int -> 'a -> bool) -> 'a array -> int option =
    (*! fun f ar -> find_mapi (fun i x -> if f i x then Some i else None) ar !*)
    fun f ar -> try Some (findi_idx f ar) with Not_found -> None

  (* ? *)
  let findi : 'a. (int -> 'a -> bool) -> 'a array -> 'a =
    fun f ar -> ar.(findi_idx f ar)

  (* ! *)
  let findi_opt : 'a. (int -> 'a -> bool) -> 'a array -> 'a option =
    fun f ar -> try Some (findi f ar) with Not_found -> None

  (* ? *)
  let find_idx : 'a. ('a -> bool) -> 'a array -> int =
    fun f ar -> findi_idx (fun _i x -> f x) ar

  (* ! *)
  let find_idx_opt : 'a. ('a -> bool) -> 'a array -> int option =
    fun f ar -> try Some (find_idx f ar) with Not_found -> None

  (* ? *)
  let mem_idx : 'a. 'a -> 'a array -> int =
    fun x ar -> find_idx ((=) x) ar

  (* !! *)
  let mem_idx_opt : 'a. 'a -> 'a array -> int option =
    fun x ar -> find_idx_opt ((=) x) ar

  (* ? *)
  let memq_idx : 'a. 'a -> 'a array -> int =
    fun x ar -> find_idx ((==) x) ar

  (* ? *)
  let memq_idx_opt : 'a. 'a -> 'a array -> int option =
    fun x ar -> find_idx_opt ((==) x) ar

  (*
  let find_all_idx : 'a. ('a -> bool) -> 'a array -> int list =
    let rec find_all_idx f ar i n acc =
      if i < n then
        if f ar.(i) then find_all_idx f ar (i+1) n (i :: acc)
        else find_all_idx f ar (i+1) n acc
      else List.rev acc
    in
    fun f ar -> find_all_idx f ar 0 (Array.length ar) []
  *)

  let find_all_idx : 'a. ('a -> bool) -> 'a array -> int Seq.t =
    let rec find_all_idx f ar i n () =
      if i < n then
        if f ar.(i) then Seq.Cons (i, find_all_idx f ar (i+1) n)
        else find_all_idx f ar (i+1) n ()
      else Seq.Nil
    in
    fun f ar -> find_all_idx f ar 0 (Array.length ar)

  (* Example use: assuming that [a] is sorted w.r.t [compare], then
   * [Array.find_idx_sorted (compare x) a] returns [Ok i] if the element [x] is
   * present in [a] at index [i], or [Error i] if it is not present and [i] is
   * the index where inserting [x] would preserve the order.
   *
   * More generally, [find_idx_sorted pred ar] assumes that the sign of the
   * predicate [pred] is weakly decreasing over successive elements of the array
   * [ar]. In other words, the array is divided into 3 chunks, which may all be
   * empty:
   *
   *   - first there are k1 ≥ 0 elements for which [pred] is positive,
   *   - then there are k2 ≥ 0 elements for which [pred] is null,
   *   - last there are k3 ≥ 0 elements for which [pred] is negative;
   *
   * Then:
   *
   *   - if k2 = 0, the function returns [Error k1]
   *     (this is the index where the predicate flips from positive to negative,
   *     thus where to insert an element which would make the predicate null);
   *   - if k2 = 1, the function returns [Ok k1]
   *     (this is the index where the predicate is null);
   *   - if k2 ≥ 1, the function returns [Ok i] for some k1 ≤ [i] < k1+k2
   *     (this is the range of indexes where the predicate is null);
   *     it is not specified which [i] is returned.
   *
   * Note that, when the predicate is positive for all elements of the array,
   * the function returns [Error (Array.length ar)].
   *)
  let find_idx_sorted : 'a. ('a -> int) -> 'a array -> (int, int) result =
    let rec find_idx_sorted f ar i j =
      assert (0 <= i && i <= j && j <= Array.length ar) ;
      if i < j then begin
        let k = ((j-i) lsr 1) + i in (* this is (i+j)/2 but avoids overflows *)
        let r = f ar.(k) in
        if r = 0 then Ok k
        else if r < 0 then
          find_idx_sorted f ar i k
        else
          find_idx_sorted f ar (k+1) j
      end
      else Error i
    in
    fun f ar -> find_idx_sorted f ar 0 (Array.length ar)

  let shuffle : 'a. 'a array -> unit =
    fun ar ->
      for i = Array.length ar - 1 downto 1 do
        let j = Random.full_int i in (* since 4.13 *)
        let x = ar.(i) in
        ar.(i) <- ar.(j) ;
        ar.(j) <- x ;
      done

  (* ! *)
  (* Note that there exists [Hashtbl.filter_map_inplace] *)
  (* NOTE: A very unsafe version of this function may allow changing the type of
   * the array elements. *)
  let mapi_inplace : 'a. (int -> 'a -> 'a) -> 'a array -> unit =
    fun f ar ->
      for i = 0 to Array.length ar - 1 do
        ar.(i) <- f i ar.(i)
      done

  (* ? *)
  let map_inplace : 'a. ('a -> 'a) -> 'a array -> unit =
    fun f ar ->
      mapi_inplace (fun _i x -> f x) ar

end

(******************************************************************************)

(**
 **  Lists
 **)

module List =
struct

  include List

  (* !! *)
  (* IMHO [zip/unzip] are better names than [combine/split], and they are also
   * consistent with module [Seq] and with Haskell (however, beware that
   * [List.combine] fails when the lengths differ, whereas [Seq.zip] stops at
   * the shortest length): *)
  let zip = combine
  let unzip = split

  (* ! *)
  (* consistency with [List.flatten] and [Seq.flat_map], and IMHO [flat_map] is
   * a more eloquent name than [concat_map]: *)
  let flat_map = concat_map

  (* ? *)
  module type TODO = sig
    (* It would make sense to throw all these variants in a submodule named
     * [List.Indexed]; this would also spare use the question of how to name
     * them (e.g. "filteri_map" or "filter_mapi" ?). Commented are those that
     * already exist as of OCaml 5.0. *)
    (*! val iteri : (int -> 'a -> unit) -> 'a list -> unit !*)
    (*! val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list !*)
    val rev_mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
    val filter_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b t
    val flat_mapi : (int -> 'a -> 'b t) -> 'a t -> 'b t
    val concat_mapi : (int -> 'a -> 'b t) -> 'a t -> 'b t
    val fold_left_mapi : (int -> 'a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a (* !! *)
    val fold_righti : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b (* ! *)
    val iter2i : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val rev_map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val fold_left2i : ('a -> int -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
    val fold_right2i : (int -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
    val for_alli : (int -> 'a -> bool) -> 'a t -> bool
    val existsi : (int -> 'a -> bool) -> 'a t -> bool
    val for_all2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val exists2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val findi : (int -> 'a -> bool) -> 'a t -> 'a
    val find_opti : (int -> 'a -> bool) -> 'a t -> 'a option
    val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
    (*! val filteri : (int -> 'a -> bool) -> 'a list -> 'a list !*)
    val find_alli : (int -> 'a -> bool) -> 'a list -> 'a list
    val partitioni : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val partition_mapi : (int -> 'a -> ('b, 'c) Either.t) -> 'a t -> 'b t * 'c t
    val to_seqi : 'a list -> (int * 'a) Seq.t (* ! *) (* done *)
  end

  (* ! *)
  let to_seqi : 'a. 'a list -> (int * 'a) Seq.t =
    let rec to_seqi_aux i xs () =
      begin match xs with
      | x :: xs' -> Seq.Cons ((i, x), to_seqi_aux (i+1) xs')
      | []       -> Seq.Nil
      end
    in
    fun xs -> to_seqi_aux 0 xs

  (* ? *)
  (* interruptible version of [fold_left]
   * TODO: the same for [iter], [find], [exists], etc.*)
  let fold_left_whilei : 'a 'b 'c.
    ('b -> int -> 'a -> ('c, 'b) Either.t) ->
    'b -> 'a t -> ('c * 'a t, 'b) Either.t =
    let rec aux f a i xs =
      begin match xs with
      | x :: xs' ->
          begin match f a i x with
          | Either.Left z  -> Either.Left (z, xs')
          | Either.Right y -> aux f y (i+1) xs'
          end
      | [] -> Either.Right a
      end
    in
    fun f a xs -> aux f a 0 xs

  (* ! *)
  let fold_left_while : 'a 'b 'c.
    ('b -> 'a -> ('c, 'b) Either.t) ->
    'b -> 'a t -> ('c * 'a t, 'b) Either.t =
    fun f a s -> fold_left_whilei (fun a i x -> f a x) a s

  (* ! *)
  let slice : 'a. int -> 'a list -> 'a list * 'a list =
    let rec slice_aux n pref suff =
      if n > 0 then
        begin match suff with
        | x :: suff' -> slice_aux (n-1) (x :: pref) suff'
        | []         -> raise (Invalid_argument "List.slice")
        end
      else (pref, suff)
    in
    fun n xs -> slice_aux n [] xs

  (* ! *)
  let rec take : 'a. int -> 'a list -> 'a list =
    fun n xs ->
      if n > 0 then
        begin match xs with
        | x :: xs' -> x :: take (n-1) xs'  (* TRMC *)
        | _        -> raise (Invalid_argument "List.take")
        end
      else []

  (* ! *)
  let rec drop : 'a. int -> 'a list -> 'a list =
    fun n xs ->
      if n > 0 then
        begin match xs with
        | _ :: xs' -> drop (n-1) xs'
        | []       -> raise (Invalid_argument "List.drop")
        end
      else xs

  (* ! *)
  let slice_whilei : 'a. (int -> 'a -> bool) -> 'a list -> 'a list * 'a list =
    let rec slice_while_aux cond i pref suff =
      begin match suff with
      | x :: suff' when cond i x -> slice_while_aux cond (i+1) (x :: pref) suff'
      | _                        -> (pref, suff)
      end
    in
    fun cond xs -> slice_while_aux cond 0 [] xs

  (* ! *)
  let slice_while : 'a. ('a -> bool) -> 'a list -> 'a list * 'a list =
    fun cond xs -> slice_whilei (fun _ -> cond) xs

  (* ? *)
  let take_whilei : 'a. (int -> 'a -> bool) -> 'a list -> 'a list =
    let rec take_while_aux cond i xs =
      begin match xs with
      | x :: xs' when cond i x -> x :: take_while_aux cond (i+1) xs' (* TRMC *)
      | _                      -> []
      end
    in
    fun cond xs -> take_while_aux cond 0 xs

  (* ? *)
  let take_while : 'a. ('a -> bool) -> 'a list -> 'a list =
    fun cond xs -> take_whilei (fun _ -> cond) xs

  (* ? *)
  let drop_whilei : 'a. (int -> 'a -> bool) -> 'a list -> 'a list =
    let rec drop_while_aux cond i xs =
      begin match xs with
      | x :: xs' when cond i x -> drop_while_aux cond (i+1) xs'
      | _                      -> xs
      end
    in
    fun cond xs -> drop_while_aux cond 0 xs

  (* ? *)
  let drop_while : 'a. ('a -> bool) -> 'a list -> 'a list =
    fun cond xs -> drop_whilei (fun _ -> cond) xs

  let pop_all_seq : 'a. 'a list -> ('a * 'a list) Seq.t =
    let rec pop_all l r () =
      begin match r with
      | []      -> Seq.Nil
      | x :: r' -> Seq.Cons ((x, List.rev_append l r'), pop_all (x::l) r')
      end
    in
    fun li -> pop_all [] li

  (* impure but efficient O(n): *)
  let shuffle : 'a. 'a list -> 'a list =
    fun xs ->
      let ar = Array.of_list xs in
      Array.shuffle ar ;
      Array.to_list ar

  (* pure but less efficient O(n × log n):
  * see also https://okmij.org/ftp/Haskell/perfect-shuffle.txt *)
  let rec shuffle_pure : 'a. 'a list -> 'a list =
    fun xs ->
      (* shuffle the list first by assigning a random integer key to each element,
      * then by sorting: *)
      let pre_shuffled =
        xs
        |>  List.map (fun x -> (Random.bits (), x))
        |>  List.sort (fun (k1,_) (k2,_) -> Int.compare k1 k2)
      in
      (* erase the keys and re-shuffle the elements that got equal keys (which is
      * unlikely): *)
      let rec reshuffle acc kxs =
        begin match kxs with
        | []         -> acc
        | (_,x)::[]  -> x :: acc
        | (k1,x1)::((k2,_)::_ as kxs') when k1 <> k2 ->
            reshuffle (x1::acc) kxs'
        | (k1,x1)::(_,x2)::((k3,_)::_ as kxs') when k1 <> k3 ->
            let acc' = if Random.bool () then x1::x2::acc else x2::x1::acc in
            reshuffle acc' kxs'
        | (k1,x1)::(_,x2)::(_,x3)::((k4,_)::_ as kxs') when k1 <> k4 ->
            let acc' =
              begin match Random.int 6 with
              | 0 -> x1::x2::x3::acc
              | 1 -> x1::x3::x2::acc
              | 2 -> x2::x1::x3::acc
              | 3 -> x2::x3::x1::acc
              | 4 -> x3::x1::x2::acc
              | _ -> x3::x2::x1::acc
              end in
            reshuffle acc' kxs'
        | (k1,x1)::(_,x2)::(_,x3)::(_,x4)::((k5,_)::_ as kxs') when k1 <> k5 ->
            let acc' =
              begin match Random.int 24 with
              |  0 -> x1::x2::x3::x4::acc
              |  1 -> x1::x2::x4::x3::acc
              |  2 -> x1::x3::x2::x4::acc
              |  3 -> x1::x3::x4::x2::acc
              |  4 -> x1::x4::x2::x3::acc
              |  5 -> x1::x4::x3::x2::acc
              |  6 -> x2::x1::x3::x4::acc
              |  7 -> x2::x1::x4::x3::acc
              |  8 -> x2::x3::x1::x4::acc
              |  9 -> x2::x3::x4::x1::acc
              | 10 -> x2::x4::x1::x3::acc
              | 11 -> x2::x4::x3::x1::acc
              | 12 -> x3::x1::x2::x4::acc
              | 13 -> x3::x1::x4::x2::acc
              | 14 -> x3::x2::x1::x4::acc
              | 15 -> x3::x2::x4::x1::acc
              | 16 -> x3::x4::x1::x2::acc
              | 17 -> x3::x4::x2::x1::acc
              | 18 -> x4::x1::x2::x3::acc
              | 19 -> x4::x1::x3::x2::acc
              | 20 -> x4::x2::x1::x3::acc
              | 21 -> x4::x2::x3::x1::acc
              | 22 -> x4::x3::x1::x2::acc
              | _  -> x4::x3::x2::x1::acc
              end in
            reshuffle acc' kxs'
        | _ ->
            (* very unlikely! *)
            let xs = List.map snd kxs in
            List.rev_append (shuffle_pure xs) acc
        end
      in
      reshuffle [] pre_shuffled

end

(******************************************************************************)

(**
 **  Sets
 **)

module Set =
struct

  include Set

  module type S =
  sig
    include S
    val compare_elt : elt -> elt -> int
    val flat_map : (elt -> t) -> t -> t
    val rev_iter : (elt -> unit) -> t -> unit
    val to_rev_seq_from : elt -> t -> elt Seq.t
  end

  module Make (Ord : OrderedType) : S with type elt = Ord.t and type t = Set.Make(Ord).t =
  struct

    include Set.Make (Ord)

    (* BUG REPORT: doc: document that [Stdlib.compare] MUST NOT be used to
     * compare sets (because a set’s internal representation is not unique). *)

    (* BUG REPORT: doc: document that [Set.compare] is not compatible with set
     * inclusion. *)

    (* ! *)
    (* useful/needed when we have some Set.S module but not the Ord module it
     * was built from: *)
    let compare_elt = Ord.compare

    (* ! *)
    (* not entirely satisfying because it is limited to sets of the same type: *)
    let flat_map f s =
      fold (fun x s' -> union (f x) s') s empty

    (* ! *)
    let rev_iter f s =
      Seq.iter f (to_rev_seq s)

    (* ? *)
    let to_rev_seq_from x s =
      (* FIXME: an internal implementation would be more efficient *)
      Seq.drop_while (fun y -> Ord.compare y x > 0) (to_rev_seq s)

    let to_seq_between x y s =
      (* FIXME: an internal implementation might be more efficient? *)
      Seq.take_while (fun z -> Ord.compare z y <= 0) (to_seq_from x s)

    let to_rev_seq_between x y s =
      (* FIXME: an internal implementation might be more efficient? *)
      Seq.take_while (fun z -> Ord.compare x z <= 0) (to_rev_seq_from y s)

  end

end

(******************************************************************************)

(**
 **  Hashtables
 **)

module Hashtbl =
struct

  include Hashtbl

  (* ! *)
  (* consistency with other modules where “find” means searching with
   * a predicate: *)
  let get = find
  let get_opt = find_opt
  let get_all = find_all

  (* ? *)
  (* consistency with [Array]: *)
  let set = add

  (* this general function is useful on its own, and also to implement other
   * functions (remove, replace, pop) efficiently (i.e. with only one hash
   * computation and one traversal): *)
  let get_update : 'k 'v. ('k, 'v) t -> 'k -> ('v option -> 'v option) -> 'v option =
    (* FIXME: an internal implementation would be more efficient (only one hash
     * computation and one traversal of the data structure) *)
    fun ht k f ->
      let old = Hashtbl.find_opt ht k in
      begin match old, f old with
      | None,   Some v'              -> Hashtbl.add ht k v'
      | Some v, Some v' when v != v' -> Hashtbl.replace ht k v' (* ensure physical equality *)
      | Some _, None                 -> Hashtbl.remove ht k
      | _ -> ()
      end ;
      old

  (* ! *)
  let update : 'k 'v. ('k, 'v) t -> 'k -> ('v option -> 'v option) -> unit =
    fun ht k f -> ignore (get_update ht k f)

  (* ? *)
  let pop_opt : 'k 'v. ('k, 'v) t -> 'k -> 'v option =
    fun ht k -> get_update ht k (fun _ -> None)

  (* !! *)
  let pop : 'k 'v. ('k, 'v) t -> 'k -> 'v =
    fun ht k -> match pop_opt ht k with Some v -> v | None -> raise Not_found

  (* !! *)
  (* TODO: better naming *)
  let get_or_set' : 'k 'v. ('k, 'v) t -> 'k -> default:(unit -> 'v) -> 'v =
    (* FIXME: an internal implementation would be more efficient (only one hash
     * computation and one traversal of the data structure) *)
    fun ht k ~default ->
      begin match Hashtbl.find_opt ht k with
      | None   -> let v = default () in Hashtbl.add ht k v ; v
      | Some v -> v
      end

  (* ? *)
  let get_or_set : 'k 'v. ('k, 'v) t -> 'k -> default:'v -> 'v =
    fun ht k ~default -> get_or_set' ht k ~default:(fun () -> default)

  (* TODO: also patch the functorized interface... *)

  (* BUG REPORT: In Feb 2022 I read the code of Hashtbl in the standard library
   * and tried to understand how it works against concurrency. My notes from
   * then:
   *
   * [Hashtbl.to_seq] could be made more robust against concurrent/later
   * modifications (specifically, resizing: currently [to_seq] can yield
   * bindings several times and skip bindings that at no point (in the history
   * of the hashtable after the creation of the sequence) are removed from the
   * hashtable).
   *
   * In short, it must use [flip_ongoing_traversal]
   *
   * ... in fact [flip_ongoing_traversal] is already not safe against
   * concurrency?
   *   - bug case: traversal starts *after* resizing has begun
   *   - bug case: two concurrent traversals
   *)

end (* module Hashtbl *)

(******************************************************************************)

(**
 **  Stacks
 **)

module Stack =
struct

  include Stack

  (* ! *)
  let of_list xs =
    (* FIXME: an internal implementation would be more efficient (no list
     * reversal/duplication) *)
    let st = create () in
    List.iter (fun x -> push x st) (List.rev xs) ;
    st

  (* BUG REPORT: doc: the doc for [Stack.of_seq] should specify in which order
   * the sequence elements are pushed. *)

end (* module Stack *)


(******************************************************************************)

(**
 **  Strings
 **)

module String =
struct

  include String

  (* ? *)
  module type TODO = sig
    (* It would make sense to throw all these variants in a submodule named
     * [String.Indexed]; this would also spare use the question of how to name
     * them (e.g. "filteri_map" or "filter_mapi" ?). Commented are those that
     * already exist as of OCaml 5.0. *)
    (*! val mapi : (int -> char -> char) -> t -> t !*)
    val fold_lefti : ('a -> int -> char -> 'a) -> 'a -> t -> 'a (* ! *) (* done *)
    val fold_righti : (int -> char -> 'a -> 'a) -> t -> 'a -> 'a (* ! *) (* done *)
    val for_alli : (int -> char -> bool) -> t -> bool
    val existsi : (int -> char -> bool) -> t -> bool
    (*! val iteri : (int -> char -> unit) -> t -> unit !*)
    (*! val to_seqi : t -> (int * char) Seq.t !*)
  end

  (* ! *)
  let rec fold_lefti (f : 'a -> int -> char -> 'a) (init : 'a) (s : string) : 'a =
    snd (String.fold_left (fun (i, a) c -> (i+1, f a i c)) (0, init) s)

  (* ! *)
  let rec fold_righti (f : int -> char -> 'a -> 'a) (s : string) (init : 'a) : 'a =
    snd (String.fold_right (fun c (i, a) -> (i+1, f i c a)) s (0, init))

  let drop (s : string) (n : int) =
    if n = 0 then
      s
    else if n > 0 then
      String.sub s n (String.length s - n)
    else
      String.sub s 0 (String.length s + n)

  (* NOTE: String.{starts_with,ends_with} have been added to Stdlib in 4.13,
   * without the optional argument ?from / ?til *)

  (* ! *)
  let starts_with ?(from : int = 0) (s : string) (sub : string) : bool =
    let len_s = String.length s in
    let len_sub = String.length sub in
    if from < 0 || len_s < from then
      raise @@ Invalid_argument "String.starts_with" ;
    if len_s < from + len_sub then
      false
    else begin
      let exception Break in
      begin try
        for i = 0 to len_sub - 1 do
          if s.[from + i] <> sub.[i] then
            raise Break
        done ;
        true
      with Break ->
        false
      end
    end

  (* ! *)
  let ends_with ?(til : int option) (s : string) (sub : string) : bool =
    let len_s = String.length s in
    let len_sub = String.length sub in
    let til = Option.value ~default:len_s til in
    if til < 0 || len_s < til then
      raise @@ Invalid_argument "String.ends_with" ;
    if til < len_sub then
      false
    else begin
      let exception Break in
      begin try
        for i = 1 to len_sub do
          if s.[til - i] <> sub.[len_sub - i] then
            raise Break
        done ;
        true
      with Break ->
        false
      end
    end

end

(******************************************************************************)

(**
 **  Bytes
 **)

module Bytes =
struct

  include Bytes

  (* ? *)
  module type TODO = sig
    (* It would make sense to throw all these variants in a submodule named
     * [Bytes.Indexed]; this would also spare use the question of how to name
     * them (e.g. "filteri_map" or "filter_mapi" ?). Commented are those that
     * already exist as of OCaml 5.0. *)
    (*! val iteri : (int -> char -> unit) -> t -> unit !*)
    (*! val mapi : (int -> char -> char) -> t -> t !*)
    val fold_lefti : ('a -> int -> char -> 'a) -> 'a -> t -> 'a
    val fold_righti : (int -> char -> 'a -> 'a) -> t -> 'a -> 'a
    val for_alli : (int -> char -> bool) -> t -> bool
    val existsi : (int -> char -> bool) -> t -> bool
    (*! val to_seqi : t -> (int * char) Seq.t !*)
  end

end

(******************************************************************************)

(**
 **  Buffers
 **)

module Buffer =
struct

  include Buffer

  (* BUG REPORT: the doc says "It provides accumulative concatenation of strings
   * in linear time"; there should be the word "amortized" here. *)

  (* [get] is a much more expected name than [nth] IMHO *)
  let get : t -> int -> char = nth

  (* ! *)
  let set : t -> int -> char -> unit =
    fun buf i c ->
      (* FIXME: needs internal implementation *)
      assert false

  (* ! *)
  let iteri : t -> (int -> char -> unit) -> unit =
    fun buf f ->
      (* FIXME: an internal implementation would be more efficient *)
      Seq.iter (fun (i, c) -> f i c) (Buffer.to_seqi buf)

  (* ! *)
  let iter : t -> (char -> unit) -> unit =
    fun buf f ->
      (* FIXME: an internal implementation would be more efficient *)
      Seq.iter f (Buffer.to_seq buf)

end (* module Buffer *)

(******************************************************************************)

(**
 **  Random
 **)

module Random =
struct

  include Random

  (* Random integer generation ([int], [Int32.t], [Int64.t], [Nativeint.t]) in
   * [Stdlib.Random] does not support the full range of integers: it cannot
   * generate negative integers, nor the maximum representable integer (because
   * the upper bound is exclusive). Here are functions that support any integer
   * range, with included bounds. *)

  (* !! *)
  let int_in_range ?(min=0) ~max =
    if min > max then
      raise (Invalid_argument "Random.int_in_range") ;
    (* we use the fact that [Int64.t] has at least one bit more than [int]: *)
    let min = Int64.of_int min
    and max = Int64.of_int max in
    let r = Random.int64 (Int64.sub (Int64.succ max) min) in
    Int64.to_int @@ Int64.add min r

  let int32_in_range ?(min=Int32.zero) ~max =
    if min > max then
      raise (Invalid_argument "Random.int32_in_range") ;
    (* we use the fact that [Int64.t] has more bits than [Int32.t]: *)
    let min = Int64.of_int32 min
    and max = Int64.of_int32 max in
    let r = Random.int64 (Int64.sub (Int64.succ max) min) in
    Int64.to_int32 @@ Int64.add min r

  let rec int64_in_range ?(min=Int64.zero) ~max =
    if min > max then
      raise (Invalid_argument "Random.int64_in_range") ;
    let d = Int64.sub max min in (* subtraction may overflow! *)
    (* no overflow and favorable case, we can use the existing function: *)
    if d >= Int64.zero && d < Int64.max_int then
      Int64.add min (Random.int64 (Int64.succ d))
    else
      (* I tried to do clever stuff by halving the interval and drawing one bit
       * separately with [Random.bool], but having to deal with parities and
       * overflows was annoying; in the end, it’s just simpler like that: *)
      let r = Random.bits64 () in
      if min <= r && r <= max then r
      else int64_in_range ~min ~max

  let nativeint_in_range ?(min=Nativeint.zero) ~max =
    if min > max then
      raise (Invalid_argument "Random.nativeint") ;
    Int64.to_nativeint
      (int64_in_range ~min:(Int64.of_nativeint min) ~max:(Int64.of_nativeint max))

end (* module Random *)

(******************************************************************************)

(**
 **  Logging / Error handling
 **)

module Log =
struct

  let current_filename = ref ""
  let current_line = ref 0

  let error fmt =
    Printf.kprintf (fun s -> print_endline s ; failwith s)
      ("ERROR: %s:%i: " ^^ fmt) !current_filename !current_line

  let warning fmt =
    Printf.eprintf
      ("WARNING: %s:%i: " ^^ fmt ^^ "\n") !current_filename !current_line

  let info fmt =
    Printf.eprintf
      ("INFO: %s:%i: " ^^ fmt ^^ "\n") !current_filename !current_line

end

(* BUG REPORT: doc: mention that all IO functions can raise [Sys_blocked] when
 * operating onnon-blocking FDs
 * https://github.com/ocaml/ocaml/pull/10545#issuecomment-902725645
 *)

(* ! *)
(* BUG REPORT: bugs in Printf’s alternate format (#):
 *   - leading zeros are not thousand-separated
 *   - [space] and [+] disable the alternate format
 * https://discuss.ocaml.org/t/pretty-printing-binary-ints/9062/12
 *)
