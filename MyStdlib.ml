(******************************************************************************)

(**
 ** Higher-order
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
let[@inline] ( %  ) g f x = g @@ f @@ x
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

(******************************************************************************)

(**
 **  Options
 **)

module Option :
sig
  include module type of Option
  val app : default:'b -> f:('a -> 'b) -> 'a option -> 'b
  val odo : ('a -> unit) -> 'a option -> unit
  module Infix :
  sig
    val ( or  ) : 'a option -> 'a -> 'a
    val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( |>= ) : 'a option -> ('a -> 'b) -> 'b option
  end
end =
struct
  include Option
  let app ~default ~f = function
    | Some x -> f x
    | None   -> default
  let odo = iter
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

(*
 *  Arrays
 *)

module Array =
struct

  include Array

  (* Example use: assuming that [a] is sorted w.r.t [compare], then
   * [Array.find_idx_sorted (compare x) a] returns [Ok i] if the element [x] is
   * present in [a] at index [i], or [Error i] if it is not present and [i] the
   * index where inserting [x] would preserve the order. *)
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

  let find_idx : 'a. ('a -> bool) -> 'a array -> int =
    let rec find_idx f ar i n =
      if i < n then
        if f ar.(i) then i
        else find_idx f ar (i+1) n
      else raise Not_found
    in
    fun f ar -> find_idx f ar 0 (Array.length ar)

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

  let shuffle : 'a. 'a array -> unit =
    fun ar ->
      for i = Array.length ar - 1 downto 1 do
        let j = Random.full_int i in (* since 4.13 *)
        let x = ar.(i) in
        ar.(i) <- ar.(j) ;
        ar.(j) <- x ;
      done

end

(******************************************************************************)

(**
 **  Lists
 **)

module List =
struct

  include List

  let rec take : 'a. int -> 'a list -> 'a list =
    fun n xs ->
      begin match n, xs with
      | n, x :: xs' when n > 0 -> x :: take (n-1) xs'
      | _                      -> []
      end

  let rec drop : 'a. int -> 'a list -> 'a list =
    fun n xs ->
      if n > 0 then
        begin match xs with
        | _ :: xs' -> drop (n-1) xs'
        | []       -> []
        end
      else xs

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

(*
 *  Strings
 *)

module String =
struct

  include String

  let drop (s : string) (n : int) =
    if n = 0 then
      s
    else if n > 0 then
      String.sub s n (String.length s - n)
    else
      String.sub s 0 (String.length s + n)

  (* NOTE: String.{starts_with,ends_with} have been added to Stdlib in 4.13,
   * without the optional argument ?from / ?til *)
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
