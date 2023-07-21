(*
 * iteration
 *)

(* [iter n f x] is [f^n x]. without [let rec]! *)
let iter : type a. int -> (a -> a) -> a -> a =
  fun n f x ->
    let module L = struct effect Step : (int -> a -> a) end in
    let step n x = if n = 0 then x else perform L.Step (pred n) (f x) in
    begin try
      step n x
    with effect L.Step k ->
      continue k step
    end

let () =
  Printf.printf "succ³⁷(5) = %i\n" @@ iter 37 succ 5

(*
 * state
 *)

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> 'a
end

module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t
  effect Get : t
  effect Put : t -> unit
  let get () = perform Get
  let put x = perform (Put x)
  let run f ~init =
    init |>
    begin match
      f ()
    with
    | res              -> fun x -> res
    | effect (Put y) k -> fun x -> continue k () y
    | effect  Get    k -> fun x -> continue k x  x
    end
end

let () =
  let module IntState = State (struct type t = int end) in
  let open IntState in
  run ~init:42 begin fun () ->
    Printf.printf "%i\n" @@ get () ;
    run ~init:~-(get ()) begin fun () ->
      Printf.printf "  %i\n" @@ get () ;
      put (get () + 1) ;
      Printf.printf "  %i\n" @@ get () ;
      put (get () + 1) ;
      Printf.printf "  %i\n" @@ get () ;
      "subfunction returned"
    end
    |> Printf.printf "%s\n" ;
    Printf.printf "%i\n" @@ get () ;
    put (get () + 1) ;
    Printf.printf "%i\n" @@ get () ;
    put (get () + 1) ;
    Printf.printf "%i\n" @@ get ()
  end

(*
 * an elaboration: references
 *)

module type REF = sig
  type 'a ref
  val ref : 'a -> 'a ref
  val (!) : 'a ref -> 'a
  val (:=) : 'a ref -> 'a -> unit
  val run : (unit -> 'a) -> 'a
end

(* first implementation: with reference IDs and a reference counter
   (uses Obj.magic) *)
module Ref : REF = struct
  type 'a ref = int
  effect New : 'a -> 'a ref
  effect Get : 'a ref -> 'a
  effect Set : ('a ref * 'a) -> unit
  let ref init = perform (New init)
  let (!)  r   = perform (Get r)
  let (:=) r x = perform (Set (r, x))
  let run f =
    let module Count = State (struct type t = int end) in
    Count.run ~init:0 begin fun () ->
      begin try
        f ()
      with
      | effect (New init) k ->
          let r = Count.get () in
          Count.put (succ r) ;
          init |>
          begin match
            continue k r
          with
          | res                               -> fun x -> res
          | effect (Get r')     k when r = r' -> fun x -> Obj.magic continue k x  x
          | effect (Set (r',y)) k when r = r' -> fun x -> Obj.magic continue k () y
          end
          (* actually, the type system cannot statically know that the
             existential type of y, introduced by catching effects Get and Set,
             is equal to the existential type of init, introduced by catching
             effect New; we as a programmer know it dynamically because the IDs
             of the references match (r = r'); so we need black magic there… *)
      end
    end
end

(* second implementation: with local effects in first‐class modules *)
module Ref : REF = struct
  module type Ref = sig
    type a
    effect Get : a
    effect Set : a -> unit
  end
  type 'a ref = (module Ref with type a = 'a)
  effect New : 'a -> 'a ref
  let ref init = perform (New init)
  let (!)  : type a. a ref -> a         = fun (module R)   -> perform  R.Get
  let (:=) : type a. a ref -> a -> unit = fun (module R) x -> perform (R.Set x)
  let run f =
    begin try
      f ()
    with
    | effect (New init) k ->
        (* trick to name the existential type introduced by the matching: *)
        (init, k) |> fun (type a) (init, k : a * (a ref, _) continuation) ->
        let module R =
          struct
            type nonrec a = a
            effect Get : a
            effect Set : a -> unit
          end
        in
        init |>
        begin match
          continue k (module R)
        with
        | result             -> fun x -> result
        | effect  R.Get    k -> fun x -> continue k x  x
        | effect (R.Set y) k -> fun x -> continue k () y
        end
    end
end

let () =
  let open Ref in
  run begin fun () ->
    let r = ref 0 in
    let s = ref 1. in
    let t = ref "" in
    for k = 1 to 6 do
      r := !r + k ;
      s := !s *. float k ;
      t := !t ^ String.make 1 (Char.chr (k + Char.code '0'))
    done ;
    Printf.printf "%i, %g, %s\n" !r !s !t
  end
