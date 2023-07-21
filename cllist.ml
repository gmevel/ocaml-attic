(* Doubly-linked lists in OCaml *)

module CLList =
struct

  type 'a t =
    {
      value : 'a ;
      mutable prev : 'a t ;
      mutable next : 'a t ;
    }

  let singleton value =
    let rec node =
      {
        value ;
        prev = node ;
        next = node ;
      }
    in
    node

  let cat li1 li2 =
    let li1_last = li1.prev
    and li2_last = li2.prev in
    li1_last.next <- li2 ;
    li2_last.next <- li1 ;
    li1.prev <- li2_last ;
    li2.prev <- li1_last ;
    li1

  let add_first value li =
    cat (singleton value) li

  let add_last li value =
    cat li (singleton value)

  (*
  let iter li f =
    let el = ref li in
    while
      f !el.value ;
      el := !el.next ;
      !el != li
    do () done
  *)

  let iter li f =
    let rec loop el =
      f el.value ;
      if el.next != li then
        loop el.next
    in
    loop li

  (*
  let iter_while li cond f =
    let el = ref li in
    while
      cond !el.value && begin
        f !el.value ;
        el := !el.next ;
        !el != li
      end
    do () done
  *)

  let iter_while li cond f =
    let rec loop el =
      if cond el.value then begin
        f el.value ;
        if el.next != li then
          loop el.next
      end
    in
    loop li

  let exists li pred =
    let rec loop el =
      pred el.value || el.next != li && loop el.next
    in
    loop li

  let exists_bounded li cond pred =
    let rec loop el =
      cond el.value && begin
        pred el.value || el.next != li && loop el.next
      end
    in
    loop li

  (*
  let fold x0 li f =
    let acc = ref x0
    and el  = ref li in
    while
      acc := f !acc !el.value ;
      el := !el.next ;
      !el != li
    do () done ;
    !acc
  *)

  let fold x0 li f =
    let rec loop acc el =
      let acc = f acc el.value in
      if el.next == li then acc else loop acc el.next
    in
    loop x0 li

  (*
  let length li =
    let len = ref 0 in
    iter li (fun _ -> incr len) ;
    !len
  *)

  let length li =
    fold 0 li (fun len _ -> succ len)

  let to_gen li =
    let el   = ref li
    and flag = ref true in
    fun () ->
      if !el != li then begin
        let v = !el.value in
        el := !el.next ;
        Some v
      end
      else if !flag then begin
        flag := false ;
        el := li.next ;
        Some li.value
      end
      else
        None

end (* module CLList *)

let timer ~label f =
  let t0 = Sys.time () in
  f () ;
  let t1 = Sys.time () in
  Printf.printf "%-20s: %g s\n" label (t1 -. t0)

module type EVALUABLE_MODULE =
sig
  val label : string
  module Eval : functor (_ : sig end) -> sig end
end

let timed_eval (module M : EVALUABLE_MODULE) =
  timer ~label:M.label (fun () -> let module Foo = M.Eval (struct end) in ())

module Test_array =
struct
  let label = "array"

  module Eval (_ : sig end) =
  struct

    let primes = Array.make 78_499 0
    let count_primes = ref 1

    let is_prime n =
      let rec loop i =
        if i = !count_primes || primes.(i) * primes.(i) > n then
          true
        else if n mod primes.(i) = 0 then
          false
        else
          loop (succ i)
      in
      loop 0

    let () =
      primes.(0) <- 2 ;
      for n = 2 to 1_000_000 do
        if is_prime n then begin
          primes.(!count_primes) <- n ;
          incr count_primes
        end
      done

  end
end

module Test_cllist =
struct
  let label = "cllist"

  module Eval (_ : sig end) =
  struct

    let primes = CLList.singleton 2

    let is_prime n =
      not @@ CLList.exists_bounded primes (fun p -> p * p <= n) (fun p -> n mod p = 0)

    let () =
      for n = 2 to 1_000_000 do
        if is_prime n then
          ignore @@ CLList.add_last primes n
      done

  end
end

module Test_cllist_gen =
struct
  let label = "cllist with to_gen"

  module Eval (_ : sig end) =
  struct

    let primes = CLList.singleton 2

    let is_prime n =
      let next_prime = CLList.to_gen primes in
      let rec loop () =
        begin match next_prime () with
        | None                     -> true
        | Some p when p * p > n    -> true
        | Some p when n mod p = 0  -> false
        | _                        -> loop ()
        end
      in
      loop ()

    let () =
      for n = 2 to 1_000_000 do
        if is_prime n then begin
          ignore @@ CLList.add_last primes n
        end
      done

  end
end

module Test_ccdeque_gen =
struct
  let label = "CCDeque with to_gen"

  module Eval (_ : sig end) =
  struct

    let primes = CCDeque.create ()

    let is_prime n =
      let next_prime = CCDeque.to_gen primes in
      let rec loop () =
        begin match next_prime () with
        | None                     -> true
        | Some p when p * p > n    -> true
        | Some p when n mod p = 0  -> false
        | _                        -> loop ()
        end
      in
      loop ()

    let () =
      CCDeque.push_front primes 2 ;
      for n = 2 to 1_000_000 do
        if is_prime n then begin
          CCDeque.push_back primes n
        end
      done

  end
end

let () =
  timed_eval (module Test_array) ;
  timed_eval (module Test_cllist) ;
  timed_eval (module Test_cllist_gen) ;
  timed_eval (module Test_ccdeque_gen) ;

(*

in evaluable modules:
  utop:
    array               : 0.896667 s
    cllist              : 1.00333 s
    cllist with to_gen  : 1.19667 s
    CCDeque with to_gen : 1.49333 s
  ocamlopt:
    array               : 0.106666 s
    cllist              : 0.186666 s
    cllist with to_gen  : 0.21 s
    CCDeque with to_gen : 0.253334 s

*)
