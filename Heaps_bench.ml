(* compile and run with:
 *
 *     ocamlfind ocamlopt -package containers -linkpkg Heap.ml bench-heaps.ml -o bench-heaps.exe
 *     ./bench-heaps.exe
 *
 *)

module H = Heap

(* Here is the heap-using code that we use for benchmarking the various
 * implementations of heaps: an incremental, purely functional version of the
 * sieve of Eratosthenes for enumerating prime numbers (this is NOT the best
 * possible version of it).
 *
 * Usage scenario:
 * - the heap is never empty, it grows over time, to size O(√nmax / log nmax);
 * - the minimum element is polled 1 or 2 times before being popped;
 * - each time an element is popped from the heap, a greater one is inserted;
 * - heaps are never merged.
 *)
module Make_PrimeSeq (Make_Heap : H.MAKE) :
sig
  val prime_seq : int -> int Seq.t
end
=
struct

  type multiple_of_prime = {
    multiple : int ;
    prime : int ;
  }

  module MultHeap =
    Make_Heap (struct
      type t = multiple_of_prime
      let leq = (<=)
    end)

  let prime_seq nmax =
    let sqrt_nmax = truncate@@sqrt@@float nmax in (* correct for nmax ≤ 2^52 *)
    let rec seq_aux n next_mults : int Seq.t = fun () ->
      (* end of the sequence: *)
      if n > nmax || n < 0 then
        Seq.Nil
      (* [n] is composite: *)
      else if (MultHeap.find_min_exn next_mults).multiple <= n then begin
        let next_mults = ref next_mults in
        while
          let (next_mults', m) = MultHeap.take_exn !next_mults in
          let new_mult = m.multiple + (m.prime lsl 1) in
          if new_mult >= 0 then
            next_mults := MultHeap.add next_mults' { m with multiple = new_mult }
          else
            next_mults := next_mults' ;
          (MultHeap.find_min_exn !next_mults).multiple <= n
        do () done ;
        seq_aux (n+2) !next_mults ()
      end
      (* [n] is prime: *)
      else begin
        let next_mults' =
          if n <= sqrt_nmax then
            MultHeap.add next_mults { multiple = n*n ; prime = n }
          else
            next_mults
        in
        Seq.Cons (n, seq_aux (n+2) next_mults')
      end
    in
    let first_mult = { multiple = 9 ; prime = 3 } in
    let next_mults = MultHeap.add MultHeap.empty first_mult in
    Seq.cons 2 @@
    Seq.cons 3 @@
    seq_aux 5 next_mults

end (* Make_PrimeSeq *)

(*
 * TESTS
 *)

let timed ?msg f x =
  begin match msg with
  | None -> ()
  | Some msg -> Printf.printf "%s %!" msg
  end ;
  Gc.compact () ;
  let t0 = Sys.time () in
  let y = f x in
  Printf.printf "--- %.3g s\n%!" (Sys.time () -. t0) ;
  y

let perf_test_prime_seq nmax desc (module Make_Heap : H.MAKE) =
  let module P = Make_PrimeSeq (Make_Heap) in
  let sum =
    timed ~msg:("incremental prime sieve ["^desc^"]") begin fun nmax ->
      Seq.fold_left (+) 0 (P.prime_seq nmax)
    end nmax
  in
  (desc, sum)

let perf_test nmax =
  (* Perf test: *)
  Printf.printf "performance test ...\n%!" ;
  [
    ("heap=LEFTIST",          (module H.Make_LeftistHeap)) ;
    ("heap=PAIRING",          (module H.Make_PairingHeap)) ;
    ("heap=BINOM(gadt)",      (module H.Make_BinomHeap_gadt)) ;
    ("heap=BINOM(gadt)+minmemo", (module H.Make_with_min_memoized (H.Make_BinomHeap_gadt))) ;
    ("heap=BINOM",            (module H.Make_BinomHeap)) ;
    ("heap=BINOM+minmemo",    (module H.Make_with_min_memoized (H.Make_BinomHeap))) ;
    ("heap=SKBINOM",          (module H.Make_SkewBinomHeap)) ;
    ("heap=SKBINOM+minmemo",  (module H.Make_with_min_memoized (H.Make_SkewBinomHeap))) ;
  ]
  |> List.map (fun (desc, heap_module) -> perf_test_prime_seq nmax desc heap_module)
  (* Correctness test: verify that all sums are equal *)
  |> List.iter (fun (desc, sum) -> Printf.printf "sum[%-24s] = %i\n" desc sum)

let () =
  perf_test 50_000_000 ;
  (*! perf_test 1_000_000_000 ; !*)
