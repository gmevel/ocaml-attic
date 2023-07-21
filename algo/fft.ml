let log2sup m =
  let k = ref 0
  and n = ref 1 in
  while !n < m do
    incr k ;
    n := !n * 2
  done ;
  (!k, !n)

let fast_pow ( *. ) =
  let rec pow acc b n =
    if n = 0 then
      acc
    else if n mod 2 = 0 then
      pow acc (b *. b) (n / 2)
    else
      pow (acc *. b) (b *. b) (n / 2)
  in
  pow

module type SIMPLEFFT_RING = sig
  type t
  (* assumptions on being a ring *)
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  (* additional assumptions: *)
  val two_inv : t (* 2 is inversible *)
  val unit_root : int -> t (* unit_root k is a (2^k)‐th unit root *)
end

module SimpleFFT (R : SIMPLEFFT_RING) = struct

  let pow = fast_pow R.( * ) R.one

  let of_int = fast_pow R.( + ) R.zero R.one

  let two = R.( one + one )

  let rec binary_digits_of_int = function
  | 0 -> []
  | n -> (if n mod 2 == 0 then R.zero else R.one) :: binary_digits_of_int (n / 2)

  let evaluate_polynomial u x =
    Array.fold_right (fun ui y -> R.( ui + x * y )) u R.zero

  let fft_with_root n0 r input =
    let res = Array.make n0 R.zero in (* whatever the initial value *)
    let rec fft_rec n r ~in_off ~res_off =
      if n = 1 then
        res.(res_off) <- if in_off < Array.length input then input.(in_off) else R.zero
      else begin
        let m = n / 2
        and r2 = R.( r * r ) in
        fft_rec m r2 ~in_off                   ~res_off ;
        fft_rec m r2 ~in_off:(in_off + n0 / n) ~res_off:(res_off + m) ;
        let ri = ref R.one in
        for i = 0 to pred m do
          let xi = res.(res_off + i)
          and yi = res.(res_off + m + i) in
          let ri_yi = R.( !ri * yi ) in
          res.(res_off + i)     <- R.( xi + ri_yi ) ;
          res.(res_off + m + i) <- R.( xi - ri_yi ) ;
          ri := R.( !ri * r )
        done
      end
    in
    fft_rec n0 r ~in_off:0 ~res_off:0 ;
    res

  let make_fft m =
    let k, n = log2sup m in
    Printf.printf "n = %i\n" n ;
    let r = R.unit_root k in
    let r_inv = pow r (pred n)
    and n_inv = pow R.two_inv k in
    let fft = fft_with_root n r
    and fft_inv input =
      fft_with_root n r_inv input
      |> Array.map (R.( * ) n_inv)
    in
    (fft, fft_inv)

  let fast_polynomial_prod u v =
    let fft, fft_inv = make_fft (Array.length u + Array.length v) in
    fft_inv (Array.map2 R.( * ) (fft u) (fft v))

  let fast_int_prod x y =
    let u = Array.of_list @@ binary_digits_of_int x
    and v = Array.of_list @@ binary_digits_of_int y in
    let w = fast_polynomial_prod u v in
    evaluate_polynomial w two
end

module Float_SimpleFFTRing : SIMPLEFFT_RING with type t = Complex.t = struct
  type t = Complex.t
  let zero = Complex.zero
  let one = Complex.one
  let ( + ) = Complex.add
  let ( - ) = Complex.sub
  let ( * ) = Complex.mul
  let two_inv = Complex.{ re = 0.5 ; im = 0. }
  let unit_root =
    let rec root z = function
      | 0 -> z
      | k -> root (Complex.neg (Complex.sqrt z)) (pred k)
    in
    root one
end

module Float_SimpleFFT = SimpleFFT (Float_SimpleFFTRing)

let pp_cpx file z =
  Printf.fprintf file "%g%+gi" z.Complex.re z.Complex.im

let test x y =
  let expected = { Complex.re = float (x * y) ; im = 0. }
  and result = Float_SimpleFFT.fast_int_prod x y in
  let d = Complex.(norm @@ sub expected result ) in
  Printf.printf "distance to correct result = %g\n" d

let () =
  Printf.printf "%a\n" pp_cpx @@ Float_SimpleFFT.fast_int_prod 45 97 ; (* expected: 4365 *)
  for _ = 0 to 9 do
    test (Random.int 10_000) (Random.int 10_000)
  done
