#!/usr/bin/ocaml nums.cma

let list_init n f =
  let rec aux i = if i = n then [] else f i :: aux (i+1) in
  aux 0

let sgn_even n = 1 - 2 * (n mod 2)

open Ratio
open Big_int

let ratio = ratio_of_int
let big = big_int_of_int

let inv_int n = div_ratio (ratio 1) (ratio n)
let sgn_ratio r = compare_ratio r (ratio 0)

let string_of_ratio (r : ratio) : string =
  let r = normalize_ratio r in
  if compare_big_int (denominator_ratio r) (big 1) = 0 then
    string_of_big_int (numerator_ratio r)
  else
    string_of_ratio r


type polynomial = ratio list

let null : polynomial = []

let rec add (p : polynomial) (q : polynomial) : polynomial = match p,q with
  | pi::p', qi::q' -> (add_ratio pi qi) :: add p' q'
  | [], r
  | r, []          -> r

let mul_scalar (r : ratio) : polynomial -> polynomial =
  List.map (mult_ratio r)

let sigmas (x : int array) : int array =
  let n = Array.length x in
  let s = Array.make (n+1) 0 in
  let rec aux k prefix first =
    s.(k-1) <- s.(k-1) + prefix;
    for i = first to n-1 do
      aux (k+1) (prefix * x.(i)) (i+1)
    done
  in
  aux 1 1 0;
  s

let polynomial_by_roots (coeff : ratio) (roots : int array) : polynomial =
  let s = sigmas roots in
  let n = Array.length roots in
  list_init (n+1) (fun k -> sgn_even (n-k) * s.(n-k))
  |> List.map (fun x -> mult_int_ratio x coeff)

let lagrange (x : int array) : polynomial array =
  let n = Array.length x in
  let discard i =
    Array.append (Array.sub x 0 i) (Array.sub x (i+1) (n-i-1))
  in
  Array.init n
   (fun i ->
     let xi = x.(i) in
     let roots = discard i in
     let prod = Array.fold_left (fun prod xj -> prod * (xi-xj)) 1 roots in
     let coeff = inv_int prod in
     polynomial_by_roots coeff roots
   )

let interpol (x : int array) (y : ratio array) : polynomial =
  lagrange x
  |> Array.mapi (fun i li -> mul_scalar y.(i) li)
  |> Array.fold_left add null


let rec string_power =  
  let string_power_digit d =
    [|"⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹"|].(d)
  in function
  | 0 -> ""
  | k -> string_power (k / 10) ^ string_power_digit (k mod 10)

let string_of_monomial (operand : bool) (k : int) (coeff : ratio) : string =
  let str_coeff = string_of_ratio (abs_ratio coeff) in
  let rec str_abs k str_coeff = match k, str_coeff with
  | 0,  _  -> str_coeff
  | 1, "1" -> "X"
  | _, "1" -> "X" ^ string_power k
  | 1,  _  -> str_coeff ^ " " ^ str_abs 1 "1"
  | _,  _  -> str_coeff ^ " " ^ str_abs k "1"
  in
  match sgn_ratio coeff with
  | 0 -> "0"
  | 1 -> (if operand then ""  else " + ") ^ str_abs k str_coeff
  | _ -> (if operand then "-" else " - ") ^ str_abs k str_coeff

let string_of_polynomial (p : polynomial) : string =
  let rec aux is_first k = function
    | []     -> ""
    | pi::p' -> begin
        match string_of_monomial is_first k pi with
        | "0" -> aux is_first (k+1) p'
        |  s  -> s ^ aux false (k+1) p'
      end
  in
  match aux true 0 p with
  | "" -> "0"
  |  s -> s


let () =
  let y =
   Sys.argv
   |> Array.to_list |> List.tl |> Array.of_list
   |> Array.map ratio_of_string
  in
  let n = Array.length y in
  let x = Array.init n (fun x -> x) in
  let p = interpol x y in
  print_endline (string_of_polynomial p)
