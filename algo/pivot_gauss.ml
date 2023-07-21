#require "batteries"
open Batteries

module Array = struct
  include Array
  let init_matrix m n f = Array.init m ((Array.init n) % f)
  let map_matrix f mat = Array.map (Array.map f) mat
  let copy_matrix mat = Array.map Array.copy mat
  let swap a i j =
    let t = a.(i) in
    a.(i) <- a.(j) ;
    a.(j) <- t
end

module type FIELD = sig
  type t
  val zero : t
  val one : t
  val ( ~- ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val is_null : t -> bool
  val compare : t -> t -> int
end

let pivot_gauss (type t) (module F : FIELD with type t = t) mat =
  let m = Array.length mat
  and n = Array.length mat.(0) in
  let mat = Array.copy_matrix mat
  and perm = Array.init m identity
  and coeffs = Array.make m F.zero
  and inversible = ref true
  and inv = Array.init_matrix m n (fun i j -> if i = j then F.one else F.zero)
  and rank = ref m
  and free_vars = ref []
  and bound_vars = ref []
  and sign = ref F.one
  in
  let rec pivot i j =
    if j = n then begin
      rank := i
    end
    else
    if i < m (*&& j < n*) then begin
      begin match find (fun i' -> not @@ F.is_null mat.(i').(j)) (i -- pred m) with
      | exception Not_found ->
          inversible := false ;
          free_vars := j :: !free_vars ;
          pivot i (succ j)
      | i' ->
          bound_vars := j :: !bound_vars ;
          let row = mat.(i') in
          let a = row.(j) in
          coeffs.(i) <- a ;
          if i <> i' then begin
            Array.swap perm i i' ;
            Array.swap mat i i' ;
            Array.swap inv i i' ;
            sign := F.( ~- !sign )
          end ;
          row.(j) <- F.one ;
          for j' = succ j to pred n do
            row.(j') <- F.( row.(j') / a )
          done ;
          for j' = 0 to pred n do
            inv.(i).(j') <- F.( inv.(i).(j') / a )
          done ;
          for i' = 0 to pred m do
            if i' <> i then begin
              let row' = mat.(i') in
              let b = row'.(j) in
              if not @@ F.is_null b then begin
                row'.(j) <- F.zero ;
                for j' = succ j to pred n do
                  row'.(j') <- F.( row'.(j') - b * row.(j') )
                done ;
                for j' = 0 to pred n do
                  inv.(i').(j') <- F.( inv.(i').(j') - b * inv.(i).(j') )
                done
              end
            end
          done ;
          pivot (succ i) (succ j)
      end
    end
  in
  pivot 0 0 ;
  let (det, inv) =
    if m <> n then
      (None, None)
    else if not !inversible then
      (Some F.zero, None)
    else
      (Some (Array.fold_left F.( * ) !sign coeffs), Some inv)
  in
  (mat, coeffs, perm, !rank, det, inv)

(**
 ** TESTING
 **)

let matrix_distance2 (type t) (module F : FIELD with type t = t) mat nat =
  let m = Array.length mat
  and n = Array.length mat.(0) in
  fold begin fun acc i ->
    fold begin fun acc j ->
      let open F in
      let d = mat.(i).(j) - nat.(i).(j) in
      acc + d * d
    end
      acc
      (0 -- pred n)
  end
    F.zero
    (0 -- pred m)

let matrix_prod (type t) (module F : FIELD with type t = t) mat nat =
  let m = Array.length mat
  and n = Array.length nat
  and p = Array.length nat.(0) in
  assert (n = Array.length mat.(0)) ;
  Array.init_matrix m p @@ fun i j ->
    fold (fun acc k -> F.( acc + mat.(i).(k) * nat.(k).(j) )) F.zero (0 -- pred n)

module type PRINTABLE_FIELD = sig
  include FIELD
  val pp : unit BatIO.output -> t -> unit
end

let print_matrix (type t) (module F : PRINTABLE_FIELD with type t = t) out mat =
  Printf.fprintf out "[" ;
  mat |> Array.iter begin fun row ->
    Printf.fprintf out "\n\t[" ;
    row |> Array.iter (Printf.fprintf out "%a\t" F.pp) ;
    Printf.fprintf out "]"
  end ;
  Printf.fprintf out " ]"

let test (type t) (module F : PRINTABLE_FIELD with type t = t) mat =
  let m = Array.length mat
  and n = Array.length mat.(0) in
  let pp_matrix = print_matrix (module F) in
  Printf.printf "test matrix = %a\n" pp_matrix mat ;
  let (pat, coeffs, perm, rank, det, inv) = pivot_gauss (module F) mat in
  Printf.printf "normal row echelon form = %a\n" pp_matrix pat ;
  Printf.printf "rank = %u\n" rank ;
  begin match det with
  | Some det -> Printf.printf "det = %a\n" F.pp det
  | _ -> ()
  end ;
  begin match inv with
  | Some inv ->
      Printf.printf "inversible\ninverse matrix = %a\n" pp_matrix inv ;
      let prod = matrix_prod (module F) mat inv
      and id = Array.init_matrix m n (fun i j -> if i = j then F.one else F.zero) in
      Printf.printf "distance2 to identity matrix = %a\n" F.pp
        (matrix_distance2 (module F) id prod)
  | _ -> ()
  end

let float_field = (module struct
    include Float
    let is_null x =
      Float.abs x <= 5e-15
    let ( ~- ) = ( -. ) 0.
    let pp out x =
      Printf.fprintf out "% -9.3g" (if is_null x then 0. else x)
  end : PRINTABLE_FIELD with type t = float)

let rational_field = (module struct
    include Q
    let is_null = Q.equal Q.zero
    let pp out x =
      Printf.fprintf out "%s" (Q.to_string x)
  end : PRINTABLE_FIELD with type t = Q.t)

let random_test () =
  test float_field (Array.init_matrix 5 5 (fun _ _ -> Random.float 10.))
(*  test rational_field (Array.init_matrix 5 5 (fun _ _ -> Q.of_float @@ Random.float 10.)) *)

let () = 
  random_test () ;
  test rational_field @@ Array.map_matrix Q.of_int [|
    [|  4;  3;  2;  1 |] ;
    [|  0; -7;  1;  0 |] ;
    [| -2;  0;  4; -3 |] ;
  |]
