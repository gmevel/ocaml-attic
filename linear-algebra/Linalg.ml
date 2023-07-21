module type RING =
sig
  type t
  val zero: t
  val one: t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( / ): t -> t -> t
  val compare: t -> t -> int
  val from_float: float -> t
  val to_float: t -> float
  val to_string: t -> string
end



module type VECTOR =
sig
  type elt
  type t = (*private*) elt array
  exception Bad_dim
  val t: int -> t
  val copy: t -> t
  val dim: t -> int
  val to_string: t -> string
  val map: (elt -> 'a) -> t -> 'a array
  val mapi: (int -> elt -> 'a) -> t -> 'a array
  val map2: (elt -> elt -> 'a) -> t -> t -> 'a array
  val iter: (elt -> unit) -> t -> unit
  val iteri: (int -> elt -> unit) -> t -> unit
  val iter2: (elt -> elt -> unit) -> t -> t -> unit
  val apply: (elt -> elt) -> t -> unit
  val applyi: (int -> elt -> elt) -> t -> unit
  val apply2: (elt -> elt -> elt) -> t -> t -> unit
  val compare: t -> t -> int
  val ( ~+ ): t -> t
  val ( ~- ): t -> t
  val ( ~* ): elt -> elt
  val ( ~/ ): elt -> elt
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): elt -> t -> t
  val ( / ): t -> elt -> t
  val ( += ): t -> t -> unit
  val ( *= ): t -> elt -> unit
  val ( -= ): t -> t -> unit
  val ( /= ): t -> elt -> unit
  val ( := ): t -> t -> unit
  val ( *. ): t -> t -> elt                (* dot product *)
  val ( ** ): t -> t -> elt array array    (* outer product *)
  val norm2: t -> elt
  val norm: t -> elt
  val unit: t -> t
  val normalize: t -> unit
  val angle2: t -> t -> elt
  val angle: t -> t -> elt
  val orthonormal: t list -> t -> t      (* Make the vector orthonormal to the given orthonormal set (can give the null vector) *)
  val gram_schmidt: t list -> t list    (* Gram-Schmidt process *)
end

module KVector (K: RING) =
struct
  type elt = K.t
  type t = elt array
  exception Bad_dim
  
  let t n = Array.create n K.zero
  let copy x = Array.copy x
  let dim = Array.length
  
  let check_dim test = if not test then raise Bad_dim
  
  let map = Array.map
  let mapi = Array.mapi
  let map2 f x y =
    check_dim (dim x = dim y);
    mapi (fun i xi -> f xi y.(i)) x
  let iter = Array.iter
  let iteri = Array.iteri
  let iter2 f x y =
    check_dim (dim x = dim y);
    iteri (fun i xi -> f xi y.(i)) x
  let apply f x = iteri (fun i xi -> x.(i) <- f xi) x
  let applyi f x = iteri (fun i xi -> x.(i) <- f i xi) x
  let apply2 f x y =
    check_dim (dim x = dim y);
    applyi (fun i xi -> f xi y.(i)) x
  
  let to_string x =
    let buf = Buffer.create (5 * dim x) in
    iter (fun xi -> Buffer.add_string buf (" " ^ K.to_string xi)) x;
    Buffer.add_string buf "]";
    let s = Buffer.contents buf in
    s.[0] <- '[';
    s
  
  let compare x y =
    let n = dim x and p = dim y in
    check_dim (n = p);
    let rec compare_rec i =
      if i = n then
        0
      else match K.compare x.(i) y.(i) with
        | 0 -> compare_rec (i+1)
        | c -> c
    in
    compare_rec 0
  
  let assign x y = apply2 (fun xi yi -> yi) x y
  
  let add x y = map2 K.(+) x y
  let scale k x = map (K.( * ) k) x
  let plus x = x
  let minus x = scale K.(zero-one) x
  let times k = k
  let over k = K.(one/k)
  let sub x y = add x (minus y)
  let divide x k = scale (over k) x
  let add_to x y = apply2 K.(+) x y
  let scale_by x k = apply (K.( * ) k) x
  let sub_from x y = add_to x (minus y)
  let divide_by x k = scale_by x (over k)
  
  let dot_prod x y =
    (*Array.fold_left K.(+) K.zero (map2 K.( * ) x y)*)
    let n = dim x in
    check_dim (n = dim y);
    let sum = ref K.zero in
    for i = 0 to n-1 do
      sum := K.( !sum + x.(i)*y.(i) )
    done;
    !sum
  
  let outer_prod x y =
    let n = dim x and p = dim y in
    let m = Array.create_matrix n p K.zero in
    for i = 0 to n-1 do
      for j = 0 to p-1 do
        m.(i).(j) <- K.( x.(i) * y.(j) )
      done
    done;
    m
  
  let norm2 x = dot_prod x x
  let norm x = K.from_float (sqrt (K.to_float (norm2 x)))
  
  let unit x = divide x (norm x)
  let normalize x = divide_by x (norm x)
  
  let angle2 x y = let xy = dot_prod x y in K.( (xy*xy) / ((norm2 x)*(norm2 y)) )
  let angle x y = K.from_float (sqrt (K.to_float (angle2 x y)))
  
  let rec orthonormal ortho_basis x = match ortho_basis with
    | []   -> let zero = t (dim x) in if 0 = compare x zero then zero else unit x
    | e::q -> orthonormal q (sub x (scale (dot_prod e x) e))
  let gram_schmidt set =
    List.rev (List.fold_left (fun set' x -> orthonormal set' x :: set') [] set)
  
  let ( ~+ ) = plus
  let ( ~* ) = times
  let ( ~- ) = minus
  let ( ~/ ) = over
  let ( + ) = add
  let ( * ) = scale
  let ( - ) = sub
  let ( / ) = divide
  let ( += ) = add_to
  let ( *= ) = scale_by
  let ( -= ) = sub_from
  let ( /= ) = divide_by
  let ( := ) = assign

  let ( *. ) = dot_prod
  let ( ** ) = outer_prod
end



module type MATRIX =
sig
  type elt
  type vector = (*private*) elt array
  type t = (*private*) elt array array
  exception Bad_dim
  exception Singular
  val t: int -> int -> t
  val copy: t -> t
  val id: int -> t
  val dim: t -> int * int
  val to_string: t -> string
  val from_vector: vector -> t
  val to_vector: t -> vector
  val row: t -> int -> vector
  val col: t -> int -> vector
  val map: (elt -> 'a) -> t -> 'a array array
  val mapi: ((int*int) -> elt -> 'a) -> t -> 'a array array
  val map2: (elt -> elt -> 'a) -> t -> t -> 'a array array
  val iter: (elt -> unit) -> t -> unit
  val iteri: ((int*int) -> elt -> unit) -> t -> unit
  val iter2: (elt -> elt -> unit) -> t -> t -> unit
  val apply: (elt -> elt) -> t -> unit
  val applyi: ((int*int) -> elt -> elt) -> t -> unit
  val apply2: (elt -> elt -> elt) -> t -> t -> unit
  val compare: t -> t -> int
  val ( ~+ ): t -> t
  val ( ~- ): t -> t
  val ( ~* ): elt -> elt
  val ( ~/ ): elt -> elt
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): elt -> t -> t
  val ( / ): t -> elt -> t
  val ( += ): t -> t -> unit
  val ( *= ): t -> elt -> unit
  val ( -= ): t -> t -> unit
  val ( /= ): t -> elt -> unit
  val ( := ): t -> t -> unit
  val trans: t -> t
  val ( ** ): t -> t -> t    (* matrix multiplication *)
  val ( *| ): t -> vector -> vector    (* right multiplication by a vector *)
  val ( |* ): vector -> t -> vector    (* left multiplication by a vector *)
  val ( |*| ): vector -> t -> vector -> elt    (* Left & right multiplication *)
  val det: t -> elt
  val inverse: t -> t
  val ( *. ): t -> t -> elt    (* dot product *)
  val norm2: t -> elt
  val norm: t -> elt
  val power_iteration: t -> elt -> vector -> (int * elt * vector)
  val inverse_iteration: t -> elt -> vector -> (int * elt * vector)
  val rayleigh_quotient_iteration: t -> elt -> vector -> (int * elt * vector)
end

module KMatrix (K: RING) =
struct
  module Vector = KVector (K)
  type elt = K.t
  type vector = Vector.t
  type t = elt array array
  exception Bad_dim = Vector.Bad_dim
  exception Singular
  
  let t n p = Array.make_matrix n p K.zero
  let copy a = Array.init (Array.length a) (fun i -> Array.copy a.(i))
  let dim m = (Array.length m, Array.length m.(0))
  let id n =
    let m = t n n in
    for i = 0 to n-1 do
      m.(i).(i) <- K.one
    done;
    m
  
  let check_dim test = if not test then raise Bad_dim
  
  let map f = Array.map (Array.map f)
  let mapi f = Array.mapi (fun i -> Array.mapi (fun j -> f (i,j)))
  let map2 f a b =
    check_dim (dim a = dim b);
    mapi (fun (i,j) aij -> f aij b.(i).(j)) a
  let iter f = Array.iter (Array.iter f)
  let iteri f = Array.iteri (fun i -> Array.iteri (fun j -> f(i,j)))
  let iter2 f a b =
    check_dim (dim a = dim b);
    iteri (fun (i,j) aij -> f aij b.(i).(j)) a
  let apply f x = iteri (fun (i,j) xij -> x.(i).(j) <- f xij) x
  let applyi f x = iteri (fun (i,j) xij -> x.(i).(j) <- f (i,j) xij) x
  let apply2 f a b =
    check_dim (dim a = dim b);
    applyi (fun (i,j) aij -> f aij b.(i).(j)) a
  
  let to_string m =
    let (n,p) = dim m in
    let buf = Buffer.create (5 * n * p) in
    Array.iter
     (fun row ->
       Array.iter (fun mij -> Buffer.add_string buf (" " ^ K.to_string mij)) row;
       Buffer.add_string buf "\n")
     m;
    let s = Buffer.contents buf in
    s.[0] <- '[';
    s.[String.length s - 1] <- ']';
    s

  let compare a b =
    let (n,p) = dim a in
    check_dim ((n,p) = dim b);
    let rec compare_rec i j =
      if j = p then
        0
      else if i = n then
        compare_rec 0 (j+1)
      else match K.compare a.(i).(j) b.(i).(j) with
        | 0 -> compare_rec (i+1) j
        | c -> c
    in
    compare_rec 0 0
  
  let from_vector x = [|x|]
  let to_vector m = Array.concat (Array.to_list m)
  let row m i = m.(i)
  let col m j = Array.map (fun row -> row.(j)) m
  
  let assign a b = apply2 (fun aij bij -> bij) a b
  
  let add a b = map2 K.(+) a b
  let scale k a = map (K.( * ) k) a
  let plus a = a
  let minus a = scale K.(zero-one) a
  let times k = k
  let over k = K.(one/k)
  let sub a b = add a (minus b)
  let divide a k = scale (over k) a
  let add_to a b = apply2 K.(+) a b
  let scale_by a k = apply (K.( * ) k) a
  let sub_from a b = add_to a (minus b)
  let divide_by a k = scale_by a (over k)
  
  let trans m =
    let (n,p) = dim m in
    let m' = t p n in
    for i = 0 to n-1 do
      for j = 0 to p-1 do
        m'.(j).(i) <- m.(i).(j)
      done
    done;
    m'
  
  let mul a b =
    let (n,p) = dim a and (p',q) = dim b in
    check_dim (p = p');
    let m = t n q in
    (*let b' = trans b in
    for i = 0 to n-1 do
      for j = 0 to q-1 do
        m.(i).(j) <- Vector.( a.(i) *. b'.(j) )
      done
    done;*)
    for i = 0 to n-1 do
      for j = 0 to q-1 do
        for k = 0 to p-1 do
          m.(i).(j) <- K.( m.(i).(j) + a.(i).(k) * b.(k).(j) )
        done
      done
    done;
    m
  
  let mul_vector_right m x =
    let (n,_) = dim m in
    let y = Vector.t n in
    for i = 0 to n-1 do
      y.(i) <- Vector.( m.(i) *. x )
    done;
    y
  
  let mul_vector_left x m =
    let n = Vector.dim x and (p,q) = dim m in
    check_dim (n = p);
    let y = Vector.t n in
    for j = 0 to q-1 do
      for i = 0 to n-1 do
        y.(j) <- K.( y.(j) + x.(i)*m.(i).(j) );
      done
    done;
    y
  
  let mul_vector_both x a y =
    Vector.( *. ) x (mul_vector_right a y)
  
  (* Assistance function: returns a matrix m with row i and column j removed *)
  let minor m i j =
    let mrow r i = Array.append (Array.sub r 0 i) (Array.sub r (i+1) ((Array.length r) - i - 1)) in
    let m' = mrow m i in
    Array.map (fun x -> mrow x j) m'
  
  (* Assistance function: returns 1 if i is even, otherwise -1 *)
  let fi i = if ((i land 1) = 0) then K.one else K.(zero-one)
                
  (* Returns the determinant of a square matrix *)
  let rec det m =
    match (Array.length m) with
    | 1 -> m.(0).(0)
    | 2 -> K.( m.(0).(0) * m.(1).(1) - m.(0).(1) * m.(1).(0) )
    | _ -> Array.fold_left K.(+) K.zero (Array.mapi (fun i _ -> K.( (fi i) * m.(0).(i) * (det (minor m 0 i)) ))  m)
   
  (* Calculate the inverse of an invertible square matrix. Raises Singular if the determinant is (close to) zero *)        
(*  let inverse m =
    let d = det m in if 0 = K.compare d K.zero then raise Singular else
    let d' = K.(one/d) in
    scale d' (trans (mapi (fun (i,j) mij -> K.( * ) (fi (i+j)) (det (minor m i j))) m))*)
(* ------- dirty copy-paste from http://www.cap-lore.com/code/ *)
let _inverse mat =
let l = Array.length mat in
let am = Array.mapi (fun m row -> (Array.append row
      (Array.init l (fun n -> if m=n then 1. else 0.)))) mat in
for i = 0 to l-1 do (let im = ref 0 and mv = ref (abs_float am.(i).(i)) in
   for j = i+1 to l-1 do (let ae = abs_float am.(j).(i) in
       if (!mv < ae) then (mv := ae; im := j)) done;
   if !mv = 0. then raise Singular;
   if !im > i then (for n = i to (2*l - 1) do
      (let s = am.(i).(n) in am.(i).(n) <- am.(!im).(n); am.(!im).(n) <- s) done);
   let r = 1. /. am.(i).(i) in
   for j = i to 2*l - 1 do (am.(i).(j) <- r *. am.(i).(j)) done;
   for k = i+1 to l-1 do (let f = am.(k).(i) in
      for j = i+1 to 2*l - 1 do (am.(k).(j) <- am.(k).(j) -. f *. am.(i).(j))
      done); done) done;
for i = 0 to l-1 do (for j = i+1 to l-1 do (let p = am.(i).(j) in
      for k = i+1 to 2*l - 1 do
         (am.(i).(k) <- am.(i).(k) -. am.(j).(k) *. p) done) done) done;
Array.map (fun row -> Array.sub row l l) am
let inverse a =
  map K.from_float (_inverse (map K.to_float a))
(* ------- end of dirty copy-paste *)
  
  let dot_prod a b =
    (*Array.fold_left (Array.fold_left K.(+)) K.zero (map2 K.( * ) a b)*)
    let (n,p) = dim a in
    check_dim ((n,p) = dim b);
    let sum = ref K.zero in
    for i = 0 to n-1 do
      sum := K.(+) !sum Vector.( a.(i) *. b.(i) )
    done;
    !sum
  let norm2 a = dot_prod a a
  let norm a = K.from_float (sqrt (K.to_float (norm2 a)))
  
  exception Break
  
  let power_iteration a value0 vector0 =
    let (n,_) = dim a in
    let a' = sub a (scale value0 (id n)) in
    let value = ref value0
    and vector = Vector.copy vector0 in
    let count = ref 0 in
    begin try while true do
      incr count;
      Vector.(:=) vector (mul_vector_right a' vector);
      Vector.normalize vector;
      let value2 = mul_vector_both vector a vector in
      if 0 = K.compare !value value2 then
        raise Break
      else
        value := value2
    done with Break -> () end;
    (!count, !value, vector)
  
  let inverse_iteration a value0 vector0 =
    let (n,_) = dim a in
    let a' = inverse (sub a (scale value0 (id n))) in
    let value = ref value0
    and vector = Vector.copy vector0 in
    let count = ref 0 in
    begin try while true do
      incr count;
      Vector.(:=) vector (mul_vector_right a' vector);
      Vector.normalize vector;
      let value2 = mul_vector_both vector a vector in
      if 0 = K.compare !value value2 then
        raise Break
      else
        value := value2
    done with Break -> () end;
    (!count, !value, vector)
  
  let rayleigh_quotient_iteration a value0 vector0 =
    let (n,_) = dim a in
    let value = ref value0
    and vector = Vector.copy vector0 in
    let count = ref 0 in
    begin try while true do
      incr count;
      Vector.(:=) vector (mul_vector_right (inverse (sub a (scale !value (id n)))) vector);
      Vector.normalize vector;
      let value2 = mul_vector_both vector a vector in
      if 0 = K.compare !value value2 then
        raise Break
      else
        value := value2
    done with Break -> () end;
    (!count, !value, vector)
  
  let ( ~+ ) = plus
  let ( ~* ) = times
  let ( ~- ) = minus
  let ( ~/ ) = over
  let ( + ) = add
  let ( * ) = scale
  let ( - ) = sub
  let ( / ) = divide
  let ( += ) = add_to
  let ( *= ) = scale_by
  let ( -= ) = sub_from
  let ( /= ) = divide_by
  let ( := ) = assign
  
  let ( ** ) = mul
  let ( *| ) = mul_vector_right
  let ( |* ) = mul_vector_left
  let ( |*| ) = mul_vector_both
  
  let ( *. ) = dot_prod
end



module Real =
struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let ( + ) = ( +. )
  let ( * ) = ( *. )
  let ( - ) = ( -. )
  let ( / ) = ( /. )
  let compare x y = if abs_float (x-.y) <= 5.*.epsilon_float then 0 else Pervasives.compare x y
  let from_float x = x
  let to_float x = x
  (*let to_string = string_of_float*)
  let to_string x = Printf.sprintf "%f" x
end

module RVector = KVector (Real)

module RMatrix = KMatrix (Real)



module Stat =
struct
  type elt = Real.t
  type t = RVector.t
  type tt = RMatrix.t
  
  let mean vectors =
    let m = RVector.t (RVector.dim (List.hd vectors)) in
    List.iter (RVector.(+=) m) vectors;
    RVector.(/) m (float (List.length vectors))
  
  let mean_covariance vectors =
    let d = RVector.dim (List.hd vectors) in
    let m = mean vectors in
    let w = RMatrix.t d d in
    List.iter (fun x -> let x' = RVector.(x-m) in RMatrix.(+=) w RVector.(x'**x'))
     vectors;
    (m, w)
  
  let covariance vectors = snd (mean_covariance vectors)
end
