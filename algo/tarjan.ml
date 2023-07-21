(** Union-find algorithm (on keys) + mapping keys to values *)

(* invisible type *)
type ('a,'b) o = V of 'a | R of int ref * 'b

(** Base type *)
type ('a,'b) t = ('a, ('a,'b) o) Hashtbl.t    

(** Other operations : create a table *)
let create () = Hashtbl.create 30

(** Mapping keys to values : assign a value to a key *)
let add t e v = Hashtbl.add t e (R (ref 0,v))

(* Auxiliary fin *)
let rec find t e = 
  match Hashtbl.find t e with
  | R(r, v) -> e, (r, v)
  | V e1 -> 
      let e2, _ as c2 = find t e1 in
      if e1 <> e2 then 
        Hashtbl.replace t e (V e2);
        c2

(** Union-find primitives on keys : merge two keys. *)
let merge t e1 e2 v =
  let f1, (r1, v1) = find t e1 in
  let f2, (r2, v2) = find t e2 in
  assert (f1 <> f2);
  let n1 = !r1 in
  let n2 = !r2 in
  let n = 1 + n1 + n2 in
  if n1 < n2 then begin
    r2 := n; 
    Hashtbl.replace t f1 (V f2);
    Hashtbl.replace t f2 (R (r1, v));
  end else begin
    r1 := n; 
    Hashtbl.replace t f2 (V f1);
    Hashtbl.replace t f1 (R (r2, v));
  end

(** Union-find primitives on keys : are two keys equivalent ? *)
let equiv t e1 e2 =
  e1 = e2 ||
    try fst (find t e1) = fst (find t e2)
    with Not_found -> false

(** Mapping keys to values : replace an old value of a key *)
let replace t e v = 
  let f, (r, _) = find t e in
  Hashtbl.replace t f (R (r, v))

(** Mapping keys to values : getting the value from a key *)
let find t e =
  try Some (snd (snd (find t e)))
  with Not_found -> None 

(** Other operations : iterate on all (key, values) couples *)
let iter_repr f t =
  Hashtbl.iter (fun x -> function V _ -> () | R (_, v) -> f x v) t

(** Other operations : convert the table into a list *)
let to_list t =
  Hashtbl.fold
    (fun x _ acc -> 
      let u = match find t x with
        | Some u -> u
        | None -> failwith "to_list"
      in
      (x, u) :: acc) t []

