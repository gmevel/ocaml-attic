let ( % ) g f x = g (f x)

(* generator: control is to the consumer;
 * this is the most expressive stream structure, and has good performance. *)
type +'a gen = unit -> 'a option
(* cursor: the purely fonctional equivalent to generator, with an explicit state
 * passing; comparable to a delayed list. *)
type +'a cur = End | Cur of 'a * (unit -> 'a cur)

(* iterator (‘sequence’) and folder: control is to the producer;
 * equivalent in performance (the best of all stream structures) and end‐user
 * expressivity (less expressive than consumer‐controled streams); some stuff
 * such as ‘map2’ or ‘zip’ can not be expressed, but inversion of control
 * (conversion to a consumer‐controled stream) can be done with effects
 * (upcoming with Multicore OCaml). *)
type +'a seq = ('a -> unit) -> unit
type +'a fld = { fold : 'b. 'b -> ('b -> 'a -> 'b) -> 'b }

module type ITERATOR = sig
  type +'a t
  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
  val iter : 'a t -> f:('a -> unit) -> unit
  val fold : 'a t -> y0:'b -> f:('b -> 'a -> 'b) -> 'b
  val map :  'a t -> f:('a -> 'b) -> 'b t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val mapfilter : 'a t -> f:('a -> 'b option) -> 'b t
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val mapconcat : 'a t -> f:('a -> 'b t) -> 'b t
  val range : ?by:int -> int -> int -> int t
  (*val init : int -> (int -> 'a) -> 'a t*)
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
end

module Gen : ITERATOR with type 'a t = 'a gen = struct
  type 'a t = 'a gen
  let empty () = None
  let cons x g =
    let r = ref true in
    fun () ->
      if !r then begin
        r := false ;
        Some x
      end
      else
        g ()
  let rec iter g ~f =
    begin match g () with
    | None   -> ()
    | Some x -> f x ; iter g ~f
    end
  let rec fold g ~y0 ~f =
    begin match g () with
    | None   -> y0
    | Some x -> fold g ~y0:(f y0 x) ~f
    end
  let map g ~f () =
    begin match g () with
    | None   -> None
    | Some x -> Some (f x)
    end
  let rec filter g ~f () =
    begin match g () with
    | None                 -> None
    | Some x as o when f x -> o
    | _                    -> filter g ~f ()
    end
  let rec mapfilter g ~f () =
    begin match g () with
    | None   -> None
    | Some x ->
        begin match f x with
        | Some y as o -> o
        | _           -> mapfilter g ~f ()
        end
    end
  let append g1 g2 () =
    begin match g1 () with
    | None        -> g2 ()
    | Some x as o -> o
    end
  let concat gg =
    let r = ref empty in
    let rec g' () =
      begin match !r () with
      | Some x as o -> o
      | None        ->
          begin match gg () with
          | Some g -> r := g ; g' ()
          | None   -> None
          end
      end
    in g'
  let mapconcat g ~f =
    map g ~f |> concat (* ok because lazy *)
  let range ?(by=1) from til = 
    let ( >= ) = if by > 0 then ( >= ) else ( <= ) in
    let r = ref from in
    fun () ->
      if !r >= til then
        None
      else begin
        r := !r + by ;
        Some (!r - by)
      end
  let init n ~f =
    range 0 n |> map ~f
  let of_list li =
    let r = ref li in
    fun () ->
      begin match !r with
      | []      -> None
      | x :: tl -> r := tl ; Some x
      end
  let to_list g =
    List.rev (fold g ~y0:[] ~f:(fun acc x -> x :: acc))
end

module Cur : ITERATOR with type 'a t = 'a cur = struct
  type 'a t = 'a cur
  let empty = End
  let cons x c = Cur (x, fun () -> c)
  let rec iter c ~f =
    begin match c with
    | End        -> ()
    | Cur (x, d) -> f x ; iter (d ()) ~f
    end
  let rec fold c ~y0 ~f =
    begin match c with
    | End        -> y0
    | Cur (x, d) -> fold (d ()) ~y0:(f y0 x) ~f
    end
  let rec map c ~f =
    begin match c with
    | End        -> End
    | Cur (x, d) -> Cur (f x, fun () -> map (d ()) ~f)
    end
  let rec filter c ~f =
    begin match c with
    | End                 -> End
    | Cur (x, d) when f x -> Cur (x, fun () -> filter (d ()) ~f)
    | Cur (_, d)          -> filter (d ()) ~f
    end
  let rec mapfilter c ~f =
    begin match c with
    | End        -> End
    | Cur (x, d) ->
        begin match f x with
        | None   -> mapfilter (d ()) ~f
        | Some y -> Cur (y, fun () -> mapfilter (d ()) ~f)
        end
    end
  let rec append c1 c2 =
    begin match c1 with
    | End        -> c2
    | Cur (x, d) -> Cur (x, fun () -> append (d ()) c2)
    end
  let rec concat cc =
    fold cc ~y0:empty ~f:append (* FIXME: inefficient O(n²) *)
  let rec mapconcat c ~f =
    map c ~f |> concat (* ok because lazy *)
  let range ?(by=1) from til =
    let ( >= ) = if by > 0 then ( >= ) else ( <= ) in
    let rec loop k =
      if k >= til then
        End
      else
        Cur (k, fun () -> loop (k + by))
    in loop from
  let rec of_list li =
    begin match li with
    | []      -> End
    | x :: tl -> Cur (x, fun () -> of_list tl)
    end
  let rec to_list c = (* not tail‐rec (but tail‐rec modulo constructors) *)
    begin match c with
    | End        -> []
    | Cur (x, d) -> x :: to_list (d ())
    end
end

module Seq : ITERATOR with type 'a t = 'a seq = struct
  type 'a t = 'a seq
  let empty g = ()
  let cons x s g =
    g x ; s g
  let iter s ~f =
    s f
  let fold s ~y0 ~f =
    let r = ref y0 in
    s (fun x -> r := f !r x) ;
    !r
  let map s ~f g =
    s (g % f)
  let filter s ~f g =
    s (fun x -> if f x then g x)
  let mapfilter s ~f g =
    s (fun x -> match f x with Some y -> g y | _ -> ())
  let append s1 s2 g =
    s1 g ; s2 g
  let concat ss g =
    ss (fun s -> s g)
  (*let mapconcat s ~f g =
    s (fun x -> f x g)*)
  let mapconcat s ~f =
    map s ~f |> concat (* ok because lazy *)
  let range ?(by=1) from til g =
    let a = (til - from) / by in
    for k = 0 to a-1 do
      g (from + by * k)
    done
  let init n ~f =
    range 0 n |> map ~f
  let of_list li g =
    List.iter g li
  let to_list s =
    List.rev (fold s ~y0:[] ~f:(fun acc x -> x :: acc))
end

module Fld : ITERATOR with type 'a t = 'a fld = struct
  type 'a t = 'a fld
  let empty = { fold = fun y0 _ -> y0 }
  let cons x {fold} =
    { fold = fun y0 f -> fold (f y0 x) f }
  let iter {fold} ~f =
    fold () (fun () -> f)
  let fold {fold} ~y0 ~f =
    fold y0 f
  let map {fold} ~f =
    { fold = fun y0 g -> fold y0 (fun acc x -> g acc (f x)) }
  let filter {fold} ~f =
    { fold = fun y0 g -> fold y0 (fun acc x -> if f x then g acc x else acc) }
  let mapfilter {fold} ~f =
    { fold = fun y0 g -> fold y0 (fun acc x -> match f x with Some y -> g acc y | _ -> acc) }
  let append f1 f2 =
    { fold = fun y0 g -> f2.fold (f1.fold y0 g) g }
  let concat {fold} =
    fold empty append
  let mapconcat folder ~f =
    map folder ~f |> concat (* ok because lazy *)
  let range ?(by=1) from til =
    let ( >= ) = if by > 0 then ( >= ) else ( <= ) in
    let rec loop k acc f =
      if k >= til then
        acc
      else
        loop (k + by) (f acc k) f
    in
    { fold = fun y0 f -> loop from y0 f }
  let init n ~f =
    range 0 n |> map ~f
  let of_list li =
    { fold = fun y0 f -> List.fold_left f y0 li }
  let to_list {fold} =
    List.rev (fold [] (fun acc x -> x :: acc))
end

let seq_of_fld : 'a fld -> 'a seq =
  fun fld f ->
    Fld.iter fld ~f

let seq_of_gen : 'a gen -> 'a seq =
  fun gen f ->
    Gen.iter gen ~f

let seq_of_cur : 'a cur -> 'a seq =
  fun cur f ->
    Cur.iter cur ~f

let fld_of_seq : 'a seq -> 'a fld =
  fun seq ->
    { fold = fun y0 f -> Seq.fold seq ~y0 ~f }

let fld_of_gen : 'a gen -> 'a fld =
  fun gen ->
    { fold = fun y0 f -> Gen.fold gen ~y0 ~f }

let fld_of_cur : 'a cur -> 'a fld =
  fun cur ->
    { fold = fun y0 f -> Cur.fold cur ~y0 ~f }

let gen_of_seq (type a) : a seq -> a gen =
  fun seq ->
    let module L = struct
        effect Yield : a -> unit
        (*exception Yield : a * (unit -> a option) -> exn*)
      end in
    let rec next = ref start
    and start () =
      begin match
        seq (fun x -> perform (L.Yield x))
        (*seq (fun x -> raise (L.Yield (x, Obj.magic())))*)
      with
      | () ->
          None
      | effect (L.Yield x) k ->
      (*| exception L.Yield (x, k) ->*)
          next := continue k ; Some x
      end
    in
    fun () -> !next ()
    (* note: there is no need to name ‘start’ here and we can inline it inside
     * the definition of ‘next’; but it fails with an obsure type error:
     *   next := continue k
     *           ^^^^^^^^^^
     *     Error: This expression has type unit -> 'a option
     *            but an expression was expected of type unit -> 'a option
     *            This instance of unit is ambiguous:
     *            it would escape the scope of its equation
     *)

(* version without recursion (but with an ugly placeholder);
   not exactly equivalent because it computes the next element one step
   in advance. *)
(*
let gen_of_seq (type a) : a seq -> a gen =
  fun seq ->
    let module L = struct
        effect Yield : a -> unit
      end in
    let next = ref (fun () -> assert false) in
    begin match
      seq (fun x -> perform (L.Yield x))
    with
    | () ->
        next := (fun () -> None)
    | effect (L.Yield x) k ->
        next := (fun () -> continue k () ; Some x)
    end ;
    fun () -> !next ()
*)

let gen_of_fld : 'a fld -> 'a gen =
  fun fld -> gen_of_seq (seq_of_fld fld)

let gen_of_cur : 'a cur -> 'a gen =
  fun cur ->
    let r = ref (fun () -> cur) in
    fun () ->
      begin match !r () with
      | End        -> r := (fun () -> raise End_of_file) ; None
      | Cur (x, d) -> r := d                             ; Some x
      end

let cur_of_seq (type a) : a seq -> a cur =
  fun seq ->
    let module L = struct
        effect Yield : a -> unit
        (*exception Yield : a * (unit -> a cur) -> exn*)
      end in
    begin match
      seq (fun x -> perform (L.Yield x))
      (*seq (fun x -> raise (L.Yield (x, Obj.magic())))*)
    with
    | () ->
        End
    | effect (L.Yield x) k ->
    (*| exception L.Yield (x, k) ->*)
        Cur (x, continue k)
    end

let cur_of_fld : 'a fld -> 'a cur =
  fun fld -> cur_of_seq (seq_of_fld fld)

let cur_of_gen : 'a gen -> 'a cur =
  fun gen ->
    let rec d () =
      begin match gen () with
      | None   -> End
      | Some x -> Cur (x, d)
      end
    in d ()


type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec make_tree ?(id=0) depth =
  if depth < 0 then
    Leaf
  else
    Node (make_tree ~id:(2*id+1) (pred depth), id, make_tree ~id:(2*id+2) (pred depth))

let rec seq_of_tree : 'a tree -> 'a seq =
  fun tree f ->
    begin match tree with
    | Leaf             -> ()
    | Node (tl, x, tr) -> seq_of_tree tl f ; f x ; seq_of_tree tr f
    end

let rec seq_of_tree_cps : 'a tree -> ('a seq -> 'b) -> 'b =
  fun tree k ->
    begin match tree with
    | Leaf             -> k Seq.empty
    | Node (tl, x, tr) -> seq_of_tree_cps tl (fun seql -> seq_of_tree_cps tr (fun seqr -> k Seq.(append seql (cons x seqr))))
    end

let seq_of_tree' tree = seq_of_tree_cps tree (fun seq -> seq)

let rec cur_of_tree : 'a tree -> 'a cur =
  fun tree ->
    begin match tree with
    | Leaf             -> End
    | Node (tl, x, tr) -> Cur.(append (cur_of_tree tl) (cons x (cur_of_tree tr)))
    end

let rec cur_of_tree_cps : 'a tree -> ('a cur -> 'b) -> 'b =
  fun tree k ->
    begin match tree with
    | Leaf             -> k End
    | Node (tl, x, tr) -> cur_of_tree_cps tl (fun curl -> cur_of_tree_cps tr (fun curr -> k Cur.(append curl (cons x curr))))
    end

let cur_of_tree' tree = cur_of_tree_cps tree (fun cur -> cur)

let rec gen_of_tree : 'a tree -> 'a gen =
  fun tree ->
    let s = Stack.create () in
    Stack.push Leaf s ; (* end marker *)
    Stack.push tree s ; (* node to start from *)
    let rec explore = function
      | Node (tl, _, _) -> Stack.push tl s ; explore tl
      | Leaf            ->
          Stack.pop s |> ignore ; (* pops the Leaf *)
          begin match Stack.pop s with
          | Node (_, x, tr)       -> Stack.push tr s ; Some x
          | Leaf                  -> None
          end
    in
    fun () ->
      explore (Stack.top s)
