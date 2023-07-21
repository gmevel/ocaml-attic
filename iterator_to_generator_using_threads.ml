(*  ocamlfind ocamlopt -thread -package threads -package containers.thread -linkpkg test.ml -o test.exe && ./test.exe *)

(***
 ***  Turning an iterator into a generator
 ***)

(* Turning an iteror into a generator without algebraic effects, using the
 * thread scheduler. *)



(******************************************************************************)

(*
 *  Using module Event
 *)

(*
let channel = Event.new_channel ()

let t =
  () |> Thread.create begin fun () ->
    for i = 0 to 100 do
      Printf.printf "sending %u...\n%!" i ;
      let e = Event.send channel i in
      Event.sync e ;
      Printf.printf "sent\n%!"
    done
  end

let () =
  let again = ref true in
  while !again do
    Printf.printf "\twaiting...\n%!" ;
    let e = Event.receive channel in
    let i = Event.sync e in
    Printf.printf "\treceived %u\n%!" i ;
    if i = 100 then
      again := false
  done
*)

(******************************************************************************)

(*
 *  Using Mutex and Condition
 *)

type +'a iter = ('a -> unit) -> unit
type +'a gen = 'a Seq.t

(* This returns a pair (gen, drop) where ‘drop ()’ destructs the generator
 * before it has completed. This is required because otherwise, if the user
 * chooses to drop the generator prematurately, there will remain a pending
 * (waiting) thread even after garbage collection.
 *
 * The result is not thread‐safe. *)
let gen_of_iter_unsafe : 'a. 'a iter -> 'a gen * (unit -> unit) =
fun it ->
  let value = ref (Obj.magic 0)
  and ended = ref false
  and c = Condition.create () (* used for thread synchronization *)
  and mp = Mutex.create ()    (* held by the producer, except when waiting *)
  and mc = Mutex.create () in (* held by the consumer, except when waiting *)
  Mutex.lock mp ;
  Mutex.lock mc ;
  (* Producer thread. *)
  let _t =
    () |> Thread.create begin fun () ->
      (* Wait that the consumer requests a first value: *)
      Condition.wait c mp ;
      (* Check whether the consumer dropped us: *)
      if !ended then
        Thread.exit () ;
      it begin fun x ->
        (* Produce a new value: *)
        value := x ;
        (* When the consumer is ready (waiting), signal the new value: *)
        Mutex.lock mc ;
        Mutex.unlock mc ;
        Condition.signal c ;
        (* Wait that the consumer requests a new value: *)
        Condition.wait c mp ;
        (* Check whether the consumer dropped us: *)
        if !ended then
          Thread.exit () ;
      end ;
      (* Produce the end: *)
      ended := true ;
      (* When the consumer is ready (waiting), signal the end: *)
      Mutex.lock mc ;
      Mutex.unlock mc ;
      Condition.signal c ;
    end
  in
  (* Consumer thread. *)
  let rec gen () =
    if !ended then
      (* The end has already been reached (either by using ‘drop’ or by a
       * previous call to ‘gen’). *)
      Seq.Nil
    else begin
      (* Request a value from the producer: *)
      (* NOTE: We can make sure that the producer is ready (waiting), but this
       * is guaranteed (as long as there is only one consumer thread) because
       * the producer thread runs only when asked to by a consumer. *)
      (*! Mutex.lock mp ; !*)
      (*! Mutex.unlock mp ; !*)
      Condition.signal c ;
      (* Wait for the answer: *)
      Condition.wait c mc ;
      (* Read the answer: *)
      if !ended then
        Seq.Nil
      else
        Seq.Cons (!value, gen)
    end
  in
  let drop () =
    if not !ended then begin
      ended := true ;
      (* Signal the producer that we dropped the generator: *)
      (* NOTE: See note above. *)
      (*! Mutex.lock mp ; !*)
      (*! Mutex.unlock mp ; !*)
      Condition.signal c ;
    end
  in
  (* Make sure that the producer is waiting: *)
  Mutex.lock mp ;
  Mutex.unlock mp ;
  (gen, drop)

(* The same, but the result is thread‐safe. *)
let gen_of_iter : 'a. 'a iter -> 'a gen * (unit -> unit) =
fun it ->
  let (gen, drop) = gen_of_iter_unsafe it in
  let m = Mutex.create () in
  let rec gen' gen () =
    Mutex.lock m ;
    let g = gen () in
    Mutex.unlock m ;
    begin match g with
    | Seq.Nil           -> Seq.Nil
    | Seq.Cons (x, gen) -> Seq.Cons (x, gen' gen)
    end
  in
  let drop' () =
    Mutex.lock m ;
    drop () ;
    Mutex.unlock m ;
  in
  (gen' gen, drop')

let iter_range n f =
  for i = 0 to n-1 do
    f i
  done

let gen_range n =
  gen_of_iter @@ iter_range n

let () =
  let (gen, drop) = gen_range 100 in
  gen |> Seq.iter begin fun x ->
    Printf.printf "%i\n%!" x ;
    if x = 10 then drop () ;
  end ;
  drop ()

(******************************************************************************)

(*
 *  Using CCBlockingQueue (blocking queue, from package “containers.thread”)
 *)


