#require "ctypes.foreign" ;;
#require "ctypes.top" ;;

module C = Ctypes ;;

(**
 * OCaml type of a comparison function:
 **)

type cmp = unit C.ptr -> unit C.ptr -> int ;;
  (* type cmp = unit ptr -> unit ptr -> int *)

(**
 * OCaml values representing (views of) C types
 * **)

(** C function type of a comparison function: **)
let fn_cmp : cmp C.fn = C.(ptr void @-> ptr void @-> returning int) ;;
  (* val fn_cmp              : cmp fn               = int(void*, void* )  *)

(** C type of a function pointer: **)
let staticfunptr_cmp = C.static_funptr fn_cmp ;;
  (* val staticfunptr_cmp  : cmp static_funptr typ = int(*)(void*, void*) *)

(** C type of a function pointer, seen from OCaml as first‐class function: **)
let foreignfunptr_cmp : cmp C.typ = Foreign.funptr fn_cmp ;;
  (* val foreignfunptr_cmp : cmp typ               = int(*)(void*, void*) *)

(**
 * bindings of C’s ‘qsort‘ using each of these views
 **)

let qsort_static  = Foreign.foreign "qsort" C.(ptr void @-> size_t @-> size_t @-> staticfunptr_cmp  @-> returning void) ;;
  (* val qsort_static  : unit ptr -> size_t -> size_t -> cmp static_funptr -> unit = <fun> *)

let qsort_foreign = Foreign.foreign "qsort" C.(ptr void @-> size_t @-> size_t @-> foreignfunptr_cmp @-> returning void) ;;
  (* val qsort_foreign : unit ptr -> size_t -> size_t -> cmp               -> unit = <fun> *)

(**
 * higher‐level wrappers
 **)

let qsort_static' cmp a =
  let module A  = C.CArray in
  let module Sz = Unsigned.Size_t in
  let open C in
  let t = A.element_type a in
  qsort_static
    (C.to_voidp @@ A.start a)
    (Sz.of_int @@ A.length a)
    (Sz.of_int @@ sizeof t)
    (C.coerce foreignfunptr_cmp staticfunptr_cmp
     @@ fun p q -> cmp !@(from_voidp t p) !@(from_voidp t q))
;;
  (* val qsort_static' : ('a -> 'a -> int) -> 'a carray -> unit = <fun> *)

let qsort_foreign' cmp a =
  let module A  = C.CArray in
  let module Sz = Unsigned.Size_t in
  let open C in
  let t = A.element_type a in
  qsort_foreign
    (C.to_voidp @@ A.start a)
    (Sz.of_int @@ A.length a)
    (Sz.of_int @@ sizeof t)
    (fun p q -> cmp !@(from_voidp t p) !@(from_voidp t q))
;;
  (* val qsort_foreign' : ('a -> 'a -> int) -> 'a carray -> unit = <fun> *)

(**
 * testing
 **)

let a = C.CArray.make Ctypes.int 10 ;;
  (* val a : int carray = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } *)
for i = 0 to 9 do C.CArray.set a i i done ; a ;;
  (* - : int carray = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } *)
qsort_static'  (fun x y -> y-x) a ; a ;;
  (* - : int carray = { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 } *)
qsort_foreign' (fun x y -> x-y) a ; a ;;
  (* - : int carray = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } *)
