(* A terrible hack to unbox small strings *)

module OptString
(*! : module type of Stdlib.String !*)
: sig
  include module type of Stdlib.String
  (* functions which are externals in Stdlib.String must be redeclared as
   * regular values here *)
  val length : string -> int
  val create : int -> bytes
  val get : string -> int -> char
  val set : bytes  -> int -> char -> unit
  val unsafe_get : string -> int -> char
  val unsafe_set : bytes  -> int -> char -> unit
  val unsafe_blit : bytes -> int -> bytes -> int -> int -> unit
  val unsafe_fill : bytes -> int -> int -> char -> unit
  (* additional functions *)
  val try_unbox : string -> string
end
= struct
  type t = string
  let _is_unboxed : string -> bool = fun s -> Obj.is_int @@ Obj.repr s
  let _get_unboxed : string -> int = Obj.magic
  let _make_unboxed : int -> string = Obj.magic
  let _unboxed_max_len = Sys.int_size lsr 3
  let _pad_len n = Sys.int_size - (n lsl 3)
  let length s =
    if _is_unboxed s then _get_unboxed s land 127 else Stdlib.String.length s
  let create = Stdlib.Bytes.create
  let make n c =
    if n < 0 then
      raise @@ Invalid_argument "OptString.make" ;
    if n <= _unboxed_max_len then
      let s = Char.code c in
      let s = s lor (s lsl  8) in
      let s = s lor (s lsl 16) in
      let s = s lor (s lsl 32) in
      _make_unboxed ((s lsl _pad_len n) lor n)
    else
      Stdlib.String.make n c
  let unsafe_get s i =
    if _is_unboxed s then
      let s = _get_unboxed s in
      Char.chr ((s lsr _pad_len (i+1)) land 255)
    else
      Stdlib.String.unsafe_get s i
  let get s i =
    let n = length s in
    if i < 0 || n <= i then
      raise @@ Invalid_argument "OptString.get: index out of bonds" ;
    unsafe_get s i
  let unsafe_set  = Stdlib.Bytes.unsafe_set
  let set         = Stdlib.Bytes.set
  let unsafe_fill = Stdlib.Bytes.unsafe_fill
  let fill        = Stdlib.Bytes.fill
  let unsafe_blit = Stdlib.String.unsafe_blit
  let blit        = Stdlib.String.blit
  let init n f =
    if n > _unboxed_max_len then
      Stdlib.String.init n f
    else begin
      let s = ref 0 in
      for i = 0 to n-1 do
        s := (!s lsl 8) lor Char.code (f i)
      done ;
      _make_unboxed ((!s lsl _pad_len n) lor n)
    end
  let copy s = s
  let sub s from len =
    if len < 0 || from < 0 || length s < from + len then
      raise @@ Invalid_argument "OptString.sub" ;
    init len (fun i -> unsafe_get s (from + i))
  let concat _ = assert false
  let iter _ = assert false
  let iteri _ = assert false
  let map _ = assert false
  let mapi _ = assert false
  let trim _ = assert false
  let escaped _ = assert false
  let index _ = assert false
  let index_opt _ = assert false
  let rindex _ = assert false
  let rindex_opt _ = assert false
  let index_from _ = assert false
  let index_from_opt _ = assert false
  let rindex_from _ = assert false
  let rindex_from_opt _ = assert false
  let contains _ = assert false
  let contains_from _ = assert false
  let rcontains_from _ = assert false
  let uppercase _ = assert false
  let lowercase _ = assert false
  let uppercase_ascii _ = assert false
  let lowercase_ascii _ = assert false
  let capitalize _ = assert false
  let uncapitalize _ = assert false
  let capitalize_ascii _ = assert false
  let uncapitalize_ascii _ = assert false
  let compare _ = assert false
  let equal _ = assert false
  let split_on_char _ = assert false
  let to_seq _ = assert false
  let to_seqi _ = assert false
  let of_seq _ = assert false
  let try_unbox s =
    if _is_unboxed s then
      s
    else begin
      let n = String.length s in
      if n > _unboxed_max_len then
        s
      else begin
        let a = ref 0 in
        for i = 0 to n-1 do
          a := (!a lsl 8) lor Char.code s.[i]
        done ;
        _make_unboxed ((!a lsl _pad_len n) lor n)
      end
    end
end

let () =
  let s = OptString.try_unbox "abcdefg" in
  Printf.printf "%B\n" (Obj.is_int @@ Obj.repr s) ;
  Printf.printf "len = %i\n" (OptString.length s) ;
  for i = 0 to OptString.length s - 1 do
    Printf.printf "  s.[%i] = %C\n" i (OptString.get s i)
  done
