(*
 * A simple library for manipulating UTF-8 and CP-1252.
 *)

(* Related:
 *   - module Uchar in standard library
 *   - library camomile (modules CamomileLibrary.{UChar,UTF8})
 *   - library uutf
 *)

(* Unicode character *)
type ucode = int (* NOTE: since OCaml 4.03 there is [Uchar.t] *)
(* UTF-8 character / string *)
type utf8 = string
(* CP-1252 character *)
type cp1252 = char

let substitute : utf8 = "�" (* U+FFFD; NOTE: since OCaml 4.06 there is [Uchar.rep] *)

(**
 **  Outputting UTF-8
 **)

let output_utf8 : out_channel -> utf8 -> unit =
  output_string

(**
 **  Encoding UTF-8
 **)

(* NOTE: We support the full 32-bit range for code points, even though Unicode
 * has been restricted to 21-bit code points.
 * FIXME: This breaks on 32-bit OCaml. *)
let byte_length_as_utf8 (uc : ucode) : int =
  assert (0 <= uc && uc < (1 lsl 32)) ;
       if uc < (1 lsl  7) then 1
  else if uc < (1 lsl 11) then 2
  else if uc < (1 lsl 16) then 3
  else if uc < (1 lsl 21) then 4
  else if uc < (1 lsl 26) then 5
  else (* uc < (1 lsl 32) *)   6

let ( .![]<- ) (bytes : bytes) (i : int) (cc : int) =
  Bytes.set bytes i (Char.chr cc)

let utf8_of_unicode (uc : ucode) : utf8 =
  assert (0 <= uc && uc < (1 lsl 32)) ;
  let bytes = Bytes.create (byte_length_as_utf8 uc) in
  if uc < (1 lsl 7) then
    bytes.![0] <- uc
  else if uc < (1 lsl 11) then begin
    bytes.![0] <- 0b110_00000 lor (uc lsr 6) ;
    bytes.![1] <- 0b10_000000 lor (uc land 0b111111) ;
  end
  else if uc < (1 lsl 16) then begin
    bytes.![0] <- 0b1110_0000 lor ((uc lsr 12)              ) ;
    bytes.![1] <- 0b10_000000 lor ((uc lsr  6) land 0b111111) ;
    bytes.![2] <- 0b10_000000 lor ( uc         land 0b111111) ;
  end
  else if uc < (1 lsl 21) then begin
    bytes.![0] <- 0b11110_000 lor ((uc lsr 18)              ) ;
    bytes.![1] <- 0b10_000000 lor ((uc lsr 12) land 0b111111) ;
    bytes.![2] <- 0b10_000000 lor ((uc lsr  6) land 0b111111) ;
    bytes.![3] <- 0b10_000000 lor ( uc         land 0b111111) ;
  end
  else if uc < (1 lsl 26) then begin
    bytes.![0] <- 0b111110_00 lor ((uc lsr 24)              ) ;
    bytes.![1] <- 0b10_000000 lor ((uc lsr 18) land 0b111111) ;
    bytes.![2] <- 0b10_000000 lor ((uc lsr 12) land 0b111111) ;
    bytes.![3] <- 0b10_000000 lor ((uc lsr  6) land 0b111111) ;
    bytes.![4] <- 0b10_000000 lor ( uc         land 0b111111) ;
  end
  else (* uc < (1 lsl 32) *) begin
    bytes.![0] <- 0b111111_00 lor ((uc lsr 30)              ) ;
    bytes.![1] <- 0b10_000000 lor ((uc lsr 24) land 0b111111) ;
    bytes.![2] <- 0b10_000000 lor ((uc lsr 18) land 0b111111) ;
    bytes.![3] <- 0b10_000000 lor ((uc lsr 12) land 0b111111) ;
    bytes.![4] <- 0b10_000000 lor ((uc lsr  6) land 0b111111) ;
    bytes.![5] <- 0b10_000000 lor ( uc         land 0b111111) ;
  end ;
  Bytes.unsafe_to_string bytes

(**
 **  Decoding UTF-8
 **)

module Reader = struct

  (* Unfortunately, OCaml has no simple way of building an [in_channel] from
   * a [string].
   * https://discuss.ocaml.org/t/how-can-i-create-an-in-channel-from-a-string/8761 *)
  type input =
    | File of in_channel
    | String of { contents : string ; mutable pos : int }

  let _get_char (in_ : input) : char =
    begin match in_ with
    | File file ->
        input_char file
    | String view  ->
        assert (0 <= view.pos && view.pos <= String.length view.contents) ;
        if view.pos >= String.length view.contents then
          raise End_of_file
        else
          view.pos <- view.pos + 1 ;
          String.get view.contents (view.pos - 1)
    end

  (* TODO: This is wasteful, the internal C implementation of [in_channel] could
   * provide [peek_char] for free. *)
  type t = {
    mutable fst_char : char option ; (* TODO: unbox this *)
    input : input ;
  }

  let of_file (file : in_channel) : t =
    { fst_char = None ; input = File file }

  let of_string (s : string) : t =
    { fst_char = None ; input = String { contents = s ; pos = 0 } }

  let peek_first_char (in_ : t) =
    begin match in_.fst_char with
    | Some c -> c
    | None   -> let c = _get_char in_.input in in_.fst_char <- Some c ; c
    end

  let get_first_char (in_ : t) =
    begin match in_.fst_char with
    | Some c -> in_.fst_char <- None ; c
    | None   -> _get_char in_.input
    end

  let unget_first_char (in_ : t) (c : char) =
    assert (in_.fst_char = None) ;
    in_.fst_char <- Some c

end

type decoded_utf8 =
  | Ucode of ucode      (* valid UTF-8 sequence for one code point; TODO: avoid boxing *)
  | Nonnormal_utf8 of ucode (* valid but overly long UTF-8 sequence, should be rejected *)
  | Malformed_utf8      (* invalid UTF-8 sequence *)
  | Nonterminated_utf8  (* partial UTF-8 sequence just before [EOF] *)
  | EOF                 (* end of input *)

let significant_bits_for_utf8_byte_length : int array =
  [| 0 ; 7 ; 11 ; 16 ; 21 ; 26 ; 32 |]

let check_utf8_is_normal (uc : ucode) (len : int) : bool =
  assert (0 < len) ;
  assert (0 <= uc && uc < (1 lsl 32)) ;
  len = 1 || (uc lsr significant_bits_for_utf8_byte_length.(len-1)) <> 0

(* After the function is read, [buf] will contain the consumed bytes (and
 * nothing else). This is useful for debugging, normality checking and UTF-8
 * error recovery. The buffer is empty only when the result is [EOF]. *)
let decode_utf8_char (in_ : Reader.t) (buf : Buffer.t) : decoded_utf8 =
  (* [decode_cont ~ucode ~pending] decodes a given number of continuation bytes,
   * i.e. bytes of the form 0b10xxxxxx.
   * [~ucode] accumulates the code point being decoded.
   * [~pending] is the expected number of continuation bytes. *)
  let rec decode_cont ~ucode ~pending =
    (*! let c = Reader.get_first_char in_ in !*)
    assert (in_.fst_char = None) ;
    let c = Reader._get_char in_.input in
    let cc = Char.code c in
    if cc lsr 6 = 0b10 then begin
      Buffer.add_char buf c ;
      let ucode = (ucode lsl 6) lor (cc land 0b00111111) in
      if pending > 1 then
        decode_cont ~ucode ~pending:(pending - 1)
      else if check_utf8_is_normal ucode (Buffer.length buf) then
        Ucode ucode
      else
        Nonnormal_utf8 ucode
    end
    else begin
      Reader.unget_first_char in_ c ;
      Malformed_utf8
    end
  in
  begin try
    Buffer.clear buf ;
    (* decode first byte: *)
    let c = Reader.get_first_char in_ in
    let cc = Char.code c in
    Buffer.add_char buf c ;
    (* decode continuation bytes if needed: *)
    if cc < 0x80 then Ucode cc
    else if cc lsr 5 = 0b110 then decode_cont ~ucode:(cc land 0b11111) ~pending:1
    else if cc lsr 4 = 0b1110 then decode_cont ~ucode:(cc land 0b1111) ~pending:2
    else if cc lsr 3 = 0b11110 then decode_cont ~ucode:(cc land 0b111) ~pending:3
    else if cc lsr 2 = 0b111110 then decode_cont ~ucode:(cc land 0b11) ~pending:4
    else if cc lsr 2 = 0b111111 then decode_cont ~ucode:(cc land 0b11) ~pending:5
    else Malformed_utf8
  with End_of_file ->
    if Buffer.length buf = 0 then
      EOF
    else
      Nonterminated_utf8
  end

let decode_utf8_seq (in_ : Reader.t) (buf : Buffer.t) : decoded_utf8 Seq.t =
  let rec decode () =
    let d = decode_utf8_char in_ buf in
    Seq.Cons (d, if d = EOF then (fun () -> Seq.Nil) else decode)
  in
  decode

let rec decode_utf8 (in_ : Reader.t) (process : Buffer.t -> decoded_utf8 -> unit) : unit =
  let buf = Buffer.create 6 in
  Seq.iter (process buf) (decode_utf8_seq in_ buf)

(**
 **  Decoding CP-1252
 **)

exception Invalid_CP1252_code of int

let unicode_of_cp1252 : cp1252 -> ucode =
  let mapping =
    [|
      (* codes from 0x80 to 0x8F: *)
      0x20AC ;      0 ; 0x201A ; 0x0192 ; 0x201E ; 0x2026 ; 0x2020 ; 0x2021 ;
      0x02C6 ; 0x2030 ; 0x0160 ; 0x2039 ; 0x0152 ;      0 ; 0x017D ;      0 ;
      (* codes from 0x90 to 0x9F: *)
           0 ; 0x2018 ; 0x2019 ; 0x201C ; 0x201D ; 0x2022 ; 0x2013 ; 0x2014 ;
      0x02DC ; 0x2122 ; 0x0161 ; 0x203A ; 0x0153 ;      0 ; 0x017E ; 0x0178 ;
    |]
  in fun c ->
    let cc = Char.code c in
    if 0x80 <= cc && cc < 0xA0 then
      let uc = mapping.(cc land 0x7F) in
      if uc = 0 then raise (Invalid_CP1252_code cc) else uc
    else
      cc

let utf8_of_cp1252_subst ?(subst : utf8 = substitute) (c : cp1252) : utf8 =
  begin try
    utf8_of_unicode (unicode_of_cp1252 c)
  with Invalid_CP1252_code cc ->
    Printf.eprintf "unknown CP-1252 code %02X; inserting “%s” instead\n" cc subst ;
    subst
  end

(* Reads input with a mixed UTF-8 / CP-1252 encoding and converts it to UTF-8.
 * Byte sequences which are valid in UTF-8 are interpreted as such, remaining
 * bytes are interpreted as CP-1252 characters. The UTF-8 output is normalized.
 *
 * Note: CP-1252 subsumes Latin-1. *)

let recode_mixed_utf8_cp1252 (in_ : Reader.t) (out : out_channel) =
  decode_utf8 in_ begin fun (buf : Buffer.t) (d : decoded_utf8) : unit ->
    begin match d with
    | Ucode uc | Nonnormal_utf8 uc ->
        output_utf8 out (utf8_of_unicode uc)
    | Malformed_utf8 | Nonterminated_utf8 ->
        String.iter
          begin fun c ->
            output_utf8 out (utf8_of_cp1252_subst c)
          end
          (Buffer.contents buf)
    | EOF -> ()
    end
  end
