let[@inline] rev_1bit (bits : int) : int =
  bits land 0b1
let[@inline] rev_2bits (bits : int) : int =
  rev_1bit (bits lsr 1) lor (rev_1bit bits lsl 1)
let[@inline] rev_4bits (bits : int) : int =
  rev_2bits (bits lsr 2) lor (rev_2bits bits lsl 2)
let[@inline] rev_8bits (bits : int) : int =
  rev_4bits (bits lsr 4) lor (rev_4bits bits lsl 4)
let[@inline] rev_16bits (bits : int) : int =
  rev_8bits (bits lsr 8) lor (rev_8bits bits lsl 8)
let[@inline] rev_32bits (bits : int) : int =
  rev_16bits (bits lsr 16) lor (rev_16bits bits lsl 16)
let[@inline] rev_64bits (bits : int) : int =
  rev_32bits (bits lsr 32) lor (rev_32bits bits lsl 32)

let rev_31bits (bits : int) : int =
  (rev_32bits bits lsr 1) lor ((bits land 1) lsl 30)
let rev_63bits (bits : int) : int =
  (rev_64bits bits lsr 1) lor ((bits land 1) lsl 62)
let rev_bits : int -> int =
  match Sys.int_size with
  | 31 -> rev_31bits
  | 63 -> rev_63bits
  | _  -> assert false

let pp_bin (n : int) : string =
  let bits = Bytes.create 63 in
  for i = 0 to 62 do
    Bytes.set bits (62-i) (if (n lsr i) land 1 = 0 then '0' else '1')
  done ;
  let bits = Bytes.unsafe_to_string bits in
  "0b_" ^ String.sub bits  0 7
  ^ "_" ^ String.sub bits  7 8
  ^ "_" ^ String.sub bits 15 8
  ^ "_" ^ String.sub bits 23 8
  ^ "_" ^ String.sub bits 31 8
  ^ "_" ^ String.sub bits 39 8
  ^ "_" ^ String.sub bits 47 8
  ^ "_" ^ String.sub bits 55 8
