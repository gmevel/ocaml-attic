open Num

let (!!) = num_of_int
let (%/) = mod_num
let (//) = quo_num

let b64digit_to_int = function
  | 'A'..'Z' as c -> Char.code c - Char.code 'A'
  | 'a'..'z' as c -> Char.code c - Char.code 'a' + 26
  | '0'..'9' as c -> Char.code c - Char.code '0' + 52
  | '+' | '-' -> 62
  | '/' | '_' -> 63
  | _ -> -1

let bits_to_bytes bits length_bits =
  let length_bytes = (length_bits + 7) / 8 in
  let bytes = Bytes.create length_bytes in
  let tmp = ref bits in
  for i = length_bytes-1 downto 0 do
    let b = (int_of_num (!tmp %/ !!256)) in
    Bytes.set bytes i (Char.chr b);
    tmp := !tmp // !!256
  done;
  bytes

let () =
  let bits = ref !!0
(*   and length_b64 = ref 0 *)
  and length_bits = ref 0
  and equalsigns = ref 0 in
  begin try while true do
    let d = input_char stdin in
    let n = b64digit_to_int d in
    if n >= 0 then begin
      bits := (!bits */ !!64) +/ !!n;
(*       incr length_b64 *)
      length_bits := !length_bits + 6
    end
    else if d = '=' then
      incr equalsigns;
  done with End_of_file -> () end;
  length_bits := !length_bits - !equalsigns;
  let bits = begin match !equalsigns with
    | 0 -> !bits
    | 1 -> !bits // !!(2*2)
    | 2 -> !bits // !!(2*2*2*2)
    | _ -> failwith "invalid number of '='"
  end
(*   and length_bytes = !length_b64 * 3/4 in *)
  (*and length_bytes = !length_bits / 8 in
  let bytes = Bytes.create length_bytes in
  let tmp = ref bits in
  for i = length_bytes-1 downto 0 do
    let b = (int_of_num (!tmp %/ !!256)) in
    Bytes.set bytes i (Char.chr b);
    tmp := !tmp // !!256
  done;*)
  in
  let tmp = ref bits in
  for i = 0 to 7 do
    let bytes = bits_to_bytes !tmp (!length_bits + i) in
    print_bytes bytes;
    print_newline ();
    tmp := !tmp */ !!2;
  done
