type +'a iterator = ('a -> unit) -> unit

module KMP :
sig
  type pattern

  val build_pattern : string -> pattern

  val find_iter : pattern:pattern -> text:string -> int iterator

  val find : pattern:pattern -> text:string -> int list

  val split : sep:pattern -> text:string -> string list
end
=
struct
  type pattern = {
    length : int ;
    text : string ;
    jump : int array ; (* function π or π' *)
    (* π and π' are defined on 0 … length.
     * π(0) = −1
     * π(j) = length k of the longest prefix of the pattern which is a strict
     *        suffix of p[0 … j−1]:
     *      = max { k < j | p[0 … k−1] = p[j−k … j−1] }
     * π'(0) = −1
     * π'(j) = | if p[k] = p[j]:  π'(k)    where k = π(j) < j
     *         | else:            k
     *)
  }

  let (*rec*) next_candidate pattern j c =
    (* precondition: j is the length of the maximal prefix of the pattern
     * that matches a suffix of a given text (of length i):
     *     pattern[0 … j−1] = text[i−j … i−1]
     *
     *           0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
     *       i                   v
     *    text   r a b a b a b a
     * pattern         a b a b a c a
     *       j         = = = = = ^
     *                 0 1 2 3 4 5 6
     *)
    (*if j < 0 || (j < pattern.length && pattern.text.[j] = c) then
      succ j
    else
      next_candidate pattern pattern.jump.(j) c*)
    let rec next j =
      if j < 0 || pattern.text.[j] = c then
        succ j
      else
        next pattern.jump.(j)
    in
    next (if j = pattern.length then pattern.jump.(j) else j)
    (* postcondition: returns the length k of the maximal prefix of the pattern
     * that matches a suffix of the given text with character c appended
     * (length i+1):
     *     pattern[0 … k−1] = text'[i−k+1 … i]
     *
     *           0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
     *       i                   v
     *    text'  r a b a b a b a B
     * pattern             a b a b a c a
     *       k             = = = = ^
     *                     0 1 2 3 4 5 6
     *)

  let build_pattern pattern =
    let length = String.length pattern in
    let jump = Array.make (succ length) 0 in
    let pattern = { length ; text = pattern ; jump } in
    jump.(0) <- -1 ;
    let j = ref ~-1 in
    for i = 1 to length do
      j := next_candidate pattern !j pattern.text.[pred i] ;
      (* with this line, we compute π (Morris‐Pratt algorithm): *)
      (*jump.(i) <- !j*)
      (* with this one, we compute π' (Knuth‐Morris‐Pratt algorithm): *)
      jump.(i) <-
        if i < length && pattern.text.[!j] = pattern.text.[i]
        then jump.(!j)
        else !j
      (* note: we can compute both at the same time, just by storing π in a
       * separate place (which is not used for further computations, as using
       * the previous values of π' is fine even for computing π). *)
      ; Printf.eprintf "π(%u) = %u,\tπ'(%u) = %i\n" i !j i jump.(i)
    done ;
    pattern

  (* note that the text is read and occurrences are found from left to right.
   * hence when building a list of all occurrences, we need to reverse it;
   * this could be avoided by going the other direction, but then we must adapt
   * the automaton. *)
  let find_iter ~pattern ~text f =
    let j = ref 0 in
    text |>
    String.iteri begin fun i text_i ->
      j := next_candidate pattern !j text_i ;
      if !j = pattern.length then
        f (i - pattern.length + 1)
    end

  let find ~pattern ~text =
    let found = ref [] in
    find_iter ~pattern ~text begin fun occ ->
      Printf.eprintf "occurrence at %u\n" occ ;
      found := occ :: !found
    end ;
    List.rev !found

  let split ~sep ~text =
    let pieces = ref [] in
    let start = ref 0 in
    let f stop =
      if !start <= stop then begin (* ignore overlapping separators *)
        let piece = String.sub text !start (stop - !start) in
        Printf.eprintf "piece «%s» at %u–%u\n" piece !start stop ;
        pieces := piece :: !pieces ;
        start := stop + sep.length
      end
      else
        Printf.eprintf "overlapping separators at %u and %u\n" stop !start
    in
    find_iter ~pattern:sep ~text f ;
    f (String.length text) ;
    List.rev !pieces
end


let () =
  let pattern = KMP.build_pattern "ababaca" in
  (*     i:  0  1  2  3  4  5  6  7
   *  π(i): -1  0  0  1  2  3  0  1
   * π'(i): -1  0 -1  0 -1  3 -1  1
   *)
  ignore @@ KMP.find pattern "rababababacat" ; (* occurences: 5 *)
  ignore @@ KMP.find pattern  "trulababacababacababacba" ; (* occurrences: 4, 10 *)
  let sep = KMP.build_pattern "abc" in
  ignore @@ KMP.split sep "abcDEFabcabcDEF" ;
  let sep = KMP.build_pattern "aba" in
  ignore @@ KMP.find  sep "abaDEFababaGHIaba" ; (* occurences: 0, 6, 8, 14 *)
  ignore @@ KMP.split sep "abaDEFababaGHIaba" ;
