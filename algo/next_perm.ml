#!env ocaml

(* https://zestedesavoir.com/forums/sujet/8052/un-probleme-de-combinatoire/ *)

type perm = int array

(* we compute the indexes i and j defined as follows:
 *     i = min { i ∈ 1…n | perm[i−1] > perm[i] }
 *     j = min { j ∈ 0…(i−1) | perm[j] > perm[i] }
 * so that:
 *     perm[0] < … < perm[j−1] < perm[i] < perm[j] < … < perm[i−1]
 *
 * then, we get the next permutation by following these steps:
 *
 * (0) initially:
 *         perm[0]   …   perm[j]   …   perm[i−1]   perm[i]
 *         ----increasing---------------------->
 *
 * (1) swapping i and j:
 *         perm[0]   …   perm[i]   …   perm[i−1]   perm[j]
 *         ----increasing---------------------->   bigger than perm[i]
 *
 * (2) reverting 0…(i−1):
 *         perm[1−1]   …   perm[i]   …   perm[0]   perm[j]
 *         <---increasing-----------------------
 *
 * for example:
 *      k   perm   i  j
 *
 *      0  «4321»
 *          ·↓     1  0
 *      1  «3421»
 *          · ↓    2  0
 *      2  «4231»
 *          ·↓     1  0
 *      3  «2431»
 *           ·↓    2  1
 *      4  «3241»
 *          ·↓     1  0
 *      5  «2341»
 *          ·  ↓   3  0
 *      6  «4312»
 *          ·↓     1  0
 *      7  «3412»
 *          · ↓    2  0
 *      8  «4132»
 *          ·↓     1  0
 *      9  «1432»
 *           ·↓    2  1
 *     10  «3142»
 *          ·↓     1  0
 *     11  «1342»
 *           · ↓   3  1
 *     12  «4213»
 *          ·↓     1  0
 *     13  «2413»
 *          · ↓    2  0
 *     14  «4123»
 *          ·↓     1  0
 *     15  «1423»
 *           ·↓    2  1
 *     16  «2143»
 *          ·↓     1  0
 *     17  «1243»
 *            ·↓   3  2
 *     18  «3214»
 *          ·↓     1  0
 *     19  «2314»
 *          · ↓    2  0
 *     20  «3124»
 *          ·↓     1  0
 *     21  «1324»
 *           ·↓    2  1
 *     22  «2134»
 *          ·↓     1  0
 *     23  «1234»
 *
 * in fact, to get permutation number k from permutation number k−1, we have:
 *     i = max { i : i! divides k }
 *     j = ((k / i!) mod (i+1)) − 1
 * so if k is provided, we do not need to inspect perm.
 *)

let swap perm i j =
  let x = perm.(i)
  and y = perm.(j) in
  perm.(i) <- y ;
  perm.(j) <- x

let next_perm =
  let exception Stop in
  fun perm ->
    let n = Array.length perm in
    begin try
      let i = ref 1 in
      while if !i = n then raise Stop ; perm.(pred !i) < perm.(!i) do
        incr i
      done ;
      let j = ref 0 in
      while perm.(!j) < perm.(!i) do
        incr j
      done ;
      let i, j = !i, !j in
      swap perm i j ;
      for k = 0 to i / 2 - 1 do
        swap perm k (i - 1 - k)
      done ;
      true
    with Stop ->
      false
    end

let next_perm_kth k =
  let exception Stop in
  let rec loop k i =
    let i' = succ i in
    let q, r = k / i', k mod i' in
    if r = 0 then
      loop q i'
    else
      (i, pred r)
  in
  fun perm ->
    let n = Array.length perm in
    begin try
      let i, j = loop k 0 in
      if i = n then raise Stop ;
      swap perm i j ;
      for k = 0 to i / 2 - 1 do
        swap perm k (i - 1 - k)
      done ;
      true
    with Stop ->
      false
    end

let iter_perms n f =
  let perm = Array.init n (fun i -> n - i) in
  let k = ref 1 in
  while f perm ; next_perm_kth !k perm do
    incr k
  done

let f perm =
  let n = Array.length perm in
  snd @@ Array.fold_left
    begin fun (sum, count) xi ->
      let sum = (sum + xi) mod n in
      (sum, if sum = 0 then succ count else count)
    end
    (0, 0)
    perm

let g n =
  let sum = ref 0 in
  let buckets = Array.make ((n + 3) / 2) 0 in
  iter_perms n begin fun perm ->
    let y = f perm in
    sum := !sum + y ;
    buckets.(y) <- buckets.(y) + 1
  end ;
  !sum, buckets
