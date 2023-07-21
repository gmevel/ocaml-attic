let quicksort cmp a =
  let swap i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  in
  let rec partition_left i j =
    if i = j then
      i
    else if cmp a.(i) a.(j) < 0 then
      partition_left i (j-1)
    else begin
      swap i j;
      partition_right (i+1) j
    end
  and partition_right i j =
    if i = j then
      i
    else if cmp a.(i) a.(j) <= 0 then
      partition_right (i+1) j
    else begin
      swap i j;
      partition_left i (j-1)
    end
  in
  let rec sort i j =
    if i < j then begin
      let p = partition_left i j in
      sort i (p-1);
      sort (p+1) j;
    end
  in
  sort 0 (Array.length a - 1)

let quicksort cmp a =
  let swap i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  in
  let rec partition i j d =
    if i = j then
      i
    else if (d < 0) = (cmp a.(i) a.(j) < 0) then
      partition i (j+d) d
    else begin
      swap i j;
      partition j (i-d) ~-d
    end
  in
  let rec sort i j =
    if i < j then begin
      let p = partition i j ~-1 in
      sort i (p-1);
      sort (p+1) j;
    end
  in
  sort 0 (Array.length a - 1)
