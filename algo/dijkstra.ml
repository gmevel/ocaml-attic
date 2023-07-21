(*
 *  Algorithme de Dijkstra avec un tas binaire
 *)


(* Pour n nœuds et m arêtes, calculer la distance d’un nœud à un autre est en
 * O((m+n)log(n)). Pour un réseau routier, m est vraisemblablement un O(n), donc
 * on obtient O(n×log(n)).
 *
 * Si le nombre r de requêtes est potentiellement grand devant n, mieux vaut
 * calculer la matrice de toutes les distances avec n exécutions de Dijkstra
 * (mémoire raisonnable si n ⩽ 200 p·ex·). La complexité totale est donc un
 * O(n²log(n) + r).
 *)

(*
 * implémentation du tas binaire
 *)

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

class heap n = object (self)
  val mutable size = n
  val         heap = Array.init n (fun i -> i)
  val      revheap = Array.init n (fun i -> i)
  val     distance = Array.make n max_int
  
  method private swap i j =
    swap heap i j;
    swap revheap heap.(i) heap.(j)
  
  method private shift_up i =
    let j = 2*i + 1 in
    if j < size then
     let k =
      if j = size-1 || distance.(heap.(j)) <= distance.(heap.(j+1)) then
        j
      else
        j+1
     in
     if distance.(heap.(k)) < distance.(heap.(i)) then begin
       self#swap i k;
       self#shift_up k
     end
  
  method private shift_down i =
    if i <> 0 then
      let j = (i-1) / 2 in
      if distance.(heap.(j)) > distance.(heap.(i)) then begin
        self#swap i j;
        self#shift_down j
      end
  
  method pop_min =
    size <- size - 1;
    self#swap 0 size;
    self#shift_up 0;
    heap.(size)
  
  method lower node d =
    distance.(node) <- d;
    self#shift_down revheap.(node)
  
  method distance node =
    distance.(node)
end

(*
 * algorithme de Dijkstra d’un point à un autre (dans un ensemble donné)
 *)

let dijkstra g start_node end_predicate =
  let heap = new heap g#size in
  let node = ref (g#encode start_node) in
  heap#lower !node 0 ;
  while not @@ end_predicate (g#decode !node) do
    let d0 = heap#distance !node in
    List.iter
     (fun (k,d) ->
       let k = g#encode k
       and d' = d0 + d in
       if d' < heap#distance k then
         heap#lower k d'
     )
     (g#edges (g#decode !node)) ;
    node := heap#pop_min
  done;
  heap#distance !node

(*
 * algorithme de Dijkstra d’un point à tous les autres
 * (peut aussi se déduire de la fonction précédente avec un bon prédicat de fin)
 *)

let dijkstra_to_all g start_node =
  let n = g#size in
  let heap = new heap n in
  let node = ref (g#encode start_node) in
  heap#lower !node 0 ;
  let count = ref 0 in
  while !count < n-1 do
    node := heap#pop_min;
    let d0 = heap#distance !node in
    List.iter
     (fun (k,d) ->
       let d' = d0 + d in
       if d' < heap#distance k then
         heap#lower k d'
     )
     (g#edges (g#decode !node)) ;
    incr count
  done;
  heap#distance % g#encode

(*
 * exemple d’utilisation (Project Euler #83)
 *)

type node =
  | Initial
  | Cell of int * int

let build_graph n matrix =
  object
    method size = n * n + 1
    method encode = function
      | Initial      -> n * n
      | Cell (y, x)  -> y * n + x
    method decode id =
      if id = n * n then Initial else Cell (id / n, id mod n)
    val edge_to = fun y x ->
      Cell (y, x), matrix.(y).(x)
    method edges = function
      | Initial      ->
          [ edge_to 0 0 ]
      | Cell (y, x)  ->
          let li = ref [] in
          if y > 0   then li := edge_to (y-1) x :: !li ;
          if y < n-1 then li := edge_to (y+1) x :: !li ;
          if x > 0   then li := edge_to y (x-1) :: !li ;
          if x < n-1 then li := edge_to y (x+1) :: !li ;
          !li
  end

;;

#use "083-matrix.ml" ;;

let () =
  let g = build_graph n matrix in
  let d = dijkstra g Initial ((=) (Cell (n-1, n-1))) in
  Printf.printf "%i\n" d
