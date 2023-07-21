(***   TRI PAR TAS   ***)



(* Maximum. *)
let max a b =
    if a < b then b else a
;;

(* Échange deux valeurs du tableau. *)
let swap t i j =
    let tmp = t.(i)  in
    t.(i) <- t.(j);
    t.(j) <- tmp
;;



(* Donne les indices du parent et des deux enfants du nœud i dans un tableau
 * modélisant un tas. *)
let parent i = (i-1) / 2;;
let child1 i = 2*i + 1;;
let child2 i = 2*i + 2;;



(* « Tamise » le nœud i dans le tas (modélisé par un tableau de n éléments),
 * c’est-à-dire le fait descendre dans le tas jusqu’à ce qu’il soit plus grand
 * que ses deux enfants. *)
let rec sift (<) t n i =
    (* Si le nœud a des enfants. *)
    if n > child1 i then
        (* Si le nœud a un fils droit et qu’il faille échanger avec celui-ci
           (c’est-à-dire si celui-ci est le plus grand des deux fils et est plus
           grand que le père). *)
        let has_greater_child2 =
          n > child2 i  && t.(child1 i) < t.(child2 i)  && t.(i) < t.(child2 i)
        in
        (* Si le nœud a un fils plus grand que lui, on l’échange avec le plus
           grand de ses fils, et on continue le tamisage. *)
        if t.(i) < t.(child1 i) || has_greater_child2 then
            let ch = if has_greater_child2 then child2 i else child1 i  in
            swap t i ch;
            sift (<) t n ch
;;



(* Transforme le tableau en tas. *)
let heapify (<) t n =
    for i = parent (n-1) downto 0 do
        sift (<) t n i
    done
;;



(* Tri par tas. *)
let heapsort (<) t =
    let n = Array.length t  in
    (* On ordonne le tableau en tas. *)
    heapify (<) t n;
    swap t 0 (n-1);
    for n = n-1 downto 2 do
        (* On recommence en ignorant l’élément qu’on vient de trier, mais cette
           fois il suffit de tamiser la racine. *)
        sift (<) t n 0;
        (* On place le plus grand élément (la racine) en dernier en l’échangeant
           avec le dernier élément. *)
        swap t 0 (n-1)
    done
;;



(* Tri par tas. *)
let heapsort (<) t =
    let n = Array.length t  in
    (* On ordonne le tableau en tas. *)
    heapify (<) t n;
    for n = n downto 2 do
        (* On place le plus grand élément (la racine) en dernier en l’échangeant
           avec le dernier élément. *)
        swap t 0 (n-1);
        (* On recommence en ignorant l’élément qu’on vient de trier, mais cette
           fois il suffit de tamiser la racine. *)
        sift (<) t (n-1) 0
    done
;;



(* Génère un tableau de n couples (k,i) avec k un entier aléatoire entre 0 et
 * max, et i l’indice de ce couple (utile pour tester la stabilité du tri). *)
let create_random_tab n max =
    let t = Array.make n (0,0)  in
    for i = 0 to n-1 do
        t.(i) <- (Random.int (max+1), i)
    done;
    t
;;



Random.self_init ();;
let t = create_random_tab 20 7;;
let cmp (a,_) (b,_) = a < b;;
heapsort cmp t;;
t;;
