(*** ALGORITHME : I-ÈME PERMUTATION D’UNE LISTE DE N ÉLÉMENTS ***)





(**  AVEC DES LISTES  **)



(* prend un couple de listes et un entier n ; ôte le n-ième élément de la seconde
 * et l’ajoute à la fin de la première. *)
let rec extractN (p,l) = 
    let consCouple e (p,l) = (p, e::l);
    in function
    | 0  -> (p @ [List.hd l], List.tl l)
    | n  -> consCouple (List.hd l) (extractN (p, List.tl l) (n-1))
;;



(* Retourne la i-ème permutation de p éléments pris parmi la liste l. *)
let permPInN l p i =
    let rec permPInN_rec set n p i = match p with
    | 0  -> set
    | p  -> extractN (permPInN_rec set n (p-1) (i/(n-p+1))) (i mod (n-p+1))
    in
    fst (permPInN_rec ([], l) (List.length l) p i)
;;

(* Retourne la i-ème permutation de la liste. *)
let permN l = permPInN l (List.length l);;





(**  AVEC DES TABLEAUX  **)



(* Décale n éléments du tableau à partir de l’indice off d’une case vers la
 * droite. *)
let rec shift1_tab_right tab off = function
    | 0  -> tab
    | n  ->
        let _ = tab.(off+n) <- tab.(off+n-1)    in
        let _ = shift1_tab_right tab off (n-1)  in
        tab
;;



(* Retourne la i-ème permutation de p éléments pris parmi le tableau tab.
 * La permutation est faite en place. *)
let perm_p_in_n_tab tab p i =
    let rec perm_p_in_n_tab_rec tab n p i = match p with
        | 0  -> tab
        | p  ->
            let j = i / (n-p+1)
            and k = i mod (n-p+1)
            in
            let _ =  perm_p_in_n_tab_rec tab n (p-1) j  in
            let e =  tab.(p-1+k)                        in
            let _ =  shift1_tab_right tab (p-1) k       in
            let () = tab.(p-1) <- e                     in
            tab
    in
    perm_p_in_n_tab_rec tab (Array.length tab) p i
;;

(* Retourne la i-ème permutation du tableau (faite en place). *)
let perm_n_tab tab = perm_p_in_n_tab tab (Array.length tab);;





(* Pour tester. *)
let rec genTestList f start end_ =
    if (start=end_)
      then []
      else f () start :: genTestList f (start+1) end_
;;

genTestList (fun () -> permN [1;2;3;4]) 0 8;;

genTestList (fun () -> permPInN [1;2;3;4] 3) 0 8;;


genTestList (fun () -> perm_n_tab [|1;2;3;4|]) 0 8;;

genTestList (fun () -> perm_p_in_n_tab [|1;2;3;4|] 2) 0 8;;
