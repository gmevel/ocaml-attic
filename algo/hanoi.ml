#!/bin/env ocaml

(***   SOLVEUR POUR LES TOURS DE HANOÏ   ***)



let print_move src dst =
    print_endline @@ string_of_int src ^ " → " ^ string_of_int dst


(* Position classique (tous les disques initialement sur le même piquet). *)

let rec solve_ordered_rec src dst = function
    | 0  -> 0
    | n  ->
        let tmp = 6-src-dst                        in
        let i   = solve_ordered_rec src tmp (n-1)  in
        let ()  = print_move src dst               in
        let j   = solve_ordered_rec tmp dst (n-1)  in
        i + 1 + j

let solve_ordered = solve_ordered_rec 1 3



(* Position quelconque.
 * La position est décrite par un tableau d’entiers entre 1 et 3 indiquant où se
 * trouve chaque disque (la 1ère valeur du tableau correspond au 1er disque le
 * plus petit, puis au 2nd, etc.). *)

let rec solve_rec pos dst = function
    | 0  -> 0
    | n  ->
        let src = pos.(n-1)  in
        if src = dst then
            solve_rec pos dst (n-1)
        else
            let tmp = 6-src-dst                in
            let i   = solve_rec pos tmp (n-1)  in
            let ()  = print_move src dst       in
            let ()  = pos.(n-1) <- dst         in
            let j   = solve_rec pos dst (n-1)  in
            i + 1 + j

let solve pos = solve_rec pos 3 (Array.length pos)


let () =
    print_endline @@ "moves: " ^ string_of_int (solve_ordered 3);
    print_endline @@ "moves: " ^ string_of_int (solve [| 3;2;1 |])
