type 'a matrix = 'a array array

let print_matrix to_string m =
  Array.iter (fun row ->
    Printf.printf " ( ";
    Array.iter (fun x -> Printf.printf "%s " (to_string x)) row;
    Printf.printf ")\n")
   m;
  Printf.printf "\n"
let print_path_matrix =
  print_matrix (function None -> "-" | Some k -> string_of_int k)
let print_distance_matrix =
  print_matrix (function -1 -> "∞" | d -> string_of_int d)


module Graph = struct
  
  type vertice = (int*int) * int
  type graph = {n: int; vertices: vertice list}
  
  let create_distance_matrix graph =
    let distances = Array.make_matrix graph.n graph.n (-1) in
    List.iter (fun ((i,j), w) -> distances.(i).(j) <- w) graph.vertices;
    for i = 0 to graph.n-1 do distances.(i).(i) <- 0 done;
    distances

  let create_path_matrix graph =
    let paths = Array.make_matrix graph.n graph.n None in
    List.iter (fun ((i,j),_) -> paths.(i).(j) <- Some j) graph.vertices;
    paths

  let create_revpath_matrix graph =
    let revpaths = Array.make_matrix graph.n graph.n None in
    List.iter (fun ((i,j),_) -> revpaths.(i).(j) <- Some i) graph.vertices;
    revpaths
  
  let warshall_step n distances paths revpaths k =
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        let dik = distances.(i).(k)
        and dkj = distances.(k).(j) in
        if dik>=0 && dkj>=0 && (distances.(i).(j)<0 || dik + dkj < distances.(i).(j)) then begin
          distances.(i).(j) <- dik + dkj;
          paths.(i).(j) <- paths.(i).(k);
          revpaths.(i).(j) <- revpaths.(k).(j)
        end
      done
    done
  
  let warshall graph =
    let n = graph.n
    and distances = create_distance_matrix graph
    and paths = create_path_matrix graph
    and revpaths = create_revpath_matrix graph in
    print_distance_matrix distances;
    print_path_matrix paths;
    for k = 0 to n-1 do
      Printf.printf "k = %u\n\n" k;
      warshall_step n distances paths revpaths k;
      print_distance_matrix distances;
      print_path_matrix paths
    done;
    (distances,paths,revpaths)
  
  let shortest_path distances paths revpaths i j =
    (*let rec revshortest_rec i j =
      match revpaths.(i).(j) with
      | None   -> [i]
      | Some k -> j :: revshortest_rec i k
    in
    let path = List.rev (revshortest_rec i j) in*)
    let rec shortest_rec i j =
      match paths.(i).(j) with
      | None   -> [j]
      | Some k -> i :: shortest_rec k j
    in
    let path = shortest_rec i j in
    Printf.printf "shortest path from %u to %u: " i j;
    if distances.(i).(j) >= 0 then begin
      Printf.printf "%u" (List.hd path);
      List.iter (fun k -> Printf.printf "→%u" k) (List.tl path);
      Printf.printf " (length %u)\n" distances.(i).(j)
    end else
        Printf.printf "-\n"
  
end


let () =
  let g =
  { Graph.n = 5;
    Graph.vertices =
    [ ((0,1), 1); ((0,3), 2); ((1,0), 2); ((1,3), 5); ((2,0), 3); ((2,1), 1);
      ((3,1), 5); ((3,2), 2) ]
  } in
  let (distances,paths,revpaths) = Graph.warshall g in
  Graph.shortest_path distances paths revpaths 2 3;
  Graph.shortest_path distances paths revpaths 3 3;
  Graph.shortest_path distances paths revpaths 2 4;
  Graph.shortest_path distances paths revpaths 4 4
