type +'a tree =
  | Leaf of 'a list
  | Node of 'a node
and +'a node =
  { lsize : int ; left : 'a tree ; rsize : int ; right : 'a tree }

type 'a tree_walker =
  {
    mutable index : int ;
    mutable index_in_leaf : int ;
    mutable leaf : 'a list ;
    mutable rev_path : ([`left | `right] * 'a node) list ;
  }

let rec walk_down w ~index ~index_in_sub tree rev_path =
  begin match tree with
  | Leaf leaf ->
      w.index         <- index ;
      w.index_in_leaf <- index_in_sub ;
      w.leaf          <- leaf ;
      w.rev_path      <- rev_path ;
  | Node node ->
      if index_in_sub < node.lsize then
        walk_down w ~index ~index_in_sub node.left  ((`left, node) :: rev_path)
      else
        let index_in_sub = index_in_sub - node.lsize in
        walk_down w ~index ~index_in_sub node.right ((`right, node) :: rev_path)
  end

let rec walk_up w ~index ~index_in_sub rev_path =
  begin match rev_path with
  | [] ->
      raise @@ Invalid_argument "index out of bounds (walk_in_tree)"
  | (`left, node) :: rev_path' ->
      if node.lsize <= index_in_sub && index_in_sub < node.lsize + node.rsize then
        let index_in_sub = index_in_sub - node.lsize in
        walk_down w ~index ~index_in_sub node.right ((`right, node) :: rev_path')
      else
        walk_up w ~index ~index_in_sub rev_path'
  | (`right, node) :: rev_path' ->
      let index_in_sub = index_in_sub + node.lsize in
      if 0 <= index_in_sub && index_in_sub < node.lsize then
        walk_down w ~index ~index_in_sub node.left ((`left, node) :: rev_path')
      else
        walk_up w ~index ~index_in_sub rev_path'
  (*
  | (side, node) :: rev_path' ->
      let index_in_sub = index_in_sub + (if side = `left then 0 else node.lsize) in
      if 0 <= index_in_sub && index_in_sub < node.lsize + node.rsize then
        walk_down w ~index ~index_in_sub (Node node) rev_path'
      else
        walk_up w ~index ~index_in_sub rev_path'
  *)
  end

let rec walk_up_left w ~index ~index_in_sub rev_path =
  begin match rev_path with
  | [] ->
      raise @@ Invalid_argument "index out of bounds (walk_in_tree)"
  | (`left, node) :: rev_path' ->
      walk_up_left w ~index ~index_in_sub rev_path'
  | (`right, node) :: rev_path' ->
      let index_in_sub = index_in_sub + node.lsize in
      if 0 <= index_in_sub then
        walk_down w ~index ~index_in_sub node.left ((`left, node) :: rev_path')
      else
        walk_up_left w ~index ~index_in_sub rev_path'
  (*
  | (side, node) :: rev_path' ->
      let index_in_sub = index_in_sub + (if side = `left then 0 else node.lsize) in
      if 0 <= index_in_sub then
        walk_down w ~index ~index_in_sub (Node node) rev_path'
      else
        walk_up_left w ~index ~index_in_sub rev_path'
  *)
  end

let rec walk_up_right w ~index ~index_in_sub rev_path =
  begin match rev_path with
  | [] ->
      raise @@ Invalid_argument "index out of bounds (walk_in_tree)"
  | (`left, node) :: rev_path' ->
      if index_in_sub < node.lsize + node.rsize then
        let index_in_sub = index_in_sub - node.lsize in
        walk_down w ~index ~index_in_sub node.right ((`right, node) :: rev_path')
      else
        walk_up_right w ~index ~index_in_sub rev_path'
  | (`right, node) :: rev_path' ->
      let index_in_sub = index_in_sub + node.lsize in
      walk_up_right w ~index ~index_in_sub rev_path'
  (*
  | (side, node) :: rev_path' ->
      let index_in_sub = index_in_sub + (if side = `left then 0 else node.lsize) in
      if index_in_sub < node.lsize + node.rsize then
        walk_down w ~index ~index_in_sub (Node node) rev_path'
      else
        walk_up_right w ~index ~index_in_sub rev_path'
  *)
  end

let make_tree_walker ?(from : int = 0) (tree : 'a tree) : 'a tree_walker =
  begin match tree with
  | Leaf leaf ->
      if from < 0 || List.length leaf <= from then
        raise @@ Invalid_argument "index out of bounds (make_tree_walker)"
  | Node node ->
      if from < 0 || node.lsize + node.rsize <= from then
        raise @@ Invalid_argument "index out of bounds (make_tree_walker)"
  end ;
  let w = { index = 0 ; index_in_leaf = 0 ; leaf = [] ; rev_path = [] } in
  walk_down w ~index:from ~index_in_sub:from tree [] ;
  w

let walk_in_tree (w : 'a tree_walker) (off : int) : unit =
  let index = w.index + off in
  let index_in_sub = w.index_in_leaf + off in
  if 0 <= index_in_sub && index_in_sub < List.length w.leaf then begin
    w.index <- index ;
    w.index_in_leaf <- index_in_sub ;
  end else if index_in_sub < 0 then
    walk_up_left w ~index ~index_in_sub w.rev_path
  else
    walk_up_right w ~index ~index_in_sub w.rev_path
    (*! walk_up w ~index ~index_in_sub w.rev_path !*)

let jump_in_tree (w : 'a tree_walker) (index : int) : unit =
  walk_in_tree w (index - w.index)

let read_tree_here (w : 'a tree_walker) : 'a =
  List.nth w.leaf w.index_in_leaf

let read_tree_at (w : 'a tree_walker) (index : int) : 'a =
  jump_in_tree w index ;
  read_tree_here w

let read_tree_prev (w : 'a tree_walker) : 'a =
  walk_in_tree w ~-1 ;
  read_tree_here w

let read_tree_next (w : 'a tree_walker) : 'a =
  walk_in_tree w 1 ;
  read_tree_here w
