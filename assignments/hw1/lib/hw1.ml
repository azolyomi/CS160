open Tree
open Hw1_sig

let todo () = failwith "Your code here"

module Listy : LISTY = struct
  let prepend (x : 'a) (xs : 'a list) : 'a list = x::xs
  let rec append (x : 'a) (xs : 'a list) : 'a list = 
    match xs with
    [] -> [x]
    | h::t -> h::append x t;;
  let rec cat (xs : 'a list) (ys : 'a list) : 'a list = 
    match ys with
    [] -> xs
    | h::t -> cat (append h xs) t
  let rec zip (xs : 'a list) (ys : 'b list) : ('a * 'b) list option = 
    match xs with
    [] -> (
      match ys with
      [] -> Some([])
      | h::t -> None
    )
    | h1::t1 -> (
      match ys with
      [] -> None
      | h2::t2 -> (
        let z = (zip t1 t2) in
        match z with
        None -> None
        | Some(x) -> Some((h1, h2)::x)
      )
      )

  (* map from the slides *)
  let rec map (f : 'a -> 'b) (xs : 'a list) : 'b list = 
    match xs with
    [] -> []
    | h::t -> (f h)::(map f t)

  (* helper function to take element x and add it to every position in list ys, yielding a list of lists *)
  let rec insert_all (x: 'a) (ys: 'a list) : 'a list list =
    match ys with
    [] -> [[x]] (* ys empty -> only one pos to add *)
    | h::t -> (* ys not empty - add to beginning of list AND beginning of result of insert_all*)
      (x::ys) :: (map (prepend h) (insert_all x t))

  (* recursive function to take a N-D array and return N-1-D array. *)
  let rec flatten (xs : 'a list list) : 'a list = 
    match xs with
    [] -> []
    | h::t -> cat h (flatten t)
    
  (* TODO: TEST RIGOROUSLY! *)
  let rec permute (xs : 'a list) : 'a list list = 
    match xs with
    [] -> [[]]
    | h::t -> (
      let p = permute t in
      (* p is a list of lists *)
      (* for each list, insert h in every spot. Works only because elements must be unique. *)
      let map_insert_all_result = map (insert_all h) p in
      (* map_insert_all is a list of lists of lists *)
      (* flatten it to just a list of lists *)
      flatten map_insert_all_result
    )
    
end

module Treey : TREEY = struct
  let rec skeleton (t : 'a tree) : unit tree = 
    match t with
    Leaf -> Leaf
    | Node (v, l, r) -> node () (skeleton l) (skeleton r)
  
  let rec selfie_rec (copy: 'a tree) (t : 'a tree) : 'a tree tree = 
    match t with
    Leaf -> Leaf
    | Node (v, l, r) -> node copy (selfie_rec copy l) (selfie_rec copy r)
  let selfie (t : 'a tree) : 'a tree tree = 
    selfie_rec t t

  let timestamp (t : 'a tree) : (int * 'a) tree =
    (* timestamp_rec should return a ((int * 'a) tree * int) with root value (n, val) paired with finish value n_done
       n_done = the last labeled number in the dfs of subtree. It's important to return,
       so that we can use it in computation of RHS to not have duplicate values. *)

    let rec timestamp_rec (t : 'a tree) (n : int) : (int * 'a) tree * int = 
      match t with
      (* If leaf node, shouldn't affect count. *)
      Leaf -> (Leaf, n)
      (* Otherwise, compute (l_done, n1) and (r_done, n2) sequentially
         Return (Node (n, v), l_done, r_done), n2 *)
      | Node (v, l, r) -> 
        let (l_done, n1) = timestamp_rec l (n+1) in
        let (r_done, n2) = timestamp_rec r (n1) in
        let finished_node = Node ((n, v), l_done, r_done) in
        (finished_node, (n2))
    in 
    let (t_done, n_done) = timestamp_rec t 0 in
    t_done

  let rec accepts (w : word) (t : tree_nfa) : bool = 
    match w with
    (* word is empty -> epsilon *)
    [] -> (
      match t with
      (* tree root is leaf *)
      Leaf -> false
      (* tree root is node *)
      | _ -> true
    )
    (* word has at least one letter
       rest of the word (could be []) stored in w' *)
    | letter::w' -> (
      match t with
      (* tree root is leaf *)
      Leaf -> false
      (* tree root is node *)
      | Node (x, l, r) -> (
        (* get next tree based on transition *)
        let next_tree =
          match letter with
          | O -> l
          | I -> r
        in 
        (* check if letter accepts *)
        let letter_accepts = 
          match next_tree with
          | Leaf -> false
          | _ -> true
        in
        (* if letter accepts and recursion accepts on next tree, return true. else false (AND)*)
        letter_accepts && accepts (w') (next_tree)
      )
    )

  let rec lookup (key : word) (t : 'a trie) : 'a option = 
    match key with
    (* word is empty -> epsilon *)
    [] -> (
      match t with
      (* tree root is leaf *)
      Leaf -> None
      (* tree root is node with None or Some x *)
      | Node (x, l, r) -> x
    )
    (* word has at least one letter
       rest of the word (could be []) stored in w' *)
    | letter::w' -> (
      match t with
      (* tree root is leaf *)
      Leaf -> None
      (* tree root is node with None or Some x *)
      | Node (x, l, r) -> (
        (* get next tree based on transition *)
        let next_tree =
          match letter with
          | O -> l
          | I -> r
        in 
        (* check if letter accepts *)
        let letter_accepts = 
          match next_tree with
          | Leaf -> false
          | _ -> true
        in
        (* if letter accepts and recursion accepts on next tree, return true. else false (AND)*)
        match letter_accepts with
        true -> lookup (w') (next_tree)
        | false -> None
      )
    )
  let rec insert (key : word) (value : 'a) (t : 'a trie) : 'a trie = 
    match key with
    (* no word -> replace root *)
    [] -> (
      match t with
      (* if leaf, make new leafy node with value *)
      Leaf -> node' (Some value)
      (* else just change value *)
      | Node (x, l, r) -> node (Some value) l r
    )
    (* some letter followed by subsequent word (could be []) *)
    | letter::key' -> (
      match t with
      (* if tree is leaf, make new node and insert on appropriate branch based on letter *)
      Leaf -> (
        let Node (_, l, r) = node' None in
        match letter with
        O -> node None (insert key' value t) r
        | I -> node None l (insert key' value t)
      )
      (* else tree is node, insert on appropriate branch based on letter *)
      | Node (x, l, r) -> (
        match letter with
        O -> node x (insert key' value l) r
        | I -> node x l (insert key' value r)
      ) 
    )
end
