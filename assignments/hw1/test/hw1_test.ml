open Tree
open Hw1
open Listy
open Treey
open Test_lib
(* The tests for Listy *)
let test_prepend () =
  (* normal test *)
  Alcotest.(check (list int)) "same lists" [ 1; 2; 3 ] (prepend 1 [ 2; 3 ]);;
  
  (* empty list *)
  Alcotest.(check (list int)) "same lists" [ 1] (prepend 1 [ ])

let test_append () =
  (* normal test *)
  Alcotest.(check (list int)) "same lists" [ 2; 3; 1 ] (append 1 [ 2; 3 ]);;
  
  (* empty list *)
  Alcotest.(check (list int)) "same lists" [ 2] (append 2 [ ])

let test_cat () =
  (* normal test *)
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3; 4 ]
    (cat [ 1; 2 ] [ 3; 4 ]);;

  (* empty second list *)
  Alcotest.(check (list int))
    "same lists" [ 1; 2]
    (cat [ 1; 2 ] []);;

  (* empty first list *)
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3; 4]
    (cat [] [ 1; 2; 3; 4]);;

  (* both lists empty *)
  Alcotest.(check (list int))
    "same lists" [ ]
    (cat [] [ ]);;

  (* different size lists *)
  Alcotest.(check (list int))
    "same lists" [ 1; 4; 2; 3; 5]
    (cat [1; 4] [2; 3; 5]);;
  

let test_zip () =
  (* normal test *)
  Alcotest.(check (option (list (pair int string))))
    "same lists"
    (Some [ (1, "a"); (2, "b") ])
    (zip [ 1; 2 ] [ "a"; "b" ]);;

  (* more elements test *)
  Alcotest.(check (option (list (pair int string))))
    "same lists"
    (Some [ (1, "a"); (2, "b"); (1, "c"); (1, "a"); (5, "e")])
    (zip [ 1; 2; 1; 1; 5 ] [ "a"; "b"; "c"; "a"; "e" ]);;

  (* both empty list *)
  Alcotest.(check (option (list (pair int string))))
    "same lists"
    (Some [ ])
    (zip [ ] [ ]);;

let test_zip_fail () =
  (* normal test *)
  Alcotest.(check (option (list (pair int string))))
    "same lists" None
    (zip [ 1 ] [ "a"; "b" ]);;

  (* more elements *)
  Alcotest.(check (option (list (pair int string))))
    "same lists" None
    (zip [ 1; 5; 1; 2; 1] [ "a"; "b"; "d" ]);;

  (* first list empty *)
  Alcotest.(check (option (list (pair int string))))
    "same lists" None
    (zip [ ] [ "a"; "b" ]);;

  (* second list empty *)
  Alcotest.(check (option (list (pair int string))))
    "same lists" None
    (zip [ 1; 3; 4 ] [ ]);;

let rec cmp_aph x y =
  match (x, y) with
  | xh :: xt, yh :: yt ->
      if xh < yh then -1 else if xh > yh then 1 else cmp_aph xt yt
  | _, _ -> 0

let test_permute () =
  (* normal test *)
  Alcotest.(check (list (list int)))
    "same lists"
    [
      [ 1; 2; 3 ];
      [ 1; 3; 2 ];
      [ 2; 1; 3 ];
      [ 2; 3; 1 ];
      [ 3; 1; 2 ];
      [ 3; 2; 1 ];
    ]
    (List.sort cmp_aph (permute [ 1; 2; 3 ]));;

  (* empty list *)
  Alcotest.(check (list (list int)))
    "same lists"
    [
      []
    ]
    (List.sort cmp_aph (permute [ ]));;

  (* one element *)
  Alcotest.(check (list (list int)))
    "same lists"
    [
      [ 1 ];
    ]
    (List.sort cmp_aph (permute [ 1 ]));;

  (* two elements *)
  Alcotest.(check (list (list int)))
    "same lists"
    [
      [ 1; 2 ];
      [ 2; 1 ];
    ]
    (List.sort cmp_aph (permute [ 1; 2 ]));;

  (* four elements *)
  Alcotest.(check (list (list int)))
    "same lists"
    [
      [ 1; 2; 3; 4];
      [ 1; 2; 4; 3];
      [ 1; 3; 2; 4];
      [ 1; 3; 4; 2];
      [ 1; 4; 2; 3];
      [ 1; 4; 3; 2];

      [ 2; 1; 3; 4];
      [ 2; 1; 4; 3];
      [ 2; 3; 1; 4];
      [ 2; 3; 4; 1];
      [ 2; 4; 1; 3];
      [ 2; 4; 3; 1];
      
      [ 3; 1; 2; 4];
      [ 3; 1; 4; 2];
      [ 3; 2; 1; 4];
      [ 3; 2; 4; 1];
      [ 3; 4; 1; 2];
      [ 3; 4; 2; 1];

      [ 4; 1; 2; 3];
      [ 4; 1; 3; 2];
      [ 4; 2; 1; 3];
      [ 4; 2; 3; 1];
      [ 4; 3; 1; 2];
      [ 4; 3; 2; 1];
    ]
    (List.sort cmp_aph (permute [ 1; 2; 3; 4 ]));;
    
    
(* The tests for Treey *)

let test_skeleton () =
  (* normal test *)
  Alcotest.(check string)
    "same trees"
    (let t1 = node' () in
     let t2 = node' () in
     let t3 = node () t1 t2 in
     let t4 = node' () in
     let t5 = node () t3 t4 in
     t5)
    (let t1 = node' 2 in
     let t2 = node' 3 in
     let t3 = node 1 t1 t2 in
     let t4 = node' 4 in
     let t5 = node 0 t3 t4 in
     [%derive.show: unit tree] (skeleton t5));;

  (* node no children *)
  Alcotest.(check string)
    "same trees"
    (let t1 = node' () in [%derive.show: unit tree] t1)
    ([%derive.show: unit tree] (skeleton (node' 5)));;

  (* leaf *)
  Alcotest.(check string)
    "same trees"
    ([%derive.show: unit tree] leaf)
    ([%derive.show: unit tree] (skeleton leaf));;

let test_selfie () =
  (* normal test *)
  Alcotest.(check string)
    "same trees"
    (let t1 = node' 1 in
     let t2 = node' 2 in
     let t3 = node 0 t1 t2 in
     let tt1 = node' t3 in
     let tt2 = node' t3 in
     let tt3 = node t3 tt1 tt2 in
     tt3)
    (let t1 = node' 1 in
     let t2 = node' 2 in
     let t3 = node 0 t1 t2 in
     [%derive.show: int tree tree] (selfie t3));;
  
  (* node no children *)
  Alcotest.(check string)
    "same trees"
    (let t1 = node' (node' 5) in [%derive.show: int tree tree] t1)
    ([%derive.show: int tree tree] (selfie (node' 5)));;

  (* leaf *)
  Alcotest.(check string)
    "same trees"
    ([%derive.show: int tree tree] leaf)
    ([%derive.show: int tree tree] (selfie leaf));;

let dfs_eg = 
  let t1 = node' "f" in 
  let t2 = node "e" t1 leaf in 
  let t3 = node "d" t2 leaf in 
  let t4 = node "c" t3 leaf in 
  let t5 = node "b" t4 leaf in 
  let t6 = node' "g" in 
  let t7 = node "a" t5 t6 in 
  t7

let test_timestamp () =
  (* normal test *)
  Alcotest.(check string)
    "same trees"
    (let t1 = node' (2, "b") in
     let t2 = node (1, "u") t1 leaf in
     let t3 = node' (4, "e") in
     let t4 = node' (5, "r") in
     let t5 = node (3, "m") t3 t4 in
     let t6 = node (0, "n") t2 t5 in
      t6)
    (let t1 = node' "b" in
     let t2 = node "u" t1 leaf in
     let t3 = node' "e" in
     let t4 = node' "r" in
     let t5 = node "m" t3 t4 in
     let t6 = node "n" t2 t5 in
     [%derive.show: (int * string) tree] (timestamp t6));;

  (* node no children *)
  Alcotest.(check string)
    "same trees"
    (let t1 = node' (0,()) in [%derive.show: (int * unit) tree] t1)
    ([%derive.show: (int * unit) tree] (timestamp (node'())));;

  (* leaf *)
  Alcotest.(check string)
    "same trees"
    ([%derive.show: (int * unit) tree] leaf)
    ([%derive.show: (int * unit) tree] (timestamp leaf));;

  (* deep tree *)
  Alcotest.(check string)
    "same trees"
    (let t1 = node' (5, "f") in 
    let t2 = node (4, "e") t1 leaf in 
    let t3 = node (3, "d") t2 leaf in 
    let t4 = node (2, "c") t3 leaf in 
    let t5 = node (1, "b") t4 leaf in 
    let t6 = node' (6, "g") in 
    let t7 = node (0, "a") t5 t6 in 
    [%derive.show: (int * string) tree] t7)
    ([%derive.show: (int * string) tree] (timestamp dfs_eg));;

  
let test_nfa () = 
  (* node no children *)
  Alcotest.(check bool) "same bool" true (accepts [ ] (node' ()));;
  Alcotest.(check bool) "same bool" false (accepts [ O ] (node' ()));;
  Alcotest.(check bool) "same bool" false (accepts [ I ] (node' ()));;
  Alcotest.(check bool) "same bool" false (accepts [ O; O ] (node' ()));;
  Alcotest.(check bool) "same bool" false (accepts [ O; I ] (node' ()));;
  Alcotest.(check bool) "same bool" false (accepts [ I; O ] (node' ()));;
  Alcotest.(check bool) "same bool" false (accepts [ I; I ] (node' ()));;

  (* leaf *)
  Alcotest.(check bool) "same bool" false (accepts [ ] leaf);;
  Alcotest.(check bool) "same bool" false (accepts [ O ] leaf);;
  Alcotest.(check bool) "same bool" false (accepts [ I ] leaf);;
  Alcotest.(check bool) "same bool" false (accepts [ O; O ] leaf);;
  Alcotest.(check bool) "same bool" false (accepts [ O; I ] leaf);;
  Alcotest.(check bool) "same bool" false (accepts [ I; O ] leaf);;
  Alcotest.(check bool) "same bool" false (accepts [ I; I ] leaf);;

  (* dfs tree *)
  Alcotest.(check bool) "same bool" true (accepts [ ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" true (accepts [ I ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ I; O ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ I; I ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" true (accepts [ O ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" true (accepts [ O; O ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ O; I ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" true (accepts [ O; O; O ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ O; O; I ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" true (accepts [ O; O; O; O] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ O; O; O; I ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" true (accepts [ O; O; O; O; O ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ O; O; O; O; I] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ O; O; O; O; O; O ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ O; O; O; O; O; I ] (skeleton dfs_eg));;
  Alcotest.(check bool) "same bool" false (accepts [ I; I; I; O] (skeleton dfs_eg));;

let nfa_eg =
  let t1 = node' () in
  let t2 = node' () in
  let t3 = node () leaf t2 in
  let t4 = node () t1 t3 in
  t4

let test_accepts_II () =
  Alcotest.(check bool) "same bool" true (accepts [ I; I ] nfa_eg)

let test_accepts_OO () =
  Alcotest.(check bool) "same bool" false (accepts [ O; O ] nfa_eg)

let option_dfs_eg = 
  let t1 = node' (Some "f") in 
  let t2 = node (Some "e") t1 leaf in 
  let t3 = node (Some "d") t2 leaf in 
  let t4 = node (Some "c") t3 leaf in 
  let t5 = node (Some "b") t4 leaf in 
  let t6 = node' (Some "g") in 
  let t7 = node (Some "a") t5 t6 in 
  t7

let test_lookup () = 
  (* node no children *)
  Alcotest.(check (option string)) "same option string" (Some "a") (lookup [ ] (node' (Some "a")));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O ] (node' (Some "a")));;
  Alcotest.(check (option string)) "same option string" None (lookup [ I ] (node' (Some "a")));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; O ] (node' (Some "a")));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; I ] (node' (Some "a")));;
  Alcotest.(check (option string)) "same option string" None (lookup [ I; O ] (node' (Some "a")));;
  Alcotest.(check (option string)) "same option string" None (lookup [ I; I ] (node' (Some "a")));;

  (* leaf *)
  Alcotest.(check (option string)) "same option string" None (lookup [ ] leaf);;
  Alcotest.(check (option string)) "same option string" None (lookup [ O ] leaf);;
  Alcotest.(check (option string)) "same option string" None (lookup [ I ] leaf);;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; O ] leaf);;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; I ] leaf);;
  Alcotest.(check (option string)) "same option string" None (lookup [ I; O ] leaf);;
  Alcotest.(check (option string)) "same option string" None (lookup [ I; I ] leaf);;

  (* dfs tree *)
  Alcotest.(check (option string)) "same option string" (Some "a") (lookup [ ] ( option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" (Some "g") (lookup [ I ] ( option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ I; O ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ I; I ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" (Some "b") (lookup [ O ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" (Some "c") (lookup [ O; O ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; I ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" (Some "d") (lookup [ O; O; O ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; O; I ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" (Some "e") (lookup [ O; O; O; O] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; O; O; I ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" (Some "f") (lookup [ O; O; O; O; O ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; O; O; O; I] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; O; O; O; O; O ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ O; O; O; O; O; I ] (option_dfs_eg));;
  Alcotest.(check (option string)) "same option string" None (lookup [ I; I; I; O] (option_dfs_eg));;

let trie_eg =
  let t1 = node' (Some 5) in
  let t2 = node' (Some 4) in
  let t3 = node None leaf t2 in
  let t4 = node (Some 3) t1 t3 in
  t4

let test_lookup_OO () =
  Alcotest.(check (option int)) "same option string" None (lookup [ O; O ] trie_eg)

let test_lookup_II () =
  Alcotest.(check (option int)) "same option string" (Some 4) (lookup [ I; I ] trie_eg)

let trie_inserted =
  let t0 = node' (Some 6) in
  let t1 = node (Some 5) t0 leaf in
  let t2 = node' (Some 4) in
  let t3 = node None leaf t2 in
  let t4 = node (Some 3) t1 t3 in
  t4

let insert_dfs_eg = 
  let t1 = node' (Some 6) in 
  let t2 = node (Some 5) t1 leaf in 
  let t3 = node (Some 4) t2 leaf in 
  let t4 = node (Some 3) t3 leaf in 
  let t5 = node (Some 2) t4 leaf in 
  let t6 = node' (Some 7) in 
  let t7 = node (Some 1) t5 t6 in 
  t7



let test_insert () =
  (* normal test *)
  Alcotest.(check string)
    "same trie"
    ([%derive.show: int trie] trie_inserted)
    ([%derive.show: int trie] (insert [ O; O ] 6 trie_eg));;

  (* start from node no children *)
    Alcotest.(check string)
    "same trie"
    ([%derive.show: int trie] (node (Some 0) leaf (node None (node None leaf (node None leaf (node' (Some 14)))) leaf)))
    ([%derive.show: int trie] (insert [ I; O; I; I ] 14 (node' (Some 0))));;
  

  (* start from leaf *)
  Alcotest.(check string)
    "same trie"
    ([%derive.show: int trie] (node None (node None (node None leaf (node None leaf (node' (Some 14)))) leaf) leaf))
    ([%derive.show: int trie] (insert [ O; O; I; I ] 14 leaf));;


  (* make dfs tree from scratch *)
  Alcotest.(check string)
    "same trie"
    ([%derive.show: int trie] insert_dfs_eg)
    ([%derive.show: int trie] (insert [O; O] 3 (insert [O] 2 (insert [O; O; O; O] 5 (insert [] 1 (insert [O; O; O] 4 (insert [ O; O; O; O; O ] 6 (insert [I] 7 leaf))))))) );;
  

(* Run it *)
let () =
  let open Alcotest in
  run "public tests"
    [
      (* Listy tests *)
      ("prepend", [ test_case "prepend" `Slow test_prepend ]);
      ("append", [ test_case "append" `Slow test_append ]);
      ("cat", [ test_case "cat" `Slow test_cat ]);
      ("zip", [ test_case "zip" `Slow test_zip ]);
      ("zip-fail", [ test_case "zip-fail" `Slow test_zip_fail ]);
      ("permute", [ test_case "permute" `Slow test_permute ]);

      (* Treey tests *)
      ("skeleton", [ test_case "skeleton" `Slow test_skeleton ]);
      ("selfie", [ test_case "selfie" `Slow test_selfie ]);
      ("timestamp", [ test_case "timestamp" `Slow test_timestamp ]);
      ("test-nfa", [ test_case "test-nfa" `Slow test_nfa ]);
      ("accepts-II", [ test_case "accepts-II" `Slow test_accepts_II ]);
      ("accepts-OO", [ test_case "accepts-OO" `Slow test_accepts_OO ]);
      ("test-lookup", [ test_case "test-lookup" `Slow test_lookup ]);
      ("lookup-II", [ test_case "lookup-II" `Slow test_lookup_II ]);
      ("lookup-OO", [ test_case "lookup-OO" `Slow test_lookup_OO ]);
      ("insert", [ test_case "insert" `Slow test_insert ]);
    ]
