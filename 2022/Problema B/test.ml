open Printf

type bintree = Leaf of int
             | Node of bintree * int * bintree

let rec print_tree_infix = function
    Leaf n ->
    Printf.printf "%d" n;
    print_newline()
  | Node (left, n, right) ->
    Printf.printf "%d" n;
    print_newline();
    print_tree_infix left;
    print_tree_infix right

    let mytree = Node(Node(Leaf 6, 3, Leaf 9), 8, Node(Leaf 7, 9, Leaf 2))
    let () = print_tree_infix mytree

    (*Create a binary tree*)
    let rec bintree = function
        | 0 -> Leaf 0
        | 1 -> Leaf 1
        | n -> Node (bintree (n - 1), n, bintree (n - 2))

    (*Faz-me o caralho do trabalho de PF!*)
    let rec print_tree_prefix = function
        Leaf n ->
        Printf.printf "%d" n;
        print_newline()
      | Node (left, n, right) ->
        Printf.printf "%d" n;
        print_newline();
        print_tree_prefix left;
        print_tree_prefix right

         (* Build all trees with given [left] and [right] subtrees. *)
let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node (mytree, l, r) :: a) all right in
    List.fold_left add_right_tree all left
      
  let rec cbal_tree n =
          if n = 0 then [Leaf 0]
          else if n mod 2 = 1 then
            let t = cbal_tree (n / 2) in
            add_trees_with t t [Leaf 0]
          else (* n even: n-1 nodes for the left & right subtrees altogether. *)
            let t1 = cbal_tree (n / 2 - 1) in
            let t2 = cbal_tree (n / 2) in
            add_trees_with t1 t2 (add_trees_with t2 t1 []);;
      val add_trees_with :
        char bintree list ->
        char binary_tree list -> char binary_tree list -> char binary_tree list =
        <fun>
      val cbal_tree : int -> char binary_tree list = <fun>
