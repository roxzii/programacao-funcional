module AVL = struct
  type height = int
  type int = height
  module Elt = Int32

  type t = Leaf | Node of t * Elt.t * t * height

  exception Impossible

  let height = function
    | Leaf -> 0
    | Node (_, _, _, h) -> h

  let empty = Leaf

  let mknode tl y tr = Node (tl, y, tr, 1 + max (height tl) (height tr))

  let rec contains x = function
    | Leaf -> false
    | Node (tl, y, tr, _) ->
        match Elt.compare x y with
        | 0 -> true
        | res when res < 0 (* x < y *) -> contains x tl
        | _ (* x > y *) -> contains x tr

  let rotr = function
    | Node (Node (t1, y2, t3, _), y4, t5, _) ->
        mknode t1 y2 (mknode t3 y4 t5)
    | _ -> raise Impossible


  let rotl = function
    | Node (t1, y2, Node (t3, y4, t5, _), _) ->
        mknode (mknode t1 y2 t3) y4 t5
    | _ -> raise Impossible


  let rec insert x = function
    | Leaf -> Node (Leaf, x, Leaf, 1)
    | (Node (tl, y, tr, _)) as node ->
        match Elt.compare x y with
        | 0 -> node
        | res when res < 0 (* x < y *) ->
            begin match insert x tl with
            | Leaf -> raise Impossible
            | (Node (tll, yl, tlr, hl)) as tl ->
                if hl - height tr <= 1 then
                  mknode tl y tr
                else
                  let tl = if height tll < height tlr
                           then rotl tl
                           else tl
                  in
                  rotr (mknode tl y tr)
            end
        | _ (* x > y *) ->
            begin match insert x tr with
            | Leaf -> raise Impossible
            | (Node (trl, yr, trr, hr)) as tr ->
                if hr - height tl <= 1 then
                  mknode tl y tr
                else
                  let tr = if height trl > height trr
                           then rotr tr
                           else tr
                  in
                  rotl (mknode tl y tr)
            end

  let rec inorder = function
    | Leaf -> []
    | Node (tl, y, tr, _) -> inorder tl @ y :: inorder tr

  let rec check_balanced = function
    | Leaf -> ()
    | Node (tl, _, tr, h) ->
        assert (h = 1 + max (height tl) (height tr));
        check_balanced tl;
        check_balanced tr

  let check_search_tree =
    let assert_lt = function
      | Some x, Some y -> assert (Elt.compare x y < 0);
      | _, _ -> ()
    in
    let rec f bl br = function
      | Leaf -> assert_lt (bl, br) (* probably redundant *)
      | Node (tl, y, tr, _) -> assert_lt (bl, Some y);
                               assert_lt (Some y, br);
                               f bl (Some y) tl;
                               f (Some y) br tr
    in
    f None None
end

let rec random_list elt_size list_hazard =
  if Random.float 1.0 > list_hazard
  then []
  else Random.int32 (Int32.of_int elt_size) :: random_list elt_size list_hazard

let nodup l =
  let hash = Hashtbl.create (List.length l) in
  List.filter (fun x ->
                 if Hashtbl.mem hash x
                 then false
                 else (Hashtbl.add hash x (); true)) l


let rec print_list = function
  | [] -> print_endline "nil"
  | h :: t -> Printf.printf "%ld :: " h;
              print_list t

let () =
  Random.init 1;
  for i = 1 to max_int do
    if i mod 1000 = 0 then Printf.printf "%10d\n%!" i;
    let l = random_list 30 0.99 in
    let t = List.fold_left (fun t x -> AVL.insert x t) AVL.empty l in
    assert (AVL.inorder t = List.sort compare (nodup l));
    AVL.check_search_tree t;
    AVL.check_balanced t;
  done

