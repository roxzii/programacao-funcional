type color = W | B;;
type quadtree = Clear
              | L of color
              | N of quadtree * quadtree * quadtree * quadtree;;
type picture = {title : string; image : quadtree };;

let rec is_clear : quadtree -> bool
      = fun qt ->
        match qt with
          Clear -> true
        | Tree (nw,ne,sw,se) -> is_clear nw && is_clear ne && is_clear sw && is_clear se
        | _ -> false

let nonblank pic = not (is_clear pic.image)

let rec chop : int -> quadtree -> quadtree
  = fun n qt ->
    if n <= 0 then Clear
    else
      match qt with
        Tree (nw,ne,sw,se) -> Tree (chop (n-1) nw, chop (n-1) ne, chop (n-1) sw, chop (n-1) se)
      | color -> color

let thumbnail {title = t; image = i} = {title = t; image = chop 8 i}

let summary pics = List.map thumbnail (List.filter nonblank pics)
