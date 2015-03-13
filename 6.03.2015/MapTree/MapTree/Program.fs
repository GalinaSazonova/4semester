type Tree<'a> =
    | Tip of 'a
    | Tree of 'a * Tree<'a> * Tree<'a>
    

let rec mapForTree tree func =
    match tree with
    | Tip t -> Tip (func t)
    | Tree (a, l, r) -> Tree (func a, mapForTree l func, mapForTree r func)
