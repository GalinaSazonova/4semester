type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Tip of 'a

let rec heightOfTree tree result =
    match tree with
    | Tree(_, l, r) -> max (heightOfTree l (result + 1)) (heightOfTree r (result + 1))
    | Tip _ -> result

let tree = Tree(1, Tree(2, Tree.Tip(3), Tree(3, Tree.Tip(4), Tree.Tip(4))), Tree.Tip(2))
(heightOfTree tree 1) |> printf ("%A")
