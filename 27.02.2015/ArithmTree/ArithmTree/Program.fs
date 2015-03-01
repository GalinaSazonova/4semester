type Sign = char
type Number = int

type Tree =
| Tip of Number
| Tree of Sign * Tree * Tree

let rec countTree tree =
    match tree with
    | Tree('+', left, right) -> (countTree left) + (countTree right)
    | Tree('-', left, right) -> (countTree left) - (countTree right)
    | Tree('*', left, right) -> (countTree left) * (countTree right)
    | Tree('/', left, right) -> (countTree left) / (countTree right)
    | Tip value -> value
    | _ -> 0
    
let tree = Tree('+', Tree('*', Tip 2, Tree('-', Tip 10, Tip 2)), Tip 4)
(countTree tree) |> printf ("Result = %A ")
