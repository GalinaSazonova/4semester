type Tree<'a> =
    | Tip of 'a
    | Tree of 'a * Tree<'a> * Tree<'a>
    //| Empty

let isEven n =
    n % 2 = 0

let rec treeToString tree = 
    match tree with
    | Tip t -> t.ToString()
    | Tree (a, l, r) -> (treeToString l).ToString() + a.ToString() + (treeToString r).ToString()

let getList tree condition =
    let rec matchElem tree condition =
        match tree with
        | Tip t -> if (condition t) then Tip t else Tip 0
        | Tree (a, l, r) -> if (condition a) then Tree (a, matchElem l condition, matchElem r condition)
                                            else Tree (0, matchElem l condition, matchElem r condition)
    
    let str = treeToString (matchElem tree condition)
    str.ToCharArray() |> Array.filter(fun x -> x <> '0')

(*let tree = Tree(5, Tree(2, Tip(1), Tip(3)), Tip(9))
getList tree isEven*)