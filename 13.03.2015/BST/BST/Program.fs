open System
open System.Collections
open System.Collections.Generic

type Tree<'a> =
|Empty
|Tip of 'a
|Tree of Tree<'a> * 'a *  Tree<'a>

type BSTree<'a when 'a:comparison> () =
    let mutable tree = Empty

    member k.TreeToList =
        let rec treeToListRec tree =
            match tree with
            |Empty -> []
            |Tip t -> [t]
            |Tree (l, m, r) -> treeToListRec l @ [m] @ treeToListRec r
        treeToListRec tree

    interface IEnumerable with 
        member v.GetEnumerator() =
                let list = ref v.TreeToList
                if !list = List.Empty then
                    { new IEnumerator with
                        member x.Current with get() = null
                        member x.Reset() = ()
                        member x.MoveNext() = false
                    }
                else
                    list:= (List.head !list)::!list
                    { new IEnumerator with
                        member t.Current with get() = (List.head !list) :> obj

                        member t.MoveNext() = 
                             match !list with
                             | [] ->
                                false
                             | h::t ->
                                if t = [] then
                                    false
                                else
                                    list := t
                                    true
                    
                        member t.Reset() =
                            list := v.TreeToList  
                    }

    member k.Add meaning =
        let rec add meaning tree =
            match tree with
            |Empty -> Tip meaning
            |Tip t -> if meaning < t then
                           Tree(Tip meaning, t, Empty)
                      elif t = meaning then
                           Tree(Empty, t, Empty)
                      else
                           Tree(Empty, meaning, Tip t)
            |Tree(l, m, r) -> if meaning < m then
                                   Tree(add meaning l, m, r)
                              elif m = meaning then
                                   Tree(l, m, r)
                              else
                                   Tree(l, m, add meaning r)
        tree <- add meaning tree

    member k.ElementIsInTree meaning =
        let rec isExist meaning tree =
            match tree with 
            |Empty -> false
            |Tip t -> t = meaning
            |Tree(l, m, r) -> if meaning < m then
                                    isExist meaning l
                              elif meaning = m then
                                    true
                              else 
                                    isExist meaning r
        isExist meaning tree

    member k.Remove meaning =
        let rec biggestR tree =
            match tree with
            |Empty -> Empty
            |Tip t -> Tip t
            |Tree(l, m, r) ->
                             match r with
                             |Empty -> Empty
                             |_ -> biggestR r

        let rec Remove meaning tree =
            match tree with
            |Empty -> Empty
            |Tip t -> if t = meaning then
                            Empty
                      else
                            tree
            |Tree(l, m, r) -> if meaning < m then Tree(Remove meaning l, m, r)
                              elif meaning > m then Tree(l, m, Remove meaning r)
                              else match l with
                                   |Empty -> r
                                   |_ -> let t = biggestR l
                                         match t with
                                         |Tip a -> Tree(Remove a l, a, r)

        if k.ElementIsInTree meaning then
            tree <- Remove meaning tree
        else printfn("There is no such element")

let tree = new BSTree<int>()
tree.Add 4
tree.Add 2
tree.Add 1
tree.Add 8
for  i in tree do
    printf "%A " i
printfn ""
tree.Remove 8
printfn "%A" (tree.ElementIsInTree 8)
for  i in tree do
    printf "%A " i
printfn ""
tree.Remove 3
printfn "%A" (tree.ElementIsInTree 4)
