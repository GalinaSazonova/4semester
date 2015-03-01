let isPalin str =
    let rec revList acPrevList =
     (*  match acPrevList with
       | head::tail -> List.append (revList tail) [head]
       | [] -> []*)
       List.fold (fun acc elem -> elem::acc) [] acPrevList
    let directOrder = List.ofArray ((string str).ToCharArray())
    let backOrder = revList directOrder
    directOrder = backOrder

let str = System.Console.ReadLine()
(isPalin str) |> printfn "%A "