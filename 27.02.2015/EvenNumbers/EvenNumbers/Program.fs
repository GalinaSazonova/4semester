let evenByFilter list =
    list |> List.filter (fun x -> x % 2 = 0) |> List.length

let evenByFold list =
    List.fold (fun acc x -> if (x % 2 = 0) then acc + 1 else acc) 0 list

let evenByMap list =
    list |> List.map (fun x -> if (x % 2 = 0) then [x] else []) |> List.concat |> List.length

(evenByFilter [1; 2; 3; 4]) |> printfn ("Even By Filter %A " )
(evenByFold [1; 2; 3; 4]) |> printfn ("Even By Fold %A " )
(evenByMap [1; 2; 3; 4]) |> printfn ("Even By Map %A " )
