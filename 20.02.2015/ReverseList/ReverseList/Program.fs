let reverseList = 

    let rec revList acPrevList =
        match acPrevList with
        | head::tail -> List.append (revList tail) [head]
        | [] -> []

    let testList = [1..5]
    (revList testList) |> List.iter (fun x -> printfn "%A  " x) 