let reverseList = 

    let rec revList acPrevList =
        (*match acPrevList with
        | head::tail -> List.append (revList tail) [head]
        | [] -> []*)
       List.fold (fun acc elem -> elem::acc) [] acPrevList


    let testList = [1..5]
    (revList testList) |> List.iter (fun x -> printf "%A  " x) 