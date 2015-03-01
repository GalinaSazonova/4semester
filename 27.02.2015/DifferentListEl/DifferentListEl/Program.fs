let areElDifferent list =
    let rec checkForExemplar exemplar list =
        match list with
        | head :: tail -> 
        if (exemplar <> head) then 
            checkForExemplar exemplar tail
        else false
        | [] -> true

    let rec checkForAllList list =
        match list with
        | head :: tail when tail <> [] -> 
        if (checkForExemplar head tail) then
            checkForAllList tail
        else false
        | _ -> true
    
    checkForAllList list

(areElDifferent [1; 2; 3]) |> printfn ("List without similar elements. This statement is %A")
