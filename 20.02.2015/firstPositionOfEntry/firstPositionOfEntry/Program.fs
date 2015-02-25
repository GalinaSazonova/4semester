let rec findPos number list position =
    match list with
    | [] -> -1
    | head :: tail ->
    if List.head list = number  then
        position
    else 
        let position = position + 1
        findPos number (List.tail list) position

findPos 1 [1; 2; 4; 6; 7] 1 |> printfn ("%A" )
