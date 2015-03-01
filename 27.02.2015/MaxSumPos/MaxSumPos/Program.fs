let findMaxSumIndex list maxSumIndex =
    let rec findMSI list curIndex maxSum maxSumIndex =
        match list with
        | [] -> maxSumIndex
        | head :: [] -> maxSumIndex
        | head :: tail -> 
        if ((head + tail.Head) > maxSum) then
            findMSI tail (curIndex + 1) (head + tail.Head) curIndex
        else
            findMSI tail (curIndex + 1) maxSum maxSumIndex

    findMSI list 1 -1 maxSumIndex

(findMaxSumIndex [1; 3; 6; 7; 2] -1) |> printfn ("%A" )