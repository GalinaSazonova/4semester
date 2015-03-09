let func0 x l = List.map (fun y -> y * x) l
let func1 x : (int)List -> (int)List = List.map (fun y -> y * x)
let func2 : int -> (int)List -> (int)List = (fun y -> (*) y) >> List.map
let func3 : int -> (int)List -> (int)List = ((*)) >> List.map

printf "Origin "
(func0 2 [1;2;3]) |> List.iter (printf"%d ")
printfn ""
printf "1 step "
(func1 2 [1;2;3]) |> List.iter (printf"%d ")
printfn ""
printf "2 step "
(func2 2 [1;2;3]) |> List.iter (printf"%d ")
printfn ""
printf "3 step "
(func3 2 [1;2;3]) |> List.iter (printf"%d ")
printfn ""