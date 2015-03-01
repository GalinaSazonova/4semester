let rec listOfPower list =
    List.map2 (fun x y -> x * y) list [1; 2 ; 4; 8; 16]

let rec involution degree baseResult =
    if degree >= 1 then
        involution (degree - 1) (baseResult * 2)
    else
        baseResult

printfn "Please, enter degree of 2"
let degreeOfBaseList = int <| System.Console.ReadLine ()
let baseList = involution (degreeOfBaseList - 1) 2
(listOfPower [baseList; baseList; baseList; baseList; baseList]) |> List.iter (fun x -> printf "%A  " x)
