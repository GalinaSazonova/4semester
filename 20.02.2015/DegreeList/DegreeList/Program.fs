let rec listOfPower check baseList list =
    if check > 0 then 
        let check = check - 1
        let baseList = baseList * 2
        let list = list @ [baseList]
        listOfPower check baseList list
    else list

let baseList = int <| System.Console.ReadLine ()
if baseList % 2 = 0 then
    (listOfPower 4 baseList [baseList]) |> List.iter (fun x -> printfn "%A  " x)
else printfn "Wrong base"