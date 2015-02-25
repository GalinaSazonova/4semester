let rec findProduct number product =
    if (number > 0) && (product > 0) then
        let product = product * (number % 10)
        let number = number / 10
        findProduct number product
    else product

let number = int <| System.Console.ReadLine ()
(findProduct number 1) |> printfn "%A  " 

