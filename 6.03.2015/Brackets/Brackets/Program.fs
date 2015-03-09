let bracketBalance list =
    let openB = List.filter (fun x -> x = '(' || x = '{' || x = '[') list
    let closeB = List.filter (fun x -> x = ')' || x = '}' || x = ']') list
    let revcloseB = List.fold (fun acc elem -> elem::acc) [] closeB
    if (openB.Length = revcloseB.Length) then
        List.fold2 (fun acc x y -> if (x = '(' && y = ')') || (x = '[' && y = ']') || (x = '{' && y = '}') then acc else false ) true openB revcloseB
    else false
printfn ("Please, enter line of brackets")
let str = System.Console.ReadLine()
let strInArray = List.ofArray ((string str).ToCharArray())
(bracketBalance strInArray) |> printfn ("Brackets in right order: %b")