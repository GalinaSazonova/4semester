(*let bracketBalance list =
    let openB = List.filter (fun x -> x = '(' || x = '{' || x = '[') list
    let closeB = List.filter (fun x -> x = ')' || x = '}' || x = ']') list
    let revcloseB = List.fold (fun acc elem -> elem::acc) [] closeB
    if (openB.Length = revcloseB.Length) then
        List.fold2 (fun acc x y -> if (x = '(' && y = ')') || (x = '[' && y = ']') || (x = '{' && y = '}') then acc else false ) true openB revcloseB
    else false*)

let bracketMatch closingB openingB =
    match closingB with
    |')' when openingB = '(' -> true
    |'}' when openingB = '{' -> true
    |']' when openingB = '[' -> true
    | _ -> false

let BracketBalance list =
    let rec balance list stack =
        match list with
        |h::t when h = '(' || h = '{' || h ='[' -> balance t (h::stack)
        |h::t ->
            if (stack <> []) then
                if (bracketMatch h stack.Head) then
                       balance t stack.Tail
                else false
             else false
        |[] -> stack = []
    let stack = []
    balance list stack

printfn ("Please, enter line of brackets")
let str = System.Console.ReadLine()
let strInArray = List.ofArray ((string str).ToCharArray())
(BracketBalance strInArray) |> printfn ("Brackets in right order: %b")