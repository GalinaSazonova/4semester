(*let bracketBalance list =
    let openB = List.filter (fun x -> x = '(' || x = '{' || x = '[') list
    let closeB = List.filter (fun x -> x = ')' || x = '}' || x = ']') list
    let revcloseB = List.fold (fun acc elem -> elem::acc) [] closeB
    if (openB.Length = revcloseB.Length) then
        List.fold2 (fun acc x y -> if (x = '(' && y = ')') || (x = '[' && y = ']') || (x = '{' && y = '}') then acc else false ) true openB revcloseB
    else false*)
let BracketBalance list =
    let rec BB list stack =
        match list with
        |h::t when h = '(' || h = '{' || h ='[' -> BB t (h::stack)
        |h::t ->
            if (stack <> []) then
                if (h = ')') then
                    if (stack.Head = '(') then
                         BB t stack.Tail
                    else false
                elif (h = '}') then
                    if (stack.Head = '{') then
                        BB t stack.Tail
                    else false
                elif (h = ']') then
                    if (stack.Head = '[') then
                        BB t stack.Tail
                    else false
                else BB t stack
             else false
        |[] -> stack = []
    let stack = []
    BB list stack

printfn ("Please, enter line of brackets")
let str = System.Console.ReadLine()
let strInArray = List.ofArray ((string str).ToCharArray())
(BracketBalance strInArray) |> printfn ("Brackets in right order: %b")