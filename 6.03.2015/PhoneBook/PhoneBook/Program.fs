let findByName (listNames:List<string>) (listNumbers:List<string>) name =
    List.filter (fun (x, y) -> x = name) (List.zip listNames listNumbers)

let findByNumber (listNames:List<string>) (listNumbers:List<string>) number =
    List.filter (fun (x, y) -> y = number) (List.zip listNames listNumbers)

let zipTwoLists (list1:List<string>) (list2:List<string>) =
 // List.fold2 (fun acc a b -> a.ToString() + b.ToString()) [] list1 list2
    (List.zip list1 list2) |> List.fold(fun acc (a, b) -> a::b::acc) []


let listOfNumber list =
    let rec findNumber (str:string) resultNumber pos =
        if (pos < str.Length) then
            match str.[pos] with
            |'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0' -> findNumber str (resultNumber + str.[pos]) (pos + 1)
            | _ -> resultNumber.ToString()
        else resultNumber.ToString()
    list |> List.map (fun x -> findNumber x ' ' 5)

let listOfName list =
    let rec findName (str:string) resultStr pos =
            match str.[pos] with
            | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'n'|'m' -> findName str (resultStr + str.[pos]) (pos + 1)
            | _ -> resultStr.ToString()
    list |> List.map (fun x -> findName x ' ' 0)

let rec interactive listOfNames listOfPhones = 
    printfn ("Enter number, please
    0 - exit
    1 - add contact
    2 - find number by name
    3 - find name by number
    4 - save data to file
    5 - read data from file")
    let taskN = int <| System.Console.ReadLine()

    match taskN with
    | 0 -> printfn "End working of phonebook, unsaved changes are deleted"
    | 1 -> printfn ("Please, write name")
           let name = System.Console.ReadLine() 
           printfn ("Please, write number")
           let number = System.Console.ReadLine()
           interactive (name::listOfNames) (number::listOfPhones)
    | 2 -> printfn ("Please, write name of contact")
           let name = System.Console.ReadLine()
           (findByName listOfNames listOfPhones name) |> printfn("%A")
           interactive listOfNames listOfPhones
    | 3 -> printfn ("Please, write number of contact")
           let number = System.Console.ReadLine()
           (findByNumber listOfNames listOfPhones number) |> printfn("%A")  
           interactive listOfNames listOfPhones          
    | 4 -> printfn ("Data has been saved")
           System.IO.File.AppendAllLines (@"test.txt", (zipTwoLists listOfNames listOfPhones))
           interactive listOfNames listOfPhones
    | 5 -> printfn ("Data has been loaded")
           let list = System.IO.File.ReadLines(@"test.txt") |> Seq.toList
           interactive (listOfName list) (listOfNumber list)
    | _ -> interactive listOfNames listOfPhones

let list = System.IO.File.ReadLines(@"test.txt") |> Seq.toList
interactive (listOfName list) (listOfNumber list)