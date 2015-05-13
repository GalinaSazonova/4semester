let FindByName (listNames:List<string>) (listNumbers:List<string>) name =
    List.filter (fun (x, y) -> x = name) (List.zip listNames listNumbers)

let FindByNumber (listNames:List<string>) (listNumbers:List<string>) number =
    List.filter (fun (x, y) -> y = number) (List.zip listNames listNumbers)

let ZipTwoLists (list1:List<string>) (list2:List<string>) =
 // List.fold2 (fun acc a b -> a.ToString() + b.ToString()) [] list1 list2
    (List.zip list1 list2) |> List.fold(fun acc (a, b) -> a::b::acc) []


let ListOfNumber list =
   (* let rec findNumber (str:string) resultNumber pos =
        if (pos < str.Length) then
            match str.[pos] with
            |'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0' -> findNumber str (resultNumber + str.[pos]) (pos + 1)
            | _ -> resultNumber.ToString()
        else resultNumber.ToString()
    list |> List.map (fun x -> findNumber x ' ' 5)*)
    list |> List.filter (fun x -> ((list |> List.findIndex(fun y -> y = x)) % 2) = 1)

let ListOfName list =
    list |> List.filter (fun x -> ((list |> List.findIndex(fun y -> y = x)) % 2) = 0)
    (*let rec findName (str:string) resultStr pos =
            match str.[pos] with
            | a when System.Char.IsLetter(a) -> findName str (resultStr + a) (pos + 1)
            //| 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'n'|'m' -> findName str (resultStr + str.[pos]) (pos + 1)
            | _ -> resultStr.ToString()
    list |> List.map (fun x -> findName x ' ' 0)
  *)  


let rec Interactive listOfNames listOfPhones = 
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
           Interactive (name::listOfNames) (number::listOfPhones)
    | 2 -> printfn ("Please, write name of contact")
           let name = System.Console.ReadLine()
           (FindByName listOfNames listOfPhones name) |> printfn("%A")
           Interactive listOfNames listOfPhones
    | 3 -> printfn ("Please, write number of contact")
           let number = System.Console.ReadLine()
           (FindByNumber listOfNames listOfPhones number) |> printfn("%A")  
           Interactive listOfNames listOfPhones          
    | 4 -> printfn ("Data has been saved")
           //System.IO.File.Delete(@"..\Debug\test.txt")
           let a = System.IO.StreamWriter(@"..\Debug\test.txt")
           (List.zip listOfNames listOfPhones) |> List.iter(fun (x, y) -> a.WriteLine(x)
                                                                          a.WriteLine(y))
           a.Close()
           Interactive listOfNames listOfPhones
    | 5 -> printfn ("Data has been loaded")
           let list = System.IO.File.ReadLines(@"..\Debug\test.txt") |> Seq.toList
           Interactive (ListOfName list) (ListOfNumber list)
    | _ -> Interactive listOfNames listOfPhones


let list = System.IO.File.ReadLines(@"..\Debug\test.txt") |> Seq.toList
Interactive (ListOfName list) (ListOfNumber list)
