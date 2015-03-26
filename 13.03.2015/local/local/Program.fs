type OS = {nameOfOS:string; Probability:int}
let rand = new System.Random()

/// PC class
type PC (Infected:bool, nameOfOS:OS, IndexNumber:int, IsJustInfected:bool) =
    let mutable infectedNow = Infected
    let mutable isJustInfected = IsJustInfected
    member p.IsJustInfected = isJustInfected
    member p.IsInfected = infectedNow
    member p.IndexNumberIs = IndexNumber
    member p.TryToInfect =
        if (rand.Next(1, 100) < nameOfOS.Probability) then
             infectedNow <- true
             isJustInfected <- true
    member p.TakeOff =
        isJustInfected <- false

let mutable numberOfInfectedPC = 0

let adjacencyMatrix = [|for x in 0..2 -> [|for y in 0..2 -> false|]|]
adjacencyMatrix.[0].[1] <- true
adjacencyMatrix.[1].[0] <- true
adjacencyMatrix.[1].[2] <- true
adjacencyMatrix.[2].[1] <- true

let osList = [{nameOfOS = "win"; Probability = 70}; {nameOfOS = "lin"; Probability = 50}; {nameOfOS = "mac"; Probability = 40}]
let pcArr1 = [new PC (Infected = false, nameOfOS = osList.[1], IndexNumber = 0, IsJustInfected = false); 
               new PC (Infected = false, nameOfOS = osList.[0], IndexNumber = 1, IsJustInfected = false); 
                new PC (Infected = false, nameOfOS = osList.[2], IndexNumber = 2, IsJustInfected = false)]

/// Makes one move of infecting
let move (pcArr: PC List) =
    pcArr |> List.iter (fun x -> x.TakeOff)
    if (numberOfInfectedPC = 0) then
       pcArr |> List.iter (fun x -> x.TryToInfect)
    else
        let checkList = pcArr |> List.filter(fun x -> x.IsInfected)

        let rec makeMove (makingMoveList:PC List) =
            match makingMoveList with
            | h::t -> pcArr |> List.iter (fun x -> if (adjacencyMatrix.[h.IndexNumberIs].[x.IndexNumberIs]) && (not x.IsInfected) 
                                                   then x.TryToInfect)
                      makeMove t
            | [] -> printf(" ")

        makeMove checkList

/// Try to unfect all computers and print state of system
let infector (pcArr:PC List) =
    let v = pcArr |> List.fold (fun acc x -> if (x.IsInfected) then 
                                                 acc + 1 
                                             else acc) 0 
    numberOfInfectedPC <- numberOfInfectedPC + v
    printfn("number Of Infected PC %A") numberOfInfectedPC
    let rec insideInfector step checkNum =
        step |> printf ("Step %A ")
        numberOfInfectedPC |> printfn (" Number Of Infected PC %A")
        //printfn(" ")
        if (checkNum <= numberOfInfectedPC) then
            printf ("All PC are infected ")
        else
            move pcArr
            printf("index number of Infected on this step: ")
            pcArr |> List.filter(fun x -> x.IsJustInfected) |> List.iter(fun x -> x.IndexNumberIs |> printf "%A, ")
            let v = pcArr |> List.fold (fun acc x -> if (x.IsJustInfected) then 
                                                                acc + 1 
                                                             else acc) 0 
            numberOfInfectedPC <- numberOfInfectedPC + v
            printfn(" ")
            //pcArr |> List.iter (fun x -> x.TakeOff)
            insideInfector (step + 1) checkNum

    insideInfector 0 3

infector pcArr1
