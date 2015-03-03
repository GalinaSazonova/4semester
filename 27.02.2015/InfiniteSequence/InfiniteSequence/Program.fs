let isPrime number =
    let rec check i =
        i > number / 2 || (number % i <> 0 && check (i + 1))
    check 2

(*let seqInfinite = Seq.initInfinite (fun index ->
    let n = 
        if (isPrime (index)) then (index)
    n
    *)

//Seq.iter (fun x -> printf "%d " x) seqInfinite
//Seq.iter2 (fun x y -> printfn " %d (№ %d)" x y) seqInfinite aSequence

let primes =
    let rec anotherPrime curIndex =
        seq { 
                if (isPrime curIndex) then 
                        yield curIndex
                yield! anotherPrime (curIndex + 1)
            }
    anotherPrime 2

printfn("Enter amount of primes you want to see")
let amount = int <| System.Console.ReadLine ()
let aSequence = seq { 1..amount }
Seq.iter2 (fun x y -> printfn " %d (№ %d)" x y) primes aSequence