let rec fibonacci n =
    match n with
    | 1 | 2 -> 1
    | n -> fibonacci(n - 1) + fibonacci(n - 2)

let isEven n =
    n % 2 = 0

let evenFibSum =
    let rec evenFib curIndex =
            seq {
                let k = fibonacci curIndex
                if (isEven k) && (k < 1000000) then
                    yield k
                if (k < 1000000) then 
                    yield! evenFib (curIndex + 1)       
            }
    evenFib 1 |> Seq.sum
   
evenFibSum |> printfn("%A")