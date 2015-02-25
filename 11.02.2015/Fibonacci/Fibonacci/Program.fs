let placeNum = 4
let rec fibonacci n =
    match n with
    | 1 | 2 -> 1
    | n -> fibonacci(n - 1) + fibonacci(n - 2)
System.Console.WriteLine(fibonacci placeNum)

