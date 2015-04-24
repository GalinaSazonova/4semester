let isPalin (number:int) =
    let rec revList acPrevList =
       List.fold (fun acc elem -> elem::acc) [] acPrevList
    let directOrder = List.ofArray ((string (number.ToString())).ToCharArray())
    let backOrder = revList directOrder
    directOrder = backOrder


let findMax =
    let rec find number1 number2 max =
        let rec find2 number1 number2 max =
            if (number1 * number2 > max) && (isPalin (number1 * number2)) then
                number1 * number2
            elif number2 > 0 then
                find2 number1 (number2 - 1) max
            else max
        let t = find2 number1 number2 max
        if t > max then
            find (number1 - 1) number2 t
        elif number1 > 0 then
            find (number1 - 1) number2 max
        else 
            max
    find 999 999 -1
    //answer 906609
