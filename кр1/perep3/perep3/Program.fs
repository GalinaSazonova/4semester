type Stack() =
    let mutable list = []

    member v.Push el = 
        list <- el::list
    
    member v.IsEmpty = 
        list =[]

    member v.Pop =
        match list with
        |[] -> failwith ("Stack is empty")
        |_ ->
            let tmp = list.Head
            list <- list.Tail
            tmp