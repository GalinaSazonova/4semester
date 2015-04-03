
type QueuePrior (Element:int, Priority1:int) =
    let queue:List<QueuePrior> = []
    member el.Key = Element
    member el.Priority = Priority1
    member el.Add =
        if (queue |> List.fold(fun acc x -> if (x.Priority < el.Priority) then acc + 1 else acc ) 0 > 0) && (queue.Head.Priority < el.Priority)then
            QueuePrior(Element = el.Key, Priority1 = el.Priority)::queue
        else
            queue |> List.append([QueuePrior(Element = el.Key, Priority1 = el.Priority)])
    member el.Get =
        //if (queue.IsEmpty) then
           
