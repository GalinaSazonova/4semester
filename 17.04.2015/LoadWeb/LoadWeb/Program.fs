open System.Net
open System.IO
open Microsoft.FSharp.Control.WebExtensions
open System.Threading
open System.Text.RegularExpressions

let lookingForPages(url : string) =
    let fetchAsync(url : string) =
        async {
            let req = WebRequest.Create(url)
            let! resp = req.AsyncGetResponse()
            let stream = resp.GetResponseStream()
            let reader = new StreamReader(stream)
            let html = reader.ReadToEndAsync()
            let! htmlTask = Async.AwaitTask(html)
            do printfn "%s --- %d" url htmlTask.Length
        }

    let read(url: string) =
        let req = WebRequest.Create(url)
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html

    let expression = new System.Text.RegularExpressions.Regex("<a.*href=\"http.*\">")
    let pages = expression.Matches(read(url))
    let working = [for url in pages -> 
                     let value = url.Value
                     fetchAsync(value.Substring(value.IndexOf("=\"") + 2 , value.IndexOf("\">") - value.IndexOf("=\"") - 2))
                     ]    
    Async.Parallel working |> Async.RunSynchronously |> ignore

lookingForPages("http://se.math.spbu.ru/SE/Members/ylitvinov/13-44/resultsSpring2015_244_Yurii")
