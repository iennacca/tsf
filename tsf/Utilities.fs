namespace tsf

module Utilities =
    type ResultBuilder () =
        member this.Errors = List.empty
        member this.Bind (m, f) =
            match m with
            | Error e ->
                printfn "Error: %A" e
            | Ok a ->
                printfn "Binding with Some(%A). Continuing" a
            Result.bind f m
        member this.Return (x) = 
            Ok x
        member this.Zero() = Ok 0

    let result = new ResultBuilder ()
