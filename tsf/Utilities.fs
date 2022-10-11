namespace tsf

module Utilities =
    type ResultBuilder () =
        member this.Bind (m, f) =
            match m with
            | Error e ->
                printfn "Error: %A" e
            | Ok a ->
                printfn "Binding with Some(%A). Continuing" a
            Result.bind f m
        member this.Return (x) = 
            Ok (x, [])
        member this.Zero() = this.Return ()
        member this.MergeSources(result1, result2) =
                match result1, result2 with
                    | Ok ok1, Ok ok2 -> Ok (ok1, ok2)   // compiler will automatically de-tuple these - very cool!
                    | Error errs1, Ok _ -> Error errs1
                    | Ok _, Error errs2 -> Error errs2
                    | Error errs1, Error errs2 -> Error (errs1 @ errs2)   // accumulate e
        member this.ReturnFrom(x) = x

    let result = new ResultBuilder ()
