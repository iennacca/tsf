namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Utilities
open tsf.Entities

module TestUtilities =
    let createRandomValues count = 
        let r = System.Random()
        Seq.init count (fun f -> r.NextDouble())

    let handleUnexpectedErrors r =
        match r with
            |Ok _ -> ()
            |Error _ -> Assert.Fail "Unexpected error encountered." 

    let getExpectedErrors r =
        match r with
            |Ok _ -> []
            |Error e -> e 

    let (>>=) m f = Result.bind f m

    let private createOV strOI oValues = 
        let length = Seq.length oValues
        result {
            let! oi =  ObservationIndex.FromString strOI
            let v = createRandomValues length
            return! Ok { OIdx = oi; Values = v }
        }

    let createIterator strOI oValues consFreq = 
        result {
            let! ov = createOV strOI oValues
            return! ObservationValueConsolidator.iterate consFreq ov
        }

    let createConsolidator strOI oValues consFreq = 
        result {
            let! ov = createOV strOI oValues
            return! ObservationValueConsolidator.consolidate consFreq ov
        }
