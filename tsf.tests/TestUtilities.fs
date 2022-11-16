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
        result {
            let! oi =  ObservationIndex.FromString strOI
            return! Ok { OIdx = oi; Values = oValues }
        }

    let createIterator consFreq strOI oValues = 
        result {
            let! ov = createOV strOI oValues
            return! ObservationValueConsolidator.iterate consFreq ov
        }

    let createConsolidator consFreq consMethod strOI oValues = 
        result {
            let! ov = createOV strOI oValues
            return! ObservationValueConsolidator.consolidate consFreq consMethod ov
        }
