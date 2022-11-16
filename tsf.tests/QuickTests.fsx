#r "netstandard"

#r "nuget: Microsoft.TestPlatform"
#r "nuget: Microsoft.CodeCoverage"
#r "nuget: MSTest.TestFramework"

#load "..\\tsf\\Utilities.fs"
#load "..\\tsf\\Entities.fs"
#load "..\\tsf.tests\\TestUtilities.fs"

open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Utilities
open tsf.Entities
open tsf.tests

result {
    let length = 10
    let nConsValues = 3
    let tail = length - 1
    let! seqOIdx = TestUtilities.createIterator Q "2001M04" (TestUtilities.createRandomValues length) 
    Assert.AreEqual (length + nConsValues, Seq.length seqOIdx) 

    seqOIdx |> Seq.iter (printfn "%A ")
    Seq.head seqOIdx |> printfn "head: %A"
    
    let! ov' = ObservationIndex.FromString "2001M4"
    Assert.AreEqual (ov', Seq.head seqOIdx)

    let! ov' = ObservationIndex.FromString "2002M1"
    Assert.AreEqual (ov', Seq.item (tail + nConsValues) seqOIdx)
} |> TestUtilities.handleUnexpectedErrors

result {
    let length = 24
    let! seqOIdx = TestUtilities.createIterator Q "2001M0" (TestUtilities.createRandomValues length)

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors

result {
    let consAdd l = 
        Seq.fold (+) 0.0 l

    let length = 24
    let! seqOIdx = TestUtilities.createConsolidator Q consAdd "2001M0" [1..length] 

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors

result {
    let length = 2
    let! seqOIdx = TestUtilities.createIterator Q "2001M11" (TestUtilities.createRandomValues length)

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors
