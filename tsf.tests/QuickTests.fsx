#r "netstandard"

#r "nuget: Microsoft.TestPlatform"
#r "nuget: Microsoft.CodeCoverage"
#r "nuget: MSTest.TestFramework"

#load "..\\tsf\\Utilities.fs"
#load "..\\tsf\\Entities.fs"
#load "..\\tsf.tests\\TestUtilities.fs"

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Utilities
open tsf.Entities
open tsf.tests

result {
    let length = 10
    let nConsValues = 3
    let tail = length - 1
    let! seqOIdx = TestUtilities.createIterator "2001M04" (TestUtilities.createRandomValues length) Q 
    Assert.AreEqual (length + nConsValues, Seq.length seqOIdx) 

    seqOIdx |> Seq.iter (printfn "%A ")
    Seq.head seqOIdx |> printfn "head: %A"
    
    let! ov' = ObservationIndex.FromString "2001M4"
    Assert.AreEqual (ov', Seq.head seqOIdx)

    let! ov' = ObservationIndex.FromString "2002M1"
    Assert.AreEqual (ov', Seq.item (tail + nConsValues) seqOIdx)
} |> TestUtilities.handleUnexpectedErrors

result {
    let length = 10
    let! seqOIdx = TestUtilities.createIterator "2001M0" (TestUtilities.createRandomValues length) Q 

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors

result {
    let length = 10
    let! seqOIdx = TestUtilities.createConsolidator "2001M0" [1..length] Q 

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors
