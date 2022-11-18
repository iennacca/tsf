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
    let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M04" (TestUtilities.createRandomValues length) 
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
    let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M0" (TestUtilities.createRandomValues length)

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors

result {
    let consAdd l = 
        Seq.fold (+) 0.0 l

    let length = 24
    let values = {0 .. length - 1} |> Seq.map float
    let! seqOIdx = ConsolidatorUtilities.createConsolidator Q consAdd "2001M0" values

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors

result {
    let length = 2
    let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M11" (TestUtilities.createRandomValues length)

    seqOIdx |> Seq.iter (printfn "%A ")
} |> TestUtilities.handleUnexpectedErrors

result {
    let length = 24
    let! oidx = ObservationIndex.FromString "2001Q3"
    
    let s = (Seq.unfold (fun acc -> Some (acc, (ObservationIndex.increment acc 1))) oidx) |> Seq.take length 
    s |> Seq.iter (printfn "%A ")
    ()
} |> TestUtilities.handleUnexpectedErrors

result {
    let consAdd (l:float seq) = 
        Seq.fold (+) 0.0 l

    let length = 24
    let values = {0..length - 1} |> Seq.map float
    let! seqOIdx = ConsolidatorUtilities.createConsolidator Q consAdd "2001M0" values

    let! oi = ObservationIndex.FromString "2001Q0"
    let v = (3.0, oi)
    Assert.AreEqual (v, Seq.head seqOIdx)

    seqOIdx |> Seq.iter (printfn "%A ")
    seqOIdx |> Seq.map (fun (x,y) -> x ) |> Seq.iter (printfn "%A ")

    let expected = seq { 3.0; 12.0; 21.0; 30.0; 39.0; 48.0; 57.0; 66.0 }
    let actual = seqOIdx |> Seq.map (fun (x,y) -> x)
    let r = Seq.fold2 (fun acc x y ->  (x = y) && acc) true expected actual
    Assert.AreEqual (true,r)

} |> TestUtilities.handleUnexpectedErrors
