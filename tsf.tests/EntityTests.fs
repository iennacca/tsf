namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Entities
open tsf.Utilities

[<TestClass>]
type EntityTests () =
    [<TestMethod>]
    member this.CanCreateAnObservationValue () =
        let testValue = 5.0
        let o =  ObservationValue.create testValue
        let f = ObservationValue.value o
        Assert.AreEqual (testValue, f)

    [<TestMethod>]
    member this.CanCreateObservationIndex () = 
        result {
            let! r = ObservationIndex.create "2000" "Q" "2"
            let! r' = ObservationIndex.create "2000" "Q" "02"
            Assert.AreEqual (r,r')
            Assert.AreEqual (Year.value r.Year, 2000 )

            let! r'' = ObservationIndex.FromString "2000Q02"
            Assert.AreEqual (r,r'')

        } |> TestUtilities.handleUnexpectedErrors

    [<TestMethod>]
    member this.CanGetObservationIndexErrors () = 
        let r = ObservationIndex.FromString "2000M13"
        Assert.AreEqual (TestUtilities.getExpectedErrors r, [InvalidObservationIndex])

        let r' = result {
            return! ObservationIndex.FromString "20000M00"
        } 
        Assert.AreEqual (TestUtilities.getExpectedErrors r', [InvalidObservationIndex])

    [<TestMethod>]
    member this.CanCreateObservationValues () = 
        let length = 5
        result {
            let! oi = ObservationIndex.FromString "2000A00"
            let ov = { OIdx = oi; Values = (TestUtilities.createRandomValues length) }
            let values = ov.Values
            Assert.AreEqual (Seq.length values, length)

            let l = Seq.toList values
            let l' = Seq.toList values
            Assert.AreEqual (List.length l, 5)
            Assert.AreEqual (List.length l', 5)
            Assert.AreNotEqual (Seq.head l, Seq.head l')
        } |> TestUtilities.handleUnexpectedErrors

    [<TestMethod>]
    member this.CanCreateAccumulatedErrors () =
        let r = result {
            let! oi = ObservationIndex.create "A" "B" "X"
            return { OIdx = oi; Values = (TestUtilities.createRandomValues 5) }
        } 
        let e = TestUtilities.getExpectedErrors r

        Assert.AreEqual (3, List.length e)
        Assert.IsTrue (List.contains InvalidType e)
        Assert.IsTrue (List.contains InvalidFormat e)

[<TestClass>]
type IteratorTests () =
    [<TestMethod>]
    member this.CanIterateOverYearlyBoundaries () =
        result {
            let length = 13
            let! oidx = ObservationIndex.FromString "2001M0"
            
            let seqOIdx = (Seq.unfold (fun acc -> Some (acc, (ObservationIndex.increment acc 1))) oidx) |> Seq.take length 
            Assert.AreEqual (oidx, Seq.head seqOIdx)
            let! oidx' = ObservationIndex.FromString "2002M0"
            Assert.AreEqual (oidx', Seq.item (length - 1) seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors


    [<TestMethod>]
    member this.CanIterateFromMonthly () =
        result {
            let length = 10
            let nConsValues = 3
            let tail = length - 1
            let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M0" (TestUtilities.createRandomValues length) 
            Assert.AreEqual (length + nConsValues, Seq.length seqOIdx) 

            let! ov' = ObservationIndex.FromString "2001M0"
            Assert.AreEqual (ov', Seq.head seqOIdx)

            let! ov'' = ObservationIndex.FromString "2001M9"
            Assert.AreEqual (ov'', Seq.item (tail + nConsValues) seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

        result {
            let length = 10
            let nConsValues = 3
            let tail = length - 1
            let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M04" (TestUtilities.createRandomValues length) 
            Assert.AreEqual (length + nConsValues, Seq.length seqOIdx) 

            let! ov' = ObservationIndex.FromString "2001M4"
            Assert.AreEqual (ov', Seq.head seqOIdx)

            let! ov'' = ObservationIndex.FromString "2002M1"
            Assert.AreEqual (ov'', Seq.item (tail + nConsValues) seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

        result {
            let values = {0..11} |> Seq.map float
            let nConsValues = 4
            let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M08" values

            Assert.AreEqual ((Seq.length values) + nConsValues, Seq.length seqOIdx) 
            let! ov' = ObservationIndex.FromString "2001M8"
            Assert.AreEqual (ov', Seq.head seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

        result {
            let length = 10
            let nConsValues = 3
            let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M09" (TestUtilities.createRandomValues 10) 
            Assert.AreEqual (length + nConsValues, Seq.length seqOIdx) 

            let! ov' = ObservationIndex.FromString "2001M9"
            Assert.AreEqual (ov', Seq.head seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

    [<TestMethod>]
    member this.CanIterateFromQuarterly () =
        result {
            let length = 8
            let values = TestUtilities.createRandomValues length
            let! seqOIdx = ConsolidatorUtilities.createIterator A "2001Q02" values 
            
            Assert.AreEqual ((Seq.length values) + 2, Seq.length seqOIdx)
            let! ov' = ObservationIndex.FromString "2001Q2" 
            Assert.AreEqual (ov', Seq.head seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

[<TestClass>]
type ConsolidatorTests () =
    [<TestMethod>]
    member this.CanConsolidateFromMonthly () =
        result {
            let consAdd l = 
                Seq.fold (+) 0.0 l

            let length = 24
            let! seqOIdx = ConsolidatorUtilities.createConsolidator Q consAdd "2001M0" [1..length] 

            let! oi = ObservationIndex.FromString "2001Q0"
            let v = (6.0, oi)
            Assert.AreEqual (v, Seq.head seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

[<TestClass>]
type UtilityTests () =
    [<TestMethod>]
    member this.CanCreateResultFromWrappedException () = 
        result { 
            let! i = ToResult int "1"
            Assert.AreEqual (1, i)

            let! i' = "2" |> ToResult int
            Assert.AreEqual (2, i')
        } |> TestUtilities.handleUnexpectedErrors

    [<TestMethod>]
    member this.CanPrintObservationIndices () =
        result {
            let length = 10
            let tail = length - 1
            let! seqOIdx = ConsolidatorUtilities.createIterator Q "2001M0" (TestUtilities.createRandomValues length)

            seqOIdx |> Seq.iter (printfn "%A ")
            Assert.AreEqual (length + 3, Seq.length seqOIdx)              
        } |> TestUtilities.handleUnexpectedErrors
