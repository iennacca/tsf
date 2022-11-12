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

    [<TestMethod>]
    member this.CanConsolidateFromMonthly () =
        result {
            let length = 10
            let nConsValues = 3
            let tail = length - 1
            let! seqOIdx = TestUtilities.createIterator "2001M0" (TestUtilities.createRandomValues length) Q 
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
            let! seqOIdx = TestUtilities.createIterator "2001M04" (TestUtilities.createRandomValues length) Q 
            Assert.AreEqual (length + nConsValues, Seq.length seqOIdx) 

            let! ov' = ObservationIndex.FromString "2001M4"
            Assert.AreEqual (ov', Seq.head seqOIdx)

            let! ov'' = ObservationIndex.FromString "2002M1"
            Assert.AreEqual (ov'', Seq.item (tail + nConsValues) seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

        result {
            let values = {0..11} |> Seq.map float
            let nConsValues = 4
            let! seqOIdx = TestUtilities.createIterator "2001M08" values Q 
            
            Assert.AreEqual ((Seq.length values) + nConsValues, Seq.length seqOIdx) 
            let! ov' = ObservationIndex.FromString "2001M8"
            Assert.AreEqual (ov', Seq.head seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

        result {
            let length = 10
            let nConsValues = 3
            let! seqOIdx = TestUtilities.createIterator "2001M09" (TestUtilities.createRandomValues 10) Q 
            Assert.AreEqual (length + nConsValues, Seq.length seqOIdx) 

            let! ov' = ObservationIndex.FromString "2001M9"
            Assert.AreEqual (ov', Seq.head seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

    [<TestMethod>]
    member this.CanConsolidateFromQuarterly () =
        result {
            let length = 8
            let values = TestUtilities.createRandomValues length
            let! seqOIdx = TestUtilities.createIterator "2001Q02" values A 
            
            Assert.AreEqual ((Seq.length values) + 2, Seq.length seqOIdx)
            let! ov' = ObservationIndex.FromString "2001Q2" 
            Assert.AreEqual (ov', Seq.head seqOIdx)
        } |> TestUtilities.handleUnexpectedErrors

[<TestClass>]
type UtilityTests () =
    let getConsOIdxSeq oidx length d =
        let unfolder acc = 
            Some (acc, (ObservationIndex.increment acc 1))

        Ok ((Seq.unfold (fun acc -> Some (acc, (ObservationIndex.increment acc 1))) oidx) |> Seq.take length) 

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
            let! seqOIdx = TestUtilities.createIterator "2001M0" (TestUtilities.createRandomValues length) Q 

            seqOIdx |> Seq.iter (printfn "%A ")
            Assert.AreEqual (length + 3, Seq.length seqOIdx)              
        } |> TestUtilities.handleUnexpectedErrors
