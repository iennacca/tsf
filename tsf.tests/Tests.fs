namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Entities
open tsf.Utilities

[<TestClass>]
type TestClass () =
    let (>>=) m f = Result.bind f m

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue (true)

    [<TestMethod>]
    member this.CanCreateAnObservationValue () =
        let testValue = 5.0
        let o =  ObservationValue.create testValue
        let f = ObservationValue.value o
        Assert.AreEqual (testValue, f)

    [<TestMethod>]
    member this.CanCreateObservationIndex () = 
        result {
            let! r = ObservationIndex.create 2000 FrequencyType.Q 2
            let! r' = ObservationIndex.create 2000 FrequencyType.Q 2
            Assert.AreEqual (r,r')
            Assert.AreEqual (Year.value r.Year, 2000 )

            let! r'' = ObservationIndex.convert "2000Q02"
            Assert.AreEqual (r,r'')

        } |> TestUtilities.handleUnexpectedErrors

    [<TestMethod>]
    member this.CanGetObservationIndexErrors () = 
        let r = ObservationIndex.create 2000 FrequencyType.M 13
        Assert.AreEqual (TestUtilities.getExpectedErrors r, [InvalidObservationIndex])

        let r' = result {
            return! ObservationIndex.convert "20000M00"
        } 
        Assert.AreEqual (TestUtilities.getExpectedErrors r', [InvalidObservationIndex])

    [<TestMethod>]
    member this.CanCreateObservationValues () = 
        let length = 5
        result {
            let! idx = ObservationIndex.create 2000 A 1

            let! r' = ObservationValues.create idx (TestUtilities.createRandomValues length) 
            let values = r'.Values
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
            let y = -1
            let! i' = ObservationIndex.create y FrequencyType.A 5
            return! ObservationValues.create i' (TestUtilities.createRandomValues 5)     
        } 
        let e = TestUtilities.getExpectedErrors r

        Assert.AreEqual (List.length e, 2)
        Assert.AreEqual (List.item 0 e, InvalidYear)
        Assert.AreEqual (List.item 1 e, InvalidObservationIndex)

    [<TestMethod>]
    member this.CanConsolidateAnObservationValueSequence () =
        result {
            let! oi =  ObservationIndex.convert "2001M01"
            let v = TestUtilities.createRandomValues 10
            let! ov = ObservationValues.create oi v
            let! ov' = ObservationValues.consolidate Q Sum ov
            Assert.AreEqual (ov', ov') 
        } |> TestUtilities.handleUnexpectedErrors
