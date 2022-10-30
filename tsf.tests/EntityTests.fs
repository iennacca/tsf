namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Entities
open tsf.Utilities

[<TestClass>]
type EntityTests () =
    let (>>=) m f = Result.bind f m

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
            let! oi = ObservationIndex.FromString "2000A01"
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
    member this.CanConsolidateAnObservationValueSequence () =
        let r = result {
            let! oi =  ObservationIndex.FromString "2001M04"
            let v = TestUtilities.createRandomValues 10
            let ov = { OIdx = oi; Values = v }
            let! ov' = ObservationValues.test Q ov
            ov' |> Seq.take 36 |> Seq.iter (printf "%A ")
            Assert.AreEqual (ov', ov') 
        }

        let e = TestUtilities.getExpectedErrors r
        Assert.AreEqual (1, List.length e)

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

