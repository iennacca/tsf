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
        let r = ObservationIndex.convert "2000A01"
        let (Ok oi) = r
        Assert.AreEqual (oi, oi)

        let r' = ObservationIndex.create 2000 FrequencyType.M 13
        let (Error e) = r'
        Assert.AreEqual (e, [InvalidObservationIndexError])

    [<TestMethod>]
    member this.CanCreateObservationValues () = 
        let length = 5
        let r = ObservationIndex.create 2000 FrequencyType.A 1
        let (Ok idx) = r

        let r' = ObservationValues.create idx (TestUtilities.createRandomValues length) 
        let (Ok obs) = r'
        let values = ObservationValues.values obs
        Assert.AreEqual (Seq.length values, length)

        let l = Seq.toList values
        let l' = Seq.toList values
        Assert.AreEqual (List.length l, 5)
        Assert.AreEqual (List.length l', 5)
        Assert.AreNotEqual (Seq.head l, Seq.head l')

    [<TestMethod>]
    member this.CanCreateObservationIndexSequences () =
        let s = FrequencyIndex.seqInfinite Q 0
        printfn "%A" s 

    [<TestMethod>]
    member this.CanCreateAccumulatedErrors () =
        let r = result {
            let y = -1
            let! i' = ObservationIndex.create y FrequencyType.A 5
            return! ObservationValues.create i' (TestUtilities.createRandomValues 5)     
        }
        let (Error r') = r

        Assert.AreEqual (List.length r' , 2)
        Assert.AreEqual (List.item 0 r', InvalidYearError)
        Assert.AreEqual (List.item 1 r', InvalidObservationIndexError)
        
    [<TestMethod>]
    member this.CanCreateFrequencyIterator () = 
        let r = result {
            return! ObservationIndex.convert "2000M1"
        }
        let (Ok r') = r

        let r = result {
            return! ObservationIndex.convert "2000D200"
        }
        let (Ok r') = r

        let r = result {
            return! ObservationIndex.convert "20000M00"
        }
        let (Error r') = r

        Assert.AreEqual (r',r')