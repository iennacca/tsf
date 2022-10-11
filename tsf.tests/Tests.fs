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
    member this.CanCreateAnObsValue () =
        let testValue = 5.0
        let o =  ObsValue.create testValue
        let f = ObsValue.value o
        Assert.AreEqual (testValue, f)

    [<TestMethod>]
    member this.CanCreateObsIndex () = 
        let r = ObsIndex.create 2000 FreqType.A 1
        let (Ok oi) = r
        Assert.AreEqual (oi, oi)

        let r' = ObsIndex.create 2000 FreqType.M 13
        let (Error e) = r'
        Assert.AreEqual (e, [InvalidObsIndexError])

    [<TestMethod>]
    member this.CanCreateObsValues () = 
        let length = 5
        let r = ObsIndex.create 2000 FreqType.A 1
        let (Ok idx) = r

        let r' = ObsValues.create (TestUtilities.createRandomValues length) idx 
        let (Ok obs) = r'
        let values = ObsValues.values obs
        Assert.AreEqual (Seq.length values, length)

        let l = Seq.toList values
        let l' = Seq.toList values
        Assert.AreEqual (List.length l, 5)
        Assert.AreEqual (List.length l', 5)
        Assert.AreNotEqual (Seq.head l, Seq.head l')

    [<TestMethod>]
    member this.CanCreateObsIndexSequences () =
        let s = FreqIndex.seqInfinite Q 0
        printfn "%A" s 

    [<TestMethod>]
    member this.CanCreateAccumulatedErrors () =
        let r = result {
            let y = -1
            let! i' = ObsIndex.create y FreqType.A 5
            return! ObsValues.create (TestUtilities.createRandomValues 5) i'     
        }
        let (Error r') = r
        
        Assert.AreEqual (List.length r' , 2)
        Assert.AreEqual (List.item 0 r', InvalidYearError)
        Assert.AreEqual (List.item 1 r', InvalidObsIndexError)