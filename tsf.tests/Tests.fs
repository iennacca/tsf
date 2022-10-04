namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Entities

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
        let oi = ObsIndex.create 2000 FreqType.A 1
        let (Ok r) = oi
        Assert.AreEqual (r, r)

    [<TestMethod>]
    member this.CanCreateObsValues () = 
        let length = 5
        let r =  ObsIndex.create 2000 FreqType.A 1

        let robs = 
            r >>= ObsValues.create (TestUtilities.createRandomValues length)
        let (Ok obs) = robs
        let values = ObsValues.values obs
        Assert.AreEqual (Seq.length values, length)
