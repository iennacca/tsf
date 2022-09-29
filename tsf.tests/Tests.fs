namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf


[<TestClass>]
type TestClass () =

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
    member this.CanCreateObsValues () = 
        let length = 5
        let ol = ObsValues.create length (fun f -> f + 1)
        printfn "%A" ol
        Assert.AreEqual (Seq.length ol, length) 
