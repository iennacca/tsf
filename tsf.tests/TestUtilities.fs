namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting


module TestUtilities =
    let createRandomValues count = 
        let r = System.Random()
        Seq.init count (fun f -> r.NextDouble())
