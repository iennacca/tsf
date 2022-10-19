namespace tsf.tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting


module TestUtilities =
    let createRandomValues count = 
        let r = System.Random()
        Seq.init count (fun f -> r.NextDouble())

    let handleUnexpectedErrors r =
        match r with
            |Ok _ -> ()
            |Error _ -> Assert.Fail "Unexpected error encountered." 

    let getExpectedErrors r =
        match r with
            |Ok _ -> []
            |Error e -> e 
