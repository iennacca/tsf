#r "netstandard"

#r "nuget: Microsoft.TestPlatform"
#r "nuget: Microsoft.CodeCoverage"
#r "nuget: MSTest.TestFramework"

#load "..\\tsf\\Utilities.fs"
#load "..\\tsf\\Entities.fs"
#load "..\\tsf.tests\\TestUtilities.fs"

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open tsf.Utilities
open tsf.Entities
open tsf.tests

result {
    let length = 10
    let! seqOIdx = TestUtilities.createIterator "2001M0" (TestUtilities.createRandomValues length) Q 

    seqOIdx |> Seq.iter (printfn "%A ")
    // Assert.AreEqual (length, Seq.length seqOIdx)              
} |> TestUtilities.handleUnexpectedErrors
