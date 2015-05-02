module Test

open NUnit.Framework
open System
open Workflow

let ring = Workflow.ring

[<TestFixture>]
type Test() =

    [<Test>]
    member t.``Test ring`` () =
        let r = ring 35 {
            let! a = 2 + 7
            let! b = a * 4
            return b
        }
        Assert.AreEqual(1, r)
