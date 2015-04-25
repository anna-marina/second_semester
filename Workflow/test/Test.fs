module Test

open NUnit.Framework
open System
open Workflow

let ring = Workflow.ring
let Node = Workflow.Node
let Nil = Workflow.Nil

[<TestFixture>]
type Test() =

    [<Test>]
    member t.``Some arithmetic`` () =
        let r = ring 35 {
            let! a = 2 + 7
            let! b = a * 4
            return b
        }
        Assert.AreEqual(1, r)
    
    [<Test>]
    member t.``Subtraction`` () =
        let r = ring 5 {
            let! a = 2 + 6
            let! b = a - 4
            return b
        }
        Assert.AreEqual(4, r)
    
    [<Test>]
    member t.``Single return`` () =
        let r = ring 5 {
            return 7
        }
        Assert.AreEqual(2, r)
