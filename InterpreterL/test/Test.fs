module Test

open NUnit.Framework
open System
open A

[<TestFixture>]
type Test() =
    
    [<TestCase("write 1","",Result="1")>]
    [<TestCase("write a","",Result="0")>]
    [<TestCase("; write 1 write 2","",Result="1 2")>]
    [<TestCase("; read a write a","5",Result="5")>]
    [<TestCase("; read a while a ; write a := a - a 1","5",
               Result="5 4 3 2 1")>]
    member t.``Test interpreter`` (x:string) (i:string) =
        let prog = x.Split([|' '|]) |> List.ofArray
        let input = A.MutableList(i.Split([|' '|]) |> List.ofArray)
        let output = A.MutableList([])
        let tree = A.ListParser(prog).parseStmt()
        let intr = A.Interpreter(tree)
        let read = input.Pop >> Int32.Parse
        let write = (fun x -> x.ToString()) >> output.Push
        intr.setRW(read, write)
        intr.run()
        let res : string array = output.List |> List.rev |> Array.ofList
        String.Join(" ", res)
