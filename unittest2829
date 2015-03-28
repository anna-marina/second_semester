open NUnit.Framework

(*27*)
type IList<'T> =
    abstract Prepend : 'T -> unit
    abstract Append : 'T -> unit
    abstract Insert : int -> 'T -> unit
    abstract RemoveFirst : unit
    abstract RemoveLast : unit
    abstract Remove : int -> unit
    abstract Find : ('T -> bool) -> 'T option
    abstract Concat : 'T IList -> unit
    abstract Size : int
    abstract Get : int -> 'T

type ConsList<'T> = Nil | Cons of 'T * ConsList<'T>

(*28*)
type AdtList<'T>() =
    let mutable c = Nil
    let mutable size = 0

    interface IList<'T> with
        member s.Size =
            size

        member s.Prepend x =
            c <- Cons (x, c)
            size <- size + 1
        
        member s.Append x =
            let rec append l =
                match l with
                | Nil -> Cons (x, Nil)
                | Cons (v, l) -> Cons (v, append l)
            c <- append c
            size <- size + 1

        member s.Insert i x =
            let rec insert j l =
                match j, l with
                | j, Cons (v, l) when j < i -> Cons (v, insert (j + 1) l)
                | j, l -> Cons (x, l)
            c <- insert 0 c
            size <- size + 1

        member s.RemoveFirst =
            c <-
                match c with
                | Nil -> Nil
                | Cons (v, l) -> size <- size - 1; l

        member s.RemoveLast =
            let rec removeLast l =
                match l with
                | Nil -> Nil
                | Cons (v, Nil) -> size <- size - 1; Nil
                | Cons (v, l) ->  Cons (v, removeLast l)
            c <- removeLast c

        member s.Remove i =
            let rec remove j l =
                match j, l with
                | _, Nil -> Nil
                | j, Cons (v, l) when j < i -> Cons (v, remove (j + 1) l)
                | j, Cons (v, l) -> size <- size - 1; l
            c <- remove 0 c

        member s.Find p =
            let rec find l =
                match l with
                | Nil -> None
                | Cons (v, l) when (p v) -> Some v
                | Cons (_, l) -> find l
            find c

        member s.Concat b =
            let rec concat j l =
                match l with
                | Cons (v, l) -> Cons (v, concat 0 l)
                | Nil when j < b.Size -> Cons (b.Get j, concat (j + 1) l)
                | _ -> Nil
            size <- size + b.Size
            c <- concat 0 c

        member s.Get i =
            let rec get j l =
                match j, l with
                | _, Nil -> failwith "Index out of bounds"
                | j, Cons (v, _) when i = j -> v
                | j, Cons (_, l) -> get (j + 1) l
            get 0 c       

(*29*)
type ArrayList<'T>() =
    let mutable arr = [||]
    let mutable size = 0

    interface IList<'T> with
        member s.Size =
            size

        member s.Prepend x =
            arr <- Array.append [|x|] arr
            size <- size + 1
        
        member s.Append x =
            arr <- Array.append arr [|x|]
            size <- size + 1

        member s.Insert i x =
            arr <- Array.concat [Array.sub arr 0 i; [|x|];
                                 Array.sub arr i (size - i)]
            size <- size + 1

        member s.RemoveFirst =
            size <- size - 1
            arr <- Array.sub arr 1 size

        member s.RemoveLast =
            size <- size - 1
            arr <- Array.sub arr 0 size

        member s.Remove i =
            size <- size - 1
            arr <- Array.append (Array.sub arr 0 i)
                                (Array.sub arr (i + 1) (size - i))

        member s.Find p =
            Array.foldBack (fun i s -> if (p i) then (Some i) else s) arr None

        member s.Concat b =
            let bx = Array.init b.Size b.Get
            arr <- Array.append arr bx
            size <- size + b.Size

        member s.Get i =
            arr.[i]

let toList(l:IList<'T>) =
    List.rev (List.fold (fun s i -> (l.Get i) :: s) [] [0..l.Size-1])

let makeAdtList (l:list<'T>) =
    let a = AdtList () :> IList<'T>
    List.iter (fun x -> a.Append x) l
    a

let makeArrayList(l:list<'T>) =
    let a = ArrayList () :> IList<'T>
    List.iter (fun x -> a.Append x) l
    a       

[<TestFixture>]
type ``ArrayList tests``() =

    [<Test>]
    member s.``Array: Prepend`` () =
        let a = makeArrayList [1;2;3]
        a.Prepend 4
        Assert.AreEqual(toList a, [4;1;2;3])

    [<Test>]
    member s.``Array: Append`` () =
        let a = makeArrayList [1;2;3]
        a.Append 4
        Assert.AreEqual(toList a, [1;2;3;4])
    
    [<Test>]
    member s.``Array: Insertion`` () =
        let a = makeArrayList [1;2;3]
        a.Insert 1 4
        Assert.AreEqual(toList a, [1;4;2;3])
        let a = makeArrayList [1;2;3]
        a.Insert 0 4
        Assert.AreEqual(toList a, [4;1;2;3])
        let a = makeArrayList [1;2;3]
        a.Insert 3 4
        Assert.AreEqual(toList a, [1;2;3;4])

    [<Test>]
    member s.``Array: Remove First`` () =
        let a = makeArrayList [1;2;3]
        a.RemoveFirst
        Assert.AreEqual(toList a, [2;3])

    [<Test>]
    member s.``Array: Remove Last`` () =
        let a = makeArrayList [1;2;3]
        a.RemoveLast
        Assert.AreEqual(toList a, [1;2])

    [<Test>]
    member s.``Array: Remove`` () =
        let a = makeArrayList [1;2;3]
        a.Remove 0
        Assert.AreEqual(toList a, [2;3])
        let a = makeArrayList [1;2;3]
        a.Remove 1
        Assert.AreEqual(toList a, [1;3])
        let a = makeArrayList [1;2;3]
        a.Remove 2
        Assert.AreEqual(toList a, [1;2])

    [<Test>]
    member s.``Array: Find`` () =
        let a = makeArrayList [1;2;3]
        let r = a.Find (fun x -> x = 1)
        Assert.AreEqual(r, Some 1)
        let r = a.Find (fun x -> x % 2 = 0)
        Assert.AreEqual(r, Some 2)
        let r = a.Find (fun x -> x > 2)
        Assert.AreEqual(r, Some 3)
        let r = a.Find (fun x -> x <> x)
        Assert.AreEqual(r, None)

    [<Test>]
    member s.``Array: Concat`` () =
        let a = makeArrayList [1;2]
        let b = makeArrayList [3;4]
        a.Concat b
        Assert.AreEqual(toList a, [1;2;3;4])
        Assert.AreEqual(toList b, [3;4])

[<TestFixture>]
type ``AdtList tests``() =

    [<Test>]
    member s.``ADT: Prepend`` () =
        let a = makeAdtList [1;2;3]
        a.Prepend 4
        Assert.AreEqual(toList a, [4;1;2;3])

    [<Test>]
    member s.``ADT: Append`` () =
        let a = makeAdtList [1;2;3]
        a.Append 4
        Assert.AreEqual(toList a, [1;2;3;4])
    
    [<Test>]
    member s.``ADT: Insertion`` () =
        let a = makeAdtList [1;2;3]
        a.Insert 1 4
        Assert.AreEqual(toList a, [1;4;2;3])
        let a = makeAdtList [1;2;3]
        a.Insert 0 4
        Assert.AreEqual(toList a, [4;1;2;3])
        let a = makeAdtList [1;2;3]
        a.Insert 3 4
        Assert.AreEqual(toList a, [1;2;3;4])

    [<Test>]
    member s.``ADT: Remove First`` () =
        let a = makeAdtList [1;2;3]
        a.RemoveFirst
        Assert.AreEqual(toList a, [2;3])

    [<Test>]
    member s.``ADT: Remove Last`` () =
        let a = makeAdtList [1;2;3]
        a.RemoveLast
        Assert.AreEqual(toList a, [1;2])

    [<Test>]
    member s.``ADT: Remove`` () =
        let a = makeAdtList [1;2;3]
        a.Remove 0
        Assert.AreEqual(toList a, [2;3])
        let a = makeAdtList [1;2;3]
        a.Remove 1
        Assert.AreEqual(toList a, [1;3])
        let a = makeAdtList [1;2;3]
        a.Remove 2
        Assert.AreEqual(toList a, [1;2])
    
    [<Test>]
    member s.``ADT: Find`` () =
        let a = makeAdtList [1;2;3]
        let r = a.Find (fun x -> x = 1)
        Assert.AreEqual(r, Some 1)
        let r = a.Find (fun x -> x % 2 = 0)
        Assert.AreEqual(r, Some 2)
        let r = a.Find (fun x -> x > 2)
        Assert.AreEqual(r, Some 3)
        let r = a.Find (fun x -> x <> x)
        Assert.AreEqual(r, None)

    [<Test>]
    member s.``ADT: Concat`` () =
        let a = makeAdtList [1;2]
        let b = makeAdtList [3;4]
        a.Concat b
        Assert.AreEqual(toList a, [1;2;3;4])
        Assert.AreEqual(toList b, [3;4])

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0
