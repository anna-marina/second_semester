open NUnit.Framework
(*20*)
type IGraph =
    abstract AddEdge : int -> int -> unit
    abstract RemoveEdge : int -> int -> unit
    abstract HasEdge : int -> int -> bool
    abstract OutByEdge : int -> int list
    abstract Size : int

(*21*)
type MatrixGraph(n : int) =
    let size = n
    let mutable matrix = Array2D.zeroCreate<bool> n n

    interface IGraph with
        member g.AddEdge s t =
            matrix.[s, t] <- true
        member g.RemoveEdge s t =
            matrix.[s, t] <- false
        member g.HasEdge s t =
            matrix.[s, t]
        member g.OutByEdge v =
            List.fold (fun s i -> 
                        if matrix.[v, i] then i :: s else s)
                        [] [0 .. size-1]
        member g.Size =
            size

(*22*)
type ListGraph(n : int) =
    let size = n
    let mutable l = Array.create<int list> n []

    interface IGraph with
        member g.AddEdge s t =
            l.[s] <- t :: l.[s]
        member g.RemoveEdge s t =
            l.[s] <- List.filter (fun x -> x <> t) l.[s]
        member g.HasEdge s t =
            List.exists (fun x -> x = t) l.[s]
        member g.OutByEdge v =
            l.[v]
        member g.Size =
            size

let isVertice v x = 
    v=x

(*23*)
let fromVertice (g : IGraph) v =
    let byEdgeFromSet r =
        List.concat (List.map g.OutByEdge r)
    let exclude exclusionPredicate list =
        List.filter (fun u -> not (exclusionPredicate u)) list
    let excludeAll exclusionList list =
        exclude (fun u -> List.exists (isVertice u) exclusionList) list
    let rec fromVerticeSet r =
        let p = excludeAll r (byEdgeFromSet r)
        if p = [] then r else fromVerticeSet (p @ r)
    fromVerticeSet [v]

(*24*)
let toVertice (g : IGraph) v =
    let destReachableFrom x =
        List.exists (isVertice v) (fromVertice g x)
    let accumulateVertice acc x =
        if destReachableFrom x then x :: acc else acc
    List.fold accumulateVertice [] [0 .. g.Size-1]

(*25*)
type IMarkedGraph<'T> =
    inherit IGraph
    abstract SetMark : int -> 'T -> unit
    abstract GetMark : int -> 'T

(*26*)
type MarkedGraph<'T>(graph : IGraph) =
    let mutable g = graph
    let mutable mx = Array.zeroCreate<'T> g.Size

    interface IMarkedGraph<'T> with
        member p.AddEdge s t =
            g.AddEdge s t
        member p.RemoveEdge s t =
            g.RemoveEdge s t
        member p.HasEdge s t =
            g.HasEdge s t
        member p.OutByEdge v =
            g.OutByEdge v
        member p.Size =
            g.Size
        member p.SetMark v m =
            mx.[v] <- m
        member p.GetMark v =
            mx.[v]   

let randomNetwork sz =  
    let getName k =
        match k with
        | 0 -> "Windows"
        | 1 -> "MacOS"
        | 2 -> "Linux"
        | _ -> ""
    let g = (MarkedGraph<string*bool>(ListGraph(sz)) :> IMarkedGraph<string*bool>)
    let r = System.Random()
    let k = r.Next(g.Size * (g.Size - 1) / 2)
    List.iter (fun i -> g.AddEdge i (i + 1); g.AddEdge (i + 1) i) [0 .. sz - 2]
    let e = List.init k (fun i -> (r.Next(sz), r.Next(sz)))
    List.iter (fun (s,t) -> if not (g.HasEdge s t) then g.AddEdge s t; g.AddEdge t s) e
    let m = List.init sz (fun i -> (i, getName (r.Next(3))))
    List.iter (fun (v,m) -> g.SetMark v (m, false)) m
    g

let simulateNet (g : IMarkedGraph<string * bool>) =
    let r = System.Random()
    let prob s =
        match s with
        | "Windows" -> 0.9
        | "MacOS" -> 0.4
        | "Linux" -> 0.01
        | _ -> 0.0
    let markName m =
        match m with
        | true -> "Infected"
        | false -> "OK"
    let printNet () =
        List.iter (fun i -> let (os, mark) = (g.GetMark i); 
                            printfn "Computer %A with %A is %A" i os (markName mark)) [0 .. g.Size-1]
    let someOfInfected l =
        List.fold (fun s i -> (snd (g.GetMark i)) || s) false l
    let allInfected () =
        List.fold (fun s i -> (snd (g.GetMark i)) && s) true [0 .. g.Size-1]
    let infect v =
        let n = g.GetMark v
        g.SetMark v (fst n, true)
    let checkVertice v =
        let x = (prob (fst ( g.GetMark v)))
        let d = r.NextDouble()
        if (someOfInfected (g.OutByEdge v)) && (d < x) then
            infect v
    let rec loop x =
        if allInfected() then ()
        else
            printfn "Step %i" (x + 1)
            List.iter checkVertice [0 .. g.Size-1]
            if x % 10 = 0 then
                printNet()
            loop (x + 1)
    infect 0
    loop 0
    printNet()


[<TestFixture>]
type TestGraph () =
    let mutable g = ListGraph (10) :> IGraph
    do
        g.AddEdge 0 1
        g.AddEdge 2 3
        g.AddEdge 1 2
        g.AddEdge 4 5
        g.AddEdge 6 7
        g.AddEdge 7 8
        g.AddEdge 8 6

    [<Test>]
    member t.``From vertex`` () =
        let f = fromVertice g 9
        Assert.AreEqual (List.sort f, [9])
        let f = fromVertice g 4
        Assert.AreEqual (List.sort f, [4;5])
        let f = fromVertice g 0
        Assert.AreEqual (List.sort f, [0;1;2;3])
        let f = fromVertice g 6
        Assert.AreEqual (List.sort f, [6;7;8])

    [<Test>]
    member t.``To vertex`` () =
        let f = toVertice g 9
        Assert.AreEqual (List.sort f, [9])
        let f = toVertice g 5
        Assert.AreEqual (List.sort f, [4;5])
        let f = toVertice g 3
        Assert.AreEqual (List.sort f, [0;1;2;3])
        let f = toVertice g 6
        Assert.AreEqual (List.sort f, [6;7;8])
             
[<EntryPoint>]
let main argv = 
    0
