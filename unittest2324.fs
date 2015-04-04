open System
open NUnit.Framework

(*20*)
type IGraph =
    abstract AddEdge : int -> int -> unit
    abstract RemoveEdge : int -> int -> unit
    abstract HasEdge : int -> int -> bool
    abstract OutByEdge : int -> int list
    abstract Size : int

let range k =
    [0..k]

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
                        [] (range (size - 1))
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
    v = x

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
    List.fold accumulateVertice [] (range (g.Size - 1))

[<TestFixture>]
type TestGraph () =
    let mutable g = ListGraph (10) :> IGraph
    let edges = [(0, 1); (2, 3); (1, 2);
                 (4, 5); (6, 7); (7, 8);
                 (8, 6)]
    do
        List.iter (fun (u, v) -> g.AddEdge u v) edges

    [<TestCase(9,Result=[|9|])>]
    [<TestCase(4,Result=[|4;5|])>]
    [<TestCase(0,Result=[|0;1;2;3|])>]
    [<TestCase(6,Result=[|6;7;8|])>]
    member t.``From vertex`` v =
        let f = fromVertice g v
        Array.ofList (List.sort f)

    [<TestCase(9,Result=[|9|])>]
    [<TestCase(5,Result=[|4;5|])>]
    [<TestCase(3,Result=[|0;1;2;3|])>]
    [<TestCase(6,Result=[|6;7;8|])>]
    member t.``To vertex`` v =
        let f = toVertice g v
        Array.ofList (List.sort f)

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

type Network (g : IMarkedGraph<string * bool>) =
    let r = System.Random()

    let probability os =
        match os with
        | "Windows" -> 0.9
        | "MacOS" -> 0.4
        | "Linux" -> 0.01
        | _ -> 1.0

    member s.All =
        (range (g.Size - 1))
    
    member s.NameOf v =
        fst (g.GetMark v)

    member s.IsInfected v =
        snd (g.GetMark v)
    
    member s.Infect v =
        g.SetMark v (s.NameOf v, true)

    member s.ProbabilisticInfect v p =
        if r.NextDouble() < p then s.Infect v

    member s.InfectByOS v =
        s.ProbabilisticInfect v (s.NameOf v |> probability)

    member s.OneOfIsInfected l =
        List.fold (fun a i -> s.IsInfected i || a) false l

    member s.AllOfAreInfected l =
        List.fold (fun a i -> s.IsInfected i && a) true l

    member s.NeighbourIsInfected v =
        s.OneOfIsInfected (g.OutByEdge v)

    member s.AllAreInfected =
        s.AllOfAreInfected s.All

    member s.MayInfect v =
        (not (s.IsInfected v)) && (s.NeighbourIsInfected v)

    member s.DoStep () =
        let mayInfect = List.filter s.MayInfect s.All
        List.iter s.InfectByOS mayInfect
    
    member s.Render =
        let side = g.Size |> float |> Math.Sqrt |> Math.Ceiling |> int
        let concatStrings =
            List.reduce (+)
        let addNewline s =
            s + "\n"
        let mapTail m l =
            match l with
            | x :: xs -> x :: (List.map m xs)
            | _ -> l
        let evenLine i =
            let lineList = List.init side (fun j -> if g.HasEdge (i * side + j) ((i - 1) * side + j) then "." else " ")
            concatStrings (mapTail (fun i -> "   " + i) lineList)
        let renderVertex i =
            let name, v = g.GetMark i
            let sub = if i % side > 0 then (if g.HasEdge i (i - 1) then ".." else "  ") else ""
            sub + name.Substring(0, 1) + (if v then "-" else "+")
        let lineIndex i =
            [i * side..(i+1) * side - 1]
        let renderLine i =
            let lineList = List.map renderVertex (lineIndex i)
            concatStrings lineList
        let renderWithEven i =
            if i > 0 then
                (evenLine i) + "\n" + (renderLine i)
            else
                renderLine i
        if side * side <> g.Size then
            ""
        else
            let table = List.init side renderWithEven
            concatStrings (List.map addNewline table)

let generateSquareNetwork (size : int) =
    let r = System.Random()
    let g = MarkedGraph(ListGraph(size * size)) :> IMarkedGraph<string * bool>
    let names = ["Windows"; "MacOS"; "Linux"]
    let randomName () =
        names.[r.Next(0, names.Length)]
    let randomBool () =
        r.Next(0, 100) < 60
    let graphMarks = List.init (size * size) (fun i -> (randomName(), false))
    let allVertices = (range (size * size - 1))
    let allFrom x =
        List.map (fun i -> (x, i)) [x + 1..size*size-1]
    let allEdges = List.concat (List.map allFrom allVertices)
    let correctEdge (u, v) =
        (u + 1    = v && u / size = v / size) ||
        (u + size = v && u % size = v % size)
    let test = correctEdge (15,20)
    let correctEdges = List.filter correctEdge allEdges
    let selectedEdges = List.filter (fun i -> randomBool ()) correctEdges
    List.iter (fun i -> g.SetMark i graphMarks.[i]) allVertices
    List.iter (fun (u, v) -> g.AddEdge u v; g.AddEdge v u) selectedEdges
    let net = Network g
    net.Infect 0
    net

let simulateNet size steps =
    let net = generateSquareNetwork size
    for i in range steps do
        net.DoStep()
        if i % 10 = 0 then
            let r = net.Render
            printfn "State %d:\n%s" i r

[<EntryPoint>]
let main argv = 
    let g = MatrixGraph(10) :> IGraph
    g.AddEdge 3 0
    g.AddEdge 5 4
    g.AddEdge 3 2
    g.AddEdge 4 3
    printfn "%A" (toVertice g 0)
    printfn "%A" (fromVertice g 4)

    simulateNet 5 100

    0
