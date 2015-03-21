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

(*23*)
let fromVertice (g : IGraph) v =
     let rec fromVerticeSet r =
        let p = List.filter (fun u -> not (List.exists (fun t -> u = t) r)) (List.concat (List.map g.OutByEdge r))
        if p = [] then r else fromVerticeSet (p @ r)
     fromVerticeSet [v]

(*24*)
let toVertice (g : IGraph) v =
    List.fold (fun s i -> if List.exists (fun x -> x = v) (fromVertice g i) then i :: s else s) [] [0 .. g.Size-1]

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
    abstract ToArray : 'T array

(*29*)
type ArrayList<'T>() =
    let mutable arr = Array.zeroCreate<'T> 0
    let mutable size = 0

    interface IList<'T> with
        member s.Size =
            size

        member s.Prepend x =
            let narr = Array.zeroCreate<'T> (size + 1)
            Array.blit arr 0 narr 1 size
            narr.[0] <- x
            arr <- narr
            size <- size + 1
        
        member s.Append x =
            let narr = Array.zeroCreate<'T> (size + 1)
            Array.blit arr 0 narr 0 size
            narr.[size] <- x
            arr <- narr
            size <- size + 1

        member s.Insert i x =
            let narr = Array.zeroCreate<'T> (size + 1)
            Array.blit arr 0 narr 0 i
            if i < size then
                Array.blit arr i narr (i + 1) (size - i)
            narr.[i] <- x
            arr <- narr
            size <- size + 1

        member s.RemoveFirst =
            if size > 0 then
                let narr = Array.zeroCreate<'T> (size - 1)
                Array.blit arr 1 narr 0 size
                arr <- narr
                size <- size - 1

        member s.RemoveLast =
            if size > 0 then
                let narr = Array.zeroCreate<'T> (size - 1)
                Array.blit arr 0 narr 0 (size - 1)
                arr <- narr
                size <- size - 1

        member s.Remove i =
            if size > 0 then
                let narr = Array.zeroCreate<'T> (size - 1)
                if i > 0 then
                    Array.blit arr 0 narr 0 (i - 1)
                Array.blit arr (i + 1) narr i (size - i - 1)
                arr <- narr
                size <- size - 1

        member s.Find p =
            Array.foldBack (fun i s -> if (p i) then (Some i) else s) arr None

        member s.Concat arrx =
            let narr = Array.zeroCreate<'T> (size + arrx.Size)
            Array.blit arr 0 narr 0 size
            Array.blit arrx.ToArray 0 narr size size
            arr <- narr
            size <- size + arrx.Size

        member s.ToArray =
            arr
             
[<EntryPoint>]
let main argv = 
    let g = MatrixGraph(10) :> IGraph
    g.AddEdge 3 0
    g.AddEdge 5 4
    g.AddEdge 3 2
    g.AddEdge 4 3
    printfn "available from one"
    printfn "%A" (toVertice g 0)
    printfn "available from one"
    printfn "%A" (fromVertice g 4)
    printfn "modeling viruses"
    let n = randomNetwork 10
    simulateNet n
    0
