module Workflow

let norm n x =
    if (x < 0) then
        (n - (-x) % n) % n
    else
        x % n

type RingBuilder(n) =
    do
        if n < 1 then failwith "Non-natural modulo"

    member this.Bind(x, f) = f (norm n x)

    member this.Return(x) = (norm n x)

let ring n = RingBuilder(n)

type 't Tree = Nil | Leaf of 't | Node of 't Tree * 't Tree

type TreeBuilder () =
    member this.Bind(x, f) =
        match x with
        | Nil -> Nil
        | Leaf x -> f x
        | Node (l, r) -> Node (this.Bind(l, f), this.Bind(r, f))

    member this.Combine(a, b) = Node (a, b)

    member this.Return(x) = Leaf x

    member this.Zero() = Nil

let treework = TreeBuilder()

let map f t =
    treework {
        let! x = t
        return f x
    }

let filter f t =
    treework {
        let! x = t
        if f x then
            return x
    }
