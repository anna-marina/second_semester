module A

open System
open System.IO
open System.Collections.Generic

type Expression =
    | Number of int
    | Variable of string
    | Operation of char * Expression * Expression

type Statement =
    | Read of string
    | Write of Expression
    | Assign of string * Expression
    | Sequence of Statement * Statement
    | If of Expression * Statement * Statement
    | While of Expression * Statement

let operators =
        ['+', (+); '-', (-); '*', (*);
         '/', (/); '%', (%); '^', (pown);] |> Map.ofList

type MutableList<'T>(l : 'T list) =
    let mutable lst = l
    member m.List =
        lst
    member m.Pop() =
        match lst with
        | x :: xs -> lst <- xs; x
        | _ -> failwith "Empty List"
    member m.Push x=
        lst <- x :: lst

type ListParser(x : string list) =
    let mutable l = MutableList(x)

    member p.parseExpr() =
        let s : string = l.Pop()
        match s with
        | _ when s.Length = 1 &&
            operators.ContainsKey(s.[0]) ->
            Operation (s.[0], p.parseExpr(), p.parseExpr())
        | _ when Char.IsDigit s.[0] || s.[0] = '-' ->
            Number (Int32.Parse s)
        | _ -> Variable s

    member p.parseStmt() =
        let s = l.Pop ()
        match s with
        | "if" -> If (p.parseExpr(), p.parseStmt(), p.parseStmt())
        | "while" -> While (p.parseExpr(), p.parseStmt())
        | ";" -> Sequence (p.parseStmt(), p.parseStmt())
        | ":=" -> Assign (l.Pop(), p.parseExpr())
        | "read" -> Read (l.Pop())
        | "write" -> Write (p.parseExpr())
        | _ -> failwith "Incorrect statement"

type Interpreter(tree) =
    let env = Dictionary()
    let mutable r = Console.ReadLine >> Int32.Parse
    let mutable w = int >> Console.WriteLine

    member p.runExpr x =
        match x with
        | Operation (t, l, r) ->
            operators.[t] (p.runExpr l) (p.runExpr r)
        | Number x -> x
        | Variable s -> if env.ContainsKey(s) then env.[s] else 0

    member p.runStmt x =
        let ok cond = (p.runExpr cond) <> 0
        match x with
        | If (cond, t, f) -> p.runStmt (if ok cond then t else f)
        | While (cond, s) -> while ok cond do p.runStmt s
        | Sequence (a, b) -> p.runStmt a; p.runStmt b
        | Assign (var, e) -> env.[var] <- p.runExpr e
        | Read var -> env.[var] <- r ()
        | Write e -> w (p.runExpr e)

    member p.run() =
        p.runStmt tree

    member p.setRW(newr, neww) =
        r <- newr
        w <- neww

let run l =
    Interpreter(ListParser(l).parseStmt()).run()

let runFile path =
    path |> File.ReadAllLines |> List.ofArray |> run

[<EntryPoint>]
let main argv =
    run [";";"read";"a";"while";"a";";";"write";"a";":=";"a";"-";"a";"1"]
    0
