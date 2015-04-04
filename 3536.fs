open System
open System.Text.RegularExpressions
open System.Collections
open System.Linq
open NUnit.Framework
open FsUnit

type Expr =
    | Number of int
    | Id of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Rem of Expr * Expr
    | Pow of Expr * Expr
    | Minus of Expr
    | Plus of Expr

type Associativity = Left | Right

type OperatorDef = { level : int;
                     assoc : Associativity;
                     tp : (Expr * Expr) -> Expr }

let split s =
    Array.append (Regex.Matches(s, "[\+\-\*\/\%\^\(\)]|\d+|\w+")
                       .Cast<Match>()
                       .Select(fun x -> x.Value)
                       .ToArray())
                 [|"~"|]

type Parser (a : string []) =
    let mutable it = a.AsEnumerable().GetEnumerator()
    let maxOperator = 2
    let operators =
        ["+", {level = 0; assoc = Left; tp = Add};
         "-", {level = 0; assoc = Left; tp = Sub};
         "*", {level = 1; assoc = Left; tp = Mul};
         "/", {level = 1; assoc = Left; tp = Div};
         "%", {level = 1; assoc = Left; tp = Rem};
         "^", {level = 2; assoc = Right; tp = Pow}] |> Map.ofList

    member p.ParseElement () =
        match it.Current with
        | "(" ->
                ignore (it.MoveNext())
                let r = p.ParseOperator 0
                ignore (it.MoveNext())
                r
        | "-" ->
                ignore (it.MoveNext())
                Minus (p.ParseElement())

        | "+" ->
                ignore (it.MoveNext())
                Plus (p.ParseElement())

        | x ->
                let r =
                    if Char.IsDigit x.[0] then
                        Number (Int32.Parse x)
                    else
                        Id x
                ignore (it.MoveNext())
                r

    member p.ParseOperator level =
        let sameLevel op =
            (Map.containsKey op operators) &&
            operators.[op].level = level
        if level = maxOperator + 1 then
            p.ParseElement()
        else
            let mutable e = p.ParseOperator (level + 1)
            let mutable cont = true
            while cont do
                let op = it.Current
                cont <- sameLevel op
                if cont then
                    ignore (it.MoveNext())
                    let r =
                        if operators.[op].assoc = Left then
                            p.ParseOperator (level + 1)
                        else
                            cont <- false
                            p.ParseOperator level
                    e <- operators.[op].tp (e, r)
            e

    member p.Parse =
        it <- a.AsEnumerable().GetEnumerator()
        ignore (it.MoveNext())
        p.ParseOperator 0

let evalContext e (m : Map<string, int>) =
    let rec eval e =
        match e with
        | Number x -> x
        | Id x -> m.[x]
        | Add (a, b) -> (eval a) + (eval b)
        | Sub (a, b) -> (eval a) - (eval b)
        | Mul (a, b) -> (eval a) * (eval b)
        | Div (a, b) -> (eval a) / (eval b)
        | Rem (a, b) -> (eval a) % (eval b)
        | Pow (a, b) -> pown (eval a) (eval b)
        | Minus x -> -(eval x)
        | Plus x -> eval x
    eval e

let calcvar s m =
    evalContext (Parser(split s).Parse) (Map.ofList m)

let calc s =
    calcvar s []

[<TestFixture>]
type TestCalc() =
    
    [<Test>]
    member t.``Numbers`` () =
        calc "0" |> should equal 0
        calc "1" |> should equal 1
        calc "12345" |> should equal 12345

    [<Test>]
    member t.``Addition and subtraction`` () =
        calc "2+3" |> should equal 5
        calc "4-2" |> should equal 2
        calc "1000-10" |> should equal 990
        calc "0-1" |> should equal -1
        calc "(2+(2+2))+(2+2)" |> should equal 10

    [<Test>]
    member t.``Unary`` () =
        calc "-3" |> should equal -3
        calc "--3" |> should equal 3
        calc "2+-3" |> should equal -1
        calc "-5++5" |> should equal 0

    [<Test>]
    member t.``Multiplication and division`` () =
        calc "10-2*3" |> should equal 4
        calc "6/2" |> should equal 3
        calc "5/2*2" |> should equal 4
        calc "12%5" |> should equal 2

    [<Test>]
    member t.``Power`` () =
        calc "2^2^3" |> should equal 256
        calc "(2^2)^3" |> should equal 64

    [<Test>]
    member t.``Variables`` () =
        calcvar "x" [("x", 6)] |> should equal 6
        calcvar "-x" [("x", 6)] |> should equal -6
        calcvar "x^2" [("x", 6)] |> should equal 36
        calcvar "x*y" [("x", 6); ("y", 4)] |> should equal 24
        calcvar "(x^y-v)/z" [("x", 5); ("y", 3); ("z", 2); ("v", 13)] 
                |> should equal 56

[<EntryPoint>]
let main argv =
    printfn "%A" (calc "1 + (2 / 3) ^2")
    printfn "%A" (calcvar "-x*3" [("x",5)])
    0
