open System
open System.IO
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

        | x when Char.IsDigit x.[0] ->
                let r = Number (Int32.Parse x)
                ignore (it.MoveNext())
                r
        | x ->
                let r = Id x
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

let eval e =
    evalContext e Map.empty

let calcvar s m =
    evalContext (Parser(split s).Parse) (Map.ofList m)

let calc s =
    calcvar s []

let toPostfix s =
    let e = Parser(split s).Parse
    let rec postfix e =
        match e with
        | Number x -> [x.ToString()]
        | Id x -> [x]
        | Add (a, b) -> (postfix a) @ (postfix b) @ ["+"]
        | Sub (a, b) -> (postfix a) @ (postfix b) @ ["-"]
        | Mul (a, b) -> (postfix a) @ (postfix b) @ ["*"]
        | Div (a, b) -> (postfix a) @ (postfix b) @ ["/"]
        | Rem (a, b) -> (postfix a) @ (postfix b) @ ["%"]
        | Pow (a, b) -> (postfix a) @ (postfix b) @ ["^"]
        (* Унарные операции с пометкой *)
        | Minus x -> (postfix x) @ ["-!"]
        | Plus x -> (postfix x) @ ["+!"]
    postfix e

let fileToPostfix src dst =
    let expr = String.Join("", File.ReadAllLines(src))
    let post = String.Join(Environment.NewLine, toPostfix expr)
    File.WriteAllText(dst, post)

let fromPostfix (e : string list) =
    let rec translate e st =
        match e, st with
        | [], st -> st
        | "+" :: e, b :: a :: st -> translate e ((Add (a, b)) :: st)
        | "-" :: e, b :: a :: st -> translate e ((Sub (a, b)) :: st)
        | "*" :: e, b :: a :: st -> translate e ((Mul (a, b)) :: st)
        | "/" :: e, b :: a :: st -> translate e ((Div (a, b)) :: st)
        | "%" :: e, b :: a :: st -> translate e ((Rem (a, b)) :: st)
        | "^" :: e, b :: a :: st -> translate e ((Pow (a, b)) :: st)
        | "+!" :: e, a :: st -> translate e ((Plus a) :: st)
        | "-!" :: e, a :: st -> translate e ((Minus a) :: st)
        | x :: e, st when Char.IsDigit x.[0] -> 
            translate e (Number (Int32.Parse x) :: st)
        | x :: e, st -> translate e ((Id x) :: st)
    (translate e []).[0]

let fileToResult src dst =
    let expr = List.ofArray (File.ReadAllLines(src))
    let res = eval (fromPostfix expr)
    File.WriteAllText(dst, res.ToString())

let splitBy (s : string) c =
    s.Split([|c|])

let parseMap (env : string) =
    let pairArray l =
        match l with
        | [|x;y|] -> (x, Int32.Parse y)
        | _ -> failwith "Error"
    let v = splitBy (env.Replace(" ", "")) ','
    let u = Array.map (fun s -> splitBy s '=') v
    let mapping = Array.map pairArray u
    List.ofArray mapping

[<TestCase("0", Result=0)>]
[<TestCase("1", Result=1)>]
[<TestCase("12345", Result=12345)>]
[<TestCase("2+3", Result= 5)>]
[<TestCase("4-2", Result=2)>]
[<TestCase("1000-10", Result=990)>]
[<TestCase("0-1", Result=(-1))>]
[<TestCase("(2+( 2 +2) ) +(2 + 2)", Result=10)>]
[<TestCase("-3", Result=(-3))>]
[<TestCase("-- 3", Result=3)>]
[<TestCase("2+ -3", Result=(-1))>]
[<TestCase("-5++  5", Result=0)>]
[<TestCase("10-2*3", Result=4)>]
[<TestCase("6 / 2", Result=3)>]
[<TestCase("5/ 2*2", Result=4)>]
[<TestCase("  12  % 5  ", Result=2)>]
[<TestCase("2^2^3", Result=256)>]
[<TestCase("(2 ^ 2) ^3", Result=64)>]
[<TestCase(" -2 ^ 2", Result=4)>]
[<TestCase("- (2 ^2)", Result=(-4))>]
let TestCalc s =
    calc s
        
[<TestCase("x+y", "x=6,y=5", Result=11)>]
[<TestCase("x", "x=6", Result=6)>]
[<TestCase("-x", "x=6", Result=(-6))>]
[<TestCase("x^2", "x=6", Result=36)>]
[<TestCase("x*y", "x=6,y=4", Result=24)>]
[<TestCase("(x ^ y - v) / z", "x=5,y=3,z=2,v=13", Result=56)>]
let TestCalcVar s (env : string) =
    calcvar s (parseMap env)

[<TestCase("2+2", Result="2 2 +")>]
[<TestCase("0", Result="0")>]
[<TestCase("-1", Result="1 -!")>]
[<TestCase("5---5", Result="5 5 -! -! -")>]
[<TestCase("1 + (2 / 3) ^2", Result="1 2 3 / 2 ^ +")>]
[<TestCase("2 *2 *3", Result="2 2 * 3 *")>]
[<TestCase("2 ^2 ^3", Result="2 2 3 ^ ^")>]
let TestToPostfix s =
    String.Join(" ", toPostfix s)

[<TestCase("2 2 +", Result=4)>]
[<TestCase("0", Result=0)>]
[<TestCase("1 -!", Result=(-1))>]
[<TestCase("5 5 -! -! -", Result=0)>]
[<TestCase("1 2 3 / 2 ^ +", Result=1)>]
[<TestCase("2 2 * 3 *", Result=12)>]
[<TestCase("2 2 3 ^ ^", Result=256)>]
[<TestCase("12 3 /", Result=4)>]
let TestEvalPostfix (s : string) =
    let v = splitBy s ' ' |> List.ofArray |> fromPostfix
    evalContext v Map.empty

[<EntryPoint>]
let main argv =
    printfn "%A" (calc "1 + (2 / 3) ^2")
    printfn "%A" (calcvar "-x*3" [("x",5)])
    let e = "5--5"
    let p = toPostfix e
    let ex = fromPostfix p
    printfn "%A" p
    printfn "%A" (calc e)
    printfn "%A" (eval ex)
    printfn "%A" (fromPostfix p |> eval)
    fileToPostfix "expr.txt" "vertical.txt"
    fileToResult "vertical.txt" "result.txt"
    0
