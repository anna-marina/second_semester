open System.Text.RegularExpressions
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System 

type Expr =
    | Number of float
    | Id of string
    | Func of string * Expr
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
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
                       .Select(fun (x:Match) -> x.Value)
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
                let r = Number (Double.Parse x)
                ignore (it.MoveNext())
                r
        | x ->
                let r = Id x
                ignore (it.MoveNext())
                if it.Current = "(" then
                    Func (x, p.ParseElement ())
                else
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


let evalContext e (m : Map<string, float>) =
    let rec eval e =
        match e with
        | Number x -> x
        | Id x -> m.[x]
        | Add (a, b) -> (eval a) + (eval b)
        | Sub (a, b) -> (eval a) - (eval b)
        | Mul (a, b) -> (eval a) * (eval b)
        | Div (a, b) -> (eval a) / (eval b)
        | Pow (a, b) -> Math.Pow(eval a, eval b)
        | Minus x -> -(eval x)
        | Plus x -> eval x
        | Func (f, x) ->
            let func =
                match f with
                | "sin" -> Math.Sin
                | "cos" -> Math.Cos
                | _ -> (fun x -> x)
            func (eval x)
    eval e

let calcvar s m =
    evalContext (Parser(split s).Parse) (Map.ofList m)

let form = new Form(Text = "F# Windows Form",
                    Visible = true,
                    TopMost = true,
                    Width = 500,
                    Height = 300)

let image = new Bitmap(500, 300,
                       Imaging.PixelFormat.Format32bppArgb)

let button = new Button(Text="Plot")
button.Parent <- form

let input = new TextBox(Text="sin(x)")
input.Width <- 400
input.Location <- new Point(300, 0)
input.Parent <- form

let plot () =
    let expr = input.Text
    let func = (fun x -> calcvar expr [("x", x)])
    let g = Graphics.FromImage(image)
    g.SmoothingMode <- 
        System.Drawing.Drawing2D.SmoothingMode.AntiAlias
    let pen = Pens.Black
    let k = 50

    g.FillRectangle(Brushes.White, 0, 0, 500, 300)
    for i in [0..k] do
        let xa = 500 / k * i
        let xb = 500 / k * (i + 1)
        let ya = -int(func(float(i) * 2.0 * Math.PI / float(k)) * 150.0) + 150
        let yb = -int(func(float(i + 1) * 2.0 * Math.PI / float(k)) * 150.0) + 150
        g.DrawLine(pen, xa, ya, xb, yb)
    g.Dispose()

let onPaint (sender:obj) (e:PaintEventArgs)  =
    let gwin = e.Graphics
    gwin.DrawImage(image, 0, 0)

plot ()
button.Click.AddHandler(fun o e -> plot (); form.Refresh())
form.Paint.AddHandler(PaintEventHandler(onPaint))
Application.Run(form)
