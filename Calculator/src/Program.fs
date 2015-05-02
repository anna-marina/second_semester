module Calculator

open System.Windows.Forms
open System.Drawing
open System.Globalization
open System

type Calculator() =
    let mutable rg = [|0.0;0.0|]
    let mutable bs = 10
    let mutable dot = false
    let mutable digitAfter = 0.1
    let mutable rc = 0
    let mutable ds = 0
    let mutable op = (+)
    let mutable err = false
    let mutable repeat = false

    member this.Clear() =
        rg <- [|0.0; 0.0|]
        rc <- 0
        ds <- 0
        op <- (+)
        this.ClearCurrent()
        err <- false
        repeat <- false

    member this.ClearCurrent() =
        rg.[rc] <- 0.0
        digitAfter <- 0.1
        dot <- false

    member this.Digit(x:int) =
        if repeat then
            this.Clear()
        ds <- rc
        if err || (x < 0) && (x > 9) then
            err <- true
        else if dot then
            rg.[rc] <- rg.[rc] + digitAfter * float(x)
            digitAfter <- digitAfter * 0.1
        else
            rg.[rc] <- rg.[rc] * 10.0 + float(x)

    member this.Check() =
        if Double.IsInfinity(rg.[ds]) || Double.IsNaN(rg.[ds]) then
            err <- true

    member this.Dot() =
        if repeat then
            this.Clear()
        dot <- true

    member this.Flush(r:bool) =
        if not(err) then
            if r || not(repeat) then 
                rg.[0] <- op rg.[0] rg.[1]
        this.Check()
        rc <- 0
        repeat <- true

    member this.Binary(f) =
        this.Flush(false)
        op <- f
        rc <- 1
        repeat <- false
        this.ClearCurrent()

    member this.Unary(f) =
        rg.[rc] <- f rg.[rc]
        this.Check()

    member this.Result() =
        ds <- 0
        this.Flush(true)

    member this.Display =
        if err then
            "E"
        else
            if bs = 10 then
                rg.[ds].ToString(CultureInfo.InvariantCulture)
            else
                let v = rg.[ds]
                let sgn = v < 0.0
                let av = Math.Abs(v)
                let ipart = Math.Truncate(av)
                let mutable fpart = av - ipart
                if ipart > float(Int32.MaxValue) then
                    err <- true
                    "E"
                else
                    let conv x =
                        if x < 10 then
                            x.ToString()
                        else
                            char(x + int('a') - 10).ToString()
                    let mutable n = int(ipart)
                    let mutable res = ""
                    if n = 0 then
                        res <- res + "0"
                    else
                        while n > 0 do
                            let dig = n % bs
                            res <- (conv dig) + res
                            n <- n / bs
                    res <- res + "."
                    for i in [1..6] do
                        fpart <- fpart * float(bs)
                        let dig = Math.Truncate(fpart)
                        fpart <- fpart - dig
                        res <- res + (int dig |> conv)
                    if sgn then
                        res <- "-" + res
                    res
    member this.SetBase(n) =
        bs <- n

let calc = new Calculator()

let form = new Form(Text = "Anna Maryina Calculator",
                    Visible = true,
                    TopMost = true,
                    Width = 400,
                    Height = 300)

let gridSize k i j w h (b:Control) =
    b.Parent <- form
    b.Location <- new Point(j * 40, i * 40)
    b.Width <- w * k
    b.Height <- h * k

let grid:(int->int->int->int->Control->unit) = gridSize 40

let box = new TextBox()
box.Text <- calc.Display
grid 0 1 6 2 box

let spin = new NumericUpDown()
spin.Minimum <- new Decimal(2)
spin.Maximum <- new Decimal(36)
spin.Value <- new Decimal(10)
grid 5 1 1 1 spin

let update() =
    calc.SetBase(int(spin.Value))
    box.Text <- calc.Display

spin.ValueChanged.AddHandler(fun o e -> update())

for i in [0..9] do
    let btn = new Button()
    let n = (i + 1) % 10
    btn.Click.AddHandler(fun o e -> calc.Digit(n); update())
    btn.Text <- (n).ToString()
    grid (i / 3 + 1) (i % 3 + 1) 1 1 btn

let dot = new Button()
dot.Text <- "."
dot.Click.AddHandler(fun o e -> calc.Dot(); update())
grid 4 2 1 1 dot

let res = new Button()
res.Text <- "="
res.Click.AddHandler(fun o e -> calc.Result(); update())
grid 4 3 1 1 res

let add = new Button()
add.Text <- "+"
add.Click.AddHandler(fun o e -> calc.Binary(+); update())
grid 1 4 1 1 add

let sub = new Button()
sub.Text <- "-"
sub.Click.AddHandler(fun o e -> calc.Binary(-); update())
grid 2 4 1 1 sub

let mul = new Button()
mul.Text <- "*"
mul.Click.AddHandler(fun o e -> calc.Binary(*); update())
grid 3 4 1 1 mul

let div = new Button()
div.Text <- "/"
div.Click.AddHandler(fun o e -> calc.Binary(/); update())
grid 4 4 1 1 div

let sin = new Button()
sin.Text <- "sin"
sin.Click.AddHandler(fun o e -> calc.Unary(Math.Sin); update())
grid 1 5 1 1 sin

let cos = new Button()
cos.Text <- "cos"
cos.Click.AddHandler(fun o e -> calc.Unary(Math.Cos); update())
grid 2 5 1 1 cos

let tg = new Button()
tg.Text <- "tg"
tg.Click.AddHandler(fun o e -> calc.Unary(Math.Tan); update())
grid 3 5 1 1 tg

let sqrt = new Button()
sqrt.Text <- "sqrt"
sqrt.Click.AddHandler(fun o e -> calc.Unary(Math.Sqrt); update())
grid 4 5 1 1 sqrt

let ln = new Button()
ln.Text <- "ln"
ln.Click.AddHandler(fun o e -> calc.Unary(Math.Log); update())
grid 1 6 1 1 ln

let exp = new Button()
exp.Text <- "e^x"
exp.Click.AddHandler(fun o e -> calc.Unary(Math.Exp); update())
grid 2 6 1 1 exp

let cls = new Button()
cls.Text <- "Clear"
cls.Click.AddHandler(fun o e -> calc.Clear(); update())
grid 5 4 2 1 cls

Application.EnableVisualStyles()
Application.Run(form)
