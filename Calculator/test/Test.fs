module Test

open NUnit.Framework
open Calculator

[<TestFixture>]
type TestCalc () =
    
    [<Test>]
    member t.``Initial`` () =
        let c = new Calculator.Calculator()
        Assert.AreEqual("0", c.Display)
    
    [<Test>]
    member t.``Input`` () =
        let c = new Calculator.Calculator()
        c.Digit(1)
        c.Digit(2)
        c.Digit(3)
        c.Dot()
        c.Digit(5)
        Assert.AreEqual("123.5", c.Display)
    
    [<Test>]
    member t.``Result`` () =
        let c = new Calculator.Calculator()
        c.Digit(2)
        c.Dot()
        c.Digit(3)
        c.Binary(+)
        c.Digit(5)
        c.Dot()
        c.Digit(1)
        c.Result()
        Assert.AreEqual("7.4", c.Display)
    
    [<Test>]
    member t.``Clear`` () =
        let c = new Calculator.Calculator()
        c.Digit(2)
        c.Dot()
        c.Digit(3)
        Assert.AreEqual("2.3", c.Display)
        c.Clear()
        Assert.AreEqual("0", c.Display)
    
    [<Test>]
    member t.``Systems`` () =
        let c = new Calculator.Calculator()
        c.Digit(3)
        c.Digit(1)
        Assert.AreEqual("31", c.Display)
        c.SetBase(16)
        Assert.AreEqual("1f.000000", c.Display)
        c.SetBase(8)
        Assert.AreEqual("37.000000", c.Display)
    
    [<Test>]
    member t.``Progression`` () =
        let c = new Calculator.Calculator()
        c.Digit(3)
        c.Binary(+)
        c.Digit(2)
        c.Result()
        Assert.AreEqual("5", c.Display)
        c.Result()
        Assert.AreEqual("7", c.Display)
        c.Result()
        Assert.AreEqual("9", c.Display)
