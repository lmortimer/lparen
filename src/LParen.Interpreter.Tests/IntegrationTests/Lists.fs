module LParen.Interpreter.Tests.IntegrationTests.Lists

open System
open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Environment
open LParen.Interpreter.Tests.Helpers
    
[<Fact>]
let ``Can define a list`` () =
    
    evaluateSingleExpressionAndAssertEquals "(list)" (Atom.List [])
    evaluateSingleExpressionAndAssertEquals "(list 1 2)" (Atom.List [Atom.Integer 1; Atom.Integer 2])
    evaluateSingleExpressionAndAssertEquals "(list true false)" (Atom.List [Atom.Boolean true; Atom.Boolean false])
    evaluateSingleExpressionAndAssertEquals "(list (+ 1 2) (+ 3 4))" (Atom.List [Atom.Integer 3; Atom.Integer 7])

[<Fact>]
let ``head returns the first item in a list`` () =
    
    evaluateSingleExpressionAndAssertEquals "(head (list 1 2))" (Atom.Integer 1)
    
[<Fact>]
let ``head fails when called on a non-list`` () =
       
    let expBool = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(head true)" (Atom.Integer 1))
    
    Assert.Equal("head expects a List. Instead got called on value: True", expBool.Message)

    let expInt = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(head 1)" (Atom.Integer 1))
    
    Assert.Equal("head expects a List. Instead got called on value: 1", expInt.Message)

[<Fact>]
let ``tail returns the last item in a list`` () =
    
    // empty list if there's only one element
    evaluateSingleExpressionAndAssertEquals "(tail (list 1))" (Atom.List [])
    evaluateSingleExpressionAndAssertEquals "(tail (list 1 2))" (Atom.List [Atom.Integer 2])
    
[<Fact>]
let ``tail fails when called on a non-list`` () =
       
    let expBool = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(tail true)" (Atom.Integer 1))
    
    Assert.Equal("tail expects a List. Instead got called on value: True", expBool.Message)

    let expInt = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(tail 1)" (Atom.Integer 1))
    
    Assert.Equal("tail expects a List. Instead got called on value: 1", expInt.Message)


[<Fact>]
let ``empty? return behaviour on lists`` () =
    
    evaluateSingleExpressionAndAssertEquals "(empty? (list 1 2))" (Atom.Boolean false)
    evaluateSingleExpressionAndAssertEquals "(empty? (list))" (Atom.Boolean true)
    
[<Fact>]
let ``empty? fails when called on a non-list`` () =
       
    let expBool = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(empty? true)" (Atom.Integer 1))
    
    Assert.Equal("empty? expects a List. Instead got called on value: True", expBool.Message)

    let expInt = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(empty? 1)" (Atom.Integer 1))
    
    Assert.Equal("empty? expects a List. Instead got called on value: 1", expInt.Message)
