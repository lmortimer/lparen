module LParen.Interpreter.Tests.IntegrationTests._1._1.BooleanLogic

open System
open Xunit

open System.Collections.Generic
open LParen.Interpreter.Common
open LParen.Interpreter.Tests.Helpers
    
[<Fact>]
let ``Can evaluate booleans`` () =
    
    let input = "true"
    let expectedOutput = Atom.Boolean true
    
    evaluateSingleExpressionAndAssertEquals input expectedOutput
    
    let inputFalse = "false"
    let expectedOutputFalse = Atom.Boolean false
    
    evaluateSingleExpressionAndAssertEquals inputFalse expectedOutputFalse
   
[<Fact>]
let ``Basic comparison methods`` () =

    evaluateSingleExpressionAndAssertEquals "(= 1 1)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(= 1 2)" (Atom.Boolean false)
    evaluateSingleExpressionAndAssertEquals "(> 2 1)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(> 1 2)" (Atom.Boolean false)
    evaluateSingleExpressionAndAssertEquals "(< 1 2)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(< 2 1)" (Atom.Boolean false)
   
[<Fact>]
let ``if`` () =

    evaluateSingleExpressionAndAssertEquals "(if true true false)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(if false true false)" (Atom.Boolean false)
    evaluateSingleExpressionAndAssertEquals "(if (> 1 2) (+ 100 100) (- 100 100))" (Atom.Integer 0)
    
[<Fact>]
let ``and`` () =
    evaluateSingleExpressionAndAssertEquals "(and true)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(and false)" (Atom.Boolean false)
    evaluateSingleExpressionAndAssertEquals "(and true true true)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(and true true false)" (Atom.Boolean false)
    evaluateSingleExpressionAndAssertEquals "(and (= 1 1) true)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(and (= 1 2) true)" (Atom.Boolean false)
  
[<Fact>]
let ``or`` () =
    evaluateSingleExpressionAndAssertEquals "(or true)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(or false)" (Atom.Boolean false)
    evaluateSingleExpressionAndAssertEquals "(or true true true)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(or true true false)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(or (= 1 1) true)" (Atom.Boolean true)
    evaluateSingleExpressionAndAssertEquals "(or (= 1 2) false)" (Atom.Boolean false)
    
[<Fact>]
let ``cond - valid cases`` () =
    evaluateSingleExpressionAndAssertEquals "(cond (true 1) (false 2))" (Atom.Integer 1)
    evaluateSingleExpressionAndAssertEquals "(cond (false 2) (true 1))" (Atom.Integer 1)
    evaluateSingleExpressionAndAssertEquals "(cond ((= 1 1) 1) ((= 2 2) 2))" (Atom.Integer 1)
    evaluateSingleExpressionAndAssertEquals "(cond ((= 1 2) (+ 0 1)) ((= 2 2) (+ 0 2)))" (Atom.Integer 2)
    
[<Fact>]
let ``cond - fails when no predicate evaluates to true`` () =
    
    let exp = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(cond (false false))" (Atom.Integer 1))
    
    Assert.Equal("No predicate passed to `cond` evaluated to true.", exp.Message)

[<Fact>]
let ``cond - fails when no expressions are passed in the clauses`` () =
    
    let exp = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(cond (true) (false))" (Atom.Integer 1))
        
    Assert.Equal("Expected clause in `cond` to be (<predicate> <expression). Instead received: $List [Boolean true]", exp.Message)
    
[<Fact>]
let ``cond - fails when predicate doesn't evaluate to a boolean`` () =
    
    let exp = Assert.Throws<Exception>(
        fun () -> evaluateSingleExpressionAndAssertEquals "(cond (false false) ((+ 1 1) true))" (Atom.Integer 1))
    
    Assert.Equal("Predicate passed to `cond` must evaluate to a boolean. List [Symbol \"+\"; Integer 1; Integer 1] does not.", exp.Message)