module LParen.Interpreter.Tests.IntegrationTests._1._1.BooleanLogic

open Xunit

open System.Collections.Generic
open LParen.Interpreter.Common
open LParen.Interpreter.Tests.Helpers
    
[<Fact>]
let ``Can evaluate booleans`` () =
    
    let input = "true"
    let expectedOutput = Atom.Boolean true
    
    evaluateSinglExpressionAndAssertEquals input expectedOutput
    
    let inputFalse = "false"
    let expectedOutputFalse = Atom.Boolean false
    
    evaluateSinglExpressionAndAssertEquals inputFalse expectedOutputFalse
   
[<Fact>]
let ``Basic comparison methods`` () =

    evaluateSinglExpressionAndAssertEquals "(= 1 1)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(= 1 2)" (Atom.Boolean false)
    evaluateSinglExpressionAndAssertEquals "(> 2 1)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(> 1 2)" (Atom.Boolean false)
    evaluateSinglExpressionAndAssertEquals "(< 1 2)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(< 2 1)" (Atom.Boolean false)
   
[<Fact>]
let ``if`` () =

    evaluateSinglExpressionAndAssertEquals "(if true true false)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(if false true false)" (Atom.Boolean false)
    evaluateSinglExpressionAndAssertEquals "(if (> 1 2) (+ 100 100) (- 100 100))" (Atom.Integer 0)
    
[<Fact>]
let ``and`` () =
    evaluateSinglExpressionAndAssertEquals "(and true)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(and false)" (Atom.Boolean false)
    evaluateSinglExpressionAndAssertEquals "(and true true true)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(and true true false)" (Atom.Boolean false)
    evaluateSinglExpressionAndAssertEquals "(and (= 1 1) true)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(and (= 1 2) true)" (Atom.Boolean false)
  
[<Fact>]
let ``or`` () =
    evaluateSinglExpressionAndAssertEquals "(or true)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(or false)" (Atom.Boolean false)
    evaluateSinglExpressionAndAssertEquals "(or true true true)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(or true true false)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(or (= 1 1) true)" (Atom.Boolean true)
    evaluateSinglExpressionAndAssertEquals "(or (= 1 2) false)" (Atom.Boolean false)
    
[<Fact>]
let ``cond`` () =
    evaluateSinglExpressionAndAssertEquals "(cond (true 1) (false 2))" (Atom.Integer 1)
    evaluateSinglExpressionAndAssertEquals "(cond (false 2) (true 1))" (Atom.Integer 1)
    evaluateSinglExpressionAndAssertEquals "(cond ((= 1 1) 1) ((= 2 2) 2))" (Atom.Integer 1)
    evaluateSinglExpressionAndAssertEquals "(cond ((= 1 2) (+ 0 1)) ((= 2 2) (+ 0 2)))" (Atom.Integer 2)