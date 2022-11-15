module LParen.Interpreter.Tests.IntegrationTests._1._1.BooleanLogic

open Xunit

open System.Collections.Generic
open LParen.Interpreter.Common
open LParen.Interpreter.Tests.Helpers
    
[<Fact>]
let ``Can evaluate booleans`` () =
    
    let input = "true"
    let expectedOutput = Atom.Boolean true
    
    evaluateAndAssertEquals input expectedOutput
    
    let inputFalse = "false"
    let expectedOutputFalse = Atom.Boolean false
    
    evaluateAndAssertEquals inputFalse expectedOutputFalse
   
[<Fact>]
let ``Basic comparison methods`` () =

    evaluateAndAssertEquals "(= 1 1)" (Atom.Boolean true)
    evaluateAndAssertEquals "(= 1 2)" (Atom.Boolean false)
    evaluateAndAssertEquals "(> 2 1)" (Atom.Boolean true)
    evaluateAndAssertEquals "(> 1 2)" (Atom.Boolean false)
    evaluateAndAssertEquals "(< 1 2)" (Atom.Boolean true)
    evaluateAndAssertEquals "(< 2 1)" (Atom.Boolean false)
   
[<Fact>]
let ``if`` () =

    evaluateAndAssertEquals "(if true true false)" (Atom.Boolean true)
    evaluateAndAssertEquals "(if false true false)" (Atom.Boolean false)
    evaluateAndAssertEquals "(if (> 1 2) (+ 100 100) (- 100 100))" (Atom.Integer 0)
    
[<Fact>]
let ``and`` () =
    evaluateAndAssertEquals "(and true)" (Atom.Boolean true)
    evaluateAndAssertEquals "(and false)" (Atom.Boolean false)
    evaluateAndAssertEquals "(and true true true)" (Atom.Boolean true)
    evaluateAndAssertEquals "(and true true false)" (Atom.Boolean false)
    evaluateAndAssertEquals "(and (= 1 1) true)" (Atom.Boolean true)
    evaluateAndAssertEquals "(and (= 1 2) true)" (Atom.Boolean false)
  
[<Fact>]
let ``or`` () =
    evaluateAndAssertEquals "(or true)" (Atom.Boolean true)
    evaluateAndAssertEquals "(or false)" (Atom.Boolean false)
    evaluateAndAssertEquals "(or true true true)" (Atom.Boolean true)
    evaluateAndAssertEquals "(or true true false)" (Atom.Boolean true)
    evaluateAndAssertEquals "(or (= 1 1) true)" (Atom.Boolean true)
    evaluateAndAssertEquals "(or (= 1 2) false)" (Atom.Boolean false)  