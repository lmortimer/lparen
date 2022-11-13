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
let ``Can use symbols in operations`` () =
    
    let environment: Environment = { 
        Symbols = Dictionary<Atom, Atom>()
    }
    
    let input = "(define x 4)"
    let expectedOutput = Atom.Symbol "x"
    
    let newEnvironment = evaluateAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let additionInput = "(+ x 5)"
    let expectedAdditionOutput = Atom.Integer 9
    
    evaluateAndAssertEqualsWithEnvironment additionInput expectedAdditionOutput newEnvironment