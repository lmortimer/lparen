module LParen.Interpreter.Tests.IntegrationTests._1._1.Define

open Xunit

open System.Collections.Generic
open LParen.Interpreter.Common
open LParen.Interpreter.Tests.Helpers
    
[<Fact>]
let ``Can define symbols`` () =
    
    let environment: Environment = { 
        Symbols = Dictionary<Atom, Atom>()
        Parent = None
    }
    
    let input = "(define x 4)"
    let expectedOutput = Atom.Symbol "x"
    
    let newEnvironment = evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let xInput = "x"
    let expectedXOutput = Atom.Integer 4
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment xInput expectedXOutput newEnvironment
   
   
[<Fact>]
let ``Can use symbols in operations`` () =
    
    let environment: Environment = { 
        Symbols = Dictionary<Atom, Atom>()
        Parent = None
    }
    
    let input = "(define x 4)"
    let expectedOutput = Atom.Symbol "x"
    
    let newEnvironment = evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let additionInput = "(+ x 5)"
    let expectedAdditionOutput = Atom.Integer 9
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment additionInput expectedAdditionOutput newEnvironment
    
[<Fact>]
let ``Can define functions`` () =
    
    let environment: Environment = { 
        Symbols = Dictionary<Atom, Atom>()
        Parent = None
    }
    
    let input = "(define (add x y) (+ x y))"
    let expectedOutput = Atom.Symbol "add"
    
    let newEnvironment = evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let additionInput = "(add 1 2)"
    let expectedAdditionOutput = Atom.Integer 3
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment additionInput expectedAdditionOutput newEnvironment
