module LParen.Interpreter.Tests.IntegrationTests.Apply

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Environment
open LParen.Interpreter.Tests.Helpers

[<Fact>]
let ``User-defined functions can be called through list syntax`` () =
    
    let environment = createEnvironment()
    
    let input = "(define (add x y) (+ x y))"
    let expectedOutput = Atom.Symbol "add"
    
    let newEnvironment = evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let additionInput = "(add 1 2)"
    let expectedAdditionOutput = Atom.Integer 3
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment additionInput expectedAdditionOutput newEnvironment


[<Fact>]
let ``User-defined functions can be called through apply form`` () =
    
    let environment = createEnvironment()
    
    let input = "(define (add x y) (+ x y))"
    let expectedOutput = Atom.Symbol "add"
    
    let newEnvironment = evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let additionInput = "(apply add 1 2)"
    let expectedAdditionOutput = Atom.Integer 3
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment additionInput expectedAdditionOutput newEnvironment
