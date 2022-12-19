module LParen.Interpreter.Tests.IntegrationTests.Eval

open System
open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Environment
open LParen.Interpreter.Tests.Helpers

[<Fact>]
let ``Eval basic stateless expression`` () =
    
    let environment = createEnvironment()
    
    let input = "(eval '42)"
    let expectedOutput = Atom.Integer 42
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    


[<Fact>]
let ``User-defined functions can be called eval`` () =
    
    let environment = createEnvironment()
    
    let input = "(define (add x y) (+ x y))"
    let expectedOutput = Atom.Symbol "add"
    
    let newEnvironment = evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let evalAddInput = "(eval '(add 2 3))"
    let expectedEvalAddOutput = Atom.Integer 5
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment evalAddInput expectedEvalAddOutput newEnvironment


[<Fact>]
let ``Symbols defined in eval appear in the environment`` () =
    
    let environment = createEnvironment()
    
    let input = "(eval '(define x 100))"
    let expectedOutput = Atom.Symbol "x"
    
    let newEnvironment = evaluateSingleExpressionAndAssertEqualsWithEnvironment input expectedOutput environment
    
    let evalAddInput = "(+ x x)"
    let expectedEvalAddOutput = Atom.Integer 200
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment evalAddInput expectedEvalAddOutput newEnvironment
