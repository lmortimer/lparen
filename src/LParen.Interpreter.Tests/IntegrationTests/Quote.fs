module LParen.Interpreter.Tests.IntegrationTests.Quote

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Environment
open LParen.Interpreter.Tests.Helpers

[<Fact>]
let ``Symbols can be evaluated and quoted`` () =
    
    let environment = createEnvironment()
    
    let defineXInput = "(define x 4)"
    let defineXExpectedOutput = Atom.Symbol "x"
    
    let envWithX = evaluateSingleExpressionAndAssertEqualsWithEnvironment defineXInput defineXExpectedOutput environment
    
    let evalXInput = "x"
    let evalXOutput = Atom.Integer 4
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment evalXInput evalXOutput envWithX |> ignore
    
    let quoteXInput = "(quote x)"
    let quoteXOutput = Atom.Symbol "x"
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment quoteXInput quoteXOutput envWithX
   

[<Fact>]
let ``Quoted symbols do not have to be defined`` () =
    
    let environment = createEnvironment()
    
    let quoteZInput = "(quote z)"
    let expectedQuoteZOutput = Atom.Symbol "z"
    
    evaluateSingleExpressionAndAssertEqualsWithEnvironment quoteZInput expectedQuoteZOutput environment