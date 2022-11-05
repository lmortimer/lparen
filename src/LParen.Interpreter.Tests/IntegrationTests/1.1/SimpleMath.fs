module LParen.Interpreter.Tests.IntegrationTests._1._1.SimpleMath

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Tests.Helpers


[<Fact>]
let ``Can add numbers`` () =
    
    let input = "(+ 137 349)"
    let expectedOutput = Atom.Integer 486
    
    evaluateAndAssertEquals input expectedOutput
    
[<Fact>]
let ``Can evaluate embedded expressions`` () =
    
    let input = "(+ (+ 1 2) (- 2 1))"
    let expectedOutput = Atom.Integer 4
    
    evaluateAndAssertEquals input expectedOutput
    
[<Fact>]
let ``Can define symbols`` () =
    
    let input = "(define x 4)"
    let expectedOutput = Atom.Integer 4
    
    evaluateAndAssertEquals input expectedOutput