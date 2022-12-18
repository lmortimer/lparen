module LParen.Interpreter.Tests.Helpers

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Environment
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval

// for use in integration tests
let evaluateSingleExpressionAndAssertEqualsWithEnvironment (input: string) (expectedOutput: Atom) (environment: Environment) =    
    
    let output =
        singleExpressionParser input
        |> Result.map (fun tokens -> eval environment tokens)
        
    match output with
    | Ok output -> Assert.Equal(expectedOutput, output)
    | Error e -> failwith e
    
    environment
    
// for use in integration tests
let evaluateSingleExpressionAndAssertEquals (input: string) (expectedOutput: Atom) =
    let environment = createEnvironment()
    
    let output =
        singleExpressionParser input
        |> Result.map (fun tokens -> eval environment tokens)
        
    match output with
    | Ok output -> Assert.Equal(expectedOutput, output)
    | Error e -> failwith e
    
// for use in parser tests
let parsesAndEquals (input: string) (expectedOutput: Atom) =
    
    singleExpressionParser input
    |> Result.map (fun tokens -> Assert.Equal(expectedOutput, tokens))
    |> ignore