module LParen.Interpreter.Tests.Helpers

open Xunit
open System.Collections.Generic

open LParen.Interpreter.Common
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval

let evaluateAndAssertEqualsWithEnvironment (input: string) (expectedOutput: Atom) (environment: Environment) =    
    
    let output =
        lParenParser input
        |> Result.map (fun tokens -> eval tokens environment)
        
    match output with
    | Ok output -> Assert.Equal(expectedOutput, output)
    | Error e -> failwith e
    
    environment

let evaluateAndAssertEquals (input: string) (expectedOutput: Atom) =
    let environment: Environment = { 
        Symbols = Dictionary<Atom, Atom>()
    }
    
    let output =
        lParenParser input
        |> Result.map (fun tokens -> eval tokens environment)
        
    match output with
    | Ok output -> Assert.Equal(expectedOutput, output)
    | Error e -> failwith e