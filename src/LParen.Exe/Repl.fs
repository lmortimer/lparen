module LParen.Exe.Repl

open System
open System.Collections.Generic

open LParen.Interpreter.Common
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval

let repl () =
    let global_environment: Environment = { 
        Symbols = Dictionary<Atom, Atom>()
    }
                
    while true do
        printf ">> " 
        let input =
            match Console.ReadLine() with
            | "a" -> "(define add (lambda (x) (+ x 1)))"
            | "b" -> "(define (add x) (+ x 1))"
            | "test" -> "(+ 50 (- 2 3 2))"
            | s -> s    
            
        try
            lParenParser input
            |> Result.map (fun tokens -> eval tokens global_environment)
            |> Result.map (fun s -> printfn $"{s}")
            |> ignore
        with
            | ex -> printfn $"Error: {ex.Message}"