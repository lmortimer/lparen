module LParen.Exe.Repl

open System
open LParen.Interpreter.Environment
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval

// Opens a classical REPL. Evaluates *all* expressions and prints their results, even
// when multiple expressions are defined in a single line.
let repl () =
    let global_environment = createEnvironment()
                
    while true do
        printf ">> " 
        let input =
            match Console.ReadLine() with
            | "a" -> "(define add (lambda (x) (+ x 1)))"
            | "b" -> "(define (add x) (+ x 1))"
            | "c" -> "(cond (false false) (true true))"
            | "d" -> "(cond ((= 1 2) 2) ((= 1 1) 1))"
            | "test" -> "(+ 50 (- 2 3 2))"
            | s -> s    

        try
            lParenParser input
            |> Result.map (fun expressionsToExecute ->
               
                expressionsToExecute
                |> List.map (fun expr ->
                    let result = eval global_environment expr

                    printfn $"{result}"
                    ))
            |> ignore
        with
            | ex -> printfn $"Error: {ex.Message}"