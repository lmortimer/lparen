open System

open System.Collections.Generic

open LParen.Interpreter.Common
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval
     
let global_environment: Environment = { 
    Symbols = new Dictionary<Atom, Atom>()
}

// global_environment.Symbols[Symbol "+"] <- (+)
            
while true do
    printf ">> " 
    let input =
        match Console.ReadLine() with
        | "a" -> "(define add (lambda (x) (+ x 1)))"
        | "b" -> "(define (add x) (+ x 1))"
        | "test" -> "(+ 50 (- 2 3 2))"
        | s -> s
        
    lParenParser input
    |> Result.map (fun tokens -> printfn "%A" tokens; eval tokens global_environment)
    |> Result.map (fun s -> printfn $"{s}")
    |> Result.mapError failwith
    |> ignore