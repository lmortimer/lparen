module LParen.Exe.ExecuteFile

open System.IO

open LParen.Interpreter.Environment
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval

// Execute a file by path. Evaluates all expressions in the file and only prints the
// result of the last expression
let executeFile (filename: string) =
    
    let global_environment = createEnvironment()
    
    let contents = File.ReadAllText(filename)
        
    try
        lParenParser contents
        |> Result.map (fun expressionsToExecute ->
            expressionsToExecute
            |> List.map (fun expr -> eval expr global_environment)
            |> List.tryLast
            |> Option.map (fun f -> printfn "%A" f)
        )

        |> ignore
    with
        | ex -> printfn $"Error: {ex.Message}"