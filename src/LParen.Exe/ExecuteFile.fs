module LParen.Exe.ExecuteFile

open System.IO
open System
open System.Collections.Generic

open LParen.Interpreter.Common
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval

let executeFile (filename: string) =
    
    let global_environment: Environment = { 
        Symbols = Dictionary<Atom, Atom>()
    }
    
    let contents = File.ReadAllText(filename)
    
    printfn "%A" contents
    
    try
        lParenParser contents
        |> Result.map (fun tokens -> printfn $"{tokens}";) //eval tokens global_environment)
        |> Result.map (fun s -> printfn $"{s}")
        |> ignore
    with
        | ex -> printfn $"Error: {ex.Message}"