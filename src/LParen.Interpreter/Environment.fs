module LParen.Interpreter.Environment

open System.Collections.Generic

open LParen.Interpreter.Common

let createEnvironment (): Environment =
    { 
        Symbols = Dictionary<Atom, Atom>()
        Parent = None
    }
let createEnvironmentWithParent (parent: Environment): Environment =
    { 
        Symbols = Dictionary<Atom, Atom>()
        Parent = Some parent
    }

let rec find (symbol: Atom) (environment: Environment): Atom option =
    
    match environment.Symbols.ContainsKey(symbol) with
        | true -> Some environment.Symbols[symbol]
        | false ->
             match environment.Parent with
             | Some parentEnvironment -> find symbol parentEnvironment
             | None -> None