module LParen.Interpreter.Environment

open System.Collections.Generic

open LParen.Interpreter.Common

let createEnvironment (): Environment =
    { 
        Symbols = Dictionary<string, Atom>()
        Parent = None
    }
let createEnvironmentWithParent (parent: Environment): Environment =
    { 
        Symbols = Dictionary<string, Atom>()
        Parent = Some parent
    }

let rec find (symbol: Atom) (environment: Environment): Atom option =
    
    let symbolName =
        match symbol with      // add
        | Symbol s -> s
        | _ -> failwith $"symbol lookup expects Atom.Symbol. Instead got value: {symbol}"
    
    match environment.Symbols.ContainsKey(symbolName) with
        | true -> Some environment.Symbols[symbolName]
        | false ->
             match environment.Parent with
             | Some parentEnvironment -> find symbol parentEnvironment
             | None -> None