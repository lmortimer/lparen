module LParen.Interpreter.Environment

open LParen.Interpreter.Common

let rec find (symbol: Atom) (environment: Environment): Atom option =
    
    match environment.Symbols.ContainsKey(symbol) with
        | true -> Some environment.Symbols[symbol]
        | false ->
             match environment.Parent with
             | Some parentEnvironment -> find symbol parentEnvironment
             | None -> None