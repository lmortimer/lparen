module LParen.Interpreter.Environment

open LParen.Interpreter.Common

let find (symbol: Atom) (environment: Environment): Atom option =
    
    match environment.Symbols.ContainsKey(symbol) with
        | true -> Some environment.Symbols[symbol]
        | false -> None