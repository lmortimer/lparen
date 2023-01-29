module LParen.Interpreter.Library.Define

open LParen.Interpreter.Common

// Execute the special form define.
// eg.
// (define x 100)
// (define (add x y) (+ x y))
let define (firstArg: Atom) (secondArg: Atom) (environment: Environment) (eval: Eval): Atom =
    
    // define can be in one of two formats
    match firstArg with
    
    // 1. assigning a value to a symbol. eg: (define x 100)
    | Symbol symbol ->
        
        let value = eval environment secondArg
        environment.Symbols[symbol] <- value
        Atom.Symbol symbol
    // 2. defining a function. eg: (define (add x y) (+ x y))
    | List symbols ->

        let symbol =
            symbols.Head // add
            |> validateSymbolOrFailWith $"define expects the symbol name to be an Atom.Symbol. Instead got value: {firstArg}"
            
        let parameters = symbols.Tail // [x; y]
               
        let func = Atom.Lambda {
            Parameters = parameters
            Body = secondArg
            Environment = environment
        }
        
        environment.Symbols[symbol] <- func
        
        Atom.Symbol symbol
    | _ -> failwith "define must be called with two arguments: symbol name and symbol value"
    