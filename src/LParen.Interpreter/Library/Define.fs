module LParen.Interpreter.Library.Define

open LParen.Interpreter.Common

// Execute the special form define.
// eg.
// (define x 100)
// (define (add x y) (+ x y))
let define (firstArg: Atom) (secondArg:Atom) (environment: Environment) (eval: Eval): Atom =
    
    // define can be in one of two formats
    match firstArg with
    
    // 1. assigning a value to a symbol. eg: (define x 100)
    | Symbol s ->
        
        let symbol = firstArg // x
        let symbolName = // x
            match symbol with
            | Symbol s -> s
            | _ -> failwith $"define expects the symbol name to be an Atom.Symbol. Instead got value: {firstArg}"
            
        let value = secondArg   // 100
        environment.Symbols[symbolName] <- eval value environment
        symbol
    // 2. defining a function. eg: (define (add x y) (+ x y))
    | List _ ->
                    
        // if the first arg is a list then extract the Atoms
        let parsedFirstArg =
            match firstArg with
            | List atoms -> atoms
            | _ -> failwith $"First argument to define expected to be a list of Atoms"
    
        let symbol = parsedFirstArg.Head // add
        let symbolName =
            match symbol with      // add
            | Symbol s -> s
            | _ -> failwith $"define expects the symbol name to be an Atom.Symbol. Instead got value: {firstArg}"
            
        let parameters = parsedFirstArg.Tail // [x; y]
               
        let func = Atom.Lambda {
            Parameters = parameters
            Body = secondArg
            Environment = environment
        }
        
        environment.Symbols[symbolName] <- func
        
        symbol
    | _ -> failwith "define must be called with two arguments"
    