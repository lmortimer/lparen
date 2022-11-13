module LParen.Interpreter.SpecialForms.BooleanLogic

open LParen.Interpreter.Common

// Execute the special form if.
// eg.
// (define x 100)
// (define (add x y) (+ x y))
let ifForm (firstArg: Atom) (secondArg:Atom) (environment: Environment) (eval: Eval): Atom =
    
    // define can be in one of two formats
    match firstArg with
    
    // 1. assigning a value to a symbol. eg: (define x 100)
    | Symbol s -> 
        let symbol = firstArg   // x
        let value = secondArg   // 100
        environment.Symbols[symbol] <- eval value environment
        symbol
    // 2. defining a function. eg: (define (add x y) (+ x y))
    | List _ ->
                    
        // if the first arg is a list then extract the Atoms
        let parsedFirstArg =
            match firstArg with
            | List atoms -> atoms
            | _ -> failwith $"First argument to define expected to be a list of Atoms"
    
        let symbol = parsedFirstArg.Head       // add
        let parameters = parsedFirstArg.Tail // [x; y]
               
        let func = Atom.Lambda {
            Parameters = parameters
            Body = secondArg
            Environment = environment
        }
        
        environment.Symbols[symbol] <- func
        
        symbol
    | _ -> failwith "define must be called with two arguments"
    