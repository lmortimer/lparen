module LParen.Interpreter.Eval

open LParen.Interpreter.Common

let inline (+) (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> Atom.Integer (x + y)
    | _ -> failwith "Add only supports integers"
    
let inline (-) (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> Atom.Integer (x - y)
    | _ -> failwith "Subtract only supports integers"
    
let rec eval(exp: Atom) (environment: Environment): Atom =

    match exp with
    | Integer x -> Atom.Integer x
    | List [Symbol "define"; firstArg; secondArg] ->
        
        // define can be in one of two formats
        match firstArg with
        
        // 1. assigning a value to a symbol. eg: (define x 100)
        | Symbol s -> 
            let symbol = firstArg   // x
            let value = secondArg   // 100
            environment.Symbols[symbol] <- eval value environment
            symbol
        // 2. defining a function. eg: (define (add x y) (+ x y))
        | List args ->
                        
            // if the first arg is a list then extract the Atoms
            let parsedFirstArg =
                match firstArg with
                | List atoms -> atoms
                | _ -> failwith $"First argument to define expected to be a list of Atoms"
        
            let symbolName = parsedFirstArg.Head       // add
            let lambdaParameters = parsedFirstArg.Tail // [x; y]
                   
            let func = Atom.Lambda {
                Parameters = lambdaParameters
                Body = secondArg
                Environment = environment
            }
            
            environment.Symbols[symbolName] <- func
            
            symbolName
        | _ -> failwith "define must be called with two arguments"
        
    | List [Symbol "lambda"; parameters; body] ->

        let parsedParameters =
            match parameters with
            | List atoms -> atoms
            | _ -> failwith $"Expected lambda parameters to be a list of symbols"
        
        Atom.Lambda {
            Parameters = parsedParameters
            Body = body
            Environment = environment
        }
        
    // builtins
    | List x when List.exists (fun v -> x.Head = v) [Atom.Symbol "+"; Atom.Symbol "-"] ->
        let operator =
            match x.Head with
            | Symbol "+" -> (+)
            | Symbol "-" -> (-)
            | _ -> failwith $"Operator {x.Head} unsupported"
            
        x.Tail
        |> List.map (fun atom -> eval atom environment)
        |> List.reduce operator
    // user defined symbols
    | Symbol s ->
        // failwith $"Attempting to locate symbol {s}"
        match environment.Symbols.ContainsKey(Symbol s) with
        | true -> environment.Symbols[Symbol s]
        | false -> failwith $"Could not locate symbol {s}"
    // call a lambda    
    | List x ->
        let symbol = x.Head
        let args = x.Tail
        
        // find the lambda we previously defined
        let callable =
            match environment.Symbols.ContainsKey(symbol) with
            | true -> environment.Symbols[symbol]
            | false -> failwith $"Could not locate callable symbol {symbol}"
        
        // evaluate the arguments being passed into the function call, these may themselves be lambda calls
        let evaluatedArgs =
            args
            |> List.map (fun atom -> eval atom environment)
        
        // to the global environment we assign the keys of the lambda's parameter symbols
        // with the evaluatedArgs passed in the function call. we match these by order.
        // then call the function itself 
        match callable with
        | Lambda lambda ->
            
            List.zip lambda.Parameters evaluatedArgs
            |> List.iter (fun (parameter, value) ->
                environment.Symbols[parameter] <- value)
            
            eval lambda.Body environment
        | _ -> failwith $"Symbol {symbol} is not callable"
    | Lambda x -> failwith $"Lambda with {x}"