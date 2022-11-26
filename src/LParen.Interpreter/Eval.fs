module LParen.Interpreter.Eval

open LParen.Interpreter.Common
open LParen.Interpreter.Library.Define
open LParen.Interpreter.Library.Lambda
open LParen.Interpreter.Library.BooleanLogic
open LParen.Interpreter.Library.Math

let rec eval: Eval = fun (exp: Atom) (environment: Environment) ->

    match exp with
    | Integer x -> Atom.Integer x
    | Boolean x -> Atom.Boolean x
    | List [Symbol "define"; firstArg; secondArg] -> define firstArg secondArg environment eval
    | List [Symbol "lambda"; parameters; body] -> lambda parameters body environment
    | List [Symbol "="; firstArg; secondArg] -> atomEquality (eval firstArg environment) (eval secondArg environment)
    | List [Symbol ">"; firstArg; secondArg] -> atomGreaterThan (eval firstArg environment) (eval secondArg environment)
    | List [Symbol "<"; firstArg; secondArg] -> atomLessThan (eval firstArg environment) (eval secondArg environment)
    | List [Symbol "if"; predicate; consequent; alternative] -> ifForm predicate consequent alternative environment eval
    | List x when x.Head = Symbol "and" -> andFormShortCircuit x.Tail environment eval
    | List x when x.Head = Symbol "or" -> orFormShortCircuit x.Tail environment eval
    | List x when x.Head = Symbol "cond" -> condForm x.Tail environment eval

        
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