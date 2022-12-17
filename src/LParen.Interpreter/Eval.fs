module LParen.Interpreter.Eval

open LParen.Interpreter.Common
open LParen.Interpreter.Environment
open LParen.Interpreter.Library.Define
open LParen.Interpreter.Library.Lambda
open LParen.Interpreter.Library.List
open LParen.Interpreter.Library.BooleanLogic
open LParen.Interpreter.Library.Math
open LParen.Interpreter.Library.Quote

let rec eval: Eval = fun (exp: Atom) (environment: Environment) ->

    match exp with
    | Integer x -> Atom.Integer x
    | Boolean x -> Atom.Boolean x
    | List [Symbol "define"; firstArg; secondArg] -> define firstArg secondArg environment eval
    | List [Symbol "quote"; atom] -> quote atom
    | List [Symbol "lambda"; parameters; body] -> lambda parameters body environment
    | List [Symbol "="; firstArg; secondArg] -> atomEquality (eval firstArg environment) (eval secondArg environment)
    | List [Symbol ">"; firstArg; secondArg] -> atomGreaterThan (eval firstArg environment) (eval secondArg environment)
    | List [Symbol "<"; firstArg; secondArg] -> atomLessThan (eval firstArg environment) (eval secondArg environment)
    | List [Symbol "if"; predicate; consequent; alternative] -> ifForm predicate consequent alternative environment eval
    | List x when x.Head = Symbol "and" -> andFormShortCircuit x.Tail environment eval
    | List x when x.Head = Symbol "or" -> orFormShortCircuit x.Tail environment eval
    | List x when x.Head = Symbol "cond" -> condForm x.Tail environment eval
    // List forms
    | List x when x.Head = Symbol "list" -> listForm x.Tail environment eval
    | List [Symbol "head"; atomList] -> headForm atomList environment eval
    | List [Symbol "tail"; atomList] -> tailForm atomList environment eval
    | List [Symbol "empty?"; atomList] -> emptyForm atomList environment eval

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
        match find (Symbol s) environment with
        | Some symbol -> symbol
        | None -> failwith $"Could not locate symbol {s}"
    // call a lambda    
    | List x ->
        let symbol = x.Head
        let args = x.Tail
                
        // find the lambda we previously defined
        let callable =
            match find symbol environment with
            | Some symbol -> symbol
            | None -> failwith $"Could not locate callable symbol {symbol}"
        
        // evaluate the arguments being passed into the function call, these may themselves be lambda calls
        let evaluatedArgs =
            args
            |> List.map (fun atom -> eval atom environment)
        
        // when executing a lambda we create its own environment (execution context) so that
        // symbols local to the lambda don't pollute anything higher in the stack.
        // we assign the keys of the lambda's parameter symbols with the evaluatedArgs passed
        // in the function call. we match these by order then call the function itself
        let lambdaEnvironment = createEnvironmentWithParent environment
        
        match callable with
        | Lambda lambda ->
            
            List.zip lambda.Parameters evaluatedArgs
            |> List.iter (fun (parameter, value) ->
                let parameterName =
                    match parameter with      // add
                    | Symbol s -> s
                    | _ -> failwith $"Lambda parameter Symbols are expected to be Atom.Symbol. Instead got: {parameter} = {value}"
                lambdaEnvironment.Symbols[parameterName] <- value)
            
            eval lambda.Body lambdaEnvironment
        | _ -> failwith $"Symbol {symbol} is not callable"
    | Lambda x -> failwith $"Lambda with {x}"