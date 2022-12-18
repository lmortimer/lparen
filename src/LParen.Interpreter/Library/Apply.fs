module LParen.Interpreter.Library.Apply

open LParen.Interpreter.Common
open LParen.Interpreter.Environment

let applyForm (parameters: Atom list) (eval: Eval) (environment: Environment): Atom =
    
    let symbolOfCallable = parameters.Head
    let argsOfCallable = parameters.Tail
    
    // find the lambda we predefined lambda
    let callable =
        match find symbolOfCallable environment with
        | Some symbol -> symbol
        | None -> failwith $"Could not locate callable symbol {symbolOfCallable}"
    
    // evaluate the arguments being passed into the function call, these may themselves be lambda calls    
    let evaluatedArgs =
        argsOfCallable
        |> List.map (fun atom -> eval environment atom)
    
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
        
        eval lambdaEnvironment lambda.Body
    | _ -> failwith $"Symbol {symbolOfCallable} is not callable"