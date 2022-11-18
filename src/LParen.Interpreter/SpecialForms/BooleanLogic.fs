module LParen.Interpreter.SpecialForms.BooleanLogic

open LParen.Interpreter.Common


// Evaluate the special form `define`.
//
// (if <predicate> <consequent> <alternative>)
//
// eg
// >> (if (= 1 1) true false)
// true
let ifForm (predicate: Atom) (consequent: Atom) (alternative: Atom) (environment: Environment) (eval: Eval): Atom =
    
    let evaluatedPredicate = eval predicate environment
    
    match evaluatedPredicate with
    | Atom.Boolean true -> eval consequent environment
    | Atom.Boolean false -> eval alternative environment
    | _ -> failwith $"Predicate passed to if must evaluate to a boolean. {predicate} does not."
    
// Evaluate the special form `and`
//
//   (and <e1> <e2> .... <en> )
//
// All expressions are evaluated in left to right order. Returns a boolean.
//
// >> (and true true true true)
// true
// >> (and false)
// false
let andForm (parameters: Atom list) (environment: Environment) (eval: Eval) =
    let evaluatedExpressions =
        parameters.Tail // head is the `and` Symbol, skip it
        |> List.map (fun atom -> eval atom environment)
        |> List.map (fun atom ->
            match atom with
            | Boolean x -> x
            | _ -> failwith $"Expressions passed to the and must evaluate to a boolean. {atom} does not.")
        |> List.reduce (&&)
        
    Atom.Boolean evaluatedExpressions
    
// Evaluate the special form `or`
//
//   (or <e1> <e2> .... <en> )
//
// All expressions are evaluated in left to right order. Returns a boolean.
// Note: Scheme spec is to return the first expression that evaluated to true
//
// >> (or true true true true)
// true
// >> (and false true)
// false
let orForm (parameters: Atom list) (environment: Environment) (eval: Eval) =
    
    let evaluatedExpressions =
        parameters.Tail // head is the `or` Symbol, skip it
        |> List.map (fun atom -> eval atom environment)
        |> List.map (fun atom ->
            match atom with
            | Boolean x -> x
            | _ -> failwith $"Expressions passed to the and must evaluate to a boolean. {atom} does not.")
        |> List.reduce (||)
        
    Atom.Boolean evaluatedExpressions
    
// Evaluate the special form `cond`
//
//   (cond
//      (<p1> <e1>)
//      (<p2> <e2>)
//      (<pn> <en>))
//
// Evaluates all <p> and <e> expressions. Returns the first <e> where <p> is true
//
// >> (cond (true 1) (true 2))
// 1
// >> (cond (false 1) (true 2))
// 2
// >> (cond ((false 1)) (else 2))
// 2
let condForm (parameters: Atom list) (environment: Environment) (eval: Eval) =
    
    // ensure that these are lists of size 2
    let conditionals =
        parameters.Tail // head is the `cond` Symbol, skip it
        |> List.map (fun atom ->
            match atom with
            | List x -> if x.Length = 2 then (x.Item(0), x.Item(1)) else failwith $"Expressions passed to cond must be lists of length 2. {atom} is not."
            | _ -> failwith $"Expressions passed to cond must be lists. {atom} is not.")
       
    let evaluatedConditionals =
        conditionals
        |> List.map (fun (c, e) -> (eval c environment, eval e environment))
       
    printfn "%A" conditionals
        
    Atom.Boolean true