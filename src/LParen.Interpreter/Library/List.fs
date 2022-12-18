module LParen.Interpreter.Library.List

open System.Security.Cryptography
open LParen.Interpreter.Common

// Define a list.
// eg.
// (list 1 2 3)
// (list (1 2) true)
let listForm (parameters: Atom list) (eval: EvalInImplicitEnvironment): Atom =
    
    let evaluatedParameters =
        parameters
        |> List.map eval
    
    Atom.List evaluatedParameters
    
// Get the first element of a list. Returns an error if the list is empty.
// eg.
// >> (head (list 1 2 3))
// 1
let headForm (param: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    let evaluatedParameter = eval param
    
    match evaluatedParameter with
    | List x -> x.Head
    | _ -> failwith $"head expects a List. Instead got called on value: {param}"

// Return a list excluding the first element. Returns an error if the list is empty.
// eg.
// >> (tail (list 1 2 3))
// (2 3)
let tailForm (param: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    let evaluatedParameter = eval param
    
    match evaluatedParameter with
    | List x -> Atom.List x.Tail
    | _ -> failwith $"tail expects a List. Instead got called on value: {param}"

// Return a boolean indicating whether the list is empty.
// eg.
// >> (empty? (list 1 2 3))
// false
// >> (empty? (list))
// true
let emptyForm (param: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    let evaluatedParameter = eval param
    
    match evaluatedParameter with
    | List x -> Atom.Boolean x.IsEmpty
    | _ -> failwith $"empty? expects a List. Instead got called on value: {param}"

let mapFn (fn: Atom) (param: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    let callable =
        match (eval fn) with
        | Lambda x -> x
        | _ -> failwith $"map's first argument must be a callable function. Instead got {fn}."
        
    let listToMap =
        match (eval param) with
        | List x -> x
        | _ -> failwith $"map's second argument must be a list. Instead got {param}."
        
    let applied = 
        listToMap
        |> List.map (fun atom -> eval atom)
        
    Atom.List applied