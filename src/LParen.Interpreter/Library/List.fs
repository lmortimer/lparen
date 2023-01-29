module LParen.Interpreter.Library.List

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
    
    param
    |> eval
    |> validateListOrFailWith $"head expects a List. Instead got called on value: {param}"
    |> List.head
    
// Return a list excluding the first element. Returns an error if the list is empty.
// eg.
// >> (tail (list 1 2 3))
// (2 3)
let tailForm (param: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    param
    |> eval
    |> validateListOrFailWith $"tail expects a List. Instead got called on value: {param}"
    |> List.tail
    |> Atom.List

// Get the first element of a list. Returns an error if the list is empty.
// eg.
// >> (head (list 1 2 3))
// 1
let listLength (param: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    param
    |> eval
    |> validateListOrFailWith $"length expects a List. Instead got called on value: {param}"
    |> List.length
    |> Atom.Integer

// Return a boolean indicating whether the list is empty.
// eg.
// >> (empty? (list 1 2 3))
// false
// >> (empty? (list))
// true
let emptyForm (param: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    param
    |> eval
    |> validateListOrFailWith $"empty? expects a List. Instead got called on value: {param}"
    |> List.isEmpty
    |> Atom.Boolean