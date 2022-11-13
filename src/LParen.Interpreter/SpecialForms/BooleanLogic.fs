module LParen.Interpreter.SpecialForms.BooleanLogic

open LParen.Interpreter.Common


// Execute the special form define.
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
    