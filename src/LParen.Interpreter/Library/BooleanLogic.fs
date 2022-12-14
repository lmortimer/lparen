module LParen.Interpreter.Library.BooleanLogic
open LParen.Interpreter.Common


// Evaluate the special form `if`.
//
// (if <predicate> <consequent> <alternative>)
//
// eg
// >> (if (= 1 1) true false)
// true
let ifForm (predicate: Atom) (consequent: Atom) (alternative: Atom) (eval: EvalInImplicitEnvironment): Atom =
    
    let evaluatedPredicate = eval predicate
    
    match evaluatedPredicate with
    | Atom.Boolean true -> eval consequent
    | Atom.Boolean false -> eval alternative
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
let andForm (expressions: Atom list) (eval: EvalInImplicitEnvironment) =
    let evaluatedExpressions =
        expressions
        |> List.map eval
        |> List.map (fun atom ->
            match atom with
            | Boolean x -> x
            | _ -> failwith $"Expressions passed to the and must evaluate to a boolean. {atom} does not.")
        |> List.reduce (&&)
        
    Atom.Boolean evaluatedExpressions
    
let andFormShortCircuit (expressions: Atom list) (eval: EvalInImplicitEnvironment) =
    
    // evaluate the atoms in parameters one by one. We stop when we get to the first false
    let hasAFalsyExpression =
        expressions
        |> List.tryFind (fun atom ->
            let evaluatedExpr = eval atom
            
            // tryFind requires the predicate function to return true when it should stop
            // confusingly this is when the atom evaluates to false
            match evaluatedExpr with
            | Atom.Boolean b when b = false -> true
            | Atom.Boolean b when b = true -> false
            | _ -> failwith $"Expressions passed to the and must evaluate to a boolean. {atom} does not.")
        
    // hasAFalsyExpression contains the first expression that evaluate to false, or None if all expressions were true 
    match hasAFalsyExpression with
    | Some _ -> Atom.Boolean false
    | None -> Atom.Boolean true
    
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
// true
let orForm (expressions: Atom list) (eval: EvalInImplicitEnvironment) =
    
    let evaluatedExpressions =
        expressions
        |> List.map eval
        |> List.map (fun atom ->
            match atom with
            | Boolean x -> x
            | _ -> failwith $"Expressions passed to `and` must evaluate to a boolean. {atom} does not.")
        |> List.reduce (||)
        
    Atom.Boolean evaluatedExpressions
    
let orFormShortCircuit (expressions: Atom list) (eval: EvalInImplicitEnvironment) =
    
    // evaluate the atoms in parameters one by one. We stop when we get to the first true
    let hasATrueExpression =
        expressions
        |> List.tryFind (fun atom ->
            let evaluatedExpr = eval atom
            
            // tryFind requires the predicate function to return true when it should stop
            // for `or`, that's when we see the first Atom.Boolean true
            match evaluatedExpr with
            | Atom.Boolean b when b = true -> true
            | Atom.Boolean b when b = false -> false
            | _ -> failwith $"Expressions passed to `or` must evaluate to a boolean. {atom} does not.")
        
    // hasAFalsyExpression contains the first expression that evaluated to true, or None if all expressions were false 
    match hasATrueExpression with
    | Some _ -> Atom.Boolean true
    | None -> Atom.Boolean false
    
// Evaluate the special form `cond`
//
//   (cond
//      (<p1> <e1>)
//      (<p2> <e2>)
//      (<pn> <en>))
//
// Evaluates all <p> and <e> expressions. Returns the first <e> where <p> is true
// Note: Scheme spec has `else`, this does not
//
// >> (cond (true 1) (true 2))
// 1
// >> (cond (false 1) (true 2))
// 2
let condForm (parameters: Atom list) (eval: EvalInImplicitEnvironment) =
    
    let predicateThatEvaluatesToTrue =
        parameters
        |> List.chunkBySize 2 // chunk to lists intending to be [<p>; <e>]
        |> List.concat
        |> List.map (fun clause -> // ensure those lists actually are [<p>; <e>]
            match clause with
            | List atoms when atoms.Length = 2 -> (atoms.Item(0), atoms.Item(1))
            | _ -> failwith $"Expected clause in `cond` to be (<predicate> <expression). Instead received: ${clause}")
        |> List.tryFind (fun (predicate, _) -> // now search for the first predicate that evaluates to true
           
            let evaluatedPredicate = eval predicate
            
            match evaluatedPredicate with
            | Atom.Boolean b when b = true -> true // tryFind returns true on this
            | Atom.Boolean b when b = false -> false
            | _ -> failwith $"Predicate passed to `cond` must evaluate to a boolean. {predicate} does not.")
        
    // predicateThatEvaluatesToTrue contains the expression for the predicate that evaluated to true
    // or None if all predicate were false 
    match predicateThatEvaluatesToTrue with
    | Some(_, expression) -> eval expression
    | None -> failwith "No predicate passed to `cond` evaluated to true."
           