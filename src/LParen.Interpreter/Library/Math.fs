module LParen.Interpreter.Library.Math

open LParen.Interpreter.Common

let inline (+) (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> Atom.Integer (x + y)
    | _ -> failwith "Add only supports integers"
    
let inline (-) (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> Atom.Integer (x - y)
    | _ -> failwith "Subtract only supports integers"
        
let atomEquality (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> if x = y then Atom.Boolean true else Atom.Boolean false
    | _ -> failwith "= only supports integers"
    
let atomGreaterThan (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> if x > y then Atom.Boolean true else Atom.Boolean false
    | _ -> failwith "> only supports integers"
    
let atomLessThan (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> if x < y then Atom.Boolean true else Atom.Boolean false
    | _ -> failwith "< only supports integers"
        