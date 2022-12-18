module LParen.Interpreter.Eval

open LParen.Interpreter.Common
open LParen.Interpreter.Environment
open LParen.Interpreter.Library.Apply
open LParen.Interpreter.Library.Define
open LParen.Interpreter.Library.Lambda
open LParen.Interpreter.Library.List
open LParen.Interpreter.Library.BooleanLogic
open LParen.Interpreter.Library.Math
open LParen.Interpreter.Library.Quote

let rec eval: Eval = fun (environment: Environment) (exp: Atom) ->

    // partially apply the current environment. most library functions need this and it saves having to pass eval and
    // the environment everywhere
    let evalWithCurrentEnvironment: EvalInImplicitEnvironment = eval environment
    
    match exp with
    | Integer x -> Atom.Integer x
    | Boolean x -> Atom.Boolean x
    | List [Symbol "define"; firstArg; secondArg] -> define firstArg secondArg environment eval
    | List [Symbol "quote"; atom] -> quote atom
    | List [Symbol "lambda"; parameters; body] -> lambda parameters body environment
    | List [Symbol "="; firstArg; secondArg] -> atomEquality firstArg secondArg
    | List [Symbol ">"; firstArg; secondArg] -> atomGreaterThan (evalWithCurrentEnvironment firstArg) (evalWithCurrentEnvironment secondArg)
    | List [Symbol "<"; firstArg; secondArg] -> atomLessThan (evalWithCurrentEnvironment firstArg) (evalWithCurrentEnvironment secondArg)
    | List [Symbol "if"; predicate; consequent; alternative] -> ifForm predicate consequent alternative evalWithCurrentEnvironment
    | List x when x.Head = Symbol "and" -> andFormShortCircuit x.Tail evalWithCurrentEnvironment
    | List x when x.Head = Symbol "or" -> orFormShortCircuit x.Tail evalWithCurrentEnvironment
    | List x when x.Head = Symbol "cond" -> condForm x.Tail evalWithCurrentEnvironment
    // List forms
    | List x when x.Head = Symbol "list" -> listForm x.Tail evalWithCurrentEnvironment
    | List [Symbol "head"; atomList] -> headForm atomList evalWithCurrentEnvironment
    | List [Symbol "tail"; atomList] -> tailForm atomList evalWithCurrentEnvironment
    | List [Symbol "empty?"; atomList] -> emptyForm atomList evalWithCurrentEnvironment
    | List x when x.Head = Symbol "apply" -> applyForm x.Tail eval environment
    | List [Symbol "map"; fn; atomList] -> mapFn fn atomList evalWithCurrentEnvironment

    // builtins
    | List x when List.exists (fun v -> x.Head = v) [Atom.Symbol "+"; Atom.Symbol "-"] ->
        let operator =
            match x.Head with
            | Symbol "+" -> (+)
            | Symbol "-" -> (-)
            | _ -> failwith $"Operator {x.Head} unsupported"
            
        x.Tail
        |> List.map evalWithCurrentEnvironment
        |> List.reduce operator
    // user defined symbols
    | Symbol s ->
        match find (Symbol s) environment with
        | Some symbol -> symbol
        | None -> failwith $"Could not locate symbol {s}"
    // call a lambda    
    | List x -> applyForm x eval environment
    | Lambda x -> failwith $"Evaluating Lambda with {x}"