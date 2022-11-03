#r "nuget: FParsec"
open System
open System.Collections.Generic
open FParsec

type Atom =
    | Integer of int
    | Symbol of string
    | List of Atom list
    | Lambda of Lambda
and
    Lambda = {
        Parameters: Atom list
        Body: Atom
        Environment: Environment
    }
and 
    Environment = {
        Symbols: Dictionary<Atom, Atom>
    }

let atomValue, atomValueRef = createParserForwardedToRef<Atom, unit>()

let parseSymbol =
    (pstring "+" >>% Atom.Symbol "+") <|>
    (pstring "-" >>% Atom.Symbol "-") <|>
    (regex "^([!#$%&*+./:<=>?@^_~a-zA-Z][!#$%&*+./:<=>?@^_~a-zA-Z0-9]*)" |>> Atom.Symbol)
    <?> "Could not parse symbol"

let parseInteger = pint32 |>> Atom.Integer

let parseList = 
    skipChar '(' >>. spaces >>. 
    (attempt (sepBy atomValue spaces1) <|> sepEndBy1 atomValue spaces1)
    .>> spaces .>> skipChar ')' 
    |>> Atom.List

do atomValueRef.Value <- choice [ 
    parseSymbol
    parseInteger
    parseList
]

let parser str =
    let languageParser = spaces >>. many atomValue .>> spaces
    
    match run languageParser str with
    | Success (h::_, _, _) -> Microsoft.FSharp.Core.Ok h
    | Failure (errorMsg, _, _) -> Microsoft.FSharp.Core.Error errorMsg
    | _ -> failwith "Shouldn't be here"

let inline (+) (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> Atom.Integer (x + y)
    | _ -> failwith "Add only supports integers"
    
let inline (-) (x: Atom) (y:Atom) =
    match (x, y) with
    | (Integer x, Integer y) -> Atom.Integer (x - y)
    | _ -> failwith "Subtract only supports integers"
    
let rec eval(exp: Atom) (environment: Environment): Atom =

    match exp with
    | Integer x -> Atom.Integer x
    | List x when x.Head = Atom.Symbol "define" ->
        let symbol = x.Item(1)
        let value = x.Item(2)
        environment.Symbols[symbol] <- eval value environment
        symbol
    | List [Symbol "lambda"; parameters; body] ->

        let parsedParameters =
            match parameters with
            | List atoms -> atoms
            | _ -> failwith $"Expected lambda parameters to be a list of symbols"
        
        Atom.Lambda {
            Parameters = parsedParameters
            Body = body
            Environment = environment
        }
        
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
        // failwith $"Attempting to locate symbol {s}"
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
  

            
let global_environment: Environment = { 
    Symbols = new Dictionary<Atom, Atom>()
}

// global_environment.Symbols[Symbol "+"] <- (+)
            
while true do
    printf ">> " 
    let input =
        match Console.ReadLine() with
        | "a" -> "(define add (lambda (x) (+ x 1)))"
        | "test" -> "(+ 50 (- 2 3 2))"
        | s -> s
        
    parser input
    |> Result.map (fun tokens -> printfn "%A" tokens; eval tokens global_environment)
    |> Result.map (fun s -> printfn $"{s}")
    |> Result.mapError failwith
    |> ignore
    