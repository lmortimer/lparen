#r "nuget: FParsec"
open System
open FParsec

// run with: `dotnet fsi 01-ints-and-symbols.fsx`
// https://isthisit.nz/posts/2022/a-simple-computation-engine-in-fsharp/

type Atom =
    | Integer of int
    | Symbol of string
    | List of Atom list

let atomValue, atomValueRef = createParserForwardedToRef<Atom, unit>()

let parseSymbol =
    (pstring "+" >>% Atom.Symbol "+") <|>
    (pstring "-" >>% Atom.Symbol "-")
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
    
let rec eval(exp: Atom): Atom =

    match exp with
    | Integer x -> Atom.Integer x
    | Symbol x -> Atom.Symbol x
    | List x ->
        let operator =
            match x.Head with
            | Symbol "+" -> (+)
            | Symbol "-" -> (-)
            | _ -> failwith $"Operator {x.Head} unsupported"
            
        x.Tail
        |> List.map eval
        |> List.reduce operator
            
while true do
    printf ">> " 
    let input =
        match Console.ReadLine() with
        | "test" -> "(+ 50 (- 2 3 2))"
        | s -> s
        
    parser input
    |> Result.map eval
    |> Result.map (fun s -> printfn $"{s}")
    |> Result.mapError failwith
    |> ignore        