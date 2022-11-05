module LParen.Interpreter.Parser

open LParen.Interpreter.Common
open FParsec

// customising error messages https://www.quanttec.com/fparsec/users-guide/customizing-error-messages.html

// let parseString =
//     skipChar '"' >>. manyChars (noneOf "\"") .>> skipChar '"' |>> Unit.String

// let parseQuote =
    // skipChar '\'' >>. lispValue |>> fun x -> Unit.List [Unit.Atom "quote"; x]

// let parseReserved=
//     (pstring "Nil" >>% Unit.Nil) <|>
//     (pstring "#t" >>% Unit.Bool true) <|>
//     (pstring "#f" >>% Unit.Bool false) 


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

let lParenParser str =
    let languageParser = spaces >>. many atomValue .>> spaces
    
    match run languageParser str with
    | Success (h::_, _, _) -> Microsoft.FSharp.Core.Ok h
    | Failure (errorMsg, _, _) -> Microsoft.FSharp.Core.Error errorMsg
    | _ -> failwith "Shouldn't be here"
