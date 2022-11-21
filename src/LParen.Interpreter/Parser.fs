module LParen.Interpreter.Parser

open LParen.Interpreter.Common
open FParsec

// customising error messages https://www.quanttec.com/fparsec/users-guide/customizing-error-messages.html

// let parseString =
//     skipChar '"' >>. manyChars (noneOf "\"") .>> skipChar '"' |>> Unit.String

// let parseQuote =
    // skipChar '\'' >>. lispValue |>> fun x -> Unit.List [Unit.Atom "quote"; x]

let parseBoolean =
    (pstring "true" >>% Atom.Boolean true) <|>
    (pstring "false" >>% Atom.Boolean false) 

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
    parseBoolean
    parseSymbol
    parseInteger
    parseList
]


let parseSingleExpression = spaces >>. atomValue .>> spaces

let parseManyExpressions = many parseSingleExpression

// for general use of the language
let lParenParser str =
    
    match run parseManyExpressions str with
    | Success (h, _, _) -> Microsoft.FSharp.Core.Ok h
    | Failure (errorMsg, _, _) -> Microsoft.FSharp.Core.Error errorMsg

// used in testing
let singleExpressionParser str =
    
    match run parseSingleExpression str with
    | Success (h, _, _) -> Microsoft.FSharp.Core.Ok h
    | Failure (errorMsg, _, _) -> Microsoft.FSharp.Core.Error errorMsg