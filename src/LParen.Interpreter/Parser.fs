module LParen.Interpreter.Parser

open LParen.Interpreter.Common
open FParsec
// customising error messages https://www.quanttec.com/fparsec/users-guide/customizing-error-messages.html

let parseString =
    skipChar '"' >>. manyChars (noneOf "\"") .>> skipChar '"' |>> Atom.String

let parseBoolean =
    (pstring "true" >>% Atom.Boolean true) <|>
    (pstring "false" >>% Atom.Boolean false) 

let atomValue, atomValueRef = createParserForwardedToRef()

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
    
let parseQuote =
    skipChar '\'' >>. atomValue |>> fun x -> Atom.List [Atom.Symbol "quote"; x]


do atomValueRef.Value <- choice [
    parseString
    parseBoolean
    parseSymbol
    parseInteger
    parseList
    parseQuote
]

let parseSingleExpression = spaces >>. atomValue .>> spaces

let parseManyExpressions = many parseSingleExpression

// for general use of the language
let lParenParser str =
    
    match run parseManyExpressions str with
    | Success (h, _, _) -> Microsoft.FSharp.Core.Ok h
    | Failure (errorValue, _, _) -> Microsoft.FSharp.Core.Error (errorValue.ToString())

// used in testing
let singleExpressionParser str =
    
    match run parseSingleExpression str with
    | Success (h, _, _) -> Microsoft.FSharp.Core.Ok h
    | Failure (errorValue, _, _) -> Microsoft.FSharp.Core.Error (errorValue.ToString())

