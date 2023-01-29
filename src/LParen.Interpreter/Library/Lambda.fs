module LParen.Interpreter.Library.Lambda

open LParen.Interpreter.Common

let lambda (parameters: Atom) (body: Atom) (environment: Environment) =
    
    let parsedParameters =
        parameters
        |> validateListOrFailWith $"Expected lambda parameters to be a list of symbols"
    
    Atom.Lambda {
        Parameters = parsedParameters
        Body = body
        Environment = environment
    }