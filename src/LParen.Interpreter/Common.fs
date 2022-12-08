module LParen.Interpreter.Common

open System.Collections.Generic

type Atom =
    | Integer of int
    | Symbol of string
    | Boolean of bool
    | List of Atom list
    | Lambda of Lambda
    with override this.ToString() =
            match this with
            | Integer x -> $"{x}"
            | Symbol x -> x
            | Boolean x -> $"{x}"
            | List x -> $"{x}"
            | Lambda x -> $"Lambda with params: {x.Parameters}"
and
    Lambda = {
        Parameters: Atom list
        Body: Atom
        Environment: Environment
    }
and 
    Environment = {
        Symbols: Dictionary<string, Atom>
        Parent: Environment option
    }

// Evaluates an Atom in an Environment
type Eval = Atom -> Environment -> Atom
