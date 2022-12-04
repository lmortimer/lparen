module LParen.Interpreter.Common

open System.Collections.Generic

type Atom =
    | Integer of int
    | Symbol of string
    | Boolean of bool
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
        Symbols: Dictionary<string, Atom>
        Parent: Environment option
    }

// Evaluates an Atom in an Environment
type Eval = Atom -> Environment -> Atom
