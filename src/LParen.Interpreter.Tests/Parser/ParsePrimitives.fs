module LParen.Interpreter.Tests.Parser.ParsersSymbolsAndAtoms

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Tests.Helpers


[<Fact>]
let ``Can parse Integers`` () =

    parsesAndEquals "42" (Atom.Integer 42)
    
[<Fact>]
let ``Can parse Symbols`` () =
    
    parsesAndEquals "hello" (Atom.Symbol "hello")
    parsesAndEquals "!hello" (Atom.Symbol "!hello")
    parsesAndEquals ":hello" (Atom.Symbol ":hello")
    parsesAndEquals ":true" (Atom.Symbol ":true")
    parsesAndEquals ":false" (Atom.Symbol ":false")
    
[<Fact>]
let ``Can parse Booleans`` () =
    
    parsesAndEquals "true" (Atom.Boolean true)
    parsesAndEquals "false" (Atom.Boolean false)