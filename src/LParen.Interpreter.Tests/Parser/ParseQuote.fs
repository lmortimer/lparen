module LParen.Interpreter.Tests.Parser.Quote

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Parser

[<Fact>]
let ``Quoted expression parses to a list with quoted symbol`` () =
    
    let inputString = "'42"
    let expected = [Atom.List [Atom.Symbol "quote"; Atom.Integer 42]]
    
    let output =
        match lParenParser inputString with
        | Ok x -> x
        | Error _ -> failwith "Shouldn't be here"
        
    Assert.Collection(output,
                      fun elem1 -> Assert.Equal(expected.Head, elem1))