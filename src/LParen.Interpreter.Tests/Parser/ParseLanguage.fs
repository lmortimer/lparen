module LParen.Interpreter.Tests.Parser.ParseLanguage

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Parser
open LParen.Interpreter.Tests.Helpers

[<Fact>]
let ``Single expression parses to larger lists`` () =

    let inputString = "42"
    let expected = [Atom.Integer 42]
        
    let output = 
        match lParenParser inputString with
        | Ok x -> x
        | Error _ -> failwith "Shouldn't be here"
    
    Assert.Collection(output,
        fun elem1 -> Assert.Equal(elem1, expected.Head))

[<Fact>]
let ``Two top level expressions parse to a list of length two`` () =

    let inputString = "(+ 1 1) (+ 2 2)"
    let expected = [Atom.List [Atom.Symbol "+"; Atom.Integer 1; Atom.Integer 1]
                    Atom.List [Atom.Symbol "+"; Atom.Integer 2; Atom.Integer 2]]
        
    let output = 
        match lParenParser inputString with
        | Ok x -> x
        | Error _ -> failwith "Shouldn't be here"
    
    Assert.Equal(2, output.Length)
    Assert.Equal(expected.Head, output.Head)
    Assert.Equal(expected.Item 1, output.Item 1)
   