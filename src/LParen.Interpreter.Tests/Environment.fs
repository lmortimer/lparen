module LParen.Interpreter.Tests.Environment

open Xunit

open LParen.Interpreter.Common
open LParen.Interpreter.Environment

[<Fact>]
let ``Can set and and find symbol in local environment`` () =
    let environment = createEnvironment()
    
    environment.Symbols.Add("Foo", Symbol "Bar")
    
    Assert.Equal(Some (Symbol "Bar"), find (Symbol "Foo") environment)
    
[<Fact>]
let ``find returns None when symbol does not exist`` () =
    let environment = createEnvironment()
        
    Assert.Equal(None, find (Symbol "Foo") environment)
    
[<Fact>]
let ``Find locates symbol in parent environment`` () =
    let parentEnvironment = createEnvironment()
    
    let childEnvironment = createEnvironmentWithParent parentEnvironment
    
    parentEnvironment.Symbols.Add("Foo", Symbol "Bar")
    
    Assert.Equal(Some (Symbol "Bar"), find (Symbol "Foo") childEnvironment)
    
[<Fact>]
let ``Find returns lowest level environment symbol`` () =
    let parentEnvironment = createEnvironment()
    
    let childEnvironment = createEnvironmentWithParent parentEnvironment
    
    childEnvironment.Symbols.Add("Foo", Symbol "Barchild")
    parentEnvironment.Symbols.Add("Foo", Symbol "Barparent")
    
    Assert.Equal(Some (Symbol "Barchild"), find (Symbol "Foo") childEnvironment)