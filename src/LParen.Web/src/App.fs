module App

open Browser.Dom

open LParen.Interpreter.Environment
open LParen.Interpreter.Parser
open LParen.Interpreter.Eval

let repl_environment = createEnvironment()

let runButton = document.querySelector("#run") :?> Browser.Types.HTMLButtonElement
let input = document.querySelector("#source") :?> Browser.Types.HTMLTextAreaElement
let output = document.querySelector("#output") :?> Browser.Types.HTMLDivElement

runButton.onclick <- fun _ ->
    
    let inputSource = input.value
    
    lParenParser inputSource
    |> Result.map (fun expressionsToExecute ->
       
        expressionsToExecute
        |> List.map (fun expr ->
            let result = eval repl_environment expr

            output.innerText <- result.ToString()
            ))
    |> ignore
