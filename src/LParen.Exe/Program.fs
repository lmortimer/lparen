open Argu

open LParen.Exe.ExecuteFile
open LParen.Exe.Repl
 
type Arguments =
    | File of path:string
    
    interface IArgParserTemplate with
            member s.Usage =
                match s with
                | File _ -> "Run a file in the interpreter"
         
[<EntryPoint>]
let main argv = 
             
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.ParseCommandLine argv
    
    let filePath = results.TryGetResult(Arguments.File)

    match filePath with
    | Some path -> executeFile path
    | None -> repl()
    
    0