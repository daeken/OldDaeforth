open System
open System.IO
open Argu
open Compiler

type LanguageBackend = 
    | GLSL
    | HLSL
    | CPP 

type Arguments = 
    | [<EqualsAssignment>] Language of LanguageBackend
    | [<MainCommand; ExactlyOnce; Last>] Filename of filename:string
with
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Language _ -> "specify the language backend"
            | Filename _ -> "specify the file to compile"

let compile lang (fn : string) =
    use sr = new StreamReader(fn)
    let code = sr.ReadToEnd()
    
    let wordtree = Tokenizer.tokenize fn code |> Parser.parse
    printfn "%A" wordtree
    ()

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "daeforth", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let fn = results.GetResult(<@ Filename @>)
    let lang = results.GetResult(<@ Language @>, defaultValue=LanguageBackend.GLSL)

    try
        compile lang fn
        0
    with
    | :? FileNotFoundException as fnf ->
        printfn "File not found: %A" fnf.FileName
        1
    | EOFError(err) ->
        printfn "Premature end of file: %s" err
        1
    | SyntaxError(err, location) ->
        printfn "Syntax error in %s line %i column %i: %s" location.File (location.Line + 1) (location.Column + 1) err
        1
