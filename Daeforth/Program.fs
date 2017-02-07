open System
open Argu
open Compiler

type LanguageBackend = 
    | GLSL
    | HLSL
    | CPP 

type Arguments = 
    | Language of LanguageBackend
    | [<MainCommand; ExactlyOnce; Last>] Filename of filename:string
with
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Language _ -> "specify the language backend"
            | Filename _ -> "specify the file to compile"

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "daeforth", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    0 // return an integer exit code
