#load "Tokenizer.fs"
#load "Parser.fs"
open Compiler

let test = @"
	: name ... ; (( Word accepting no defined parameters ))
	: name ( arg arg2 ) ... ; (( Word accepting two arguments, arg and arg2 ))
    [ 0 1 2 ] =[ foo bar baz ]  (( Assign foo=0, bar=1, baz=2 ))
	[ 0 1 2 ] =>[ foo bar baz ] (( Macro local assignment ))
"

printfn "%A" (Tokenizer.tokenize test)
printfn "%A" (Tokenizer.tokenize test |> Parser.stripComments)

