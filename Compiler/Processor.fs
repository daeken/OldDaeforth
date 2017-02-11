namespace Compiler

module Processor =
    let buildAst (topLevel:Word) =
        let rec compileWord word =
            let mutable unnamedArgs = 0
            let popStack stack =
                match stack with
                | head :: rest -> head, rest
                | [] ->
                    unnamedArgs <- unnamedArgs - 1
                    ArgumentReference unnamedArgs, []
            // The elements returned by this are reverse of stack order (aka code order)
            let findMagic stack =
                let rec sub stack acc =
                    match stack with
                    | (Magic, magic) :: rest -> acc, Some magic, rest
                    | head :: rest -> head :: acc |> sub rest
                    | [] -> acc, None, []
                sub stack []
            let rec compileNext tokens stack topLocation =
                match tokens with
                | (Token.Float x, _) :: rest -> (Type.Float, Value(Float x)) :: stack, rest, None
                | (Token.Integer x, _) :: rest -> (Type.Int, Value(Int x)) :: stack, rest, None
                | (Token.String x, _) :: rest -> (Type.String, Value(String x)) :: stack, rest, None
                | (Token.Other x, location) :: rest -> compileToken x rest location stack
                | [] -> raise (EOFError ("Reached end of word while parsing", topLocation))
            and compileToken token rest location stack =
                match token with
                | "[" -> 
                    (Magic, ArrayStart location) :: stack, rest, None
                | "]" ->
                    let values, magic, stack = findMagic stack
                    match magic with
                    | Some (ArrayStart _) -> (Type.Array (Some values.Length), Array values) :: stack, rest, None
                    | _ -> raise (SyntaxError ("Unexpected ] in word", location))
                | x ->
                    printfn "Stack: %A" stack
                    raise (SyntaxError ("Unknown token '" + x + "'", location))
            
            let rec compileAll tokens stack acc =
                match tokens with
                | [] -> stack, acc
                | _ ->
                    let stack, tokens, out = compileNext tokens stack Location.Generated
                    match out with
                    | Some x -> x :: acc |> compileAll tokens stack
                    | None -> compileAll tokens stack acc
            
            let {Name=name; Signature=signature; Block=block; Words=words; Macros=macros} = word
            let stack, all = compileAll block [] []

            // XXX: This should output a Word, not a block.
            Block all
        
        compileWord topLevel