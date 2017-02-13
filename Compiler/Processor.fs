namespace Compiler

open Utility

module Processor =
    let buildAst (topLevel:Word) =
        let rec compileWord word icmacros icwords =
            let macros = joinMaps icmacros word.Macros
            let words = joinMaps icwords word.Words

            let mutable unnamedArgs = 0
            let popStack stack =
                match stack with
                | head :: rest -> head, rest
                | [] ->
                    unnamedArgs <- unnamedArgs - 1
                    (Unknown, ArgumentReference unnamedArgs), []
            // Returns reversed elements for simplicity sake
            let popTwo stack =
                let second, stack = popStack stack
                let first, stack = popStack stack
                first, second, stack
            // The elements returned by this are reverse of stack order (aka code order)
            let findMagic stack =
                let rec sub stack acc =
                    match stack with
                    | (Magic, magic) :: rest -> acc, Some magic, rest
                    | head :: rest -> head :: acc |> sub rest
                    | [] -> acc, None, []
                sub stack []

            let compileBinaryOp token a b =
                match token with
                | "+" -> Binary (Add, a, b)
                | "-" -> Binary (Subtract, a, b)
                | "*" -> Binary (Multiply, a, b)
                | "/" -> Binary (Divide, a, b)
                | "%" -> Binary (Modulo, a, b)
                | "&" -> Binary (And, a, b)
                | "|" -> Binary (Or, a, b)
                | "^" -> Binary (Xor, a, b)
                | _ -> raise ICE

            let rec compileNext tokens stack topLocation =
                match tokens with
                | (Token.Float x, _) :: rest -> (Type.Float, Value(Float x)) :: stack, rest, None
                | (Token.Integer x, _) :: rest -> (Type.Int, Value(Int x)) :: stack, rest, None
                | (Token.String x, _) :: rest -> (Type.String, Value(String x)) :: stack, rest, None
                | (Token.Other x, location) :: rest -> compileToken x rest location stack
                | [] -> raise (EOFError ("Reached end of word while parsing", topLocation))
            and compileToken token rest location stack =
                match token with
                | _ when macros.ContainsKey(token) ->
                    stack, macros.[token].Block @ rest, None
                | "[" -> 
                    (Magic, ArrayStart location) :: stack, rest, None
                | "]" ->
                    let values, magic, stack = findMagic stack
                    match magic with
                    | Some (ArrayStart _) -> (Type.Array (Some values.Length), Array values) :: stack, rest, None
                    | _ -> raise (SyntaxError ("Unexpected ] in word", location))
                | _ ->
                    let ostack = compileStackWord token stack
                    match ostack with
                    | Some stack -> stack, rest, None
                    | None ->
                        printfn "Stack: %A" stack
                        raise (SyntaxError ("Unknown token '" + token + "'", location))
            and compileStackWord token stack =
                match token with
                | "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" ->
                    let a, b, stack = popTwo stack
                    Some ((Unknown, compileBinaryOp token a b) :: stack)
                | "swap" ->
                    let a, b, stack = popTwo stack
                    Some (a :: b :: stack)
                | _ -> None
            
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
        
        compileWord topLevel Map.empty Map.empty