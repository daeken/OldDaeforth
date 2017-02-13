namespace Compiler

open Utility

module Processor =
    let buildAst (topLevel:Word) =
        let rec compileWord word icmacros icwords (icmlocals : Map<string, TypedNode>) =
            let macros = joinMaps icmacros word.Macros
            let words = joinMaps icwords word.Words

            let mutable mlocals = icmlocals

            let mutable unnamedArgs = 0
            let pop stack =
                match stack with
                | head :: rest -> head, rest
                | [] ->
                    unnamedArgs <- unnamedArgs - 1
                    (Unknown, Location.Generated, ArgumentReference unnamedArgs), []
            // Returns reversed elements for simplicity sake
            let popTwo stack =
                let second, stack = pop stack
                let first, stack = pop stack
                first, second, stack
            // The elements returned by this are reverse of stack order (aka code order)
            let findMagic stack =
                let rec sub stack acc =
                    match stack with
                    | (Magic, _, magic) :: rest -> acc, Some magic, rest
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
                | (Token.Float x, location) :: rest -> (Type.Float, location, Value(Float x)) :: stack, rest, None
                | (Token.Integer x, location) :: rest -> (Type.Int, location, Value(Int x)) :: stack, rest, None
                | (Token.String x, location) :: rest -> (Type.String, location, Value(String x)) :: stack, rest, None
                | (Token.Other x, location) :: rest -> compileToken x rest location stack
                | [] -> raise (EOFError ("Reached end of word while parsing", topLocation))
            and compileToken token rest location stack =
                match token with
                | _ when macros.ContainsKey(token) ->
                    stack, macros.[token].Block @ rest, None
                | _ when token.StartsWith("=>") ->
                    let rv, stack = pop stack
                    mlocals <- mlocals.Add(token.Substring(2), rv)
                    stack, rest, None
                | _ when token.StartsWith("=") ->
                    let rv, stack = pop stack
                    let lv = Unknown, LocalReference (token.Substring(1))
                    stack, rest, Some (Type.Unit, location, Assignment (lv, rv))
                | _ ->
                    let ostack = compileStackWord token stack location
                    match ostack with
                    | Some stack -> stack, rest, None
                    | None ->
                        printfn "Stack: %A" stack
                        raise (SyntaxError ("Unknown token '" + token + "'", location))
            and compileStackWord token stack location =
                match token with
                | _ when mlocals.ContainsKey(token) ->
                    Some (mlocals.[token] :: stack)
                | "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" ->
                    let a, b, stack = popTwo stack
                    Some ((Unknown, location, compileBinaryOp token a b) :: stack)
                | "[" -> Some ((Magic, location, ArrayStart) :: stack)
                | "]" ->
                    let values, magic, stack = findMagic stack
                    match magic with
                    | Some ArrayStart -> Some ((Type.Array (Some values.Length), location, Array values) :: stack)
                    | _ -> raise (SyntaxError ("Unexpected ] in word", location))
                | "swap" ->
                    let a, b, stack = popTwo stack
                    Some (a :: b :: stack)
                | "dup" ->
                    let x, stack = pop stack
                    Some (x :: x :: stack)
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
        
        compileWord topLevel Map.empty Map.empty Map.empty