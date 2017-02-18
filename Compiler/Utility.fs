namespace Compiler

module Utility = 
    let joinMaps (p:Map<'a, 'b>) (q:Map<'a, 'b>) =
        Map(Seq.concat [ Map.toSeq p; Map.toSeq q ])
    
    let stringToList (x:string) = [for c in x -> string c]

    let listToString (x:string list) = 
        let sb = System.Text.StringBuilder(x.Length)
        x |> List.iter (sb.Append >> ignore)
        sb.ToString()
    
    let joinString (sep:string) (x:string list) =
        let rec sub first inp acc =
            match inp with
            | head::rest ->
                sub false rest (if first then head else acc + sep + head)
            | [] -> acc
        
        sub true x ""
    
    let tokensToString (tokens:LocatedToken list) =
        let trans value =
            match value with
            | Token.Float x, _ -> sprintf "%f" x
            | Token.Integer x, _ -> sprintf "%i" x
            | Token.String x, _ -> sprintf "%A" x
            | Other x, _ -> x
        
        tokens |> List.map trans |> joinString " "
    
    let rec astToString (node:TypedNode) =
        let typeToString typ =
            match typ with
            | Unknown -> "unknown"
        
        let joinChildren children =
            let full = children |> joinString "\n"
            let sub = List.map (fun x -> "  " + x) (Array.toList (full.Split [| '\n' |]))
            sub |> joinString "\n"
        
        let childBuilder x =
            (x |> List.map astToString |> joinChildren)
        
        let _, _, node = node

        match node with
        | Value x -> sprintf "- value: %A" x
        | WordReference x -> "- word-reference " + x
        | LocalReference x -> "- local " + x
        | GlobalReference (x, flags) -> sprintf "- global %s (%A)" x flags
        | ArgumentReference x -> sprintf "- argument %i" x
        | Block x ->
            "- block:\n" + childBuilder x
        | RawBlock x -> sprintf "- raw-block: { %s }" (tokensToString x)
        | Unary (op, rvalue) ->
            sprintf "- unary %A\n%s" op (childBuilder [rvalue])
        | Binary (op, lvalue, rvalue) ->
            sprintf "- binary %A\n%s" op (childBuilder [lvalue; rvalue])
        | Cast (typ, rvalue) ->
            sprintf "- cast to %s\n%s" (typeToString typ) (childBuilder [rvalue])
        | Call (callee, args) ->
            "- call:\n" + childBuilder (callee :: args)
        | Array elems ->
            "- array\n" + childBuilder elems
        | Index (root, index) ->
            "- index:\n" + childBuilder [root; index]
        | MemberReference (root, memb) ->
            "- member:\n" + childBuilder [root] + "\n  - " + memb
        | Assignment (target, rvalue) ->
            "- assignment:\n" + childBuilder [target; rvalue]
        | _ -> sprintf "- %A" node