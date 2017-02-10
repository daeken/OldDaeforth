namespace Compiler

type Argument = { Name : string option; Stored : bool }
type Signature = Argument list
type Macro = { Name : string; Block : LocatedToken list }
type Word = { Name : string; Signature : Signature option; Block : LocatedToken list; Words : Map<string, Word>; Macros : Map<string, Macro> }

module Parser =
    let stripComments tokens =
        let rec sub inp startLocations acc =
            match inp with
            | (Token.Other "((", location) :: rest -> sub rest (location :: startLocations) acc
            | (Token.Other "))", location) :: rest ->
                match startLocations with
                | sloc::startLocations -> sub rest startLocations acc
                | [] -> raise (SyntaxError ("Unexpected close of comment", location))
            | token :: rest ->
                match startLocations with
                | [] -> token :: acc |> sub rest []
                | _ -> sub rest startLocations acc
            | [] ->
                match startLocations with
                | location::_ -> raise (EOFError ("Unexpected EOF while looking for end of comment", location))
                | [] -> List.rev acc
        sub tokens [] []
    
    let parseSignature block =
        let rec sub inp startLocation inReturns args =
            match inp with
            | (Token.Other ")", _) :: rest ->
                Some (List.rev args), rest
            | (Token.Other "->", location) :: rest ->
                if inReturns then raise (SyntaxError ("Unexpected -> inside returns area of signature", location))
                sub rest startLocation true args
            | (Token.Other x, location) :: rest when not inReturns ->
                let parts = x.Split [|':'|] |> Array.toList
                let name, stored =
                    match parts with
                    | [""; _] -> None, false
                    | ["$"] | ["$"; _] -> raise (SyntaxError ("Stored parameters must be named", location))
                    | [name] | [name; _] ->
                        if name.StartsWith("$") then
                            Some (name.Substring(1)), true
                        else
                            Some name, false
                    | _ -> raise (SyntaxError ("Unexpected token in signature", location))
                let arg = { Name=name; Stored=stored }
                sub rest startLocation false (arg::args)
            | (Token.Other x, location) :: rest when inReturns -> sub rest startLocation true args
            | (_, location) :: _ ->
                raise (SyntaxError ("Unexpected token in signature", location))
            | [] ->
                raise (EOFError ("Unexpected end of signature", startLocation))
                
        match block with
        | (Token.Other "(", location) :: rest ->
            sub rest location false [] // XXX: Check that all names are None or Some -- no mix and match
        | _ ->
            None, block
    
    let parseShorthand (token:string) location =
        let token = [for c in token -> string c]
        let token, modifiers = 
            match token with
            | ["/"; "{"]          -> ["{"], ["map"]
            | "/" :: "*" :: rest  -> rest, ["map"]
            | "/" :: rest         -> "&" :: rest, ["map"]
            | ["\\"; "{"]         -> ["{"], ["reduce"]
            | "\\" :: "*" :: rest -> rest, ["reduce"]
            | "\\" :: rest        -> "&" :: rest, ["reduce"]
            | "*" :: "*" :: rest  -> token, []
            | ["*"; "{"]          -> ["{"], ["call"]
            | "*" :: rest         -> rest, ["call"]
            | _                   -> token, []
        let sb = System.Text.StringBuilder(token.Length)
        token |> List.iter (sb.Append >> ignore)
        sb.ToString(), [for elem in modifiers -> (Token.Other elem, location)]
    
    // This does not rewrite block shorthand!
    let rewriteShorthand tokens =
        let rec rewriteBlock inp acc =
            match inp with
            | (Token.Other token, location) :: rest ->
                let newtoken, modifiers = parseShorthand token location
                if (token.Contains("{")) && not (newtoken.Contains("{")) then
                    (Token.Other token, location) :: acc |> rewriteBlock rest
                else
                    (List.rev modifiers) @ ((Token.Other newtoken, location) :: acc) |> rewriteBlock rest
            | x :: rest -> x :: acc |> rewriteBlock rest
            | [] -> acc
        
        rewriteBlock tokens [] |> List.rev
    
    let rewriteBlock name tokens =
        let signature, sblock = parseSignature tokens

        let varName vname = sprintf "macro_%s_%s" name vname

        let rec buildPreamble spec (names:Map<string, string>) acc =
            match spec with
            | {Name=name; Stored=stored}::spec ->
                match name with
                | Some name -> (Other((if stored then "=$" else "=>") + varName name), Location.Generated) :: acc |> buildPreamble spec (names.Add(name, varName name))
                | None -> acc |> buildPreamble spec names
            | [] -> acc, names
        
        let rec rewriteNames inp (names:Map<string, string>) acc =
            match inp with
            | (Token.Other x, location) :: rest ->
                let name =
                    if (names.ContainsKey(x)) then names.[x]
                    else x
                (Token.Other name, location) :: acc |> rewriteNames rest names
            | token :: rest -> token :: acc |> rewriteNames rest names
            | [] -> List.rev acc

        let block, names =
            match signature with
            | Some x -> buildPreamble (List.rev x) Map.empty sblock
            | None -> buildPreamble [{Name=Some "_"; Stored=false}] Map.empty sblock
        
        rewriteNames block names []

    let generateWordTree tokens =
        let rec parseWordOrMacro inp startLocation topLevel inMacro block words macros =
            match inp with
            | (Token.Other ":", location) :: rest ->
                if inMacro then raise (SyntaxError ("Words nested within macros disallowed", location))
                match rest with
                | (Token.Other name, location) :: rest ->
                    let rest, sblock, swords, smacros = parseWordOrMacro rest location false false [] Map.empty Map.empty
                    let signature, sblock = parseSignature sblock
                    let sblock = rewriteShorthand sblock
                    parseWordOrMacro rest startLocation topLevel false block (words.Add(name, {Word.Name=name; Signature=signature; Block=sblock; Words=swords; Macros=smacros})) macros
                | (_, location) :: rest -> raise (SyntaxError ("Expected word name", location))
                | [] -> raise (EOFError ("Expected word name", location))
            | (Token.Other ":m", location) :: rest ->
                if inMacro then raise (SyntaxError ("Nested macros disallowed", location))
                match rest with
                | (Token.Other name, location) :: rest ->
                    let rest, sblock, _, _ = parseWordOrMacro rest location false true [] Map.empty Map.empty
                    let sblock = sblock |> rewriteShorthand |> rewriteBlock name
                    parseWordOrMacro rest startLocation topLevel false block words (macros.Add(name, {Macro.Name=name; Block=sblock}))
                | (_, location) :: rest -> raise (SyntaxError ("Expected macro name", location))
                | [] -> raise (EOFError ("Expected macro name", location))
            | (Token.Other ";", location) :: rest ->
                if topLevel then raise (SyntaxError ("Unexpected ';' at top level", location))
                rest, (List.rev block), words, macros
            | token :: rest ->
                parseWordOrMacro rest startLocation topLevel inMacro (token :: block) words macros
            | [] ->
                if topLevel = false then raise (EOFError ("Expected end of word/macro", startLocation))
                [], (List.rev block), words, macros
        
        match tokens with
        | (_, location)::_ ->
            let _, block, words, macros = parseWordOrMacro tokens location true false [] Map.empty Map.empty
            {Name="__topLevel"; Signature=None; Block=block; Words=words; Macros=macros}
        | [] ->
            {Name="__topLevel"; Signature=None; Block=[]; Words=Map.empty; Macros=Map.empty}
    
    let parse tokens =
        tokens |> stripComments |> generateWordTree
    