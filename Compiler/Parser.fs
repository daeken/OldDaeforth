namespace Compiler

type Macro = { Name : string; Block : LocatedToken list }
type Word = { Name : string; Block : LocatedToken list; Words : Map<string, Word>; Macros : Map<string, Macro> }

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

    let generateWordTree tokens =
        let rec parseWordOrMacro inp startLocation topLevel inMacro block words macros =
            match inp with
            | (Token.Other ":", location) :: rest ->
                if inMacro then raise (SyntaxError ("Words nested within macros disallowed", location))
                match rest with
                | (Token.Other name, location) :: rest ->
                    let rest, sblock, swords, smacros = parseWordOrMacro rest location false false [] Map.empty Map.empty
                    parseWordOrMacro rest startLocation topLevel false block (words.Add(name, {Word.Name=name; Block=sblock; Words=swords; Macros=smacros})) macros
                | (_, location) :: rest -> raise (SyntaxError ("Expected word name", location))
                | [] -> raise (EOFError ("Expected word name", location))
            | (Token.Other ":m", location) :: rest ->
                if inMacro then raise (SyntaxError ("Nested macros disallowed", location))
                match rest with
                | (Token.Other name, location) :: rest ->
                    let rest, sblock, _, _ = parseWordOrMacro rest location false true [] Map.empty Map.empty
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
            {Name="__topLevel"; Block=block; Words=words; Macros=macros}
        | [] ->
            {Name="__topLevel"; Block=[]; Words=Map.empty; Macros=Map.empty}
    
    let parse tokens =
        tokens |> stripComments |> generateWordTree
    