namespace Compiler

type Macro = { Name : string; Block : LocatedToken list }
type Word = { Name : string; Block : LocatedToken list; Words : Map<string, Word>; Macros : Map<string, Macro> }

module Parser =
    let stripComments tokens =
        let rec sub inp depth acc =
            match inp with
            | (Token.Other "((", _) :: rest -> sub rest (depth + 1) acc
            | (Token.Other "))", location) :: rest ->
                if depth = 0 then raise (SyntaxError ("Unexpected close of comment", location))
                sub rest (depth - 1) acc
            | token :: rest when depth = 0 -> token :: acc |> sub rest 0
            | token :: rest -> sub rest depth acc
            | [] ->
                if depth <> 0 then raise (EOFError "Unexpected EOF while looking for end of comment")
                List.rev acc
        sub tokens 0 []

    let generateWordTree tokens =
        let rec parseWordOrMacro inp topLevel inMacro block words macros =
            match inp with
            | (Token.Other ":", location) :: rest ->
                if inMacro then raise (SyntaxError ("Words nested within macros disallowed", location))
                match rest with
                | (Token.Other name, _) :: rest ->
                    let rest, sblock, swords, smacros = parseWordOrMacro rest false false [] Map.empty Map.empty
                    parseWordOrMacro rest topLevel false block (words.Add(name, {Word.Name=name; Block=sblock; Words=swords; Macros=smacros})) macros
                | (_, location) :: rest -> raise (SyntaxError ("Expected word name", location))
                | [] -> raise (EOFError "Expected word name")
            | (Token.Other ":m", location) :: rest ->
                if inMacro then raise (SyntaxError ("Nested macros disallowed", location))
                match rest with
                | (Token.Other name, _) :: rest ->
                    let rest, sblock, _, _ = parseWordOrMacro rest false true [] Map.empty Map.empty
                    parseWordOrMacro rest topLevel false block words (macros.Add(name, {Macro.Name=name; Block=sblock}))
                | (_, location) :: rest -> raise (SyntaxError ("Expected macro name", location))
                | [] -> raise (EOFError "Expected macro name")
            | (Token.Other ";", location) :: rest ->
                if topLevel then raise (SyntaxError ("Unexpected ';' at top level", location))
                rest, (List.rev block), words, macros
            | token :: rest ->
                parseWordOrMacro rest topLevel inMacro (token :: block) words macros
            | [] ->
                if topLevel = false then raise (EOFError ("Expected end of word/macro"))
                [], (List.rev block), words, macros
        
        let _, block, words, macros = parseWordOrMacro tokens true false [] Map.empty Map.empty
        {Name="__topLevel"; Block=block; Words=words; Macros=macros}
    