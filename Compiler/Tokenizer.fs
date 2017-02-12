namespace Compiler

open System
open System.Text.RegularExpressions

type Token =
    | Float of single
    | Integer of int
    | String of string
    | Other of string
type Location = { File : string; Position : int; Line : int; Column : int; Length : int } with
    static member Generated = { File="Generated"; Position=0; Line=0; Column=0; Length=0 }
type LocatedToken = Token * Location

exception ICE    
exception SyntaxError of string * Location
exception EOFError of string * Location

module Tokenizer =
    
    let tokenize filename source = 
        let locatedExplode s =
            let rec subExplode position line column pieces source =
                match source with
                | f::rest ->
                    let pieces = ((f, { File = filename; Position = position; Line = line; Column = column; Length = 1 }) :: pieces)
                    match f with
                    | '\n' -> subExplode (position + 1) (line + 1) 0 pieces rest
                    | _ -> subExplode (position + 1) line (column + 1) pieces rest
                | [] -> List.rev pieces
            [for c in s -> c] |> subExplode 0 0 0 []
        
        
        let rec parseString inp startLocation cur =
            match inp with
            | ('"', _)::rest -> (Token.String cur, rest)
            | ('\\', _)::rest ->
                match rest with
                | ('n', _)::rest -> cur + "\n" |> parseString rest startLocation
                | ('t', _)::rest -> cur + "\t" |> parseString rest startLocation
                | ('\\', _)::rest -> cur + "\\" |> parseString rest startLocation
                | ('"', _)::rest -> cur + "\"" |> parseString rest startLocation
                | (_, location)::_ -> raise (SyntaxError ("Unknown string escape", location))
                | [] -> raise (EOFError ("Unexpected EOF while parsing string escape", startLocation))
            | (c, _)::rest -> cur + (string c) |> parseString rest startLocation
            | _ -> raise (EOFError ("Unexpected EOF while parsing string", startLocation))
        
        let parseToken inp =
            let rec get inp cur =
                match inp with
                | (c, _)::rest ->
                    match c with
                    | ' ' | '\t' | '\n' | '\r' -> (cur, rest)
                    | _ -> cur + (string c) |> get rest
                | [] -> cur, []
            
            let token, rest = get inp ""

            match token with
            | _ when Regex.Match(token, @"^#-?[0-9]+$").Success ->
                Token.Integer (Int32.Parse(token.Substring(1))), rest
            | _ when Regex.Match(token, @"^-?([0-9]+\.[0-9]*|\.[0-9]+|[0-9]+)$").Success ->
                Token.Float (Single.Parse token), rest
            | _ -> Token.Other token, rest

        let rec makeTokens inp tokens = 
            match inp with
            | (ch, location)::rest ->
                match ch with
                | ' ' | '\t' | '\n' | '\r' -> makeTokens rest tokens
                | '"' ->
                    let str, rest = parseString rest location ""
                    let location = {location with Length = inp.Length - rest.Length - (if rest.Length = 0 then 0 else 1)}
                    (str, location) :: tokens |> makeTokens rest
                | _ ->
                    let token, rest = parseToken inp
                    let location = {location with Length = inp.Length - rest.Length - (if rest.Length = 0 then 0 else 1)}
                    (token, location) :: tokens |> makeTokens rest
            | [] -> List.rev tokens
        
        (source |> locatedExplode |> makeTokens) []