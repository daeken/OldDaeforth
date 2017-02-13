namespace Compiler

module Utility = 
    let joinMaps (p:Map<'a, 'b>) (q:Map<'a, 'b>) =
        Map(Seq.concat [ Map.toSeq p; Map.toSeq q ])
    
    let stringToList (x:string) = [for c in x -> string c]

    let listToString (x:string list) = 
        let sb = System.Text.StringBuilder(x.Length)
        x |> List.iter (sb.Append >> ignore)
        sb.ToString()