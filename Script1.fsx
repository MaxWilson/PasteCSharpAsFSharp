#I __SOURCE_DIRECTORY__
#load "Parsing.fs"
open Packrat

type Param = Param of name: string * type': string * attributes: string list
type ProgramFragment =
    | Namespaces of string list
    | Function of name: string * args: Param list

let (|Program|_|) = function
    | Any(any, rest) ->
        Some ([Namespaces([any])], rest)
    | _ -> None

let parse = parser (|Program|_|)

parse "System.Text"