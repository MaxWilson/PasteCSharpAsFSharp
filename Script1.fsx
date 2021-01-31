#I __SOURCE_DIRECTORY__
#load "Parsing.fs"

type Param = Param of name: string * type': string * attributes: string list
type AST =
    | Namespaces of string list
    | Function of name: string * args: Param list
