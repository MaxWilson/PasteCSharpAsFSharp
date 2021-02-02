module PasteCSharpAsFSharp

open Wilson.Packrat

module Types =
    type Param = Param of name: string * typeName: string * attributes: string list
    type CommentKind =
        | Line
        | Block
        | BlankLine
    type Comment = bool * CommentKind * string
    type ProgramFragment =
        | Namespaces of (string * Comment list) list
        | Comment of Comment
        | Function of returnType: string * name: string * args: Param list * body: ProgramFragment list

#nowarn "40" // We need recursive data structures to declare a left-recursive packrat grammar,
// but we're not doing anything crazy with recursion like calling recursive functions
// inside the ctor, so we don't need the warning.
module Parse =
    open Types
    let identifierCharacters = alphanumeric + Set['.']
    let (|InlineWS|) = function
        | Chars inlineWhitespace (_, rest) -> rest
        | rest -> rest
    let (|Comment|_|) ctor = function
        | InlineWS (Str "//" (CharsExcept (Set.ofList ['\r';'\n']) (comment, rest))) -> Some (ctor (false, Line, comment), rest)
        | InlineWS (Str "\n" (Str "\n" _ as rest)) -> Some (ctor (true, BlankLine, ""), rest)
        | WS (Str "//" (CharsExcept (Set.ofList ['\r';'\n']) (comment, rest))) -> Some (ctor (true, Line, comment), rest)
        | _ -> None
    let rec (|EOL|) commentCtor = function
        | Comment commentCtor (comment, EOL commentCtor (eol, rest)) -> [comment]@eol, rest
        | Optional "\n" rest -> [], rest
    let (|Namespace|_|) = function
        | Keyword "using" (Chars identifierCharacters (namespace', Str ";" (EOL id (eols, rest)))) ->
            Some((namespace',eols), rest)
        | _ -> None
    let rec (|Namespaces|_|) = pack <| function
        | Namespace(namespace', Namespaces(more, rest)) ->
            Some(namespace'::more, rest)
        | Namespace(namespace', rest) ->
            Some([namespace'], rest)
        | _ -> None
    let rec (|DelimitedList|_|) delimiter (|Inner|_|) = pack <| function
        | Inner(v, OWS (Str delimiter (DelimitedList delimiter (|Inner|_|)(more, rest)))) ->
            Some(v::more, rest)
        | Inner(v, rest) -> Some([v], rest)
        | _ -> None
    module Type =
        let validChars = alphanumeric + Set.ofList ['.';'<';'>']
        let (|Name|_|) = function
            | OWS (Chars validChars (name, rest)) -> Some(name, rest)
            | _ -> None
    module Expression =
        let (|Expressions|_|) = function
            | _ -> None
    module Statement =
        let (|Statements|_|) = function
            | rest -> Some([], rest)
        let (|Block|_|) = function
            | OWS (Char('{', OWS(Statements(statements, OWS(Char('}', OWS rest)))))) ->
                Some([], rest)
            | _ -> None
    module Function =
        let rec (|Modifiers|) = function
            | Keyword "public" (Modifiers rest) -> rest
            | Keyword "static" (Modifiers rest) -> rest
            | Keyword "async" (Modifiers rest) -> rest
            | rest -> rest
        let (|Parameter|_|) = function
            | OWS(Type.Name(typeName, WS(Word(name, rest)))) ->
                Some (Param(typeName, name, []), rest)
            | _ -> None
        let (|Parameters|_|) = function
            | OWS(Char ('(', DelimitedList "," (|Parameter|_|) (parameters, Char(')', rest)))) ->
                Some(parameters, rest)
            | _ -> None
        let (|Declaration|_|) = function
            | Modifiers(Type.Name(returnType,
                            Word(functionName,
                                Parameters(parameters,
                                    Statement.Block(body, rest))))) ->
                Some(Function(returnType, functionName, parameters, body), rest)
            | _ -> None
    let (|ProgramFragment|_|) = function
        | Namespaces(namespaces, rest) ->
            Some ([Types.Namespaces namespaces], rest)
        | Comment ProgramFragment.Comment (c, rest) -> Some([c], rest)
        | Function.Declaration(f, rest) -> Some([f], rest)
        | _ -> None
    let rec (|Program|_|) = pack <| function
        | ProgramFragment(fragments, Program(program, rest)) ->
            Some (fragments@program, rest)
        | ProgramFragment(fragments, OWS(rest)) ->
            Some (fragments, rest)
        | _ -> None

let parse (input: string) =
    match ParseArgs.Init (input.TrimStart()) with
    | Parse.Program(p, End) -> Ok p
    | _ -> Error "I can't understand that C# snippet, please make sure it is valid."
open Types
let render (program: Result<ProgramFragment list, string>) =
    let spaces indentLevel =
        String.replicate (indentLevel * 4) " "
    let join separator vals = System.String.Join((separator: string), (vals: string seq))
    let renderList f input = join "" (List.map f input)
    let rec renderComment indentLevel = function
        | (separateLine, Line, comment) ->
            $"""{if separateLine then "\n" + spaces indentLevel else " "}//{comment}"""
        | (separateLine, CommentKind.Block, comment) ->
            $"""{if separateLine then "\n" + spaces indentLevel else " "}(*{comment}*)"""
        | (separateLine, BlankLine, comment) ->
            $"""{if separateLine then "\n" else ""}"""
    let rec recur indentLevel = function
        | [] -> []
        | Namespaces(namespaces)::rest ->
            (namespaces |> List.map (fun (ns, comments) -> $"\nopen {ns}{renderList (renderComment indentLevel) comments}"))
                @ (recur indentLevel rest)
        | Comment(comment)::rest -> (renderComment indentLevel comment)::(recur indentLevel rest)
        | Function(typeName, functionName, parameters, body)::rest ->
            let paramsTxt = join "," (parameters |> List.map (fun (Param(typeName, paramName, _)) -> $"({paramName}: {typeName})"))
            $"""let {functionName}({paramsTxt}) = ()"""::(recur indentLevel rest)
    match program with
    | Error msg -> msg
    | Ok program ->
        System.String.Join("", recur 0 program).Trim()
