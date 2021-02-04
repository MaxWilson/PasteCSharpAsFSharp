module PasteCSharpAsFSharp

open Wilson.Packrat

module Types =
    type Param = Param of name: string * typeName: string * attributes: string list
    type CommentKind =
        | Line
        | Block
        | BlankLine
    type Comment = bool * CommentKind * string
    type Literal =
        | Number of int64
        | String of string
    type Expression =
        | Literal of Literal
        | VariableDeref of string
        | MemberAccess of lhs:Expression * afterDot:Expression
        | MethodCall of method:Expression * arguments: Expression list
        | NewCreate of typeName: string * arguments: Expression list
    type Statement =
        | Comment of Comment
        | Declare of typeName: string * name: string * Expression
        | Assign of name: string * Expression
        | TryCatch of Statement list * Catch list
        | IfThenElse of Expression * Statement * Statement option
        | Block of Statement list
        | Using of usingStatement: Statement * body:Statement // usingStatement will only be declare or assign in valid C# code so we could make this more specific, but the parser should already take care of it
        | Throw of expr: Expression
        | Do of expr: Expression
    and Catch = Catch of exnType: string option * exnName: string option * Statement list
    type FunctionDefinition = {
        returnType: string
        name: string
        parameters: Param list
        body: Statement list
        }
    type ProgramFragment =
        | Namespaces of (string * Comment list) list
        | Comment of Comment
        | Function of FunctionDefinition

#nowarn "40" // We need recursive data structures to declare a left-recursive packrat grammar,
// but we're not doing anything crazy with recursion like calling recursive functions
// inside the ctor, so we don't need the warning.
module Parse =
    open Types
    let identifierCharacters = alphanumeric + Set['.']
    let (|WSStr|_|) input = (|OWS|) >> (|Str|_|) input
    let (|Identifier|_|) = (|OWS|) >> (|Chars|_|) identifierCharacters
    let (|InlineWS|) = function
        | Chars inlineWhitespace (_, rest) -> rest
        | rest -> rest
    let (|Comment|_|) ctor = function
        | InlineWS (Str "//" (CharsExcept (Set.ofList ['\r';'\n']) (comment, rest))) -> Some (ctor (false, Line, comment), rest)
        | InlineWS (Str "\n" (Str "\n" _ as rest)) -> Some (ctor (true, BlankLine, ""), rest)
        | WSStr "//" (CharsExcept (Set.ofList ['\r';'\n']) (comment, rest)) -> Some (ctor (true, Line, comment), rest)
        | _ -> None
    let rec (|EOL|) commentCtor = function
        | Comment commentCtor (comment, EOL commentCtor (eol, rest)) -> [comment]@eol, rest
        | Optional "\n" rest -> [], rest
    let (|Namespace|_|) = function
        | Keyword "using" (Identifier(namespace', Str ";" (EOL id (eols, rest)))) ->
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
        let validChars = alphanumeric + Set.ofList ['.';'<';'>';'[';']']
        let (|Name|_|) = (|OWS|) >> (|Chars|_|) validChars
    module Expression =
        let identChars = alphanumeric + Set.ofList ['_']
        let (|EscapedString|_|) ((ctx, ix): ParseInput) =
            let (|Escaped|_|) ((ctx, ix): ParseInput) =
                let str = ctx.input
                let len = ctx.input.Length
                let sb = System.Text.StringBuilder()
                let rec recur ix =
                    if ix >= ctx.input.Length then ix
                    elif ctx.input.[ix] = '\\' && ix + 1 < ctx.input.Length then
                        let c =
                            match ctx.input.[ix + 1] with
                            | 't' -> '\t'
                            | 'r' -> '\r'
                            | 'n' -> '\n'
                            | '\\' -> '\\'
                            | '\"' -> '\"'
                            | c ->
                                sb.Append '\\' |> ignore // not really an escape sequence
                                c
                        sb.Append c |> ignore
                        recur (ix + 2)
                    else
                        match ctx.input.[ix] with
                        | '"' -> ix
                        | c ->
                            sb.Append c |> ignore
                            if ix + 1 < ctx.input.Length then
                                recur(ix+1)
                            else ix
                match recur ix with
                | ix' when ix' < ctx.input.Length ->
                    Some(sb.ToString(), ((ctx, ix'): ParseInput))
                | _ -> None
            match (ctx, ix) with
            | WSStr "\"" (Escaped (str, WSStr "\"" rest)) -> Some(str, rest)
            | _ -> None
        let (|Literal|_|) = function
            | OWS(Str "-" (Chars numeric (number, rest))) -> Some(Number -(System.Int64.Parse(number)), rest)
            | OWS(Chars numeric (number, rest)) -> Some(Number (System.Int64.Parse(number)), rest)
            | EscapedString (v, rest) -> Some(String v, rest)
            | _ -> None
        let (|VariableName|_|) = (|OWS|) >> (|Chars|_|) identChars
        let rec (|Expression|_|) = pack <| function
            | WSStr "await" (Expression(expr, rest)) -> Some(expr, rest)
            | WSStr "new" (Type.Name(typeName, WSStr "(" (Arguments (args, WSStr ")" rest)))) ->
                Some(Expression.NewCreate(typeName, args), rest)
            | Expression(lhs, Char('.', Expression(rhs, rest))) -> Some(Expression.MemberAccess(lhs, rhs), rest)
            | Expression(lhs, WSStr "(" (Arguments (args, WSStr ")" rest))) -> Some(Expression.MethodCall(lhs, args), rest)
            | Literal(l, rest) -> Some(Literal l, rest)
            | VariableName(v, rest) -> Some(Expression.VariableDeref v, rest)
            | _ -> None
        and (|Arguments|_|) = function
            | Expression(expr, WSStr "," (Arguments (more, rest))) -> Some(expr::more, rest)
            | Expression (expr, rest) -> Some([expr], rest)
            | rest -> Some([], rest)
        let (|Expressions|_|) = function
            | _ -> None
    module Statement =
        let (|LValue|_|) = function
            | Identifier(id, rest) -> Some (id, rest)
            | _ -> None
        let (|Assign|_|) = function
            | Type.Name(typeName, LValue(varName, OWS(Str "=" (Expression.Expression(expr, rest))))) ->
                Some(Statement.Declare(typeName, varName, expr), rest)
            | LValue(varName, Str "=" (Expression.Expression(expr, rest))) ->
                Some(Statement.Assign(varName, expr), rest)
            | _ -> None
        let rec (|Statement|_|) = function
            | Comment id (comment, rest) -> Some(Statement.Comment comment, rest)
            | WSStr "try"
                (Block'(b,
                    WSStr "catch" (
                        WSStr "(" (
                            Type.Name(exnType,
                                (OptionalInput (|Identifier|_|)
                                    (identifier,
                                        WSStr ")"
                                            (Block'(catchBlock,
                                                rest)))))))))
                ->
                    Some(Statement.TryCatch(b, [Catch(Some exnType, identifier, catchBlock)]), rest)
            | Assign(statement, WSStr ";" rest) ->
                Some(statement, rest)
            | Expression.Expression(expr, WSStr ";" rest) -> Some(Statement.Do expr, rest)
            | WSStr "throw" (Expression.Expression(expr, WSStr ";" rest)) -> Some(Statement.Throw expr, rest)
            | WSStr "using" (WSStr "(" (Assign(usingStmt, WSStr ")" (StatementOrBlock(body, rest))))) ->
                Some(Statement.Using(usingStmt, body), rest)
            | rest -> None
        and (|Statements|_|) = pack <| function
            | Statement(s, Statements(more, rest)) -> Some(s::more, rest)
            | rest -> Some([], rest)
            | _ -> None
        and (|Block'|_|) = function
            | WSStr "{" (Statements(statements, WSStr "}" rest)) ->
                Some(statements, rest)
            | _ -> None
        and (|StatementOrBlock|_|) = function
            | Block'(stmts, rest) -> Some(Types.Statement.Block stmts, rest)
            | Statement(stmt, rest) -> Some(stmt, rest)
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
            | WSStr "(" (DelimitedList "," (|Parameter|_|) (parameters, WSStr ")" rest)) ->
                Some(parameters, rest)
            | _ -> None
        let (|Declaration|_|) = function
            | Modifiers(Type.Name(returnType,
                            Word(functionName,
                                Parameters(parameters,
                                    Statement.Block'(body, rest))))) ->
                Some(Function({ returnType = returnType; name = functionName; parameters = parameters; body = body }), rest)
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
        | Function(definition)::rest ->
            let paramsTxt = join "," (definition.parameters |> List.map (fun (Param(typeName, paramName, _)) -> $"({paramName}: {typeName})"))
            $"""let {definition.name}({paramsTxt}) = ()"""::(recur indentLevel rest)
    match program with
    | Error msg -> msg
    | Ok program ->
        System.String.Join("", recur 0 program).Trim()