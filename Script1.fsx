#I __SOURCE_DIRECTORY__
#load "Parsing.fs"
open Packrat

module Types =
    type Param = Param of name: string * type': string * attributes: string list
    type CommentKind =
        | Line
        | Block
        | BlankLine
    type Comment = bool * CommentKind * string
    type ProgramFragment =
        | Namespaces of (string * Comment list) list
        | Comment of Comment
        | Function of name: string * args: Param list * body: ProgramFragment list

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
    let (|ProgramFragment|_|) = function
        | Namespaces(namespaces, rest) ->
            Some ([Types.Namespaces namespaces], rest)
        | Comment ProgramFragment.Comment (c, rest) -> Some([c], rest)
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
    let renderList f input = System.String.Join("", List.map f input)
    let rec renderComment indentLevel = function
        | (separateLine, Line, comment) ->
            $"""{if separateLine then "\n" + spaces indentLevel else " "}//{comment}"""
        | (separateLine, Block, comment) ->
            $"""{if separateLine then "\n" + spaces indentLevel else " "}(*{comment}*)"""
        | (separateLine, BlankLine, comment) ->
            $"""{if separateLine then "\n" else ""}"""
    let rec recur indentLevel = function
        | [] -> []
        | Namespaces(namespaces)::rest ->
            (namespaces |> List.map (fun (ns, comments) -> $"\nopen {ns}{renderList (renderComment indentLevel) comments}"))
                @ (recur indentLevel rest)
        | Comment(comment)::rest -> (renderComment indentLevel comment)::(recur indentLevel rest)
    match program with
    | Error msg -> msg
    | Ok program ->
        System.String.Join("", recur 0 program).Trim()

let convert input = parse input |> render |> printfn "%s"

convert """
using System;
using Azure.Storage.Blobs;
using Azure.Storage.Blobs.Models;
using System.Collections.Generic;
using System.Threading.Tasks; // will this show up?
using System.Text;
// do we need this?
using System.IO;
using Azure.Identity;

// end


// really the end

"""

let sample1 = """
using System;
using Azure.Storage.Blobs;
using Azure.Storage.Blobs.Models;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Text;
using System.IO;
using Azure.Identity;

// Some code omitted for brevity.

static public async Task UploadBlob(string accountName, string containerName, string blobName, string blobContents)
{
    // Construct the blob container endpoint from the arguments.
    string containerEndpoint = string.Format("https://{0}.blob.core.windows.net/{1}",
                                                accountName,
                                                containerName);

    // Get a credential and create a client object for the blob container.
    BlobContainerClient containerClient = new BlobContainerClient(new Uri(containerEndpoint),
                                                                    new DefaultAzureCredential());

    try
    {
        // Create the container if it does not exist.
        await containerClient.CreateIfNotExistsAsync();

        // Upload text to a new block blob.
        byte[] byteArray = Encoding.ASCII.GetBytes(blobContents);

        using (MemoryStream stream = new MemoryStream(byteArray))
        {
            await containerClient.UploadBlobAsync(blobName, stream);
        }
    }
    catch (Exception e)
    {
        throw e;
    }
}"""

convert sample1
