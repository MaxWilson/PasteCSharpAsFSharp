#I __SOURCE_DIRECTORY__
#load "Parsing.fs"
open Packrat

module Types =
    type Param = Param of name: string * type': string * attributes: string list
    type ProgramFragment =
        | Namespaces of string list
        | Comment of string
        | BlankLine
        | Function of name: string * args: Param list * body: ProgramFragment list

#nowarn "40" // We need recursive data structures to declare a left-recursive packrat grammar,
// but we're not doing anything crazy with recursion like calling recursive functions
// inside the ctor, so we don't need the warning.
module Parse =
    open Types
    let identifierCharacters = alphanumeric + Set['.']
    let (|EOL|) = function
        | Optional "\n" rest -> rest
    let (|Namespace|_|) = function
        | Keyword "using" (Chars identifierCharacters (namespace', Str ";" (EOL rest))) ->
            Some(namespace', rest)
        | _ -> None
    let rec (|Namespaces|_|) = pack <| function
        | Namespace(namespace', Namespaces(more, rest)) ->
            Some(namespace'::more, rest)
        | Namespace(namespace', rest) ->
            Some([namespace'], rest)
        | _ -> None
    let (|Comment|_|) = function
        | OWS(Str "//" (CharsExcept (Set.ofList ['\r';'\n']) (comment, EOL rest))) -> Some (ProgramFragment.Comment comment, rest)
        | _ -> None
    let rec (|Function|_|) = pack <| function
        | Optional "public" (Function f) -> Some f
        | Optional "static" (Function f) -> Some f
        |
    let (|ProgramFragment|_|) = function
        | Str "\n" rest -> Some (BlankLine, rest)
        | Namespaces(namespaces, rest) ->
            Some (Types.Namespaces namespaces, rest)
        | Comment c -> Some c
        | _ -> None
    let rec (|Program|_|) = pack <| function
        | ProgramFragment(fragment, Program(program, rest)) ->
            Some (fragment::program, rest)
        | ProgramFragment(fragment, OWS(rest)) ->
            Some ([fragment], rest)
        | _ -> None

let parse (input: string) =
    match ParseArgs.Init (input.TrimStart()) with
    | Parse.Program(p, End) -> Ok p
    | _ -> Error "I can't understand that C# snippet, please make sure it is valid."
open Types
let render (program: Result<ProgramFragment list, string>) =
    let spaces indentLevel =
        String.replicate (indentLevel * 4) " "
    let rec recur indentLevel = function
        | [] -> []
        | Namespaces(namespaces)::rest ->
            (namespaces |> List.map (sprintf "open %s"))
                @ (recur indentLevel rest)
        | Comment(comment)::rest ->
            $"{spaces indentLevel}//{comment}"::(recur indentLevel rest)
        | BlankLine::[] -> [] // skip blank line at end
        | BlankLine::rest ->
            ""::(recur indentLevel rest)
    match program with
    | Error msg -> msg
    | Ok program ->
        System.String.Join("\n", recur 0 program)

let convert input = parse input |> render |> printfn "%s"

convert """
using System;
using Azure.Storage.Blobs;
using Azure.Storage.Blobs.Models;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Text;
// do we need this?
using System.IO;
using Azure.Identity;

// end

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
