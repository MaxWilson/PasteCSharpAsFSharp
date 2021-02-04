#I __SOURCE_DIRECTORY__
#load "Packrat.fs"
#load "PasteCSharpAsFSharp.fs"
open Wilson.Packrat
open PasteCSharpAsFSharp
open Render

let convert input = parse input |> render |> System.Windows.Forms.Clipboard.SetText

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

(* OUTPUT:
open System
open Azure.Storage.Blobs
open Azure.Storage.Blobs.Models
open System.Collections.Generic
open System.Threading.Tasks
open System.Text
open System.IO
open Azure.Identity

// Some code omitted for brevity.
let UploadBlob((accountName: string),(containerName: string),(blobName: string),(blobContents: string)) = async {
    // Construct the blob container endpoint from the arguments.
    let containerEndpoint: string = System.String.Format("https://{0}.blob.core.windows.net/{1}", accountName, containerName)

    // Get a credential and create a client object for the blob container.
    let containerClient: BlobContainerClient = new BlobContainerClient(new Uri(containerEndpoint), new DefaultAzureCredential())

    try
        // Create the container if it does not exist.
        do! containerClient.CreateIfNotExistsAsync() |> Async.AwaitTask |> Async.Ignore

        // Upload text to a new block blob.
        let byteArray: byte[] = Encoding.ASCII.GetBytes(blobContents)

        use stream: MemoryStream = new MemoryStream(byteArray)
        do! containerClient.UploadBlobAsync(blobName, stream) |> Async.AwaitTask |> Async.Ignore
    with :? Exception as e ->
        e |> raise
    }
*)
