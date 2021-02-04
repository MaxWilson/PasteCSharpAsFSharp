#r "nuget: Microsoft.Azure.Services.AppAuthentication, 1.6.0"
#r "nuget: Azure.Identity"
#r "nuget: Azure.Storage"
#r "nuget: Azure.Storage.Blobs"
#r "nuget: Azure.Core"

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