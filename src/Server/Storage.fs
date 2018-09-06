module Storage

open Shared
open Giraffe

open Microsoft.WindowsAzure.Storage;
open Microsoft.WindowsAzure.Storage.Table;

// make this configurable
let connectionString = "DefaultEndpointsProtocol=https;AccountName=shortcutsstorage;AccountKey=BpJFsqo1rl5Y25Fn4IYg4SjhqjwtMc2Mm4qL9CJM2ECZJBwnqoEtUKyM1a3c99CLx/6qxLvNGoAiZcWRc2PQMg==;EndpointSuffix=core.windows.net" // CloudConfigurationManager.GetSetting("StorageConnectionString")

let init () = 
    task {
        // Parse the connection string and return a reference to the storage account.
        let storageAccount = CloudStorageAccount.Parse(connectionString)

        // Create the table client.
        let tableClient = storageAccount.CreateCloudTableClient();

        // Retrieve a reference to the table.
        let table = tableClient.GetTableReference("shortcuts");

        // Create the table if it doesn't exist.
        let! success = table.CreateIfNotExistsAsync()

        if success then
            return Ok table
        else
            return Error "Unable to initiate storage client client"
    }


type Shortcut(username : string, shortcut : string, originalUrl : string, count : int) =
    inherit TableEntity(partitionKey = username, rowKey = shortcut)

    member val OriginalUrl = originalUrl with get, set
    member val Count = count with get, set

let insert (table : CloudTable) username shortcut originalUrl =
    // create entity
    let shortcut = Shortcut(username, shortcut, originalUrl, 0)
    // Create the TableOperation object that inserts the customer entity.
    let insertOperation = TableOperation.Insert(shortcut);
    // Execute the insert operation.
    do! table.ExecuteAsync(insertOperation) |> ignore
    // return the shortcut
    shortcut

let getByUsername table username = task {
        // Construct the query operation
        let query = (new TableQuery()).Where(TableQuery.GenerateFilterCondition("PartitionKey", QueryComparisons.Equal, username))
        // execute query
        let token = TableContinuationToken()
        let! queryResult = table.ExecuteQuerySegmentedAsync(query, token)
        // transform the result
        return 
            queryResult
            |> Seq.cast<Shortcut>
            |> Seq.map (fun record -> { originalUrl = record.OriginalUrl ; shortUrl = record.RowKey ; count = record.Count })
            |> Seq.toList
    }
