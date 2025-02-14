---
title: Azure Data Storage Compared Blob, File, Table, Cosmos DB, SQL
description: Code Samples in Python and C#
slug: azure-store-data
date: 2023-12-05
image: post/Articles/IMAGES/azure2.png
categories:
  - Azure Cosmos DB
  - SQL
  - MSSQl
  - Cloud
  - DevOps
  - CI\CD
  - CSharp
  - DotNet
  - Python
  - Microsoft Azure Cloud
tags:
  - Azure
  - Cloud
  - Storage
  - Python
  - Blob
  - Storage
  - File
  - Storage
  - NoSQL
  - Database
  - Code
  - Examples
draft: false
weight: 312
lastmod: 2025-02-14T18:51:46.251Z
---
So, you're diving into Azure and wondering, *"What's the best way to store my precious data?"*

## Why Azure for Data Storage?

Azure is like that fancy buffet with all-you-can-eat options for data storage. Whether you’re dealing with images, logs, documents, or structured data, Azure's got you covered.

Here's the menu:

1. **Blob Storage**: For unstructured data like files, images, and videos.
2. **File Storage**: For network file shares.
3. **Table Storage**: For NoSQL key-value stores.
4. **Cosmos DB**: For scalable NoSQL databases.
5. **SQL Database**: For relational data.

Let's dig into each of these with code samples in Python and C#.

***

## 1. Azure Blob Storage: When You Just Need a Bucket of Files 🪣

Blob Storage is perfect for when you need a place to dump files—think of it like a cloud-based USB drive.

### Python Code 🐍

```python
from azure.storage.blob import BlobServiceClient

connection_string = "<YourConnectionString>"
container_name = "my-container"
blob_name = "hello.txt"

# Initialize client
client = BlobServiceClient.from_connection_string(connection_string)
container_client = client.get_container_client(container_name)

# Upload blob
with open("hello.txt", "rb") as data:
    container_client.upload_blob(name=blob_name, data=data)

print("Blob uploaded successfully!")
```

### C# Code ⚙️

```csharp
using Azure.Storage.Blobs;
using System;
using System.IO;

string connectionString = "<YourConnectionString>";
string containerName = "my-container";
string blobName = "hello.txt";

BlobServiceClient blobServiceClient = new BlobServiceClient(connectionString);
BlobContainerClient containerClient = blobServiceClient.GetBlobContainerClient(containerName);

using FileStream fs = File.OpenRead("hello.txt");
containerClient.UploadBlob(blobName, fs);

Console.WriteLine("Blob uploaded successfully!");
```

**Blob Storage Verdict:**

* Great for unstructured data.
* Easily scalable.
* Not ideal if you need querying or indexing.

***

## 2. Azure File Storage: Your Cloud-Based Shared Drive 📂

Think of Azure File Storage as your network drive in the sky.

### Python Code 🐍

```python
from azure.storage.fileshare import ShareFileClient

connection_string = "<YourConnectionString>"
share_name = "my-share"
file_name = "notes.txt"

file_client = ShareFileClient.from_connection_string(connection_string, share_name, file_name)

with open("notes.txt", "rb") as data:
    file_client.upload_file(data)

print("File uploaded successfully!")
```

### C# Code ⚙️

```csharp
using Azure.Storage.Files.Shares;
using System;
using System.IO;

string connectionString = "<YourConnectionString>";
string shareName = "my-share";
string fileName = "notes.txt";

ShareFileClient fileClient = new ShareFileClient(connectionString, shareName, fileName);

using FileStream fs = File.OpenRead("notes.txt");
fileClient.Upload(fs);

Console.WriteLine("File uploaded successfully!");
```

**File Storage Verdict:**

* Perfect for shared file access.
* SMB protocol support.
* Not designed for large-scale analytics workloads.

***

## 3. Azure Table Storage: The NoSQL Key-Value Whisperer 🔑

Azure Table Storage offers a super-simple NoSQL solution for key-value data.

### Python Code 🐍

```python
from azure.data.tables import TableServiceClient, TableEntity

connection_string = "<YourConnectionString>"
table_name = "mytable"

# Create table client
table_service = TableServiceClient.from_connection_string(connection_string)
table_client = table_service.get_table_client(table_name)

# Insert entity
table_client.create_entity(entity={
    "PartitionKey": "users",
    "RowKey": "1",
    "Name": "Alice",
    "Age": 30
})

print("Entity inserted successfully!")
```

### C# Code ⚙️

```csharp
using Azure.Data.Tables;
using System;

string connectionString = "<YourConnectionString>";
string tableName = "mytable";

TableClient tableClient = new TableClient(connectionString, tableName);

var entity = new TableEntity("users", "1")
{
    { "Name", "Alice" },
    { "Age", 30 }
};

tableClient.AddEntity(entity);

Console.WriteLine("Entity inserted successfully!");
```

**Table Storage Verdict:**

* Simple and fast for key-value data.
* Great for logs and telemetry.
* Limited querying capabilities.

***

## 4. Azure Cosmos DB: The Swiss Army Knife of NoSQL 🌌

Cosmos DB supports multiple NoSQL models with global distribution.

### Python Code 🐍

```python
from azure.cosmos import CosmosClient

url = "<YourCosmosDBUrl>"
key = "<YourKey>"
database_name = "mydb"
container_name = "mycontainer"

client = CosmosClient(url, key)
database = client.create_database_if_not_exists(id=database_name)
container = database.create_container_if_not_exists(id=container_name, partition_key=PartitionKey(path="/id"))

item = {"id": "1", "name": "Alice", "age": 30}
container.create_item(item)

print("Item inserted successfully!")
```

### C# Code ⚙️

```csharp
using Microsoft.Azure.Cosmos;
using System;
using System.Threading.Tasks;

string endpointUrl = "<YourCosmosDBUrl>";
string primaryKey = "<YourKey>";
string databaseName = "mydb";
string containerName = "mycontainer";

CosmosClient cosmosClient = new CosmosClient(endpointUrl, primaryKey);
Database database = await cosmosClient.CreateDatabaseIfNotExistsAsync(databaseName);
Container container = await database.CreateContainerIfNotExistsAsync(containerName, "/id");

var item = new { id = "1", name = "Alice", age = 30 };
await container.CreateItemAsync(item, new PartitionKey(item.id));

Console.WriteLine("Item inserted successfully!");
```

**Cosmos DB Verdict:**

* Supports various NoSQL models.
* Global distribution built-in.
* Pricier than Table Storage.

***

## 5. Azure SQL Database: Old-School Relational Goodness 📊

When you need the comfort of good ol' SQL.

### Python Code 🐍

```python
import pyodbc

server = "<YourServer>"
database = "mydb"
username = "<YourUsername>"
password = "<YourPassword>"

conn_str = f"DRIVER={{ODBC Driver 17 for SQL Server}};SERVER={server};DATABASE={database};UID={username};PWD={password}"

with pyodbc.connect(conn_str) as conn:
    cursor = conn.cursor()
    cursor.execute("CREATE TABLE IF NOT EXISTS Users (Id INT, Name NVARCHAR(50))")
    cursor.execute("INSERT INTO Users (Id, Name) VALUES (?, ?)", 1, "Alice")
    conn.commit()

print("SQL data inserted successfully!")
```

### C# Code ⚙️

```csharp
using System;
using System.Data.SqlClient;

string connectionString = "Server=<YourServer>;Database=mydb;User Id=<YourUsername>;Password=<YourPassword>;";

using SqlConnection connection = new SqlConnection(connectionString);
connection.Open();

string createTableQuery = "CREATE TABLE IF NOT EXISTS Users (Id INT, Name NVARCHAR(50))";
string insertQuery = "INSERT INTO Users (Id, Name) VALUES (1, 'Alice')";

using SqlCommand cmd = new SqlCommand(createTableQuery, connection);
cmd.ExecuteNonQuery();

cmd.CommandText = insertQuery;
cmd.ExecuteNonQuery();

Console.WriteLine("SQL data inserted successfully!");
```

**SQL Database Verdict:**

* The classic choice for relational data.
* Azure manages scaling, backups, and security.
* Requires schema design upfront.

***

# Key Ideas

* Azure offers various storage options for different needs.
* Python and C# both have robust libraries for interacting with Azure.
* Blobs are great for files; tables are great for logs; Cosmos DB is fantastic for global apps.
* Choose based on your workload and data requirements.

# References

1. [Azure Storage Overview](https://docs.microsoft.com/en-us/azure/storage/)
2. [Azure Blob Storage](https://docs.microsoft.com/en-us/azure/storage/blobs/)
3. [Azure File Storage](https://docs.microsoft.com/en-us/azure/storage/files/)
4. [Azure Table Storage](https://docs.microsoft.com/en-us/azure/storage/tables/)
5. [Azure Cosmos DB](https://docs.microsoft.com/en-us/azure/cosmos-db/)
6. [Azure SQL Database](https://docs.microsoft.com/en-us/azure/azure-sql/)
7. [Python SDK for Azure](https://pypi.org/project/azure-storage-blob/)
8. [C# SDK for Azure](https://www.nuget.org/packages/Azure.Storage.Blobs)
