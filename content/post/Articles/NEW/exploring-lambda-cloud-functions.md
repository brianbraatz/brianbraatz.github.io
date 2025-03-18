---
title: Exploring Lambda and Cloud Functions in GCP, AWS, & Azure Cloud
description: CheatSheet and Code Examples in Python and C#
slug: exploring-lambda-and-cloud-functions
date: 2023-07-15
image: post/Articles/IMAGES/lambcloudwide.jpg
categories:
  - Cloud
  - Amazon Cloud-AWS
  - Amazon Lambda
  - Google Cloud Functions
  - Azure Functions
  - CSharp
  - DotNet
  - Python
  - AWS-Lambda
  - Azure-Functions
  - DynamoDb
tags:
  - AWS
  - GCP
  - Azure
  - Cloud
  - Functions
  - Serverless
  - Lambda
  - Cloud
  - Python
  - CPP
  - GoogleCloud
  - GoogleAppEngine
  - Azure-Functions
draft: false
weight: 42
categories_ref:
  - Cloud
  - Amazon Cloud-AWS
  - Amazon Lambda
  - Google Cloud Functions
  - Azure Functions
  - CSharp
  - DotNet
  - Python
  - AWS-Lambda
  - Azure-Functions
  - DynamoDb
slug_calculated: https://brianbraatz.github.io/p/exploring-lambda-and-cloud-functions
lastmod: 2025-03-14T16:40:20.844Z
---
<!-- 
# Exploring Lambda and Cloud Functions in GCP, AWS, & Azure Cloud
-->

## A Brief History

<!-- 
Before diving into serverless computing, let’s get acquainted with the three biggest cloud platforms: **Amazon Web Services (AWS), Google Cloud Platform (GCP), and Microsoft Azure**.
-->

* **Amazon Web Services (AWS)**: Launched in 2006, AWS started with Simple Storage Service (S3) and Elastic Compute Cloud (EC2). Since then, it has become huge in services, offering computing, storage, machine learning, and more.
* **Google Cloud Platform (GCP)**: Google jumped into the cloud market in 2008, leaning on its expertise in search. GCP offers in AI, machine learning, and big data services.
* **Microsoft Azure**: Initially called Windows Azure (2008). Huge Surprise: (grin) this cloud platform gained traction by integrating seamlessly with Microsoft’s enterprise tools. Azure now competes strongly with AWS for enterprise business .

## What Are Lambda, Cloud Functions, and Azure Functions?

### AWS Lambda

AWS Lambda lets you run code without provisioning or managing servers. It supports multiple languages, executes in response to triggers (S3 events, API Gateway, etc.), and scales automatically.

### Google Cloud Functions

Google Cloud Functions provide a fully managed, event-driven execution environment. They are deeply integrated with GCP services like Pub/Sub, Cloud Storage, and Firestore.

### Azure Functions

Azure Functions offer a serverless compute service with deep integration into Microsoft’s ecosystem. They support both consumption-based pricing and premium plans for higher performance needs.

## Similarities and Differences

| Feature                    | AWS Lambda                        | Google Cloud Functions    | Azure Functions                            |
| -------------------------- | --------------------------------- | ------------------------- | ------------------------------------------ |
| **Trigger Events**         | S3, API Gateway, DynamoDB         | Pub/Sub, Cloud Storage    | Event Grid, Blob Storage                   |
| **Supported Languages**    | Python, C#, Node.js, Go, Java     | Python, Node.js, Go, Java | Python, C#, Node.js, Java                  |
| **Execution Time Limit**   | 15 minutes                        | 9 minutes                 | 5 minutes (consumption plan)               |
| **Cold Start Performance** | Slower due to container boot time | Faster                    | Varies depending on plan                   |
| **Integration**            | AWS ecosystem                     | GCP ecosystem             | Microsoft ecosystem                        |
| **Pricing Model**          | Pay per execution time            | Pay per execution time    | Consumption-based, premium plans available |

## Common Problems They Solve

* Running backend logic without managing servers
* Handling API requests without a full-fledged backend
* Processing files in storage (e.g., image processing, video encoding)
* Handling real-time streaming data
* Responding to database changes
* Automating workflows

## Code Samples

### AWS Lambda

#### Python

```python
import json

def lambda_handler(event, context):
    return {
        'statusCode': 200,
        'body': json.dumps('Hello from AWS Lambda!')
    }
```

#### C\#

```csharp
using System;
using Amazon.Lambda.Core;

[assembly: LambdaSerializer(typeof(Amazon.Lambda.Serialization.SystemTextJson.DefaultLambdaJsonSerializer))]

public class Function
{
    public string FunctionHandler(string input, ILambdaContext context)
    {
        return "Hello from AWS Lambda!";
    }
}
```

### Google Cloud Functions

#### Python

```python
def hello_world(request):
    return "Hello from Google Cloud Functions!", 200
```

#### C\#

```csharp
using Google.Cloud.Functions.Framework;
using Microsoft.AspNetCore.Http;
using System.Threading.Tasks;

public class Function : IHttpFunction
{
    public async Task HandleAsync(HttpContext context)
    {
        await context.Response.WriteAsync("Hello from Google Cloud Functions!");
    }
}
```

### Azure Functions

#### Python

```python
import azure.functions as func

def main(req: func.HttpRequest) -> func.HttpResponse:
    return func.HttpResponse("Hello from Azure Functions!")
```

#### C\#

```csharp
using System.IO;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Azure.WebJobs;
using Microsoft.Azure.WebJobs.Extensions.Http;
using Microsoft.AspNetCore.Http;
using System.Threading.Tasks;

public static class Function
{
    [FunctionName("HelloWorld")]
    public static async Task<IActionResult> Run(
        [HttpTrigger(AuthorizationLevel.Function, "get", "post")] HttpRequest req)
    {
        return new OkObjectResult("Hello from Azure Functions!");
    }
}
```

## Key Ideas Table

| Concept                | Explanation                                 |
| ---------------------- | ------------------------------------------- |
| AWS Lambda             | Serverless compute on AWS                   |
| Google Cloud Functions | Serverless compute on GCP                   |
| Azure Functions        | Serverless compute on Azure                 |
| Event-Driven           | All three execute based on triggers         |
| Cold Start             | Delay when functions start after inactivity |
| Pricing                | Pay per execution model                     |

## References

* https://aws.amazon.com/lambda/
* https://cloud.google.com/functions/
* https://azure.microsoft.com/en-us/products/functions/
