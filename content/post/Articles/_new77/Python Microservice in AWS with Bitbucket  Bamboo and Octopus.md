---
title: Python + C# Microservices in AWS -Bitbucket,  Bamboo and Octopus
description: Full Example of Python and C# Microservices into AWS
slug: python-and-csharp-microservice-bitbucket-bamboo-octopus
date: 2019-12-04
image: post/Articles/IMAGES/pythoncsharpmicroservicebamboooctopus.png
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Pulumi
  - Microservice
  - CSharp
  - Bitbucket
  - Bamboo
  - Octopus
  - CI\CD
tags:
  - Microservice
  - Python
  - CSharp
  - AWS
  - Infrastructure
  - CI/CD
  - Bitbucket
  - Bamboo
  - Octopus
draft: false
weight: 20
categories_ref:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Pulumi
  - Microservice
  - CSharp
  - Bitbucket
  - Bamboo
  - Octopus
  - CI\CD
slug_calculated: https://brianbraatz.github.io/p/python-and-csharp-microservice-bitbucket-bamboo-octopus
lastmod: 2025-03-18T22:01:24.673Z
---
<!-- 
# Full Example of Python and C# Microservice that Has a Method That Divides Numbers

## Introduction

So, you've got two numbers, and you want to divide them? Great! But instead of just using a calculator like a normal person, let’s over-engineer this into a full-blown **microservice** with **Python and C#**, deploy it to **AWS**, and automate the whole thing with **CI/CD pipelines using Bitbucket, Bamboo, and Octopus Deploy**. Because... why not? 🚀
-->

## Python Microservice in AWS with Bitbucket,  Bamboo and Octopus

In this glorious adventure, we’ll:

1. Build a simple microservice in **Python** and **C#** that divides two numbers.
2. Package it up and deploy it to **AWS** using **Infrastructure as Code (IaC)**.
3. Set up a **CI/CD pipeline** to automate the deployment using **Bitbucket, Bamboo, and Octopus Deploy**.

***

## Step 1: Build the Microservices

### Python Microservice 🐍

Let’s keep it simple with **FastAPI**, because who has time for Flask boilerplate?

```python
from fastapi import FastAPI, HTTPException

app = FastAPI()

@app.get("/divide")
def divide(a: float, b: float):
    if b == 0:
        raise HTTPException(status_code=400, detail="Division by zero is not allowed!")
    return {"result": a / b}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

Run it:

```sh
uvicorn main:app --host 0.0.0.0 --port 8000
```

Test it:

```sh
curl "http://localhost:8000/divide?a=10&b=2"
```

### C# Microservice 🤖

Now let’s write the same thing in **ASP.NET Core**.

```csharp
using Microsoft.AspNetCore.Mvc;

[ApiController]
[Route("api")]
public class DivideController : ControllerBase
{
    [HttpGet("divide")]
    public IActionResult Divide([FromQuery] double a, [FromQuery] double b)
    {
        if (b == 0)
            return BadRequest("Division by zero is not allowed!");
        
        return Ok(new { result = a / b });
    }
}
```

Run it:

```sh
dotnet run
```

Test it:

```sh
curl "http://localhost:5000/api/divide?a=10&b=2"
```

Boom! Two microservices ready. Let’s move on. 💪

***

## Step 2: Deploy to AWS with Infrastructure as Code (IaC)

We’ll use **Terraform** for this. Here’s a sample **Terraform** script to deploy an **EC2 instance** with Docker.

```hcl
provider "aws" {
  region = "us-east-1"
}

resource "aws_instance" "microservice" {
  ami           = "ami-12345678"
  instance_type = "t2.micro"

  user_data = <<-EOF
              #!/bin/bash
              docker run -d -p 8000:8000 my-python-microservice
              EOF
}
```

Apply it:

```sh
terraform init
terraform apply
```

Boom! Microservice is on AWS. ☁️

***

## Step 3: Automate with CI/CD (Bitbucket + Bamboo + Octopus)

### **Bitbucket Pipelines**

Add this to `.bitbucket-pipelines.yml` to automate testing and building.

```yaml
image: python:3.9

pipelines:
  default:
    - step:
        script:
          - pip install -r requirements.txt
          - pytest
```

### **Bamboo CI/CD**

1. Create a **Bamboo Plan**.
2. Add a **Build Step** to run tests.
3. Add a **Deployment Step** to push to AWS.

### **Octopus Deploy**

1. Create a **Deployment Process**.
2. Define **Environments** (Dev, Staging, Prod).
3. Add an **AWS Deployment Target**.

***

## **Key Ideas Table**

| Step               | Description                                |
| ------------------ | ------------------------------------------ |
| Build Microservice | Python (FastAPI) and C# (ASP.NET Core)     |
| Deploy to AWS      | Using Terraform for Infrastructure as Code |
| Automate CI/CD     | Bitbucket Pipelines, Bamboo, Octopus       |

***

## **Reference Links**

* https://fastapi.tiangolo.com/
* https://docs.microsoft.com/en-us/aspnet/core/
* https://www.terraform.io/
* https://bitbucket.org/
* https://www.atlassian.com/software/bamboo
* https://octopus.com/

There you go! 🎉 A **Python & C# Microservice**, **AWS Deployment**, and **CI/CD Automation**. Now go forth and divide… your workload! 😆
