---
title: Building a Hybrid On-Prem Cloud App with a Secret API!
description: Examples of mixing cloud and on prem servers
slug: experience-building-a-hybrid
date: 2020-01-25
image: post/Articles/IMAGES/hybridcloud.png
categories:
  - Cloud
  - Amazon Cloud-AWS
  - Azure Cloud
  - Google Cloud-GCP
  - Python
  - CSharp
  - DotNet
  - Microservices
  - Docker
  - Mobile
  - IOS
  - Android
tags:
  - Hybrid
  - Cloud
  - Aws
  - Azure
  - Google
  - Cloud
  - On-Premise
  - Docker
  - Microservices
  - Python
  - C#
  - Cloud
  - Computing
  - Security
draft: false
weight: 30
lastmod: 2025-03-03T13:46:54.559Z
---
<!--
# Experience Building a Hybrid On-Premise Cloud Application, and How to Do It with AWS, Azure, and Google Cloud  
-->

**Background and Related project: eFit Aware IOS and Android Mobile Apps**\
[eFit Aware - Android iPhone Mobile with Azure Cloud Sync](/post/Articles/_EfitAware/eFit%20Aware%20-%20Android%20iPhone%20Mobile%20with%20Azure%20Cloud%20Sync.md)

## Introduction

**Hybrid cloud architecture** is one of those things that **sounds fancy**, but in reality, it's just a **way to mix on-premise computing with cloud services**.

There are many reasons to **not put everything in the cloud**, and I once had a **real-world use case** for this.

> **The Problem:** Our algorithms were **highly sensitive**, and we didn't want to put **even the compiled code on a cloud VM**. The best solution? **Keep our algorithm running on-premise while using the cloud for everything else**.

This article will show **how to build a similar hybrid cloud setup** using the latest features in **AWS, Azure, and Google Cloud**.

***

## What is a Hybrid Cloud?

A **hybrid cloud** is an **IT architecture** where some workloads run **on-premise** (your own servers) while others run in **the public cloud** (AWS, Azure, Google Cloud).

✅ **Why Use a Hybrid Cloud?**

* **Security** – Keep sensitive data/code on-prem.
* **Performance** – Keep high-speed processing local.
* **Cost Savings** – Reduce cloud costs for predictable workloads.
* **Compliance** – Some industries have **regulations** requiring on-premise storage.

### **How I Built a Hybrid Cloud Before Modern Cloud Services**

Back in the day, I **built a hybrid cloud system** using an **early version of Azure**. It worked, but it was **painful**:

* We had **a secret algorithm** running **on a local server in our office**.
* Our cloud-based app **called the on-premise server via a VPN**.
* We **manually managed connections**.

Now, let's **do this properly** with modern **AWS, Azure, and Google Cloud**.

***

## The Hybrid Cloud Architecture

💡 **Use Case:** We will build a **super-secret microservice** that **divides two numbers** (yeah, real spy stuff 🕵️‍♂️).

📌 **How it works:**

1. The **secret division algorithm** runs **on-premise** (Python & C# microservices).
2. A **public cloud microservice (AWS/Azure/GCP)** acts as a **proxy API**.
3. The cloud **forwards API requests** to the on-premise service.

### **Architecture Diagram**

<!--
~~~plaintext
+-----------------------------------+
| Cloud API Gateway (AWS/Azure/GCP) |
+-----------------------------------+
          ⬇ Calls API
+-----------------------------------+
| Cloud Proxy Service (Docker)      |
+-----------------------------------+
          ⬇ Calls On-Prem Service
   +----------------------------+
   | On-Prem Python/C# Service  |
   | (Runs in my office)        |
   +----------------------------+
~~~
-->

![](/post/Articles/_new6/Pasted%20image%2020250210061420.png)

***

## Step 1: Write the Super-Secret Microservice (Python & C#)

### **Python Version** (Runs On-Prem)

```python
from flask import Flask, request, jsonify

app = Flask(__name__)

@app.route('/divide', methods=['POST'])
def divide():
    data = request.json
    try:
        result = data['a'] / data['b']
        return jsonify({"result": result})
    except ZeroDivisionError:
        return jsonify({"error": "Division by zero!"}), 400

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5001)
```

### **C# Version** (Runs On-Prem)

```csharp
using Microsoft.AspNetCore.Mvc;

[ApiController]
[Route("api/[controller]")]
public class DivideController : ControllerBase
{
    [HttpPost]
    public IActionResult Divide([FromBody] DivisionRequest request)
    {
        if (request.B == 0)
            return BadRequest("Division by zero!");

        return Ok(new { result = request.A / request.B });
    }
}

public class DivisionRequest
{
    public double A { get; set; }
    public double B { get; set; }
}
```

***

## Step 2: Deploy the Microservice in Docker (On-Prem)

Create a **Dockerfile**:

```dockerfile
FROM python:3.9
WORKDIR /app
COPY app.py .
RUN pip install flask
CMD ["python", "app.py"]
```

Build and Run:

```sh
docker build -t secret-division .
docker run -p 5001:5001 secret-division
```

Now, we have **our secret microservice running under my desk**.

***

## Step 3: Build the Cloud Proxy Microservice

### **Cloud Proxy Service (Python Flask)**

```python
import requests
from flask import Flask, request, jsonify

app = Flask(__name__)
ON_PREM_SERVICE = "http://my-office-server:5001/divide"

@app.route('/divide', methods=['POST'])
def proxy_divide():
    data = request.json
    response = requests.post(ON_PREM_SERVICE, json=data)
    return response.json(), response.status_code

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)
```

***

## Step 4: Deploy the Proxy Service in AWS, Azure, and Google Cloud

### **AWS: Use Elastic Beanstalk or Lambda + API Gateway**

4. Deploy the proxy microservice using **AWS Elastic Beanstalk**.
5. Set up **AWS API Gateway** to expose it publicly.
6. Configure **AWS Site-to-Site VPN** to connect to the on-prem service.

### **Azure: Use Azure App Service + VPN Gateway**

7. Deploy the proxy microservice to **Azure App Service**.
8. Use **Azure API Management** to expose it.
9. Configure **Azure VPN Gateway** to reach the on-prem service.

### **Google Cloud: Use Cloud Run + Hybrid Connectivity**

10. Deploy the proxy to **Google Cloud Run**.
11. Expose it via **Google API Gateway**.
12. Use **Google Cloud Interconnect** to talk to on-prem services.

***

## Key Takeaways

* **Hybrid Cloud = Best of Both Worlds** → Security + Cloud Benefits.
* **We kept our sensitive division algorithm on-prem while using cloud for API proxying.**
* **Modern hybrid cloud solutions** (AWS VPN, Azure Hybrid Cloud, Google Interconnect) make this easy.

***

## References

13. [AWS Hybrid Cloud](https://aws.amazon.com/hybrid/)
14. [Azure Hybrid Cloud](https://azure.microsoft.com/en-us/solutions/hybrid-cloud/)
15. [Google Hybrid Cloud](https://cloud.google.com/hybrid-cloud)
