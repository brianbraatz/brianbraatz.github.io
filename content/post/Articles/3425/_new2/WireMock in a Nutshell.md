---
title: WireMock in a Nutshell
description: " mock APIs for testing, debugging, and development without relying on real servers."
slug: wiremock-nutshell
date: 2018-07-22
image: post/Articles/IMAGES/41.jpg
categories:
  - Software Development
  - Testing
  - DevOps
tags:
  - WireMock
  - API Mocking
  - Service Virtualization
  - Testing
draft: false
weight: 712
categories_ref:
  - Software Development
  - Testing
  - DevOps
slug_calculated: https://brianbraatz.github.io/p/wiremock-nutshell
lastmod: 2025-03-14T16:40:18.299Z
---
# WireMock in a Nutshell: Mock APIs Like a Pro

## What is WireMock?

Alright, let’s talk **WireMock**—aka **your best friend when APIs aren't behaving**.

WireMock is a **flexible API mocking tool** that lets you simulate real API behavior *without* calling the actual service. Need a fake API that responds like a real one? WireMock has your back.

It’s perfect for:

✅ **Developing when the backend isn’t ready**\
✅ **Testing edge cases without breaking real systems**\
✅ **Avoiding API rate limits and expensive third-party calls**\
✅ **Simulating slow or faulty responses for better error handling**

Think of it as an **API stunt double**—it acts like the real API, but without the risk of things going *boom* in production. 💥

***

## How Does WireMock Work?

It’s **stupidly simple**. You just:

1. **Define an API endpoint** (`/get-user`)
2. **Set up a fake response** (e.g., return user data)
3. **Run WireMock**
4. **Make requests to your local mock server**

Here’s what that looks like in practice.

***

## Setting Up WireMock

### 🛠️ 1. Install WireMock

#### **Option 1: Run as a Standalone JAR**

If you have **Java**, you can just download and run it:

```sh
java -jar wiremock-standalone-<version>.jar
```

This starts WireMock on port **8080** by default.

#### **Option 2: Run via Docker**

Prefer containers? No problem.

```sh
docker run -p 8080:8080 wiremock/wiremock
```

Now, WireMock is running locally, listening for API calls.

***

### 🔧 2. Create a Mock API

Let’s say we want to mock a **GET /user/123** API that returns user data.

#### **Option 1: Using JSON Mapping**

Create a file:

📄 `mappings/get-user.json`

```json
{
  "request": {
    "method": "GET",
    "url": "/user/123"
  },
  "response": {
    "status": 200,
    "body": "{ \"id\": 123, \"name\": \"John Doe\" }",
    "headers": {
      "Content-Type": "application/json"
    }
  }
}
```

Now, when you send a request to **http://localhost:8080/user/123**, you get:

```json
{
  "id": 123,
  "name": "John Doe"
}
```

#### **Option 2: Using HTTP API**

You can also create mocks dynamically using WireMock’s admin API:

```sh
curl -X POST http://localhost:8080/__admin/mappings -H "Content-Type: application/json" -d '{
  "request": {
    "method": "GET",
    "url": "/user/123"
  },
  "response": {
    "status": 200,
    "body": "{ \"id\": 123, \"name\": \"John Doe\" }"
  }
}'
```

Now, the same **/user/123** endpoint will return **mocked data**! 🎉

***

## Simulating Slow or Broken APIs

WireMock isn’t just for happy paths. You can also simulate **timeouts, slow responses, or errors**.

### 🕒 Simulating a Slow Response

Want to see how your app behaves when an API takes 5 seconds to respond?

```json
{
  "request": {
    "method": "GET",
    "url": "/slow-api"
  },
  "response": {
    "status": 200,
    "body": "{ \"message\": \"This took a while...\" }",
    "fixedDelayMilliseconds": 5000
  }
}
```

Now, any request to **/slow-api** will take **5 seconds** to return.

### 🔥 Simulating a 500 Server Error

If you want to test how your app reacts to server failures, just return a **500 status code**:

```json
{
  "request": {
    "method": "GET",
    "url": "/error"
  },
  "response": {
    "status": 500,
    "body": "{ \"error\": \"Internal Server Error\" }"
  }
}
```

Perfect for testing **error handling and retry logic**. 🛠️

***

## WireMock in Java

If you’re coding in **Java**, you can use WireMock as a **JUnit test server**:

```java
import com.github.tomakehurst.wiremock.WireMockServer;
import static com.github.tomakehurst.wiremock.client.WireMock.*;

public class WireMockExample {
    public static void main(String[] args) {
        WireMockServer wireMockServer = new WireMockServer(8080);
        wireMockServer.start();

        wireMockServer.stubFor(get(urlEqualTo("/user/123"))
            .willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody("{ \"id\": 123, \"name\": \"John Doe\" }")));

        System.out.println("WireMock running...");
    }
}
```

Now, **localhost:8080/user/123** will return mock data from your Java app.

***

## Why WireMock is Awesome

🚀 **Works with Any Tech Stack** – Frontend, backend, mobile apps—WireMock doesn’t care. It just mocks.

⚡ **Super Fast Testing** – Run API tests in milliseconds instead of waiting on real services.

🔄 **Dynamic & Flexible** – Easily create, update, or remove mock APIs via JSON or API calls.

🛠️ **Great for Dev & QA** – Developers can use it for local testing; QA can use it for automated test suites.

***

## Final Thoughts

WireMock is a **lifesaver** when you’re working with APIs that are slow, expensive, unreliable, or *just not ready yet*.

If you’ve ever thought,\
*"Ugh, I wish I could just fake this API for now!"*—\
**WireMock is exactly what you need.**

G

## 🔑 Key Ideas

| Key Idea                      | Summary                                                                             |
| ----------------------------- | ----------------------------------------------------------------------------------- |
| **What is WireMock?**         | A tool for mocking APIs locally for development and testing.                        |
| **Why use it?**               | Simulate API responses, avoid slow or costly dependencies, and test error handling. |
| **How to use it?**            | Install, create mock endpoints, and run locally.                                    |
| **Can it simulate failures?** | Yes—timeouts, slow responses, and error codes are all supported.                    |
| **Best for?**                 | Developers, testers, and CI/CD pipelines.                                           |
