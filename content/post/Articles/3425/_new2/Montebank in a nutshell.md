---
title: Mountebank ina Nutshell
description: Mock APIs, databases, and  services
slug: mountebank-overview
date: 2019-04-10
image: post/Articles/IMAGES/36.jpg
categories:
  - Software Development
  - Testing
  - DevOps
tags:
  - Mountebank
  - API Mocking
  - Service Virtualization
  - Testing
draft: false
weight: 654
categories_ref:
  - Software Development
  - Testing
  - DevOps
slug_calculated: https://brianbraatz.github.io/p/mountebank-overview
lastmod: 2025-03-14T16:40:18.231Z
---
# Mountebank: The Swiss Army Knife of API Mocking

## What is Mountebank?

<!-- Okay, so you've heard of **WireMock**, but what if I told you there's another tool that can mock **more than just HTTP APIs**? Meet **Mountebank**—a powerful, open-source **service virtualization tool** that lets you **mock APIs, databases, TCP services, SMTP servers, and more**.  

Think of it like WireMock, but with **extra superpowers**. 💪  

It’s perfect for:   -->

✅ **Mocking REST, SOAP, TCP, and SMTP services**\
✅ **Testing APIs before they even exist**\
✅ **Simulating slow or flaky services**\
✅ **Injecting errors to test edge cases**\
✅ **Running everything in a lightweight Node.js-based setup**

<!-- If you're working with **microservices, CI/CD pipelines, or complex testing scenarios**, **Mountebank** is your new best friend.   -->

***

## How Does Mountebank Work?

It works by creating **imposters**—fake services that act like the real thing.

Here’s how it goes down:

1. **Install & Run Mountebank**
2. **Create an imposter (fake service)**
3. **Send requests to the imposter instead of the real API**
4. **Mountebank responds just like a real service**

Sounds cool? Let’s set it up! 🚀

***

## Setting Up Mountebank

### 🛠️ 1. Install Mountebank

Since Mountebank runs on **Node.js**, installing it is as easy as:

```sh
npm install -g mountebank
```

Then, start Mountebank with:

```sh
mb --allowInjection
```

By default, it runs on **port 2525** and is ready to create imposters.

***

### 🔧 2. Create a Mock API (Imposter)

Now, let’s create a fake API that returns **user data** when you hit `/user/123`.

#### **Option 1: Using a JSON Config File**

Create a file called `imposter.json`:

```json
{
  "port": 3000,
  "protocol": "http",
  "stubs": [
    {
      "predicates": [
        { "equals": { "method": "GET", "path": "/user/123" } }
      ],
      "responses": [
        {
          "is": {
            "statusCode": 200,
            "headers": { "Content-Type": "application/json" },
            "body": "{ \"id\": 123, \"name\": \"John Doe\" }"
          }
        }
      ]
    }
  ]
}
```

Now, load this imposter into Mountebank:

```sh
curl -X POST http://localhost:2525/imposters -H "Content-Type: application/json" -d @imposter.json
```

Boom! 🎉 Now, calling **http://localhost:3000/user/123** will return:

```json
{
  "id": 123,
  "name": "John Doe"
}
```

***

#### **Option 2: Using the HTTP API**

You can also create an imposter dynamically using Mountebank’s API:

```sh
curl -X POST http://localhost:2525/imposters -H "Content-Type: application/json" -d '{
  "port": 3000,
  "protocol": "http",
  "stubs": [
    {
      "predicates": [
        { "equals": { "method": "GET", "path": "/user/123" } }
      ],
      "responses": [
        {
          "is": {
            "statusCode": 200,
            "headers": { "Content-Type": "application/json" },
            "body": "{ \"id\": 123, \"name\": \"John Doe\" }"
          }
        }
      ]
    }
  ]
}'
```

Now, **localhost:3000/user/123** is a fully functional mock API!

***

## Simulating Failures and Delays

Want to test how your app handles **timeouts** or **errors**?

### 🕒 Simulating a Slow Response

```json
{
  "is": {
    "statusCode": 200,
    "body": "{ \"message\": \"This took forever...\" }",
    "_behaviors": {
      "wait": 5000
    }
  }
}
```

Now, calling this endpoint will **delay for 5 seconds** before responding.

### 🔥 Simulating a 500 Server Error

```json
{
  "is": {
    "statusCode": 500,
    "body": "{ \"error\": \"Internal Server Error\" }"
  }
}
```

Perfect for testing **how your app handles failures**!

***

## Mountebank vs. WireMock

So, which one should you use?

| Feature                   | Mountebank                  | WireMock          |
| ------------------------- | --------------------------- | ----------------- |
| **Supports HTTP APIs?**   | ✅ Yes                       | ✅ Yes             |
| **Supports SOAP?**        | ✅ Yes                       | ❌ No              |
| **Supports TCP & SMTP?**  | ✅ Yes                       | ❌ No              |
| **Runs on Node.js?**      | ✅ Yes                       | ❌ No (Java-based) |
| **Dynamic API creation?** | ✅ Yes                       | ✅ Yes             |
| **Best for?**             | Full service virtualization | Mocking REST APIs |

If you’re **only** working with HTTP-based APIs, **WireMock is great**.

But if you need to **mock multiple protocols (HTTP, TCP, SMTP, etc.), Mountebank is the way to go**.

***

## Why Mountebank is Awesome

✅ **Mock Anything** – Not just HTTP, but also TCP, SMTP, and more.\
✅ **Super Fast** – No need to spin up full services; imposters respond instantly.\
✅ **Great for CI/CD** – Easily create and destroy mock services on demand.\
✅ **Open Source & Lightweight** – Runs on **Node.js** with zero dependencies.\
✅ **Powerful Failure Simulation** – Test real-world failures without breaking things.

<!-- ---

## Final Thoughts  

Mountebank is **like WireMock on steroids**—it lets you mock **entire systems** without needing real services.  

If you're building **microservices, working with multiple protocols, or need deep testing**, **Mountebank is a game-changer**.  

Go ahead, **mock all the things!** 🚀   -->

***

## 🔑 Key Ideas

| Key Idea                      | Summary                                                                 |
| ----------------------------- | ----------------------------------------------------------------------- |
| **What is Mountebank?**       | A service virtualization tool for mocking APIs, TCP, and SMTP services. |
| **Why use it?**               | Develop and test without relying on real services.                      |
| **How to set it up?**         | Install via npm, create imposters, and run locally.                     |
| **Can it simulate failures?** | Yes, you can inject delays, errors, and timeouts.                       |
| **Best for?**                 | Mocking multiple protocols in complex testing scenarios.                |

```


```
