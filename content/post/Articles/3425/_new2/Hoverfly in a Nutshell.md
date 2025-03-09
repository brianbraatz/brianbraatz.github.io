---
title: "Hoverfly in a Nutshell: Lightweight API Mocking for Developers"
description: Simulate HTTP and HTTPS services in testing, development, and CI/CD pipelines.
slug: hoverfly-nutshell
date: 2016-10-05
image: post/Articles/IMAGES/44.jpg
categories:
  - Software Development
  - Testing
  - DevOps
tags:
  - Hoverfly
  - API Mocking
  - Service Virtualization
  - Testing
draft: false
weight: 485
lastmod: 2025-03-08T12:43:00.063Z
---
# Hoverfly in a Nutshell: Lightweight API Mocking for Developers

## What is Hoverfly?

<!-- 
Alright, so we've talked about **WireMock** and **Mountebank**, but now let's meet **Hoverfly**—a lightweight, **proxy-based** service virtualization tool that makes API simulation easy.  

Hoverfly shines when you need to:   -->

✅ **Intercept & record real API calls for later replay**\
✅ **Simulate APIs without manually defining every response**\
✅ **Mock HTTPS services (without self-signed cert nightmares)**\
✅ **Test latency, failures, and rate limits effortlessly**\
✅ **Run inside CI/CD pipelines without headaches**

It works by sitting **between your app and the real API**, capturing traffic, and playing it back **like a time-traveling API recorder**. 🕰️

***

## How Hoverfly Works

Hoverfly runs in different **modes**, depending on your use case:

1. **Capture Mode** – Records real API traffic.
2. **Simulate Mode** – Replays recorded interactions as mock responses.
3. **Modify Mode** – Intercepts requests and modifies responses on the fly.
4. **Synthesize Mode** – Generates dynamic responses based on request parameters.

Unlike WireMock (which requires defining responses manually), Hoverfly can **learn from real traffic** and then act as a **stand-in API**.

***

## Setting Up Hoverfly

### 🛠️ 1. Install Hoverfly

You can grab Hoverfly from the [official releases](https://github.com/SpectoLabs/hoverfly) or install it via:

#### **Linux & macOS**

```sh
curl -L https://github.com/SpectoLabs/hoverfly/releases/latest/download/hoverfly -o hoverfly
chmod +x hoverfly
sudo mv hoverfly /usr/local/bin/
```

#### **Windows (via Chocolatey)**

```sh
choco install hoverfly
```

Now, check if it’s working:

```sh
hoverfly -version
```

***

### 🚀 2. Start Hoverfly

Run Hoverfly in **simulation mode**:

```sh
hoverfly -webserver
```

This starts Hoverfly as a **local API mock server** on port **8500**.

Now, any request sent to Hoverfly **can be intercepted, recorded, or simulated**!

***

### 🎬 3. Capture Real API Calls

Hoverfly can **record API interactions**, so you don’t have to define every response manually.

Start recording:

```sh
hoverctl start
hoverctl mode capture
```

Now, send real API requests through Hoverfly:

```sh
curl --proxy http://localhost:8500 https://jsonplaceholder.typicode.com/todos/1
```

Hoverfly **captures** the request and response, storing them for later playback.

Stop capturing:

```sh
hoverctl stop
```

***

### 🔁 4. Replay API Responses (Simulation Mode)

Now, restart Hoverfly in **simulate mode**:

```sh
hoverctl start
hoverctl mode simulate
```

Now, calling the same API **doesn’t hit the real server**—Hoverfly **replays the response from memory**:

```sh
curl --proxy http://localhost:8500 https://jsonplaceholder.typicode.com/todos/1
```

Boom! 🎉 Now you can test **without needing internet access or the real API**.

***

## Simulating Errors & Slow Responses

Want to test how your app handles **timeouts** or **rate limits**? Hoverfly makes it **stupidly simple**.

### 🕒 Add Artificial Delays

```sh
hoverctl delay --url-pattern ".*" --delay 2000ms
```

Now, every request **will take 2 seconds** to respond.

### 🔥 Simulate a 500 Error

Modify a response on the fly:

```sh
hoverctl response-template --path "/api/v1/users" --status 500 --body "{ \"error\": \"Internal Server Error\" }"
```

Now, requests to `/api/v1/users` **return a 500 error** instead of real data.

***

## Using Hoverfly in CI/CD Pipelines

One of Hoverfly’s **killer features** is how easily it integrates into **CI/CD workflows**.

🔹 **Mock APIs in automated tests** so your CI doesn’t fail due to flaky services.\
🔹 **Record once, replay forever**—no need to hit real APIs during testing.\
🔹 **Run in Docker or Kubernetes** for scalable service virtualization.

For example, in **JUnit (Java),** you can mock API responses:

```java
import io.specto.hoverfly.junit.rule.HoverflyRule;
import static io.specto.hoverfly.junit.core.HoverflyMode.SIMULATE;

@Rule
public HoverflyRule hoverflyRule = HoverflyRule.inSimulationMode();

@Test
public void testMockedApi() {
    stubFor(get(urlEqualTo("/users/1"))
        .willReturn(aResponse()
            .withStatus(200)
            .withHeader("Content-Type", "application/json")
            .withBody("{ \"id\": 1, \"name\": \"Jane Doe\" }")));

    // Now make the actual API call...
}
```

Now your **tests pass** even if the real API is down! 🎉

***

## Hoverfly vs. WireMock vs. Mountebank

So, which tool should you use?

| Feature                           | Hoverfly                   | WireMock           | Mountebank                 |
| --------------------------------- | -------------------------- | ------------------ | -------------------------- |
| **Mock REST APIs?**               | ✅ Yes                      | ✅ Yes              | ✅ Yes                      |
| **Mock HTTPS APIs?**              | ✅ Yes (Built-in)           | ⚠️ Harder          | ⚠️ Needs workarounds       |
| **Capture API Calls?**            | ✅ Yes                      | ❌ No               | ✅ Yes                      |
| **Modify Responses Dynamically?** | ✅ Yes                      | ❌ No               | ✅ Yes                      |
| **Simulate Network Delays?**      | ✅ Yes                      | ✅ Yes              | ✅ Yes                      |
| **Supports TCP & SMTP?**          | ❌ No                       | ❌ No               | ✅ Yes                      |
| **Best for?**                     | Recording & replaying APIs | Manual API mocking | Mocking multiple protocols |

🔹 **Use WireMock** if you want **precise, code-defined API mocks**.\
🔹 **Use Mountebank** if you need **mocking for HTTP, TCP, or SMTP**.\
🔹 **Use Hoverfly** if you need **API recording, HTTPS support, and easy CI/CD integration**.

***

## Why Hoverfly is Awesome

✅ **Effortless API Recording** – No need to manually write mocks.\
✅ **First-Class HTTPS Support** – Works without painful cert setups.\
✅ **Dynamic Response Modification** – Easily tweak responses on the fly.\
✅ **CI/CD Friendly** – Runs in **Docker, Kubernetes, and headless environments**.\
✅ **Tiny & Fast** – Uses minimal resources compared to heavyweight mocking tools.

***

<!-- ## Final Thoughts  

Hoverfly is like **a DVR for APIs**—it records real traffic and lets you replay it whenever you want.  

If you’re tired of **hardcoding API mocks**, dealing with **flaky services**, or struggling with **HTTPS testing**, **Hoverfly is the perfect solution**.  

Give it a try, **mock everything**, and never let slow or unreliable APIs slow you down again! 🚀   -->

***

## 🔑 Key Ideas

| Key Idea              | Summary                                                                      |
| --------------------- | ---------------------------------------------------------------------------- |
| **What is Hoverfly?** | A lightweight, proxy-based API mocking tool.                                 |
| **Why use it?**       | Capture & replay API calls, simulate failures, and speed up testing.         |
| **How to use it?**    | Install Hoverfly, record traffic, replay responses.                          |
| **Supports HTTPS?**   | Yes, built-in support.                                                       |
| **Best for?**         | Automating API testing, CI/CD pipelines, and simulating real-world failures. |
