---
title: What's New in ASP.NET Core 7
description: Casual Dive into the Latest Features
slug: whats-new-in-asp-net-core7
date: 2022-12-15
image: post/Articles/IMAGES/ASP.NET_.png
categories:
  - ASP.NET Core
  - Web Development
  - Software Updates
tags:
  - ASP.NET
  - Core
  - Web
  - Development
  - Blazor
  - Minimal
  - APIs
  - gRPC
  - SignalR
  - HTTP/3
  - Output
  - Caching
  - Rate
  - Limiting
draft: "False"
weight: "131"
categories_ref:
  - ASP.NET Core
  - Web Development
  - Software Updates
lastmod: 2025-03-14T15:45:04.783Z
---
<!-- 

# What's New in ASP.NET Core 7: A Casual Dive into the Latest Features

Hey there, fellow code wranglers! 🎉 ASP.NET Core 7 has just dropped, and it's packed with goodies that'll make your developer heart sing.

Let's take a laid-back stroll through the coolest new features, shall we?
-->

## Servers and Runtime: Speed and Control, Baby!

* **Rate Limiting**: Ever had that one friend who just won't stop texting?

Now you can set boundaries—at least with your server.

Limit request rates with flexible endpoint configurations and policies.

No more server overwhelm! [Learn more](https://learn.microsoft.com/aspnet/core/fundamentals/rate-limiting)

* **Output Caching**: Remember that time you memorized all the answers for trivia night?

This is like that but for your server responses.

Cache 'em and serve 'em up hot and fast. [Learn more](https://learn.microsoft.com/aspnet/core/performance/caching/output)

* **Request Decompression**: Got incoming requests all zipped up?

No worries.

ASP.NET Core 7 can now handle compressed content straight outta the box. [Learn more](https://learn.microsoft.com/aspnet/core/performance/request-decompression)

* **HTTP/3 Support**: Faster, more reliable connections?

Yes, please!

Say hello to built-in HTTP/3 support, riding on the shiny new QUIC protocol. [Learn more](https://learn.microsoft.com/aspnet/core/fundamentals/servers/kestrel/http3)

* **WebSockets over HTTP/2**: WebSockets are cool.

HTTP/2 is cool.

Together?

Super cool.

Now you can run WebSockets over HTTP/2 connections. [Learn more](https://learn.microsoft.com/aspnet/core/fundamentals/websockets)

* **WebTransport (Experimental)**: Think of it as WebSockets' cooler cousin.

Create streams and datagrams over HTTP/3.

It's experimental, so tread lightly! [Learn more](https://datatracker.ietf.org/doc/html/draft-ietf-webtrans-http3)

## Minimal APIs: Less Code, More Power

* **Endpoint Filters**: Ever wish you could run some code before or after your route handlers without the hassle?

Now you can with endpoint filters.

It's like having a bouncer for your endpoints. [Learn more](https://learn.microsoft.com/aspnet/core/fundamentals/minimal-apis/filters)

* **Typed Results**: Strongly typed results in minimal APIs?

You betcha!

Keep things neat and type-safe. [Learn more](https://learn.microsoft.com/aspnet/core/fundamentals/minimal-apis/typed-results)

* **Route Groups**: Organize your endpoints like a pro with route groups.

Group 'em up with a common prefix and keep your codebase tidy. [Learn more](https://learn.microsoft.com/aspnet/core/fundamentals/minimal-apis)

## gRPC: Now with Extra JSON Flavor

* **JSON Transcoding**: Expand your gRPC services' horizons by exposing them as JSON-based APIs.

It's like giving them a second language. [Learn more](https://learn.microsoft.com/aspnet/core/grpc/json-transcoding)

* **OpenAPI with JSON Transcoding (Experimental)**: Generate OpenAPI specs for your gRPC JSON transcoded services.

It's experimental, so handle with care! [Learn more](https://learn.microsoft.com/aspnet/core/grpc/json-transcoding#openapi-support)

* **gRPC Health Checks**: Keep an eye on your gRPC server apps' health with built-in health checks.

Because nobody likes unexpected downtime. [Learn more](https://learn.microsoft.com/aspnet/core/grpc/health-checks)

* **gRPC Client `AddCallCredentials`**: Need to send authorized requests?

Now your gRPC clients can use bearer tokens with ease. [Learn more](https://learn.microsoft.com/aspnet/core/grpc/authn-and-authz#client)

## SignalR: Better Communication, Happier Apps

* **Client Results**: Ever wanted your clients to send results back to the server in response to server requests?

Now they can!

It's like having a two-way street for your data. [Learn more](https://learn.microsoft.com/aspnet/core/signalr/streaming)

## MVC: Nullable Awesomeness

* **Nullable View and Page Models**: Embrace the null!

Now your view and page models support nullable reference types, making null state checking a breeze. [Learn more](https://learn.microsoft.com/aspnet/core/mvc/models/validation)

## Blazor: Web Development, Evolved

* **Custom Elements**: Build standard HTML custom elements with Blazor and integrate them into any JavaScript-based app.

It's like having your cake and eating it too. [Learn more](https://learn.microsoft.com/aspnet/core/blazor/components/custom-elements)

* **Handle Location Changing Events**: Intercept location changes to create custom navigation experiences.

Because who doesn't like a personalized touch? [Learn more](https://learn.microsoft.com/aspnet/core/blazor/components/routing)

* **Bind After/Get/Set Modifiers**: Run async logic after data binding and control how data binding gets and sets data.

More control, less hassle. [Learn more](https://learn.microsoft.com/aspnet/core/blazor/components/data-binding)

## Performance Improvements: Faster Than Ever

* **HTTP/2 Performance Boosts**: Kestrel's been hitting the gym.

Expect reduced CPU usage and higher throughput for HTTP/2 requests. [Learn more](https://devblogs.microsoft.com/dotnet/asp-net-core-updates-in-dotnet-7-preview-4/)

* **gRPC JSON Transcoding Performance**: Serialize those messages faster and with less memory.

Your app just got a turbo boost. [Learn more](https://devblogs.microsoft.com/dotnet/asp-net-core-updates-in-dotnet-7-preview-7/)

## Get Started Today!

Ready to dive in?

Grab the .NET 7 SDK and start exploring these features.

Your code will thank you.

Happy coding! 🚀

***

**Key Ideas Recap:**

| Feature               | Description                                                              |
| --------------------- | ------------------------------------------------------------------------ |
| Rate Limiting         | Control the rate of incoming requests to prevent overload.               |
| Output Caching        | Cache responses to serve repeated requests more efficiently.             |
| Request Decompression | Automatically handle compressed incoming requests.                       |
| HTTP/3 Support        | Leverage the latest HTTP protocol for faster, more reliable connections. |
