---
title: Docker Containerization of C# Blazor SignalR Microservice
description: Docker Containerization of C# Blazor SignalR Microservice
slug: docker-containerization-signalr-microservice
date: 2022-04-02
image: post/Articles/IMAGES/lockerlogo.png
categories:
  - CSharp
  - DotNet
  - Docker
  - Cloud
  - Microservices
  - SignalR
tags:
  - Docker
  - Blazor
  - CORS
  - NGINX
  - Signalr
  - Microservices
  - Webassembly
  - Server
  - Websockets
  - WebDevelopment
  - CSharp
  - DotNet
draft: false
weight: 30
categories_ref:
  - CSharp
  - DotNet
  - Docker
  - Cloud
  - Microservices
  - SignalR
lastmod: 2025-03-14T15:45:09.011Z
---
# Docker Containerization of C# Blazor SignalR Microservice

So, you've got yourself a fancy Blazor app with SignalR running in Docker, and now your users are seeing some cryptic error messages instead of your beautiful UI?

Well, you're not alone!

(note; we are HOPING you didnt deploy a broken app to production :) ... )

This is the classic "Docker networking meets SignalR" problem, and today, we’re going to fix it.

But first, let's break things down.

## What is Docker?

Docker is like a lunchbox for your apps.

It packs everything needed to run your application—code, dependencies, and even the kitchen sink—into a neat little container that works anywhere.

Think of it as a portable environment that runs your app consistently, whether on your machine or in the cloud.

[Wikipedia - Docker](https://en.wikipedia.org/wiki/Docker_\(software\))

## What is Blazor?

Blazor is Microsoft's attempt to make .NET developers feel cool again.

It lets you build interactive web UIs using C# instead of JavaScript.

There are two main flavors of Blazor:

* **Blazor WebAssembly (client-side)** - Runs entirely in the browser, no server needed (like React but C#-ified).
* **Blazor Server (server-side)** - Renders UI changes on the server and sends updates via SignalR.

[Blazor on Microsoft Docs](https://learn.microsoft.com/en-us/aspnet/core/blazor/)

## What is SignalR?

SignalR is Microsoft's real-time WebSockets magic sauce that lets your server talk to clients in real-time. It’s great for things like chat apps, live dashboards, and stock tickers.

[Wikipedia - SignalR](https://en.wikipedia.org/wiki/SignalR)

## Challenges of Running SignalR in Docker

Running SignalR inside Docker can be tricky because:

1. **IP Address Issues**: Your container has a different IP than the host.
2. **CORS & WebSockets**: Cross-Origin Resource Sharing and WebSocket connections might not be properly configured.
3. **Reverse Proxy Conflicts**: If you're using NGINX or another reverse proxy, you need to forward WebSockets correctly.

## How to Fix It

### 1. Configure SignalR Correctly

Make sure your SignalR hub is reachable from outside the container:

```csharp
var hubConnection = new HubConnectionBuilder()
    .WithUrl("http://<external-ip-or-domain>/hub")
    .Build();
```

### 2. Expose Docker Ports Properly

```bash
docker run -p 5000:5000 -p 5001:5001 your-blazor-app
```

### 3. Set `ASPNETCORE_URLS`

```bash
ASPNETCORE_URLS=http://+:5000;https://+:5001
```

### 4. Configure CORS

```csharp
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAll", builder =>
    {
        builder.AllowAnyOrigin()
               .AllowAnyMethod()
               .AllowAnyHeader();
    });
});
app.UseCors("AllowAll");
```

### 5. Verify WebSockets in Reverse Proxy (NGINX Example)

```nginx
server {
    listen 80;
    location / {
        proxy_pass http://your-container-ip:5000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";
        proxy_set_header Host $host;
    }
}
```

<!-- 


## Conclusion
Dockerizing Blazor SignalR apps can be tricky, but with these fixes, you’ll be on your way to real-time awesomeness. Now go forth and conquer the web!
-->

***

## References

* [Docker on Wikipedia](https://en.wikipedia.org/wiki/Docker_\(software\))
* [Blazor Docs](https://learn.microsoft.com/en-us/aspnet/core/blazor/)
* [SignalR on Wikipedia](https://en.wikipedia.org/wiki/SignalR)
* [WebSockets on MDN](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API)
