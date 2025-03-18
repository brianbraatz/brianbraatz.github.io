---
title: What's New in ASP.NET Core 8?
description: Casual Dive into the Latest Features
slug: whats-new-aspnet-core-8
date: 2023-12-15
image: post/Articles/IMAGES/ASP.NET_.png
categories:
  - ASP.NET Core
  - Web Development
  - .NET 8
tags:
  - ASP.NET
  - Core
  - Blazor
  - SignalR
  - Minimal
  - APIs
  - AOT
  - Performance
draft: false
weight: 35
categories_ref:
  - ASP.NET Core
  - Web Development
  - .NET 8
slug_calculated: https://brianbraatz.github.io/p/whats-new-aspnet-core-8
lastmod: 2025-03-14T16:40:12.721Z
---
<!-- 
If you thought ASP.NET Core 7 was a game-changer, buckle up—because ASP.NET Core 8 is here with even more cool stuff. Microsoft keeps pushing the boundaries, making web development faster, easier, and more efficient. Let’s dive into the juicy updates! -->

## Blazor Goes Full-Stack

Blazor is stepping up big time in .NET 8.

Now, it’s not just for WebAssembly or server-side rendering—it’s a full-stack web UI framework with multiple rendering options:

* **Static Server Rendering (SSR):** Pre-renders HTML on the server for super-fast initial page loads.
* **Interactive Server Rendering:** Uses Blazor’s real-time magic to make pages interactive without reloading.
* **Interactive WebAssembly Rendering:** Runs directly in the browser via WebAssembly.
* **Interactive Auto Rendering:** A hybrid approach that starts on the server, then switches to WebAssembly.

Plus, there’s a new **Blazor Web App** template that unifies everything.

No more jumping between different hosting models—just one template to rule them all!

If you ever did work with the previous version, comparing and deciding on what model you were going to use was messy, because the code from each of the starter templates was very different..

I am glad that did this...

## SignalR Gets Smarter

SignalR now has **stateful reconnect**, meaning if a connection drops for a moment, your app won’t freak out.

It keeps track of messages and resends any that got lost, so users don’t even notice the hiccup.

## Native AOT Compilation = Blazing Fast Performance

**Native Ahead-of-Time (AOT) compilation** is now supported. This means:

* Faster startup times
* Smaller app sizes
* Lower memory usage

It’s especially great for **gRPC, minimal APIs, and worker services**, where performance is king.

## Minimal APIs Keep Getting Better

Minimal APIs are already super clean, but ASP.NET Core 8 adds even more sugar:

* **User Override Culture:** More flexibility with localization.
* **Native AOT Support:** Combine minimal APIs with AOT for ultra-fast microservices.

If you love writing less boilerplate, minimal APIs are still the way to go.

## Better Monitoring with ASP.NET Core Metrics

Monitoring your app just got easier. ASP.NET Core 8 introduces **built-in metrics** using `System.Diagnostics.Metrics`. This means:

* Better insights into app performance
* Easier debugging and diagnostics
* Proactive monitoring before issues escalate

## Wrapping Up

ASP.NET Core 8 builds on everything great from Core 7 and cranks it up a notch. Whether you're into Blazor, real-time apps, or just want faster, more efficient code, there’s something in here for you.

***

## Key Ideas

| Feature                  | Summary                                                 |
| ------------------------ | ------------------------------------------------------- |
| **Blazor Enhancements**  | Unified Web UI framework with multiple rendering modes. |
| **SignalR Improvements** | Stateful reconnect for better real-time interactions.   |
| **Native AOT**           | Faster startup, smaller app size, lower memory usage.   |
| **Minimal API Upgrades** | Improved localization and AOT support.                  |
| **ASP.NET Core Metrics** | Built-in monitoring tools for better insights.          |
