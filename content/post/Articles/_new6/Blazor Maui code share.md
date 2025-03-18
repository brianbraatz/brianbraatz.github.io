---
title: Blazor Hybrid with MAUI- Share code?
description: Can You Share Code with Your Website? Should You?
slug: blazor-hybrid-with-maui-can-you-share-code
date: 2024-07-19
image: post/Articles/IMAGES/blazor.png
categories:
  - Cloud
  - Maui
  - WPF
  - Blazor
  - Web Development
  - Mobile
  - IOS
  - Android
  - Cross Platform
  - ASP.NET
  - Asp.Net
  - Mobile
tags:
  - Blazor
  - Blazor
  - Hybrid
  - Maui
  - Code
  - Sharing
  - Csharp
  - Web
  - Development
  - Mobile
  - Development
  - Cross-Platform
  - Architecture
  - Webassembly
draft: false
weight: 145
categories_ref:
  - Cloud
  - Maui
  - WPF
  - Blazor
  - Web Development
  - Mobile
  - IOS
  - Android
  - Cross Platform
  - ASP.NET
  - Asp.Net
  - Mobile
slug_calculated: https://brianbraatz.github.io/p/blazor-hybrid-with-maui-can-you-share-code
lastmod: 2025-03-14T16:40:28.699Z
---
<!--
# Advantages and Disadvantages of Blazor Hybrid with MAUI. Can You Share Code with Your Website? Should You? How to Structure Your MAUI Project to Share Code with Your Blazor Website. Alternative Approaches Pros and Cons
-->

## Introduction

So, you’ve heard about **Blazor Hybrid** and **MAUI**, and now you’re wondering:

* *Can I share code between my MAUI app and my Blazor website?*
* *Should I even try?*

Short answer: **Yes, but…**

Long answer: It depends on your **project structure, goals, and how much frustration you can handle**.

<!--
In this article, we’ll explore:

- The **history of Blazor Hybrid & MAUI**
- **How to structure a project** to share code between MAUI and a Blazor website
- The **pros and cons** of different approaches
- **Code examples** showing how to make it all work
-->

***

## A Brief History of Blazor Hybrid and MAUI

### **Blazor: From WebAssembly to Hybrid Apps**

Blazor started as a **WebAssembly-based framework** for running **.NET in the browser**. Over time, it evolved into **Blazor Server, Blazor WebAssembly, and Blazor Hybrid**.

### **MAUI: Xamarin’s Successor**

.NET MAUI (**Multi-platform App UI**) is the evolution of **Xamarin**, allowing developers to build **cross-platform native apps** with a **single codebase**.

Maui covers iPhone, iPad, Android, Windows, and Mac OS.

### **Blazor Hybrid + MAUI: The Best of Both Worlds?**

Blazor Hybrid **embeds a Blazor WebView inside a MAUI app**, allowing you to reuse **Blazor components** in **desktop and mobile applications**.

***

## Pros and Cons of Blazor Hybrid with MAUI

| Feature              | Pros                                             | Cons                         |
| -------------------- | ------------------------------------------------ | ---------------------------- |
| **Code Sharing**     | Reuse Blazor components across web & MAUI        | May require refactoring      |
| **Performance**      | Runs .NET natively, faster than WebAssembly      | Some overhead with WebView   |
| **UI Consistency**   | Blazor UI looks the same on all platforms        | Not truly native UI          |
| **Ease of Learning** | Developers familiar with Blazor can reuse skills | XAML developers may struggle |
| **Flexibility**      | Works on Windows, macOS, iOS, and Android        | Limited native API access    |

***

## Can You Share Code Between MAUI and Blazor?

**Yes!** The key is **structuring your project correctly**. Here’s how:

### **Best Way to Share Code**: Use a **Shared .NET Standard Library**

1. Create a **.NET Standard Class Library** for **shared business logic**.
2. Add the shared library to **both your Blazor website and MAUI app**.
3. Reference this library instead of duplicating logic.

***

## Structuring Your MAUI + Blazor Project for Code Sharing

### **Project Structure Example**

```plaintext
/MySolution
  ├── SharedLibrary (Contains shared logic)
  ├── BlazorWebsite (ASP.NET Blazor WebAssembly)
  ├── MauiApp (MAUI Blazor Hybrid App)
```

### **Shared Code Example** (Inside `SharedLibrary`)

```csharp
public class WeatherService
{
    public string GetWeather()
    {
        return "Sunny with a chance of Blazor!";
    }
}
```

### **Using the Shared Code in Blazor**

```razor
@inject WeatherService WeatherService

<h1>Weather: @WeatherService.GetWeather()</h1>
```

### **Using the Shared Code in MAUI**

```csharp
var weatherService = new WeatherService();
var weather = weatherService.GetWeather();
Console.WriteLine(weather);
```

***

## Should You Share Code Between MAUI and Blazor?

It **depends** on your project’s needs. If you’re building:

4. **A web + mobile app that shares logic** → YES, share code via a **shared library**.
5. **A web app that looks truly native on mobile** → MAYBE, but Blazor Hybrid has limitations.
6. **A performance-critical mobile app** → NO, use MAUI’s native controls instead.

***

## Alternative Approaches and Their Pros & Cons

| Approach                        | Pros               | Cons                    |
| ------------------------------- | ------------------ | ----------------------- |
| **Blazor Hybrid**               | Easy code reuse    | Not truly native UI     |
| **Blazor WebAssembly + API**    | Fully web-based    | Requires API for mobile |
| **MAUI with Razor Components**  | Native UI + Blazor | More complexity         |
| **Separate MAUI & Blazor Apps** | Best performance   | More maintenance        |

***

## Key Ideas

* **Blazor Hybrid allows Blazor components to run in MAUI apps, but it’s not truly native.**
* **A shared .NET Standard library is the best way to share code.**
* **If performance is a priority, MAUI’s native approach is better.**
* **Blazor WebAssembly + API is a great alternative for cross-platform apps.**

***

## References

7. [Blazor Hybrid Documentation](https://learn.microsoft.com/en-us/aspnet/core/blazor/hybrid)
8. [MAUI Documentation](https://learn.microsoft.com/en-us/dotnet/maui/)
9. [Blazor WebAssembly vs Blazor Server](https://dotnet.microsoft.com/en-us/apps/aspnet/web-apps/blazor)
10. [How to Structure a MAUI & Blazor Project](https://devblogs.microsoft.com/dotnet/how-to-structure-blazor-and-maui/)
11. [Blazor Hybrid vs Blazor WebAssembly](https://www.thinktecture.com/en/blazor/blazor-hybrid-vs-blazor-webassembly/)
