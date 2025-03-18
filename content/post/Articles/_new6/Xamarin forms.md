---
title: Xamarin.Forms vs Xamarin.Native
description: Which One Should You Choose?
slug: xamarinforms-vs-xamarinnative
date: 2017-01-19
image: post/Articles/IMAGES/xmarinlogo.png
categories:
  - XAML
  - Xamarin
  - WPF
  - Xamarin Forms
  - Xamarin Native
  - Cross Platform
  - Android
  - IOS
  - iPhone
  - Windows
  - Mac OS
  - Mobile
tags:
  - Xamarin
  - Mobile
  - Development
  - Csharp
  - Cross-Platform
  - Performance
  - Ui
  - Design
  - App
  - Development
  - Project
  - Structure
draft: false
weight: 233
categories_ref:
  - XAML
  - Xamarin
  - WPF
  - Xamarin Forms
  - Xamarin Native
  - Cross Platform
  - Android
  - IOS
  - iPhone
  - Windows
  - Mac OS
  - Mobile
slug_calculated: https://brianbraatz.github.io/p/xamarinforms-vs-xamarinnative:-which-one-should-you-choose
lastmod: 2025-03-18T18:29:16.049Z
---
<!--

# Xamarin.Forms vs Xamarin.Native: Which One Should You Choose?
-->

## Introduction

So, you're building a **Xamarin app**, and now you're faced with **a big decision**:

Do you go with **Xamarin.Forms** or **Xamarin.Native (Xamarin.Android & Xamarin.iOS)?**

Short answer: **It depends on your project’s needs**.

Long answer? Let’s **break it all down**—from **performance** to **appearance**, to **code complexity and project structure**.

***

## What Came Before Xamarin? A Brief History

Before **Xamarin**, developing mobile (iPhone-Android) apps meant writing **separate** applications for **iOS and Android**.

1. **Native Development:**
   * **iOS** → Swift / Objective-C
   * **Android** → Java / Kotlin
   * **Pros** → Full control over UI & performance
   * **Cons** → **Expensive** (two separate codebases)

2. **Hybrid Development:**

> * **Cordova, Ionic, PhoneGap** → Run web-based apps inside a web view

* **Pros** → Cross-platform with a single codebase
* **Cons** → **Slow performance** and **limited native feel**

Then in **2011**, **Xamarin was born**, allowing developers to write **C# code** for both **iOS and Android**, using **Mono (a cross-platform .NET implementation)**.

> **Further Reading:** [Xamarin Wikipedia](https://en.wikipedia.org/wiki/Xamarin)

***

## Xamarin.Forms vs Xamarin.Native: The Basics

| Approach           | Description                                                                                                 |
| ------------------ | ----------------------------------------------------------------------------------------------------------- |
| **Xamarin.Forms**  | A **cross-platform UI toolkit** where you write XAML code that runs on **both** iOS and Android.            |
| **Xamarin.Native** | Uses **Xamarin.iOS and Xamarin.Android** to build **platform-specific UI**, but still share business logic. |

### **Key Differences**

| Feature           | Xamarin.Forms           | Xamarin.Native      |
| ----------------- | ----------------------- | ------------------- |
| **Code Sharing**  | 90%+ (UI + Logic)       | 60-80% (Logic only) |
| **Performance**   | Slower (UI abstraction) | Faster (Native UI)  |
| **Appearance**    | Standardized UI         | Fully Native UI     |
| **Complexity**    | Easier                  | Harder              |
| **Customization** | Limited                 | Full Control        |

***

## Performance: Which One is Faster?

**Xamarin.Native wins on performance.** Why?

* **Xamarin.Forms** abstracts UI elements, adding overhead.
* **Xamarin.Native** uses platform-specific UI controls, resulting in **faster rendering**.

If your app needs **high performance** (e.g., games, animations), **go native**.

***

## UI & Appearance: Which Looks Better?

| UI Feature                     | Xamarin.Forms       | Xamarin.Native         |
| ------------------------------ | ------------------- | ---------------------- |
| **Native Look & Feel**         | Close, but not 100% | Full Native Experience |
| **Animations & Customization** | Limited             | Full Control           |
| **Complexity**                 | Simpler             | More Code              |

For **business apps** → Xamarin.Forms is fine.\
For **highly polished consumer apps** → Xamarin.Native is better.

***

## Project Structure: How They Differ

### **Xamarin.Forms Project Structure**

```plaintext
/MyApp
  ├── MyApp (Shared Code)
  ├── MyApp.iOS (Platform-specific Code)
  ├── MyApp.Android (Platform-specific Code)
```

Most of the **UI and logic** lives in the **shared project**, making maintenance **easier**.

### **Xamarin.Native Project Structure**

```plaintext
/MyApp
  ├── MyApp.Core (Shared Business Logic)
  ├── MyApp.iOS (iOS UI + Native Code)
  ├── MyApp.Android (Android UI + Native Code)
```

With **Xamarin.Native**, UI is built separately for **each platform**, resulting in **more customization, but more work**.

***

## Pros and Cons of Each Approach

| Feature      | Xamarin.Forms                            | Xamarin.Native                          |
| ------------ | ---------------------------------------- | --------------------------------------- |
| **Pros**     | Faster development, less code            | Full native control, better performance |
| **Cons**     | Less customization, performance overhead | More work, harder maintenance           |
| **Best For** | Business apps, CRUD apps                 | High-performance, complex apps          |

***

## When to Choose Xamarin.Forms vs Xamarin.Native

| Use Case                            | Best Choice    |
| ----------------------------------- | -------------- |
| **Enterprise Apps**                 | Xamarin.Forms  |
| **Simple Business Apps**            | Xamarin.Forms  |
| **Highly Custom UI**                | Xamarin.Native |
| **Games or Performance-Heavy Apps** | Xamarin.Native |
| **Quick Prototyping**               | Xamarin.Forms  |
| **Apps with Heavy Animations**      | Xamarin.Native |

***

## The Future: Is Xamarin Dead?

Microsoft is **replacing Xamarin with .NET MAUI** (**Multi-platform App UI**). MAUI is the **evolution of Xamarin.Forms**, with better **performance** and **native integration**.

> **Further Reading:** [MAUI vs Xamarin](https://learn.microsoft.com/en-us/dotnet/maui/what-is-maui)

So, if you're starting a new project, you **should consider MAUI** instead.

***

## Key Takeaways

* **Xamarin.Forms** → Best for **business apps, quick development, and code-sharing**.
* **Xamarin.Native** → Best for **performance, custom UI, and highly interactive apps**.
* **Performance:** Xamarin.Native is **faster**.
* **Customization:** Xamarin.Native gives **more control**.
* **Future:** Microsoft is **shifting to .NET MAUI**.

***

## References

1. [Xamarin Wikipedia](https://en.wikipedia.org/wiki/Xamarin)
2. [Xamarin.Forms vs Xamarin.Native](https://devblogs.microsoft.com/xamarin/)
3. [Xamarin vs MAUI](https://learn.microsoft.com/en-us/dotnet/maui/)
4. [Xamarin Performance Optimization](https://docs.microsoft.com/en-us/xamarin/android/app-fundamentals/performance)
5. [Xamarin.Forms UI Limitations](https://xamarinhelp.com/xamarin-forms-vs-native/)
