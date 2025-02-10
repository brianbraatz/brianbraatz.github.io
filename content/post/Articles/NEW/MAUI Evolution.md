---
title: Evolution of MAUI From Mono to Xamarin to MAUI
description: How we went from Mono to Xamarin to MAUI
slug: from-mono-to-xamarin-to-maui
date: 2024-12-05
image: post/Articles/IMAGES/mauibeach.jpg
categories:
  - Maui
  - Mono
  - Xamarin
  - WPF
  - CSharp
  - GUI
  - DotNet
  - Mobile
  - Cross Platform
  - Mac OS
  - IOS
  - Android
tags:
  - Maui
  - WPF
  - Dotnet
  - Desktop
  - Development
  - Cross-platform
  - CSharp
  - Mobile
  - XAML
draft: false
weight: 287
lastmod: 2025-02-09T22:31:35.793Z
---
<!-- 
title: Evolution of MAUI From Mono to Xamarin to MAUI
description: How we went from Mono to Xamarin to MAUI
slug: from-mono-to-xamarin-to-maui
date: 2024-12-05
image: post/Articles/IMAGES/brickwall.jpg
categories: 
tags:
  - Go
  - Firewall
  - Networking
  - Security
  - DNS
draft: false
weight: 477


title: "Evolution of MAUI From Mono to Xamarin to MAUI"
description: How we went from Mono to Xamarin to MAUI
slug: from-mono-to-xamarin-to-maui
date: 2025-12-05
image: post/Articles/IMAGES/mauibeach.jpg
categories: 
tags:
  - Maui
  - Mono
  - Xamarin
  - Silverlight
  - Moonlight
  - Dotnet
  - Microsoft
  - History
  - Development
draft: false
weight: 147
-->

Ah, MAUI. The grand unification theory of .NET UI development.

How did we get here?......

## 🌅 The Dawn of Time: Mono (2004)

### 💡 What Was Mono?

Before .NET was truly cross-platform, Microsoft kept .NET development firmly locked inside Windows.

That didn’t sit well with **Miguel de Icaza**, an open-source warrior (weenie?) who thought, *"Hey, what if .NET could run on Linux and macOS?"*

And thus, **Mono** was born—a project aimed at making .NET applications work everywhere.

* **Who made it?** Miguel de Icaza and the Mono team at Ximian (later Novell, later Xamarin, later Microsoft).

* **Why did it exist?** Because cross-platform .NET wasn’t a thing yet.

* **What could you build?** Console apps, desktop apps (with GTK#), and later, mobile apps.

### 🛠️ Mono Code Sample

```csharp
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Hello from Mono!");
    }
}
```

Run this on Linux, and boom! **Cross-platform C# in 2004.** Revolutionary!

It might not seem exciting now.. but back then Cross- Platform was tricky and complicated... much more than now...

***

## 🎇 The Silverlight & Moonlight Saga (2007–2012)

### 💡 What Was Silverlight?

Microsoft, in a bid to take on Adobe Flash (yeah, remember Flash?), created **Silverlight**—a browser plugin for rich web apps using .NET and XAML.

It was like WPF but... Ran in a browser! (great idea? or insane? you decide.. :) ).

* **Who made it?** Microsoft.

* **Why did it exist?** To compete with Flash. Spoiler: It lost.

* **What could you build?** Interactive web apps with .NET.

NOTE: The code basially ran as an AxctiveX plugin... Once ActiveX lost favor due to security issues with compiled code running on a browser... Silverlight kind of died with it..

### 💡 What Was Moonlight?

Since Silverlight was Windows-only, the Mono team created **Moonlight**, a Linux-compatible version. It almost worked!

* **Who made it?** Mono/Ximian team.

* **Why did it exist?** Because Linux users also wanted fancy web apps.

* **What happened?** Microsoft lost interest in Silverlight, and Moonlight died with it.

### 🌑 Silverlight/Moonlight Code Sample

```xml
<UserControl x:Class="SilverlightApp.MainPage"
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml">
    <Grid>
        <TextBlock Text="Hello, Silverlight!" />
    </Grid>
</UserControl>
```

Silverlight was actually pretty cool, but then **HTML5 happened**, and Microsoft pulled the plug. **RIP Silverlight (2007–2012).**

***

## 📱 The Rise of Xamarin (2011–2020)

### 💡 What Was Xamarin?

After Silverlight’s fall, the Mono team pivoted to **mobile development**. Enter **Xamarin**, which allowed developers to build native **iOS and Android apps** using C#.

* **Who made it?** The Mono team, now called Xamarin.

* **Why did it exist?** Because writing the same app **three times** (Objective-C, Java, and C#) was torture.

* **What could you build?** iOS, Android, and later UWP apps.

### 🛠️ Xamarin Code Sample

```csharp
using Xamarin.Forms;

public class App : Application
{
    public App()
    {
        MainPage = new ContentPage
        {
            Content = new Label
            {
                Text = "Hello, Xamarin!",
                VerticalOptions = LayoutOptions.Center,
                HorizontalOptions = LayoutOptions.Center,
            }
        };
    }
}
```

**Fun fact:** Xamarin.Forms was originally a separate thing, but people liked it enough that Microsoft turned it into **MAUI**.

***

## 🔥 Enter .NET MAUI (2021–Present)

### 💡 What Is MAUI?

Microsoft, tired of juggling **WPF, UWP, Xamarin, and whatever Silverlight was**, decided to merge everything into **one framework**: **.NET MAUI (Multi-platform App UI)**.

* **Who made it?** Microsoft.

* **Why does it exist?** To unify Xamarin and .NET UI development under **one** framework.

* **What can you build?** iOS, Android, macOS, Windows, and Blazor hybrid apps.

!!!!!!!!!!!!!!!!

### 🛠️ .NET MAUI Code Sample

```csharp
using Microsoft.Maui.Controls;

public class App : Application
{
    public App()
    {
        MainPage = new ContentPage
        {
            Content = new Label
            {
                Text = "Hello, .NET MAUI!",
                VerticalOptions = LayoutOptions.Center,
                HorizontalOptions = LayoutOptions.Center,
            }
        };
    }
}
```

***

## 📊 Evolution Table: Mono → Xamarin → MAUI

| Era      | Technology  | Supported Platforms             | Key Feature                     | What Happened?                  |
| -------- | ----------- | ------------------------------- | ------------------------------- | ------------------------------- |
| **2004** | Mono        | Linux, macOS, Windows           | First cross-platform .NET       | Kept .NET alive outside Windows |
| **2007** | Silverlight | Windows, Web (via plugin)       | Rich web apps                   | Killed by HTML5                 |
| **2008** | Moonlight   | Linux (Silverlight alternative) | Open-source Silverlight         | Microsoft lost interest         |
| **2011** | Xamarin     | iOS, Android                    | Native mobile apps with C#      | Acquired by Microsoft in 2016   |
| **2021** | .NET MAUI   | iOS, Android, macOS, Windows    | Unified UI framework in .NET 6+ | The ultimate evolution          |

***
