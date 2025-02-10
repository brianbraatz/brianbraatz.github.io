---
title: MAUI vs WPF
description: Includes Side by Side code
slug: maui-vs-wpf-desktop
date: 2022-02-20
image: post/Articles/IMAGES/mauilogo1.png
categories:
  - CSharp
  - DotNet
  - Maui
  - WPF
  - XAML
tags:
  - Maui
  - WPF
  - Dotnet
  - Desktop
  - Development
  - Cross-platform
  - CSharp
draft: false
weight: 171
lastmod: 2025-02-09T22:32:08.178Z
---
# MAUI vs WPF

Ah, WPF. The granddaddy of .NET desktop development. If WPF was a house, it’d be a **solid, well-built mansion from the early 2000s**—reliable, but maybe a bit outdated.

Then comes .NET MAUI, a **modern, multi-platform condo with all the fancy smart-home features**.

So, which one should you use? Should you abandon WPF like an old VHS tape or embrace MAUI?

***

## 🧐 What Are These Two?

### **WPF – The Desktop Veteran**

Windows Presentation Foundation (WPF) has been **the go-to framework for Windows desktop applications** since .NET 3.0.

It’s **battle-tested**, full of **rich UI capabilities**, and deeply integrated with Windows.

But here’s the catch: **it’s Windows-only**.

If your goal is **cross-platform support**, WPF is about as flexible as a brick.

### **MAUI – The Future of Multi-Platform UI**

.NET MAUI (Multi-platform App UI) is Microsoft’s **shiny new framework** that lets you **build applications for Windows, Mac, iOS, and Android**—all from a **single codebase**.

While it originally evolved from Xamarin.Forms (which focused on mobile), MAUI **extends its reach to desktops**.

***

## 🔥 Key Differences

| Feature                   | WPF                     | MAUI                                   |
| ------------------------- | ----------------------- | -------------------------------------- |
| **Supported Platforms**   | Windows only            | Windows, macOS, iOS, Android           |
| **UI Framework**          | XAML-based              | XAML-based (but more flexible)         |
| **Graphics Engine**       | DirectX-based rendering | Skia/WinUI-based rendering             |
| **Performance**           | Optimized for Windows   | Optimized for cross-platform           |
| **Best Use Case**         | Windows desktop apps    | Cross-platform apps (desktop + mobile) |
| **Dependency on Windows** | Full                    | Minimal (Windows is just one platform) |

***

## 🚀 Where MAUI Shines

1. **Cross-Platform Development**\
   WPF is great **if you only care about Windows**, but MAUI lets you target **multiple platforms from one codebase**.

2. **Modern UI Capabilities**\
   MAUI provides **better UI flexibility**, including **handlers** instead of the old renderer system from Xamarin.

3. **Performance Boosts**\
   With .NET 6/7 and beyond, MAUI **optimizes performance** better than older WPF applications.

4. **Better for Cloud-Connected and Mobile Apps**\
   If your app **needs to be cloud-connected, responsive, and work across multiple platforms**, MAUI is **the way to go**.

***

## 😰 Should You Migrate?

* **If your app is Windows-only** and you don’t need cross-platform support, **stick with WPF**.
* **If you want future-proof, cross-platform development**, MAUI is **a better long-term bet**.

WPF isn’t going away anytime soon, but Microsoft is **heavily investing in MAUI**, so if you’re starting fresh, **you might want to go with MAUI**.

***

## 🏗️ WPF vs MAUI – Class Equivalents

If you’re coming from WPF, you might be wondering: **"Where did all my familiar classes go in MAUI?"**\
Don’t worry, here’s a cheat sheet to help you transition smoothly:

| **WPF Class**              | **MAUI Equivalent**                 | **Description**                                                                           |
| -------------------------- | ----------------------------------- | ----------------------------------------------------------------------------------------- |
| `Window`                   | `MauiApp` & `Page`                  | In MAUI, there's no direct `Window` class; instead, you structure your app using `Pages`. |
| `Application`              | `MauiApp` & `AppShell`              | `MauiApp` is the new way to configure your app, and `AppShell` provides navigation.       |
| `UserControl`              | `ContentView`                       | In MAUI, reusable components are `ContentView` instead of `UserControl`.                  |
| `Grid`, `StackPanel`, etc. | `Grid`, `StackLayout`, `FlexLayout` | Layouts work similarly but have slight naming differences.                                |
| `Button`                   | `Button`                            | Buttons exist in both, but MAUI optimizes them for touch-based interfaces.                |
| `TextBlock`                | `Label`                             | In MAUI, `Label` is the primary way to display text.                                      |
| `Image`                    | `Image`                             | Image rendering remains mostly the same.                                                  |
| `ListBox` / `DataGrid`     | `CollectionView` / `ListView`       | MAUI uses `CollectionView` for more modern list-based UI.                                 |

As you can see, while WPF and MAUI have similarities, **MAUI is more mobile-friendly and cross-platform-focused**.

***

## 🆚 Code Comparison – WPF vs MAUI

### **1. Setting Up a Simple Window/Page**

#### **WPF (MainWindow.xaml & MainWindow.xaml.cs)**

```xml
<Window x:Class="MyApp.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="WPF Window" Height="350" Width="525">
    <Grid>
        <TextBlock Text="Hello, WPF!" HorizontalAlignment="Center" VerticalAlignment="Center"/>
    </Grid>
</Window>
```

```csharp
public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
    }
}
```

#### **MAUI (MainPage.xaml & MainPage.xaml.cs)**

```xml
<ContentPage xmlns="http://schemas.microsoft.com/dotnet/2021/maui"
             xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
             x:Class="MyApp.MainPage">
    <VerticalStackLayout>
        <Label Text="Hello, MAUI!" 
               VerticalOptions="Center" 
               HorizontalOptions="Center" />
    </VerticalStackLayout>
</ContentPage>
```

```csharp
public partial class MainPage : ContentPage
{
    public MainPage()
    {
        InitializeComponent();
    }
}
```

### **2. Handling Button Click Events**

#### **WPF (Button Click Event Handling)**

```xml
<Button Content="Click Me" Click="Button_Click"/>
```

```csharp
private void Button_Click(object sender, RoutedEventArgs e)
{
    MessageBox.Show("Hello from WPF!");
}
```

#### **MAUI (Button Click Event Handling)**

```xml
<Button Text="Click Me" Clicked="OnButtonClicked"/>
```

```csharp
private void OnButtonClicked(object sender, EventArgs e)
{
    DisplayAlert("Alert", "Hello from MAUI!", "OK");
}
```

**Key Differences:**

* WPF uses `MessageBox.Show()`, while MAUI uses `DisplayAlert()`.
* MAUI’s `Clicked` event is the equivalent of WPF’s `Click` event.

***

## 🛠️ Visual Studio Setup – WPF vs MAUI

### **Setting Up a WPF Project**

1. Open **Visual Studio** (2022 recommended).
2. Click **Create a new project**.
3. Select **WPF App (.NET 6/7)**.
4. Configure your project name, location, and .NET version.
5. Click **Create**, and you’re ready to start coding!

### **Setting Up a .NET MAUI Project**

6. **Ensure Visual Studio 2022 (v17.3 or later) is installed**.
7. Open Visual Studio and go to **Tools > Get Tools and Features**.
8. Install **.NET Multi-platform App UI development** under the **Workloads** tab.
9. Click **Create a new project**, then choose **.NET MAUI App**.
10. Configure your project name and location, then hit **Create**.

**Key Differences:**

* **WPF only requires the .NET desktop development workload**, whereas **MAUI requires the .NET Multi-platform App UI workload**.
* **MAUI projects need extra setup for platform-specific configurations**.

***

## 🤔 Key Ideas

* **WPF is a rock-solid choice for Windows-only applications.**
* **MAUI is the future if you need cross-platform support.**

If you’re happy with WPF and just building for Windows, **stay put**. But if you see a future where your app **runs on macOS, mobile, and beyond**, **MAUI is worth the investment**.

***

## 🔗 Reference Links

* [Microsoft’s Official .NET MAUI Documentation](https://learn.microsoft.com/en-us/dotnet/maui/)
* [WPF Documentation](https://learn.microsoft.com/en-us/dotnet/desktop/wpf/)
* [Choosing Between WPF and MAUI](https://learn.microsoft.com/en-us/dotnet/)
* [Getting Started with .NET MAUI](https://learn.microsoft.com/en-us/dotnet/maui/get-started/)

***

## 🛠️ Some Maui Frameworks to check out

* **MVVM for your .NET MAUI projects?**: [A](https://www.reddit.com/r/dotnetMAUI/comments/12arji3/mvvm_frameworks_for_maui/?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC93d3cucmVkZGl0LmNvbVwvclwvZG90bmV0TUFVSVwvY29tbWVudHNcLzEyYXJqaTNcL212dm1fZnJhbWV3b3Jrc19mb3JfbWF1aVwvIiwiZXZlbnRJbmZvX2NvbnZlcnNhdGlvbklkIjoiUVpWZTNISG9uUmFDZThwQ3RGWU14IiwiZXZlbnRJbmZvX2NsaWNrU291cmNlIjoiY2l0YXRpb25MaW5rIiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IjQ5dGtud0hwR1o0aUZvSFZjcWNVQSJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1)

* **ReactiveUI**: This is an advanced MVVM framework that integrates reactive programming principles [B](https://learn.microsoft.com/en-us/answers/questions/485866/mvu-reactiveui-and-mvvm-in-net-maui?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC9sZWFybi5taWNyb3NvZnQuY29tXC9lbi11c1wvYW5zd2Vyc1wvcXVlc3Rpb25zXC80ODU4NjZcL212dS1yZWFjdGl2ZXVpLWFuZC1tdnZtLWluLW5ldC1tYXVpIiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IjQ5dGtud0hwR1o0aUZvSFZjcWNVQSIsImV2ZW50SW5mb19jb252ZXJzYXRpb25JZCI6IlFaVmUzSEhvblJhQ2U4cEN0RllNeCIsImV2ZW50SW5mb19jbGlja1NvdXJjZSI6ImNpdGF0aW9uTGluayJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1). It's great for handling complex data flows and asynchronous operations [B](https://learn.microsoft.com/en-us/answers/questions/485866/mvu-reactiveui-and-mvvm-in-net-maui?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC9sZWFybi5taWNyb3NvZnQuY29tXC9lbi11c1wvYW5zd2Vyc1wvcXVlc3Rpb25zXC80ODU4NjZcL212dS1yZWFjdGl2ZXVpLWFuZC1tdnZtLWluLW5ldC1tYXVpIiwiZXZlbnRJbmZvX2NvbnZlcnNhdGlvbklkIjoiUVpWZTNISG9uUmFDZThwQ3RGWU14IiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IjQ5dGtud0hwR1o0aUZvSFZjcWNVQSJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).

* **MVU (Model-View-Update)**: This is a variation of MVVM that focuses on a more functional programming approach [B](https://learn.microsoft.com/en-us/answers/questions/485866/mvu-reactiveui-and-mvvm-in-net-maui?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC9sZWFybi5taWNyb3NvZnQuY29tXC9lbi11c1wvYW5zd2Vyc1wvcXVlc3Rpb25zXC80ODU4NjZcL212dS1yZWFjdGl2ZXVpLWFuZC1tdnZtLWluLW5ldC1tYXVpIiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IjQ5dGtud0hwR1o0aUZvSFZjcWNVQSIsImV2ZW50SW5mb19jbGlja1NvdXJjZSI6ImNpdGF0aW9uTGluayIsImV2ZW50SW5mb19jb252ZXJzYXRpb25JZCI6IlFaVmUzSEhvblJhQ2U4cEN0RllNeCJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1). It's similar to React and can be a good choice if you're familiar with those concepts [B](https://learn.microsoft.com/en-us/answers/questions/485866/mvu-reactiveui-and-mvvm-in-net-maui?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC9sZWFybi5taWNyb3NvZnQuY29tXC9lbi11c1wvYW5zd2Vyc1wvcXVlc3Rpb25zXC80ODU4NjZcL212dS1yZWFjdGl2ZXVpLWFuZC1tdnZtLWluLW5ldC1tYXVpIiwiZXZlbnRJbmZvX2NvbnZlcnNhdGlvbklkIjoiUVpWZTNISG9uUmFDZThwQ3RGWU14IiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IjQ5dGtud0hwR1o0aUZvSFZjcWNVQSJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).

* **Prism**: While it's more commonly associated with WPF, Prism can also be used with MAUI for more advanced navigation and modularization [A](https://www.reddit.com/r/dotnetMAUI/comments/12arji3/mvvm_frameworks_for_maui/?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY29udmVyc2F0aW9uSWQiOiJRWlZlM0hIb25SYUNlOHBDdEZZTXgiLCJldmVudEluZm9fbWVzc2FnZUlkIjoiNDl0a253SHBHWjRpRm9IVmNxY1VBIiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvd3d3LnJlZGRpdC5jb21cL3JcL2RvdG5ldE1BVUlcL2NvbW1lbnRzXC8xMmFyamkzXC9tdnZtX2ZyYW1ld29ya3NfZm9yX21hdWlcLyJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).

* **TinyMVVM**: A lightweight MVVM framework that can be a good fit if you want something simpler and less boilerplate [A](https://www.reddit.com/r/dotnetMAUI/comments/12arji3/mvvm_frameworks_for_maui/?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY29udmVyc2F0aW9uSWQiOiJRWlZlM0hIb25SYUNlOHBDdEZZTXgiLCJldmVudEluZm9fbWVzc2FnZUlkIjoiNDl0a253SHBHWjRpRm9IVmNxY1VBIiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvd3d3LnJlZGRpdC5jb21cL3JcL2RvdG5ldE1BVUlcL2NvbW1lbnRzXC8xMmFyamkzXC9tdnZtX2ZyYW1ld29ya3NfZm9yX21hdWlcLyJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).

* **Shield MVVM**: This framework provides type safety and additional features for MAUI [A](https://www.reddit.com/r/dotnetMAUI/comments/12arji3/mvvm_frameworks_for_maui/?copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC93d3cucmVkZGl0LmNvbVwvclwvZG90bmV0TUFVSVwvY29tbWVudHNcLzEyYXJqaTNcL212dm1fZnJhbWV3b3Jrc19mb3JfbWF1aVwvIiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IjQ5dGtud0hwR1o0aUZvSFZjcWNVQSIsImV2ZW50SW5mb19jb252ZXJzYXRpb25JZCI6IlFaVmUzSEhvblJhQ2U4cEN0RllNeCJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
