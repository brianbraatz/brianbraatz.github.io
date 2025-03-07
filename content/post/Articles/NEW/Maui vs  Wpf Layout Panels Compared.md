---
title: Maui vs  Wpf Layout Panels Compared
description: Quick Overview-Cheatsheet of Maui vs Wpf Layout
slug: maui-vs-wpf-layout
date: 2022-02-20
image: post/Articles/IMAGES/mauilogo3.png
categories:
  - Maui
  - WPF
  - XAML
  - GUI
  - Mobile
tags:
  - Maui
  - WPF
  - Dotnet
  - DesktopDevelopment
  - Cross-platform
  - CSharp
  - Mobile
draft: false
weight: 43
lastmod: 2025-03-07T00:55:20.257Z
---
Both WPF and .NET MAUI offer a variety of layout panels to help you organize and arrange UI elements, but there are some differences in the specific panels available and their usage.

### Layout Panels in WPF

WPF provides several built-in layout panels, including:

1. **Canvas**: Allows you to position elements using absolute coordinates.
2. **DockPanel**: Arranges children based on their Dock properties (e.g., Top, Bottom, Left, Right).
3. **Grid**: A flexible layout panel that uses rows and columns to arrange elements.
4. **StackPanel**: Arranges elements either horizontally or vertically in a single line.
5. **WrapPanel**: Similar to StackPanel but wraps elements to a new line when they exceed the container's width.
6. **Border**: Adds a border around its child elements and can also apply padding.

### Layout Panels in .NET MAUI

.NET MAUI also offers several layout panels, with some similarities to WPF but also some unique options:

1. **StackLayout**: Arranges elements either horizontally or vertically in a single line [A](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fbWVzc2FnZUlkIjoiRlNUMmFDcFUxdGpocm51WThkZGQ1IiwiZXZlbnRJbmZvX2NsaWNrU291cmNlIjoiY2l0YXRpb25MaW5rIiwiZXZlbnRJbmZvX2NvbnZlcnNhdGlvbklkIjoiUVpWZTNISG9uUmFDZThwQ3RGWU14IiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvbGVhcm4ubWljcm9zb2Z0LmNvbVwvZW4tdXNcL2RvdG5ldFwvbWF1aVwvdXNlci1pbnRlcmZhY2VcL2xheW91dHNcLz92aWV3PW5ldC1tYXVpLTkuMCJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
2. **Grid**: Similar to WPF's Grid, it uses rows and columns to arrange elements [A](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY29udmVyc2F0aW9uSWQiOiJRWlZlM0hIb25SYUNlOHBDdEZZTXgiLCJldmVudEluZm9fbWVzc2FnZUlkIjoiRlNUMmFDcFUxdGpocm51WThkZGQ1IiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvbGVhcm4ubWljcm9zb2Z0LmNvbVwvZW4tdXNcL2RvdG5ldFwvbWF1aVwvdXNlci1pbnRlcmZhY2VcL2xheW91dHNcLz92aWV3PW5ldC1tYXVpLTkuMCJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
3. **AbsoluteLayout**: Allows you to position elements using absolute positioning with properties like X, Y, Width, and Height [B](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/custom?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fbWVzc2FnZUlkIjoiRlNUMmFDcFUxdGpocm51WThkZGQ1IiwiZXZlbnRJbmZvX2NsaWNrU291cmNlIjoiY2l0YXRpb25MaW5rIiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvbGVhcm4ubWljcm9zb2Z0LmNvbVwvZW4tdXNcL2RvdG5ldFwvbWF1aVwvdXNlci1pbnRlcmZhY2VcL2xheW91dHNcL2N1c3RvbT92aWV3PW5ldC1tYXVpLTkuMCIsImV2ZW50SW5mb19jb252ZXJzYXRpb25JZCI6IlFaVmUzSEhvblJhQ2U4cEN0RllNeCJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
4. **FlexLayout**: Based on CSS Flexbox, it provides more advanced layout options with properties like FlexDirection, JustifyContent, and AlignItems [C](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/flexlayout?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fbWVzc2FnZUlkIjoiRlNUMmFDcFUxdGpocm51WThkZGQ1IiwiZXZlbnRJbmZvX2NvbnZlcnNhdGlvbklkIjoiUVpWZTNISG9uUmFDZThwQ3RGWU14IiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvbGVhcm4ubWljcm9zb2Z0LmNvbVwvZW4tdXNcL2RvdG5ldFwvbWF1aVwvdXNlci1pbnRlcmZhY2VcL2xheW91dHNcL2ZsZXhsYXlvdXQ/dmlldz1uZXQtbWF1aS05LjAiLCJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsifQ%3D%3D\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
5. **HorizontalStackLayout**: A more performant alternative to StackLayout for horizontal arrangements [A](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fbWVzc2FnZUlkIjoiRlNUMmFDcFUxdGpocm51WThkZGQ1IiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvbGVhcm4ubWljcm9zb2Z0LmNvbVwvZW4tdXNcL2RvdG5ldFwvbWF1aVwvdXNlci1pbnRlcmZhY2VcL2xheW91dHNcLz92aWV3PW5ldC1tYXVpLTkuMCIsImV2ZW50SW5mb19jbGlja1NvdXJjZSI6ImNpdGF0aW9uTGluayIsImV2ZW50SW5mb19jb252ZXJzYXRpb25JZCI6IlFaVmUzSEhvblJhQ2U4cEN0RllNeCJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
6. **VerticalStackLayout**: Similar to StackLayout but specifically for vertical arrangements [A](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC9sZWFybi5taWNyb3NvZnQuY29tXC9lbi11c1wvZG90bmV0XC9tYXVpXC91c2VyLWludGVyZmFjZVwvbGF5b3V0c1wvP3ZpZXc9bmV0LW1hdWktOS4wIiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IkZTVDJhQ3BVMXRqaHJudVk4ZGRkNSIsImV2ZW50SW5mb19jb252ZXJzYXRpb25JZCI6IlFaVmUzSEhvblJhQ2U4cEN0RllNeCIsImV2ZW50SW5mb19jbGlja1NvdXJjZSI6ImNpdGF0aW9uTGluayJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).

### Key Differences

* **FlexLayout**: This is unique to .NET MAUI and provides more advanced layout options inspired by CSS Flexbox [C](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/flexlayout?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fY29udmVyc2F0aW9uSWQiOiJRWlZlM0hIb25SYUNlOHBDdEZZTXgiLCJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC9sZWFybi5taWNyb3NvZnQuY29tXC9lbi11c1wvZG90bmV0XC9tYXVpXC91c2VyLWludGVyZmFjZVwvbGF5b3V0c1wvZmxleGxheW91dD92aWV3PW5ldC1tYXVpLTkuMCIsImV2ZW50SW5mb19tZXNzYWdlSWQiOiJGU1QyYUNwVTF0amhybnVZOGRkZDUifQ%3D%3D\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
* **AbsoluteLayout**: While WPF does not have a direct equivalent, .NET MAUI offers AbsoluteLayout for absolute positioning [B](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/custom?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fY2xpY2tEZXN0aW5hdGlvbiI6Imh0dHBzOlwvXC9sZWFybi5taWNyb3NvZnQuY29tXC9lbi11c1wvZG90bmV0XC9tYXVpXC91c2VyLWludGVyZmFjZVwvbGF5b3V0c1wvY3VzdG9tP3ZpZXc9bmV0LW1hdWktOS4wIiwiZXZlbnRJbmZvX2NvbnZlcnNhdGlvbklkIjoiUVpWZTNISG9uUmFDZThwQ3RGWU14IiwiZXZlbnRJbmZvX21lc3NhZ2VJZCI6IkZTVDJhQ3BVMXRqaHJudVk4ZGRkNSJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
* **HorizontalStackLayout and VerticalStackLayout**: These are more specific versions of StackLayout in .NET MAUI, providing better performance and clarity [A](https://learn.microsoft.com/en-us/dotnet/maui/user-interface/layouts/?view=net-maui-9.0\&copilot_analytics_metadata=eyJldmVudEluZm9fY29udmVyc2F0aW9uSWQiOiJRWlZlM0hIb25SYUNlOHBDdEZZTXgiLCJldmVudEluZm9fY2xpY2tTb3VyY2UiOiJjaXRhdGlvbkxpbmsiLCJldmVudEluZm9fbWVzc2FnZUlkIjoiRlNUMmFDcFUxdGpocm51WThkZGQ1IiwiZXZlbnRJbmZvX2NsaWNrRGVzdGluYXRpb24iOiJodHRwczpcL1wvbGVhcm4ubWljcm9zb2Z0LmNvbVwvZW4tdXNcL2RvdG5ldFwvbWF1aVwvdXNlci1pbnRlcmZhY2VcL2xheW91dHNcLz92aWV3PW5ldC1tYXVpLTkuMCJ9\&citationMarker=9F742443-6C92-4C44-BF58-8F5A7C53B6F1).
