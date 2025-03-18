---
title: Silverlight Conversion to Angular with the T4 Template Engine
description: Silverlight Conversion Can also work wth WPF-Maui-XAML
slug: silverlight-conversion-angular-t4
date: 2021-12-03
image: post/Articles/IMAGES/t4logo.png
categories:
  - Silverlight
  - Angular
  - Maui
  - T4
  - XAML
  - WPF
  - CSharp
  - DotNet
  - Web Development
  - Typescript
  - Mobile
tags:
  - Maui
  - WPF
  - XAML
  - Silverlight
  - Angular
  - T4
  - Template
  - Engine
  - Code
  - Generation
  - TypeScript
  - CSharp
draft: false
weight: 312
categories_ref:
  - Silverlight
  - Angular
  - Maui
  - T4
  - XAML
  - WPF
  - CSharp
  - DotNet
  - Web Development
  - Typescript
  - Mobile
slug_calculated: https://brianbraatz.github.io/p/silverlight-conversion-angular-t4
lastmod: 2025-03-14T16:40:27.834Z
---
# Maui-WPF-XAML-Silverlight Conversion to Angular with the T4 Template Engine

## Introduction: A Tale of Many Frameworks

Once upon a time, in the golden age of .NET, there was **Silverlight**, a glorious attempt by Microsoft to take on Flash. It had XAML, it had C#, and it had dreams. But then, like many great things (*cough* Windows Phone *cough*), it was abandoned.

Meanwhile, **WPF** (Windows Presentation Foundation) continued to thrive as the king of desktop UI frameworks, and **XAML** became a staple for defining UI in .NET applications.

Then came **MAUI**, Microsoft's latest evolution, trying to unify UI development across Windows, macOS, iOS, and Android. At the heart of it all? **XAML**.

## My Real Life Experience porting a HUGE Silveright App to Angular...

But what happens when you're stuck with a **massive Silverlight application** and need to move to something modern like **Angular**?

Read on for my exeprience attacking the problem with the T4 Templates Engine..

## The Problem: Converting Silverlight to Angular

<!-- 
I once had a project with **a huge Silverlight app**, and the team decided it was time to move to **Angular**. But here’s the catch: **we couldn't just convert the layout directly**.  

However, if we could at least **generate Angular HTML files and TypeScript**, that would be a solid starting point. So, I devised a plan:  
-->

The Basic Plan was This

1. Parse the Silverlight **XAML** and roughly counvert it to to **Angular HTML**, ignoring layout
2. Extract C# **code-behind** and inject it into **TypeScript** code behind.
3. Store the **original XAML** and **C#** as comments in the Angular files for reference.
4. Automate the whole thing using **T4 templates** in C#.

This is not a 100% solutuion, but it does end up with an **Angular** project that has ALOT of the boring work done..

With this project - you can mostly live in the converted angular project, and attack one screen at a time, manually porting over the code..

The goal is to give the programmer a decent starting point and eemoved alot of inefficient time copying and pasting etc...

## How I Did It

Some bit of "good" news?? Is the Silverlight code was not using MVVM, so it was much easier to parse the connection between the Silverlight XAML and the codebehind.

My Attack:

* **Find all button click events** in XAML.
* **Extract the event handlers from the C# code-behind**.
* **Insert the C# logic into the equivalent Angular TypeScript methods**.

Here’s a simplified version of the **T4 template** I used:

### **T4 Template to Convert Silverlight XAML to Angular**

```csharp
<#@ template language="C#" hostspecific="true" #>
<#@ assembly name="System.Core" #>
<#@ import namespace="System.IO" #>
<#@ import namespace="System.Text.RegularExpressions" #>

<#
string xamlFile = @"C:\Path\To\Silverlight.xaml";
string csFile = @"C:\Path\To\Silverlight.xaml.cs";

string xamlContent = File.ReadAllText(xamlFile);
string csContent = File.ReadAllText(csFile);

// Convert XAML buttons to Angular buttons
string htmlContent = Regex.Replace(xamlContent, @"<Button\s+Name=""(\w+)""\s+Click=""(\w+)""", 
    @"<button (click)=""$2()"">");

// Extract event handlers from C#
MatchCollection eventHandlers = Regex.Matches(csContent, @"private void (\w+)_Click.*?{(.*?)}", 
    RegexOptions.Singleline);

StringBuilder tsContent = new StringBuilder();
foreach (Match match in eventHandlers)
{
    tsContent.AppendLine($"public {match.Groups[1]}() {{ {match.Groups[2]} }}");
}

#>
// Angular Component HTML
<!DOCTYPE html>
<html>
<body>
<#= htmlContent #>
<!-- Original XAML for reference -->
<!--
<#= xamlContent #>
-->
</body>
</html>

// Angular Component TypeScript
export class SilverlightComponent {
<#= tsContent.ToString() #>

/* Original C# Code-Behind for Reference */
/*
<#= csContent #>
*/
}
```

## The Output

For a Silverlight XAML file like this:

```xml
<Button Name="SaveButton" Click="SaveButton_Click">Save</Button>
```

The Angular HTML output would be:

```html
<button (click)="SaveButton_Click()">Save</button>

<!-- Original XAML for reference -->
<!--
<Button Name="SaveButton" Click="SaveButton_Click">Save</Button>
-->
```

And the TypeScript file:

```typescript
export class SilverlightComponent {
    public SaveButton_Click() { 
        // Original C# logic inserted here
    }

    /* Original C# Code-Behind for Reference */
    /*
    private void SaveButton_Click(object sender, RoutedEventArgs e) {
        MessageBox.Show("Saving data...");
    }
    */
}
```

## This is VERY dumbed down.. Compared to the original project

The example here is very dumbed down and simple. In the original project, I had to support many XAML controls and convert grids to Prime-Ng grids with Angular.

The template generation and all the XAML controls I had to convert was alot of work. But it had a good result, i ended up with around 300 **Angular** pages converted from XAML- into Angular.

This ended up with a project that made it practical for a very small team to convert a 300 page silverlight application into Angular in a resonable amount of time..

<!--
## The Next Chapter: Moving to React  

Eventually, the project **moved from Angular to React**, which meant redoing the whole process. But that’s a story for another day—**stay tuned for the React version** of this madness!  
-->

## Applying This to MAUI & WPF

Since **WPF, MAUI, and Silverlight** all share XAML, you can use the **same technique** using those dialects as your source or your target.

## Key Takeaways

* **Silverlight is dead, but XAML lives on.**
* **Migrating from XAML-based frameworks to Angular is possible with T4 templates.**
* **You can automatically generate Angular HTML & TypeScript from Silverlight XAML and C# code.**
* **Keeping original XAML and C# as comments in the Angular files helps developers during migration.**
* **This technique works for WPF and MAUI as well.**

## References

1. [Microsoft MAUI Documentation](https://learn.microsoft.com/en-us/dotnet/maui/)
2. [WPF Documentation](https://learn.microsoft.com/en-us/dotnet/desktop/wpf/)
3. [Silverlight Documentation (RIP)](https://learn.microsoft.com/en-us/previous-versions/silverlight/)
4. [T4 Templates](https://learn.microsoft.com/en-us/visualstudio/modeling/code-generation-and-t4-text-templates)
