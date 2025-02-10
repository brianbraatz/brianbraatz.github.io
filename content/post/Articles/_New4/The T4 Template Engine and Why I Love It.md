---
title: The T4 Template Engine and Why I Love It
description: Examples of Generating HTML, C#, Python, YAML and Postscript
slug: t4-template-engine-and-why-i-love-it
date: 2024-11-30
image: post/Articles/IMAGES/t4logo.png
categories:
  - T4
  - HTML
  - CSharp
  - DotNet
  - Python
  - YAML
  - Postscript
  - PDF
  - Document Imaging
tags:
  - T4
  - Template
  - Engine
  - Python
  - Golang
  - HTML
  - YAML
  - PostScript
  - CPP
  - go
  - GoLang
  - dotnet
  - Scripting
  - linux
  - windows
  - CodeGeneration
draft: false
weight: 371
lastmod: 2025-02-09T21:32:17.365Z
---
# T4 Template Engine and Why I Love It

T4 Templates: The only thing that makes generating repetitive code **fun**!\
If you don’t like writing **boilerplate code**, then T4 is about to be your new best friend.

This article shows **how to use T4 to generate**:

* **HTML, PostScript, YAML, JSON, Python, and Golang files**
* **How to pass parameters into templates**
* **How to loop through arrays dynamically**

***

## A Brief History of T4

T4 Templates have been around since the **Visual Studio 2005** era, hiding in the shadows while developers praised (or cursed) other tools. Microsoft introduced it as a **code generation tool** to automate tedious, repetitive code-writing tasks.

A quick timeline of T4’s history:

| Release Year | Visual Studio Version | Notable Features                                                     |
| ------------ | --------------------- | -------------------------------------------------------------------- |
| 2005         | VS 2005               | First introduced                                                     |
| 2008         | VS 2008               | Improved syntax and debugging                                        |
| 2010         | VS 2010               | Officially documented, better integration                            |
| 2012         | VS 2012               | Performance improvements                                             |
| 2013         | VS 2013               | More Visual Studio integration                                       |
| 2015         | VS 2015               | More performance optimizations                                       |
| 2017         | VS 2017               | Continued support, but declining attention                           |
| 2019         | VS 2019               | Still works, but slowly becoming a legacy tool                       |
| 2022         | VS 2022               | Can be used, but alternatives like Source Generators are taking over |

T4 exists because **repeating yourself is boring**, and Microsoft wanted a way to generate code dynamically, especially for frameworks like **Entity Framework**, **ASP.NET MVC**, and **Web API**.

### How Visual Studio Uses T4

If you’ve ever wondered, *"Where is T4 hiding in Visual Studio?"*, the answer is **everywhere!** Microsoft uses T4 for all sorts of code generation magic. Here are a few places:

* **Entity Framework Models** – When you generate `.edmx` files, T4 scripts generate the C# classes.
* **MVC Views (`.tt` files)** – ASP.NET MVC scaffolding runs T4 scripts to create controllers and views.
* **Code Behind Files** – Various Visual Studio extensions use T4 to generate `.designer.cs` files.
* **Custom Code Generators** – Tools like **CodeSmith** and **DevExpress** leverage T4 for automation.

## How to Use T4

There are **two main ways** to use T4 templates:

1. **Inside Visual Studio** – Right-click a project → Add → Text Template (`.tt`).
2. **Command Line** – Run `TextTransform.exe` to process `.tt` files outside Visual Studio.

***

## How to Pass Parameters to T4

To use **external input** in T4, you define parameters using **<#@ parameter #>** directives.\
We’ll use:

* **`TemplateInputTextProvidedByTheCaller`** – A string value
* **`TemplateArrayOfStringsProvidedByCaller`** – An array of strings

These values will be passed into **all** the templates.

***

### Example: Generating C# Code with T4

Let’s say you need to generate **multiple classes** with boilerplate code. Instead of writing them manually (like a caveman), you can use T4.

Create a file called `Models.tt` in your project and add this code:

```csharp
<#@ template language="C#" #>
<#@ output extension=".cs" #>
<#  
    var classes = new string[] { "User", "Product", "Order" }; 
#>

namespace GeneratedModels 
{
<# 
    foreach (var className in classes) 
    { 
#>
    public class <#= className #>
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }
<#  
    } 
#>
}
```

When you save this `.tt` file, **Visual Studio automatically generates the C# code** in `Models.cs`:

```csharp
namespace GeneratedModels
{
    public class User
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

    public class Product
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

    public class Order
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }
}
```

No more copy-pasting code like a chump!

### Running T4 from the Command Line

If you don’t want to use Visual Studio, you can run the T4 engine manually.

Open **Command Prompt** and run:

```sh
TextTransform.exe Models.tt
```

This will generate the same `Models.cs` file.

## Other Uses for T4

Besides **C# code generation**, T4 can be used for:

* **SQL Scripts** – Generate dynamic SQL statements.
* **JavaScript** – Create JS files dynamically based on backend logic.
* **XML and JSON** – Automate configurations.
* **HTML & CSS** – Generate frontend templates.

***

## Generating a **Static HTML Page**

```csharp
<#@ template language="C#" #>
<#@ output extension=".html" #>
<#@ parameter name="TemplateInputTextProvidedByTheCaller" type="System.String" #>
<#@ parameter name="TemplateArrayOfStringsProvidedByCaller" type="System.String[]" #>

<!DOCTYPE html>
<html lang="en">
<head>
    <title><#= TemplateInputTextProvidedByTheCaller #></title>
</head>
<body>
    <h1><#= TemplateInputTextProvidedByTheCaller #></h1>
    <ul>
    <# foreach (var item in TemplateArrayOfStringsProvidedByCaller) { #>
        <li><#= item #></li>
    <# } #>
    </ul>
</body>
</html>
```

***

## Generating a **Weirdly Formatted PostScript File**

```csharp
<#@ template language="C#" #>
<#@ output extension=".ps" #>
<#@ parameter name="TemplateInputTextProvidedByTheCaller" type="System.String" #>
<#@ parameter name="TemplateArrayOfStringsProvidedByCaller" type="System.String[]" #>

/Times-Roman findfont 24 scalefont setfont
100 700 moveto
(<#= TemplateInputTextProvidedByTheCaller #>) show

<# foreach (var shape in TemplateArrayOfStringsProvidedByCaller) { #>
100 650 moveto
(<#= shape #>) show
<# } #>

showpage
```

***

## Generating a **YAML Configuration File**

```csharp
<#@ template language="C#" #>
<#@ output extension=".yaml" #>
<#@ parameter name="TemplateInputTextProvidedByTheCaller" type="System.String" #>
<#@ parameter name="TemplateArrayOfStringsProvidedByCaller" type="System.String[]" #>

name: "<#= TemplateInputTextProvidedByTheCaller #>"
version: 1.0
settings:
<# foreach (var setting in TemplateArrayOfStringsProvidedByCaller) { #>
  <#= setting #>: true
<# } #>
```

***

## Generating an **appsettings.json** File

```csharp
<#@ template language="C#" #>
<#@ output extension=".json" #>
<#@ parameter name="TemplateInputTextProvidedByTheCaller" type="System.String" #>
<#@ parameter name="TemplateArrayOfStringsProvidedByCaller" type="System.String[]" #>

{
    "ApplicationName": "<#= TemplateInputTextProvidedByTheCaller #>",
    "Settings": {
<# foreach (var key in TemplateArrayOfStringsProvidedByCaller) { #>
        "<#= key #>": true,
<# } #>
    }
}
```

***

## Generating a **Python File with Embedded Strings**

```csharp
<#@ template language="C#" #>
<#@ output extension=".py" #>
<#@ parameter name="TemplateInputTextProvidedByTheCaller" type="System.String" #>
<#@ parameter name="TemplateArrayOfStringsProvidedByCaller" type="System.String[]" #>

# <#= TemplateInputTextProvidedByTheCaller #>
strings = [
<# foreach (var word in TemplateArrayOfStringsProvidedByCaller) { #>
    "<#= word #>",
<# } #>
]

print("Generated list:", strings)
```

***

## Generating a **Go File with Embedded Strings**

```csharp
<#@ template language="C#" #>
<#@ output extension=".go" #>
<#@ parameter name="TemplateInputTextProvidedByTheCaller" type="System.String" #>
<#@ parameter name="TemplateArrayOfStringsProvidedByCaller" type="System.String[]" #>

package main

import "fmt"

// <#= TemplateInputTextProvidedByTheCaller #>
func main() {
    strings := []string{
<# foreach (var word in TemplateArrayOfStringsProvidedByCaller) { #>
        "<#= word #>",
<# } #>
    }

    fmt.Println("Generated list:", strings)
}
```

## Key Points in the Above Samples

* **T4 can accept parameters**, making it **flexible for automation**.
* **You can generate HTML, PostScript, YAML, JSON, Python, and Go files** dynamically.
* **Use `TextTransform.exe` to run T4 from the command line**.
* **Loops allow T4 to iterate over arrays**, making it super powerful for code generation.

***

## Running These T4 Templates

To run T4 templates with parameters in **Visual Studio**:

1. Add a `.tt` file to your project.
2. Define `TemplateInputTextProvidedByTheCaller` and `TemplateArrayOfStringsProvidedByCaller`.
3. Save the file—**Visual Studio will auto-generate the output**.

For **command-line execution**, use:

```sh
TextTransform.exe MyTemplate.tt -p TemplateInputTextProvidedByTheCaller="Hello World" -p TemplateArrayOfStringsProvidedByCaller="['One', 'Two', 'Three']"
```

***

<!-- 
# T4 Template Engine and Why I Love It  

T4 Templates: The only thing that makes generating repetitive code **fun**!  
If you don’t like writing **boilerplate code**, then T4 is about to be your new best friend.  

This article shows **how to use T4 to generate**:
- **HTML, PostScript, YAML, JSON, Python, and Golang files**  
- **How to pass parameters into templates**  
- **How to loop through arrays dynamically**  
- **How to run T4 on Linux**  
-->

***

## Can You Run T4 on Linux?

Yes, but with some **caveats**.

T4 is **natively supported in Windows through Visual Studio**, but there’s no **official** support for it on Linux. However, there are **alternatives** that allow you to run T4 templates on Linux:

### 1. **Using Mono’s `t4` Command**

The **Mono Project** provides a `t4` command that works on Linux.

#### 🔹 Install Mono

First, install Mono on your system:

**Ubuntu/Debian:**

```sh
sudo apt update
sudo apt install mono-complete
```

**Fedora:**

```sh
sudo dnf install mono-complete
```

**Arch Linux:**

```sh
sudo pacman -S mono
```

#### 🔹 Run T4 Templates

Once Mono is installed, you can use **`t4`** to process templates:

```sh
t4 MyTemplate.tt
```

This will generate the output file **just like on Windows**.

***

### 2. **Using `dotnet-t4` (A .NET Core Alternative)**

If you’re working with **.NET Core or .NET 5+**, you can use `dotnet-t4`, an **open-source cross-platform T4 processor**.

#### 🔹 Install `dotnet-t4`

```sh
dotnet tool install -g dotnet-t4
```

#### 🔹 Run a T4 Template

```sh
dotnet-t4 MyTemplate.tt
```

This works on **Linux, Mac, and Windows**, making it a great **cross-platform** option.

***

### 3. **Using Roslyn Source Generators (Modern Alternative)**

Microsoft is **phasing out T4 in favor of Source Generators**, which work **natively in .NET 5+**.

If you’re starting a **new .NET Core/.NET 5+ project**, consider **Source Generators** instead of T4.

Learn more:\
🔗 [Roslyn Source Generators](https://devblogs.microsoft.com/dotnet/introducing-c-source-generators/)

***

## References

## Is T4 Dead?

Well… *kind of*. Microsoft has been moving toward **Source Generators** in **.NET 5+**, which are more powerful and performant. But T4 still works in **Visual Studio 2022**, and for quick-and-dirty code generation, it’s still **king**.

If you want something more modern, check out:

* **Roslyn Source Generators** – The fancy, official replacement.
* **Scriban** – A templating engine that doesn’t depend on Visual Studio.
* **Razor Templates** – If you’re already using ASP.NET Core.

***

## Key Takeaways

* **T4 is awesome** for automating repetitive code generation.
* **It’s built into Visual Studio** and used for EF models, MVC views, and more.
* **You can use it inside VS or from the command line**.
* **Microsoft is slowly replacing it with Roslyn Source Generators**, but it’s still useful today.
* **T4 is Windows-native but works on Linux with Mono (`t4`) or `dotnet-t4`.**
* **Mono’s `t4` command is the easiest way to run T4 on Linux.**
* **`dotnet-t4` is a cross-platform alternative for .NET Core/.NET 5+.**
* **For modern .NET development, consider Roslyn Source Generators.**

***

## References

* [Microsoft Docs on T4](https://learn.microsoft.com/en-us/visualstudio/modeling/code-generation-and-t4-text-templates)
* [T4 Parameter Passing](https://learn.microsoft.com/en-us/visualstudio/modeling/how-to-use-t4-text-templates-with-parameters)
* [T4 and Entity Framework Code Generation](https://learn.microsoft.com/en-us/ef/ef6/modeling/designer/codegen/)
* [Roslyn Source Generators (Newer Alternative)](https://devblogs.microsoft.com/dotnet/introducing-c-source-generators/)
* [T4 on Mono (`t4` Command)](https://www.mono-project.com/)
* [dotnet-t4: Cross-Platform T4 Processor](https://github.com/mono/t4)
* [Roslyn Source Generators (Newer Alternative)](https://devblogs.microsoft.com/dotnet/introducing-c-source-generators/)
