---
title: Razor Pages in a Nutshell
description: ""
slug: razor-pages-in-a-nutshell
date: 2018-07-22
image: post/Articles/IMAGES/razor.png
categories:
  - ASP.NET
  - Web Development
  - Razor Pages
tags:
  - Asp.net
  - Web Development
  - Razor Pages
  - C#
  - Dotnet
  - Mvc
draft: false
weight: 482
categories_ref:
  - ASP.NET
  - Web Development
  - Razor Pages
slug_calculated: https://brianbraatz.github.io/p/razor-pages-in-a-nutshell
lastmod: 2025-03-14T16:40:17.145Z
---
[The Ottoman](https://barbarossabrothers.com/products/the-ottoman)

<!-- # Razor Pages in a Nutshell -->

<!-- 
Alright, folks! Gather around, because today we’re talking about **Razor Pages**—the thing that Microsoft came up with to make our lives easier (or harder, depending on how much JavaScript you've been forced to write lately). -->

## A Little History Lesson (Yes, You Have to Read This)

Back in the day, Microsoft gave us **WebForms**, which was like trying to build a modern skyscraper out of LEGOs. Then came **ASP.NET MVC**, which was much better but also forced us to write a lot of boilerplate code just to get a simple page running.

Then, in .NET Core 2.0 (circa 2017), Microsoft introduced **Razor Pages**. It was their way of saying:

> "Hey, what if we made something that didn't require a full MVC structure, but still let you use Razor syntax?"

And thus, **Razor Pages** were born. It’s like MVC, but without the "C" (Controller), because apparently, nobody likes writing controllers.

## What is Razor Pages, Really?

Imagine if MVC decided to hit the gym, shed some complexity, and came back leaner and more focused. That’s **Razor Pages**.

Each page is self-contained with its own **.cshtml file** (HTML + Razor) and a **PageModel** (C# code-behind). No more dealing with routing headaches—each page gets its own URL automagically.

It’s perfect for apps that aren’t huge, where you just need a simple, structured way to build pages without drowning in the usual MVC setup.

## Setting Up Razor Pages (Because You’re Curious, Right?)

First, create an **ASP.NET Core** project and enable Razor Pages in `Startup.cs` (or `Program.cs` in newer versions):

```csharp
var builder = WebApplication.CreateBuilder(args);
builder.Services.AddRazorPages();
var app = builder.Build();

app.UseRouting();
app.MapRazorPages();

app.Run();
```

Boom! Razor Pages are now in business.

## A Simple Razor Page Example

Every Razor Page has two files:

1. `Index.cshtml` → The front-end page
2. `Index.cshtml.cs` → The PageModel (code-behind file)

Here’s what `Index.cshtml` might look like:

```html
@page
@model IndexModel

<!DOCTYPE html>
<html>
<head>
    <title>Welcome!</title>
</head>
<body>
    <h1>Hello, @Model.Message</h1>
</body>
</html>
```

And here’s `Index.cshtml.cs`:

```csharp
using Microsoft.AspNetCore.Mvc.RazorPages;

public class IndexModel : PageModel
{
    public string Message { get; private set; } = "World!";
}
```

That’s it! No controller, no routing files, no drama. Just a page and its logic, neatly packaged together.

## Handling Forms in Razor Pages

Need to process a form? Razor Pages has got you covered. Here’s a simple example:

### The Form (`Contact.cshtml`)

```html
@page
@model ContactModel

<form method="post">
    <label>Name:</label>
    <input type="text" asp-for="Name" />
    <button type="submit">Submit</button>
</form>
<p>@Model.Message</p>
```

### The Page Model (`Contact.cshtml.cs`)

```csharp
using Microsoft.AspNetCore.Mvc.RazorPages;

public class ContactModel : PageModel
{
    public string Message { get; private set; }
    public string Name { get; set; }

    public void OnPost()
    {
        Message = $"Hello, {Name}!";
    }
}
```

Simple, clean, and no unnecessary ceremony. Forms just work!

## Why Should You Use Razor Pages?

* **Less Boilerplate** – No need to define controllers and routes manually.
* **More Structure** – Each page has its logic, making code more organized.
* **Built-in Model Binding** – Form submissions are straightforward.
* **Great for Small to Medium Apps** – If you don’t need the full MVC structure, Razor Pages is the way to go.

## When Should You *Not* Use Razor Pages?

* If you're building a **huge enterprise app** with tons of APIs—stick to MVC.
* If you need to **share logic between multiple views**—MVC is better for that.
* If you just enjoy suffering—go ahead, write everything in JavaScript.

<!-- ## Final Thoughts

Razor Pages is **like MVC, but without the headaches**. It’s great for simple apps where you just want to display data and process forms without dealing with controllers and routing madness.

So, if you’ve been dreading the thought of spinning up yet another MVC project, give Razor Pages a shot. You might actually enjoy it! -->

***

## Key Ideas

| Concept              | Summary                                                 |
| -------------------- | ------------------------------------------------------- |
| What is Razor Pages? | A simpler, page-based alternative to MVC                |
| History              | Introduced in ASP.NET Core 2.0 (2017)                   |
| Setup                | `AddRazorPages()` and `MapRazorPages()` in `Startup.cs` |
| Basic Structure      | `*.cshtml` (view) + `*.cshtml.cs` (PageModel)           |
| Form Handling        | Uses model binding, just like MVC                       |
| When to Use It?      | Great for small-to-medium projects                      |
| When Not to Use It?  | Not ideal for large, complex applications               |

***

## References

1. [Microsoft Docs on Razor Pages](https://learn.microsoft.com/en-us/aspnet/core/razor-pages/)
2. [ASP.NET Core GitHub Repository](https://github.com/dotnet/aspnetcore)
3. [Official ASP.NET Blog](https://devblogs.microsoft.com/aspnet/)

***
