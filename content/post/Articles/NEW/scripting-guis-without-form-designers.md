---
title: Command line C# Scripting with GUIs
description: And NO form designers
slug: scripting-guis-without-form-designers-in-c#
date: 2021-07-24
image: post/Articles/IMAGES/14.jpg
categories:
  - Scripting
  - WPF
  - Maui
  - CSharp
  - DotNet
tags:
  - WPF
  - Xaml
  - WinForms
  - GUI
  - Scripting
  - Code
  - CPP
draft: false
weight: 450
lastmod: 2025-02-09T23:09:57.257Z
---
# Scripting GUIs Without Form Designers in C\#

## Introduction

So you want to build a GUI without dragging and dropping a bunch of widgets like it's 1999?

Welcome to the world of hardcore UI scripting—where real devs define their buttons in **code** like warriors.

In this article, we’ll create **WPF** and **WinForms** GUIs **entirely in C# code**, no form designers allowed.

And, because we like to keep things simple, our example app will have just **one text field and a button**.

The button will close the app and print whatever you typed into the console. Simple, yet effective—just like a well-placed `Console.WriteLine`.

***

## WPF Example (XAML? We don’t need it!)

WPF is usually all about **XAML**, but who needs that when you have raw C#? Here’s how you create a **WPF window with a text box and a button**, entirely in code:

```csharp
using System;
using System.Windows;
using System.Windows.Controls;

class Program
{
    [STAThread]
    static void Main()
    {
        Application app = new Application();
        Window window = new Window
        {
            Title = "WPF Code-Only GUI",
            Width = 300,
            Height = 200
        };

        StackPanel panel = new StackPanel();

        TextBox textBox = new TextBox();
        Button button = new Button { Content = "Submit" };

        button.Click += (sender, e) =>
        {
            Console.WriteLine("Entered text: " + textBox.Text);
            window.Close();
        };

        panel.Children.Add(textBox);
        panel.Children.Add(button);
        window.Content = panel;

        app.Run(window);
    }
}
```

Yes, it really is that simple! No messing around with `.xaml` files, no separate designer views—just **pure** C#.

***

## WinForms Example (Old School Cool)

Now let’s go even **older school** with **WinForms**. It's been around since the dinosaurs (okay, since .NET 1.0), but it's still kicking.

```csharp
using System;
using System.Windows.Forms;

class Program
{
    [STAThread]
    static void Main()
    {
        Application.EnableVisualStyles();
        Application.SetCompatibleTextRenderingDefault(false);

        Form form = new Form
        {
            Text = "WinForms Code-Only GUI",
            Width = 300,
            Height = 150
        };

        TextBox textBox = new TextBox { Top = 20, Left = 20, Width = 200 };
        Button button = new Button { Text = "Submit", Top = 50, Left = 20 };

        button.Click += (sender, e) =>
        {
            Console.WriteLine("Entered text: " + textBox.Text);
            form.Close();
        };

        form.Controls.Add(textBox);
        form.Controls.Add(button);

        Application.Run(form);
    }
}
```

## Boom! No designers, no `Form1.cs`, just **raw** WinForms magic.

## Compiling and Running the Code from the Command Line

You don’t need Visual Studio to compile these examples! You can do it straight from the command line using the **.NET CLI**.

### Compiling the WPF Example

1. Open a terminal or command prompt.

2. Navigate to the folder where you saved the WPF example.

3. Run the following command to compile it:

   ```sh
   csc /r:PresentationFramework.dll WpfExample.cs
   ```

4. Run the compiled executable:

   ```sh
   WpfExample.exe
   ```

### Compiling the WinForms Example

1. Open a terminal or command prompt.

2. Navigate to the folder where you saved the WinForms example.

3. Compile it using:

   ```sh
   csc /r:System.Windows.Forms.dll WinFormsExample.cs
   ```

4. Run the compiled executable:

   ```sh
   WinFormsExample.exe
   ```

If you don’t have `csc` (the C# compiler) available, make sure you have the **.NET SDK** installed. You can check by running:

```sh
dotnet --version
```

If `dotnet` is installed, you can also compile using:

```sh
dotnet new console -n MyApp
cd MyApp
dotnet add package Microsoft.Windows.Compatibility
dotnet run
```

And that’s it! No IDE needed, just **raw command-line power**.

***

## Conclusion

Who needs **drag-and-drop UI designers** when you can create full-fledged **GUIs in pure C# code**? Whether you’re rolling with **WPF** or keeping it **old-school with WinForms**, scripting UI is totally doable.

### Key Ideas

| Concept                   | Explanation                                       |
| ------------------------- | ------------------------------------------------- |
| **WPF in Code**           | Creating a WPF UI purely in C# without using XAML |
| **WinForms in Code**      | Defining a WinForms UI in C# without the designer |
| **Event Handling**        | Using event handlers to capture button clicks     |
| **Application Lifecycle** | Running and closing an app programmatically       |

***

## References

* WPF Documentation: <https://learn.microsoft.com/en-us/dotnet/desktop/wpf/>
* WinForms Documentation: <https://learn.microsoft.com/en-us/dotnet/desktop/winforms/>
* C# Programming Language: [https://en.wikipedia.org/wiki/C\_Sharp\_(programming\_language)](https://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29)
* .NET Framework: <https://en.wikipedia.org/wiki/.NET_Framework>

***
