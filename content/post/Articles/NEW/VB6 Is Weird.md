---
title: 10 Reasons Why Visual Basic 6 is Really Weird
description: Actually.. beyond weird.....
slug: 10-reasons-why-visual-basic-6-is-really-weird
date: 2020-05-19
image: post/Articles/IMAGES/vb6psychwide.png
categories: 
tags:
  - Visual
  - Basic
  - VB6
  - Microsoft
  - Legacy
  - Software
  - Programming
  - Weird
  - Features
  - Windows
  - History
draft: false
weight: 324
lastmod: 2025-02-03T23:33:51.480Z
---
# 10 Reasons Why Visual Basic 6 is Really Weird

Ah, **Visual Basic 6**.

If you’ve ever used it, you know exactly what I mean when I say **this language was WEIRD**. It was quirky, inconsistent, and yet—somehow—**wildly popular**.

## A Brief History of Visual Basic 6

Back in the **early 1990s**, Microsoft was trying to make **Windows programming more accessible**.

Most languages at the time (like C and Pascal) required **writing tons of code just to make a simple UI**.

Microsoft wanted something **easier**.

Enter **Alan Cooper**.

In 1988, he built a prototype called **"Tripod"**, a drag-and-drop UI builder.

He showed it to **Bill Gates**, and Gates saw its potential.

This evolved into **Visual Basic**, which was officially released in **1991**.

**Visual Basic 6 (VB6)**, the last “real” VB version, came out in **1998** and was designed for **Windows 95, 98, and NT**.

It was used in thousands of enterprise applications and became **one of the most widely used programming languages of its time**.

Then, in 2002, Microsoft killed it.

They introduced **Visual Basic .NET**, which was more like C# and lacked many of VB6’s weirder features.

**And guess what? No one liked it.**\
The weirdness of VB6 was part of its charm.

So, what made **VB6 so bizarre**?.....

## 1. The **GoTo** Statement Was a First-Class Citizen

Most modern languages **hate** `GoTo`. It’s considered **evil**, making code unreadable and spaghetti-like. But in **VB6**, `GoTo` wasn’t just allowed—it was **encouraged**!

```vb
Sub Example()
    Dim x As Integer
    x = 10
    If x = 10 Then GoTo Skip
    MsgBox "This will never run!"
Skip:
    MsgBox "Skipped successfully!"
End Sub
```

Want to make your code **impossible to maintain**? Just sprinkle some `GoTo` everywhere.

## 2. Everything Was **Variant** by Default

Forget **strong typing**. In VB6, if you didn’t specify a type, **everything was a Variant**.

```vb
Dim mysteryVariable  ' What type is it? Who knows??
mysteryVariable = "Hello"
mysteryVariable = 1234  ' Works just fine!
```

The **Variant** type could hold anything—numbers, strings, objects, **your hopes and dreams**—which led to some **hilarious runtime errors**.

## 3. Error Handling Was... "Unique"

Try-catch? Exceptions? Nah. VB6 had **"On Error Resume Next"**, which **literally ignored** all errors.

```vb
On Error Resume Next
Dim x As Integer
x = 5 / 0  ' No crash, just moves on!
```

This was basically **a “YOLO” mode for programming**.

## 4. Controls Were Stored in a .FRM File as Text

Instead of defining UI in code, VB6 stored forms in `.frm` files. But guess what? **These were just text files!**

That means you could **open a VB6 form in Notepad** and **manually edit** its properties.

## 5. Timers Had a Minimum Interval of 1 Second

If you needed **millisecond precision**, too bad. VB6 timers **only updated every second**. Need something faster? **Hack your way around it.**

I have not found memories of having to write many OCX custom controls (plugings) to add things to Visual Basic or get around weird language issues...

## 6. Default Values Were Sometimes **Random**

Variables in VB6 didn’t always initialize to zero or null.

Sometimes, they’d just **contain garbage values from memory**.

```vb
Dim x As Integer
MsgBox x  ' What will this print? Who knows?!
```

**Schrödinger’s Integer.**

## 7. Object-Oriented? Kinda, But Not Really

VB6 had **classes**, but no real **inheritance**. If you wanted to reuse functionality, you had to get **creative**.

## 8. Drag-and-Drop UI But No Proper Code Separation

You could **build entire applications** just by **dragging buttons and textboxes around**. But all logic went into the **same file**, making large projects **a nightmare to manage**.

## 9. `DoEvents` Was a Required Hack

In VB6, long-running loops **froze the entire UI**.

To prevent this, you had to **manually yield** to the system using `DoEvents`.

```vb
Do While True
    DoEvents  ' Let Windows breathe a little
Loop
```

Congratulations, you just created **an infinite loop that doesn’t freeze Windows!**

## 10. Forms Are Both Global Variables and Classes !?!?!!?

**!?!?!?!!?!?!?!?!?!!?**

You create a new VB6 project, and you get **Form1** automatically. You can just type:

```vb
Form1.Show
```

So, **Form1 must be a global variable**, right? Well... kind of. Because you can also do this:

```vb
Dim f As New Form1
f.Show
```

You can even mix these together, using `Form1.Show` alongside multiple dynamically created instances:

```vb
Dim forms(5) As Form1
Dim fglobal As New Form1
For i = 0 To 4
    Set forms(i) = New Form1
    forms(i).Show
Next
fglobal.Show
```

**SO NOW YOU HAVE 6 FORMS ON THE SCREEN..**

**Is this brilliant???, or completely stupid?**

That’s a mental exercise for the reader.

***

## VB.NET: The Normal Version No One Wanted

When Microsoft released **VB.NET**, it **fixed** all of VB6’s weirdness.

It became **a normal .NET language**, much closer to **C#**. And guess what? **Nobody liked it.**

It turns out **people love weird languages**.

The strange, quirky nature of VB6 was a **big part of its charm**. Once it became just another .NET language, it lost its appeal.

Honestlly.. I ALWAYS hated Visual Basic and Still do..

BUT I respect it.. for being weird.. :)

***

## Key Ideas

| Feature                | Why It Was Weird                         |
| ---------------------- | ---------------------------------------- |
| `GoTo`                 | Encouraged spaghetti code                |
| Variant Type           | Everything could be anything             |
| `On Error Resume Next` | Errors? Just ignore them!                |
| Forms as Text Files    | You could edit them in Notepad           |
| Timers                 | Minimum interval: 1 second               |
| Default Values         | Could be random garbage                  |
| Object-Oriented?       | No real inheritance                      |
| Drag-and-Drop UI       | No real code separation                  |
| `DoEvents`             | Manual UI thread handling                |
| VB.NET                 | Made VB normal—and killed its popularity |

***

## References

* https://en.wikipedia.org/wiki/Visual\_Basic\_(classic)
* https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-basic-6
* https://www.cooper.com/prototypes/tripod-the-origins-of-visual-basic/
