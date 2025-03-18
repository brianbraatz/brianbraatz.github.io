---
title: Introspective Whitebox Unit Testing with Mole and Pex
description: History of this Curious Microsoft Research Creation
slug: moles-and-pex-unit-testing
date: 2020-12-14
image: post/Articles/IMAGES/mammal_mole.jpg
categories:
  - Unit Testing
  - DotNet
  - CSharp
  - Testing
tags:
  - Moles
  - Pex
  - Unit
  - Testing
  - Net
  - Mocking
  - Microsoft
  - Visual
  - Studio
  - Testing
  - Framework
  - Fakes
  - Framework
  - Csharp
draft: false
weight: 35
categories_ref:
  - Unit Testing
  - DotNet
  - CSharp
  - Testing
slug_calculated: https://brianbraatz.github.io/p/moles-and-pex-unit-testing
lastmod: 2025-03-14T16:40:30.122Z
---
[Moles](https://www.nwf.org/Educational-Resources/Wildlife-Guide/Mammals/Moles)

<!--

# The History of Moles and Pex: The Dynamic Duo of .NET Unit Testing  
-->

<!-- 
Ever tried to write unit tests and thought, *"Man, this would be a lot easier if I could just hijack this method and make it do what I want?"* Well, good news: Microsoft Research felt the same way! Enter **Pex** and **Moles**, two tools that made unit testing in .NET a whole lot less painful.  
-->

## Pex: The Smart Test Generator

Pex was first released in 2007 as a **Visual Studio add-in** designed to automatically generate test cases for your code.

(!!!)\
What?

<!-- 
It’s like having an overachieving intern who keeps poking at your methods, trying to find ways to break them.  
-->

Pex works by doing **white-box testing**, which means it actually looks at your code, figures out different execution paths, and then creates test cases to hit all of them.

If there’s an edge case, Pex will find it (and laugh at your fragile code while doing so).

For example, given this simple method:

```csharp
public int Add(int a, int b)
{
    return a + b;
}
```

Pex will generate test cases for **all** sorts of input values, including:

* Regular numbers like `Add(2, 3)`
* Edge cases like `Add(int.MaxValue, 1)` (to see if your code explodes)
* Zero, negative numbers, and all the weird scenarios you didn’t think of

Pretty neat, huh?

More on Pex here: [Microsoft Research – Pex](https://www.microsoft.com/en-us/research/project/pex-and-moles-isolation-and-white-box-unit-testing-for-net/)

***

## Moles: The Ultimate Code Impersonator

Then there’s **Moles**, which arrived in 2009 to solve another annoying problem in unit testing: **dependencies**.

We’ve all been there—trying to write a unit test, only to realize our method depends on some external service, a database call, or (*shudder*) `DateTime.Now`.

Moles fixes this by letting you **replace any .NET method with your own delegate**. This means you can swap out static, non-virtual, and even sealed methods—things that were previously impossible to mock.

For example, let’s say you have a method that depends on `DateTime.Now`:

```csharp
public bool IsWeekend()
{
    return DateTime.Now.DayOfWeek == DayOfWeek.Saturday || DateTime.Now.DayOfWeek == DayOfWeek.Sunday;
}
```

Unit testing this normally is a pain because `DateTime.Now` changes every time you run it. But with Moles, you can do this:

```csharp
[TestMethod]
public void TestWeekend()
{
    MDateTime.NowGet = () => new DateTime(2025, 2, 10); // Forces a fixed date (Monday)
    
    Assert.IsFalse(IsWeekend());
}
```

Boom! No more flaky tests because of time-dependent logic.

More on Moles here: [Microsoft Research – Moles](https://www.microsoft.com/en-us/research/project/moles-isolation-framework-for-net/)

***

## Pex + Moles = A Match Made in Unit Testing Heaven

Pex and Moles worked *really* well together.

Pex would generate smart test cases, and Moles would isolate dependencies so you could actually run those tests.

The combination was a game-changer for .NET developers.

But alas, all good things must come to an end—or rather, evolve.

In **2012**, Microsoft rolled Pex and Moles into something new: the **Fakes Framework**, which became a built-in part of Visual Studio.

So while Pex and Moles may no longer be around in their original form, their legacy lives on!

***

## Can You Use Pex and Moles in Modern C#?

**Short answer: Nope. But there are alternatives.**

Pex and Moles were designed for .NET Framework (pre-.NET Core days) and **are no longer actively maintained**.

If you're working with modern C# (.NET Core, .NET 5, 6, 7, or later), you'll need to use other tools.

### **Alternatives to Pex**

Pex's core idea—automated test generation—lives on in a tool called **Microsoft IntelliTest**, which is available in some versions of Visual Studio Enterprise.

IntelliTest does what Pex did: analyzes your code and generates unit tests automatically.

If you want something open-source, check out:

* **Stryker.NET** – Mutation testing for C#
* **AutoFixture** – Generates test data automatically
* **Fuzzing tools** – Tools like **SharpFuzz** can help you discover edge cases

### **Alternatives to Moles**

Moles' core idea—mocking dependencies, including static and sealed methods—is now handled by **Microsoft Fakes**, which is built into **Visual Studio Enterprise**.

If you’re not using Enterprise, try:

* **Moq** – The most popular mocking framework, but it **can't** mock statics
* **NSubstitute** – Another great option, but also **no statics**
* **JustMock (Telerik)** – A commercial alternative that **can** mock static methods
* **TypeMock** – Another commercial option that **can** mock pretty much anything

Basically, if you need **static method mocking** like Moles did, **Microsoft Fakes, JustMock, or TypeMock** are your best bets.

***

## Wrapping Up

* **Pex**: Automatically generated test cases by analyzing your code.
* **Moles**: Let you replace any .NET method, even static and sealed ones, for better isolation in unit tests.
* **Fakes Framework**: The modern successor to Moles, built into Visual Studio Enterprise.
* **IntelliTest**: The modern successor to Pex for automated test case generation.

If you’re writing **modern** .NET tests, you’ll want to look into **Microsoft Fakes, IntelliTest, Moq, and JustMock** to get the same benefits Pex and Moles provided.
