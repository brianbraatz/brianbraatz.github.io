---
title: PASCAL In a Nutshell
description: PASCAL In a Nutshell
slug: pascal-in-a-nutshell
date: 2021-03-31
image: post/Articles/IMAGES/Pascal.jpg
categories:
  - Pascal
  - History
tags:
  - Pascal
  - Borland
  - Delphi
  - Programming
  - Languages
  - Software
  - Development
  - Apple
  - II
draft: false
weight: 430
categories_ref:
  - Pascal
  - History
lastmod: 2025-03-14T15:45:13.041Z
---
# PASCAL In a Nutshell

## The Glorious History of Pascal

Back in **1970**, when bell-bottoms were cool and computers filled entire rooms, a Swiss computer scientist named **Niklaus Wirth** thought, *"Wouldn't it be great if students could learn programming with a language that actually made sense?"*

And thus, **Pascal** was born—a **structured, strongly typed, and readable** programming language that was perfect for education and beyond. It was designed to teach **good programming practices** before spaghetti code became a universal issue.

### Pascal vs. The Competition

Back in the day, Pascal had to compete with **Fortran, COBOL, and C**. Here’s how they stacked up:

| Feature                | Pascal           | C             | Fortran            | COBOL         |
| ---------------------- | ---------------- | ------------- | ------------------ | ------------- |
| Readability            | ✅ Super readable | ❌ Not so much | ❌ Meh              | ❌ Too verbose |
| Popular in Academia?   | ✅ Yes            | ✅ Somewhat    | ✅ Yes (math-heavy) | ❌ Not really  |
| Speed                  | ⚡ Fast enough    | ⚡ Very fast   | ⚡ Fast             | 🐢 Slow       |
| Structured Programming | ✅ Yes            | ✅ Yes         | ❌ Kinda            | ❌ Nope        |

## Pascal on 9-Bit Computers (Like the Apple II)

One of Pascal’s **biggest claims to fame** was its presence on the **Apple II**. Apple’s co-founder **Steve Jobs** was a huge fan of structured programming and made sure Pascal was **the go-to language for the Apple Lisa and Macintosh**.

Pascal was also **the language of choice for early Macintosh development**, before C took over. If you’ve ever written old-school Apple software, **chances are, you did it in Pascal**.

## Pascal 101: The Basics

### 1. Hello, World! in Pascal

```pascal
program HelloWorld;
begin
  writeln('Hello, Pascal World!');
end.
```

### 2. Variables and Loops

```pascal
program LoopExample;
var
  i: Integer;
begin
  for i := 1 to 5 do
    writeln('This is line ', i);
end.
```

### 3. Functions in Pascal

```pascal
function Square(x: Integer): Integer;
begin
  Square := x * x;
end;

begin
  writeln('Square of 5 is ', Square(5));
end.
```

## The Famous Pascal Bubble Sort

Sorting algorithms are **the bread and butter of programming**, so here’s a **Bubble Sort in Pascal**:

```pascal
program BubbleSortDemo;
var
  arr: array[1..5] of Integer = (5, 3, 8, 1, 2);
  i, j, temp: Integer;
begin
  for i := 1 to 4 do
    for j := 1 to 5 - i do
      if arr[j] > arr[j + 1] then
      begin
        temp := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := temp;
      end;

  for i := 1 to 5 do
    writeln(arr[i]);
end.
```

## Enter Borland Delphi: Pascal Goes Modern

Pascal was **great**, but by the 1990s, it needed a **serious upgrade**. That’s when **Borland Delphi** stepped in, bringing:

* **Object-Oriented Programming (OOP)**
* **A Visual UI Framework**
* **A Fast Compiler**

Delphi **revitalized Pascal**, making it a powerful tool for **Windows application development**. It’s still around today, proving that **Pascal refuses to die!**

## Wrapping Up

Pascal **might not be as popular as Python or JavaScript today**, but its **legacy lives on** in modern programming languages. If you ever want to understand **structured programming**, Pascal is **a great place to start**.

So go ahead—fire up an old Apple II emulator, write some Pascal, and experience **programming history** firsthand! 🚀

## Key Takeaways

* **Pascal was created by Niklaus Wirth in 1970** as an easy-to-read structured programming language.
* **It was heavily used in academia** and early **Apple II and Macintosh development**.
* **Pascal was readable, structured, and beginner-friendly**, unlike C.
* **Borland Delphi modernized Pascal** with **Object-Oriented Programming** and **Visual UI Design**.
* **Pascal is still used today** in some niche applications (and by hardcore enthusiasts).

## References

* [Pascal on Wikipedia](https://en.wikipedia.org/wiki/Pascal_\(programming_language\))
* [Niklaus Wirth Biography](https://en.wikipedia.org/wiki/Niklaus_Wirth)
* [Borland Delphi](https://en.wikipedia.org/wiki/Delphi_\(software\))
* [Pascal Bubble Sort Example](https://www.geeksforgeeks.org/bubble-sort/)
