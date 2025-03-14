---
title: Comparing Logo to PostScript
description: A comparison of Logo and PostScript
slug: logo-postscript
date: 2008-01-25
image: post/Articles/IMAGES/postscriptlogo.png
categories:
  - Postscript
  - Document Imaging
  - PDF
  - Logo Language
  - Languages
tags:
  - Logo
  - PostScript
  - Turtle
  - Graphics
  - Vector
  - Graphics
  - Programming
  - Drawing
draft: false
weight: 439
categories_ref:
  - Postscript
  - Document Imaging
  - PDF
  - Logo Language
  - Languages
lastmod: 2025-03-14T15:45:22.716Z
---
<!--
See 
[Logo programming language](post/Articles/_new6/Logo%20programming%20language.md)
-->

# Logo to PostScript: Equivalent Drawing Commands

## Introduction

**Logo** and **PostScript** are both programming languages that handle **vector-based drawing**, but they take different approaches. **Logo** is **procedural**, often used in **education**, whereas **PostScript** is **stack-based** and designed for **printing and high-quality graphics**.

In this article, we'll convert **Logo turtle graphics code** into **PostScript**, demonstrating how common **Logo commands** translate into **PostScript equivalents**.

***

## 1. Draw a Square

### **Logo Code:**

```logo
REPEAT 4 [ FORWARD 100 RIGHT 90 ]
```

### **PostScript Equivalent:**

```postscript
newpath
100 100 moveto % Move to starting point
100 0 rlineto  % Draw right
0 100 rlineto  % Draw up
-100 0 rlineto % Draw left
closepath
stroke
```

***

## 2. Draw a Triangle

### **Logo Code:**

```logo
REPEAT 3 [ FORWARD 100 RIGHT 120 ]
```

### **PostScript Equivalent:**

```postscript
newpath
100 100 moveto % Move to starting point
100 0 rlineto  % First side
-50 86.6 rlineto % Second side (Height of equilateral triangle)
closepath
stroke
```

***

## 3. Draw a Circle

### **Logo Code:**

```logo
REPEAT 360 [ FORWARD 1 RIGHT 1 ]
```

### **PostScript Equivalent:**

```postscript
newpath
200 200 50 0 360 arc  % Draw a circle centered at (200,200) with radius 50
stroke
```

***

## 4. Create a Custom Procedure (Draw a Star)

### **Logo Code:**

```logo
TO STAR
  REPEAT 5 [ FORWARD 100 RIGHT 144 ]
END
```

### **PostScript Equivalent:**

```postscript
newpath
100 100 moveto % Move to starting point
5 {
  100 0 rlineto   % Move forward
  144 rotate      % Rotate right 144 degrees
} repeat
closepath
stroke
```

***

## 5. Draw a Spiral

### **Logo Code:**

```logo
REPEAT 100 [ FORWARD REPCOUNT RIGHT 20 ]
```

### **PostScript Equivalent:**

```postscript
newpath
100 100 moveto % Move to starting point
0 0 translate
1 100 {
  dup % Duplicate loop counter on stack
  0 rlineto  % Move forward by counter value
  20 rotate  % Rotate right 20 degrees
} for
stroke
```

***

## Key Differences Between Logo and PostScript

| Feature               | Logo                                    | PostScript                              |
| --------------------- | --------------------------------------- | --------------------------------------- |
| **Design Purpose**    | Education, Turtle Graphics              | Professional vector graphics & printing |
| **Syntax Style**      | Procedural, command-driven              | Stack-based, postfix notation           |
| **Graphics Model**    | Relative movement (Turtle)              | Absolute Cartesian coordinates          |
| **Looping Mechanism** | `REPEAT` keyword                        | `repeat` or `for` loops                 |
| **Circle Drawing**    | Approximate using `FORWARD` and `RIGHT` | `arc` function for precise circles      |
| **Output Use Case**   | Interactive learning                    | High-quality printing and PDFs          |

***

## Conclusion

Both **Logo and PostScript** offer **powerful ways to generate vector-based graphics**, but they **serve different audiences**. While **Logo excels in teaching programming concepts**, **PostScript is used in professional printing and graphic design**.

If you want to **experiment with Logo**, check out online interpreters like:

* [Turtle Academy](https://turtleacademy.com/)
* [JSLogo](http://www.calormen.com/jslogo/)

For **PostScript programming**, consider trying:

* [Ghostscript](https://www.ghostscript.com/)
* [PostScript Wikipedia](https://en.wikipedia.org/wiki/PostScript)

Let me know if you have any questions or need more examples! 🚀
