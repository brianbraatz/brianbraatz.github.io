---
title: Logo In a Nutshell
description: "The Logo Programming Language: History, Motivation, Apple II, Drawing Robots, and Its Connection to PostScript"
slug: the-logo-programming-language:-history-motivation-apple-ii-drawing-robots-and-its-connection-to-postscript
date: 2024-03-31
image: post/Articles/IMAGES/logologo.png
categories:
  - Postscript
  - Document Imaging
  - PDF
  - Logo Language
tags:
  - Logo
  - Programming
  - Language
  - History
  - Of
  - Computing
  - Apple
  - Ii
  - Turtle
  - Graphics
  - Postscript
  - Educational
  - Programming
  - Drawing
  - Robots
draft: false
weight: 263
lastmod: 2025-02-10T17:48:37.960Z
---
<!-- 
See
[Comparing logo to postscript](post/Articles/_new6/Comparing%20logo%20to%20postscript.md)
-->

# The Logo Programming Language: History, Motivation, Apple II, Drawing Robots, and Its Connection to PostScript

## Introduction

Once upon a time (the **1960s**), a group of researchers thought:\
*"Hey, what if kids could learn programming in a way that wasn’t terrifying?"*

Thus, **Logo** was born.

It wasn't just a programming language—it was an **educational movement** designed to teach kids **problem-solving, creativity, and logic**. Best of all, it came with **Turtle Graphics**, which let users **draw by moving a virtual "turtle"** across the screen.

> **But wait, there's more!**\
> Logo was also **connected to early robots**, had a **major role on the Apple II**, and even has **similarities to PostScript**, the language behind **printing and vector graphics**.

<!-- 
In this article, we’ll explore:  

- The **history and motivation** behind Logo.  
- How it influenced **Apple II programming**.  
- The **robotic turtles that drew physical art**.  
- A **comparison to PostScript** (another language focused on graphics).  
- **10 fun examples** of Logo programs.  
- **A giant list of links** to run Logo in your browser and dive deeper into its history.  
-->

***

## A Brief History of Logo

* **Created in 1967** at MIT by **Seymour Papert**, **Wally Feurzeig**, and colleagues.
* Inspired by **Piaget’s theories of child learning**.
* Originally ran on **big mainframes**, but later became famous on **Apple II, Commodore 64, and IBM PCs**.
* Introduced **Turtle Graphics**, a way to teach kids programming through **visual movement**.
* Used in **education worldwide**, influencing modern languages like **Scratch**.

> **Further Reading:**
>
> * [Wikipedia: Logo (Programming Language)](https://en.wikipedia.org/wiki/Logo_\(programming_language\))
> * [History of Logo at MIT](https://el.media.mit.edu/logo-foundation/)

***

## Logo’s Connection to the Apple II

**Apple loved Logo** because it aligned with their goal of making computers **accessible to schools**.

* Apple Logo was a **big deal in the 1980s**, bundled with Apple II computers.
* **Steve Jobs supported** using Logo for education.
* Kids used it to **draw shapes, animate turtles, and learn procedural programming**.

💡 **Fun Fact:** Some early **Macintosh engineers** learned programming using **Logo on the Apple II**!

> **Further Reading:**
>
> * [Apple II and Logo](https://apple2history.org/operating-systems/)

***

## Drawing Robots and Physical Turtle Graphics

Before **computer screens**, there were **real robots**!

* **The first Logo “turtles”** were **actual robots with pens attached**.
* Kids programmed the robot to move and draw **on real paper**.
* Some modern robots (like Bee-Bot) still **use Logo-like commands**.

💡 **Fun Fact:** The original **turtle robots** were basically **Roombas that drew pictures** instead of vacuuming!

> **Further Reading:**
>
> * [Turtle Robots in Education](https://en.wikipedia.org/wiki/Turtle_\(robot\))

***

## Logo’s Relationship to PostScript

**PostScript** (invented in 1982) is another **graphical programming language**. Like Logo, it:

* Uses **commands to draw graphics**.
* Focuses on **vector-based drawing** (used in printing & PDFs).
* Has **a stack-based syntax** (unlike Logo’s procedural commands).

### **Comparison Table: Logo vs. PostScript**

| Feature             | Logo                 | PostScript                      |
| ------------------- | -------------------- | ------------------------------- |
| **Designed For**    | Kids, education      | Printing, professional graphics |
| **Graphics Model**  | Turtle Graphics      | Cartesian coordinates           |
| **Syntax**          | Procedural           | Stack-based                     |
| **Primary Usage**   | Learning programming | Professional document rendering |
| **Example Devices** | Apple II, PC         | Printers, Adobe Illustrator     |

💡 **Verdict:** **Logo is for fun, PostScript is for serious graphic design**—but both share the **concept of programmable drawing**.

> **Further Reading:**
>
> * [PostScript Wikipedia](https://en.wikipedia.org/wiki/PostScript)

***

## Logo Syntax Overview

| Command                            | Meaning                                  |
| ---------------------------------- | ---------------------------------------- |
| `FORWARD 50`                       | Move forward 50 steps                    |
| `BACK 30`                          | Move back 30 steps                       |
| `RIGHT 90`                         | Turn right 90 degrees                    |
| `LEFT 45`                          | Turn left 45 degrees                     |
| `PENUP`                            | Stop drawing                             |
| `PENDOWN`                          | Start drawing                            |
| `REPEAT 4 [ FORWARD 50 RIGHT 90 ]` | Draw a square                            |
| `TO TRIANGLE`                      | Define a new procedure called "TRIANGLE" |
| `SETCOLOR [255 0 0]`               | Set drawing color to red                 |

***

## 10 Logo Code Examples

### **1. Draw a Square**

```logo
REPEAT 4 [ FORWARD 100 RIGHT 90 ]
```

### **2. Draw a Triangle**

```logo
REPEAT 3 [ FORWARD 100 RIGHT 120 ]
```

### **3. Draw a Circle**

```logo
REPEAT 360 [ FORWARD 1 RIGHT 1 ]
```

### **4. Create a Custom Procedure**

```logo
TO STAR
  REPEAT 5 [ FORWARD 100 RIGHT 144 ]
END
```

### **5. Draw a Spiral**

```logo
REPEAT 100 [ FORWARD REPCOUNT RIGHT 20 ]
```

***

## Where to Run Logo Online

* [Turtle Academy](https://turtleacademy.com/) – A simple online Logo interpreter.
* [JSLogo](http://www.calormen.com/jslogo/) – JavaScript-powered Logo.
* [Microsoft Small Basic](https://smallbasic.github.io/) – Supports turtle graphics.

> **Further Reading:**
>
> * [Logo Interpreter GitHub](https://github.com/jrincayc/Logo)
> * [History of Turtle Graphics](https://cs.wellesley.edu/~cs110/reading/TurtleHistory.pdf)

***

## Key Takeaways

* **Logo was an educational programming language** focused on **graphics and learning**.
* **It had a huge influence on Apple II**, early robotics, and modern programming education.
* **PostScript shares some concepts** but is used for **professional graphic design**.
* **You can still run Logo today**—try one of the online Logo interpreters!

***

## References

1. [Logo Programming Language Wikipedia](https://en.wikipedia.org/wiki/Logo_\(programming_language\))
2. [History of Turtle Graphics](https://cs.wellesley.edu/~cs110/reading/TurtleHistory.pdf)
3. [PostScript and Its Use](https://en.wikipedia.org/wiki/PostScript)
4. [Online Logo Interpreters](https://turtleacademy.com/)
