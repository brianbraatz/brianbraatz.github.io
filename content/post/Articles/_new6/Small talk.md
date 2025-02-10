---
title: Smalltalk in a Nutshell
description: The Smalltalk Programming Language Explored
slug: the-smalltalk-programming-language:-history-motivation-relationship-to-modern-languages-and-10-code-examples
date: 2024-06-17
image: post/Articles/IMAGES/smalltalklogo.png
categories: 
tags:
  - Smalltalk
  - Programming
  - Language
  - Object-Oriented
  - Programming
  - History
  - Of
  - Computing
  - Software
  - Development
  - Modern
  - Languages
  - Dynamic
  - Languages
  - Syntax
  - Comparison
draft: false
weight: 248
lastmod: 2025-02-10T15:26:47.423Z
---
<!--
# The Smalltalk Programming Language: History, Motivation, Relationship to Modern Languages, and 10 Code Examples
-->

## Introduction

If you've ever wondered where **modern object-oriented programming (OOP) came from**, you need to know about **Smalltalk**.

Smalltalk **invented a lot of what we take for granted today**—things like **object-oriented programming, dynamic typing, and even the modern graphical user interface (GUI).**

<!--

This article explores:  

- The **history and motivation** behind Smalltalk.  
- How it influenced **modern programming languages**.  
- **10 real code examples** of Smalltalk in action.  
- A **table of Smalltalk syntax** compared to modern languages.  
-->

***

## The History of Smalltalk

Smalltalk was created in the **1970s** at **Xerox PARC (Palo Alto Research Center)**—the same legendary lab that gave us the **GUI, laser printers, and Ethernet**.

### **Why Was Smalltalk Created?**

* **Alan Kay and his team** wanted to build a **language designed for humans, not machines**.
* They needed a system where **everything was an object**, unlike procedural languages of the time.
* It became **the first true object-oriented language**, influencing **Java, Python, Ruby, and even JavaScript**.

💡 **Fun Fact:** Smalltalk was **a major influence on Apple’s macOS and iOS development**.

> **Further Reading:**
>
> * [Wikipedia: Smalltalk](https://en.wikipedia.org/wiki/Smalltalk)
> * [The History of Smalltalk](http://worrydream.com/EarlyHistoryOfSmalltalk/)

***

## Smalltalk’s Influence on Modern Languages

| Feature                     | Smalltalk                         | Modern Equivalent     |
| --------------------------- | --------------------------------- | --------------------- |
| **Everything is an Object** | ✅ Yes                             | ✅ Python, Ruby, Java  |
| **Dynamic Typing**          | ✅ Yes                             | ✅ Python, JavaScript  |
| **Message Passing**         | ✅ Yes (instead of function calls) | ✅ Java, C++ (methods) |
| **Interactive Development** | ✅ Yes (Live coding)               | ✅ Python REPL, Lisp   |
| **Garbage Collection**      | ✅ Yes (early adopter)             | ✅ Java, Python, C#    |

💡 **Verdict:** If you’ve used **Python, Ruby, or JavaScript**, you’ve used **concepts invented by Smalltalk**!

***

## Smalltalk Syntax Table

| Concept                 | Smalltalk Code                   | Equivalent in Python / Java                                              |                                                                 |
| ----------------------- | -------------------------------- | ------------------------------------------------------------------------ | --------------------------------------------------------------- |
| **Hello World**         | `'Hello, World!' print.`         | `print("Hello, World!")` / `System.out.println("Hello, World!");`        |                                                                 |
| **Variables**           | `x := 42.`                       | `x = 42` / `int x = 42;`                                                 |                                                                 |
| **Loops**               | \`1 to: 10 do: \[:i              | i print.]\`                                                              | `for i in range(1, 11): print(i)` / `for (int i=1; i<=10; i++)` |
| **Conditionals**        | `x > 5 ifTrue: [ 'High' print ]` | `if x > 5: print('High')` / `if (x > 5) { System.out.println('High'); }` |                                                                 |
| **Functions (Methods)** | `square: x ^ x * x.`             | `def square(x): return x * x` / `int square(int x) { return x * x; }`    |                                                                 |
| **Classes**             | `Object subclass: #Person.`      | `class Person:` / `class Person {}`                                      |                                                                 |
| **Message Passing**     | `5 squared`                      | `5**2` / `Math.pow(5, 2)`                                                |                                                                 |
| **Blocks (Closures)**   | \`\[:x                           | x + 10] value: 5.\`                                                      | `lambda x: x + 10`                                              |

***

## 10 Smalltalk Code Examples

### **1. Hello, World!**

```smalltalk
'Hello, World!' print.
```

### **2. Declaring Variables**

```smalltalk
x := 42.
```

### **3. If-Else Statement**

```smalltalk
x > 10 ifTrue: [ 'X is greater than 10' print ].
```

### **4. For Loop**

```smalltalk
1 to: 5 do: [:i | ('Iteration: ', i print) ].
```

### **5. Function Definition (Method)**

```smalltalk
square: x
    ^ x * x.
```

### **6. Arrays**

```smalltalk
myList := #(1 2 3 4 5).
```

### **7. Exception Handling**

```smalltalk
[ 10 / 0 ] on: ZeroDivide do: [ :ex | 'Division by zero error!' print ].
```

### **8. Creating a Class**

```smalltalk
Object subclass: #Person.
```

### **9. Using Blocks (Closures)**

```smalltalk
myBlock := [:x | x + 10].
myBlock value: 5.  "Returns 15"
```

### **10. Concurrency (Futures)**

```smalltalk
future := [ 100 factorial ] fork.
```

***

## Why is Smalltalk Still Important?

✅ **It pioneered modern OOP concepts**—without Smalltalk, **Java, Python, and Ruby wouldn’t exist**.\
✅ **It introduced live coding and interactive development environments (IDEs).**\
✅ **It remains one of the most elegant and readable programming languages ever designed.**

💡 **Even though Smalltalk isn’t mainstream, it lives on through its influence in modern programming languages.**

> **Want to Try Smalltalk?** Check out these online interpreters:
>
> * [Pharo Smalltalk](https://pharo.org/)
> * [Squeak Smalltalk](https://squeak.org/)

***

## Key Takeaways

* **Smalltalk was the first true object-oriented language**—influencing **Java, Python, Ruby, and JavaScript**.
* **It introduced dynamic typing, garbage collection, and live coding** long before they became standard.
* **Its syntax is simple, elegant, and highly readable.**
* **Even today, Smalltalk is used in research, education, and some enterprise applications.**

***

## References

1. [Smalltalk Wikipedia](https://en.wikipedia.org/wiki/Smalltalk)
2. [The History of Smalltalk](http://worrydream.com/EarlyHistoryOfSmalltalk/)
3. [Squeak Smalltalk](https://squeak.org/)
4. [Pharo Smalltalk](https://pharo.org/)
