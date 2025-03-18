---
title: "Python Monkey Patching: Hacking Your Code Like a Pro"
description: 
slug: python-monkey-patching-hacking-your-code-like-a-pro
date: 2021-02-09
image: post/Articles/IMAGES/mokeytooltrimmed.png
categories:
  - Python
  - Python-Monkey Patching
  - Aspect Orientated Programming-AOP
  - Algorithms
  - Testing
  - Unit Testing
tags:
  - Python
  - Monkey
  - Patching
  - Hacking
  - Dynamic
  - Modification
  - Code
  - Injection
  - Metaprogramming
draft: false
weight: 104
categories_ref:
  - Python
  - Python-Monkey Patching
  - Aspect Orientated Programming-AOP
  - Algorithms
  - Testing
  - Unit Testing
slug_calculated: https://brianbraatz.github.io/p/python-monkey-patching-hacking-your-code-like-a-pro
lastmod: 2025-03-14T16:40:22.384Z
---
<!-- 
# Python Monkey Patching: Hacking Your Code Like a Pro
-->

## Introduction

Have you ever wanted to modify a function or class **at runtime** without touching the original source code?

This dark art is called **monkey patching**, and it’s one of Python’s most notorious (and useful) features.

But wait—**why is it called monkey patching?**\
Legend has it that it comes from the idea of "monkeying around" with code.

And like an actual monkey, it can be cute and entertaining, but it can also go absolutely bananas and wreck your entire program. 🐒

In this article, we’ll explore what monkey patching is, why it exists, its common uses, and whether other languages (like Go, C#, and C++) can pull off similar tricks.

***

## What is Monkey Patching?

Monkey patching is the practice of **dynamically modifying or extending** modules, classes, or functions **at runtime**.

Essentially, you can override or add new behavior to existing code without modifying its original source.

Here’s an example:

```python
class Dog:
    def bark(self):
        return "Woof!"

dog = Dog()
print(dog.bark())  # Woof!

# Monkey patching the bark method
def new_bark(self):
    return "Meow! (I'm confused)"

Dog.bark = new_bark  # Replacing the method dynamically

print(dog.bark())  # Meow! (I'm confused)
```

This works because Python allows you to modify class definitions **on the fly**. No recompilation, no waiting—just instant chaos!

***

## History and Motivation Behind Monkey Patching

Monkey patching emerged as a practical solution when modifying third-party libraries **wasn't an option**.

Imagine you’re using a library, but you need to tweak one small function—should you rewrite the whole thing?

Nope! Just patch it like a sneaky ninja.

### Why is it important?

* **Quick Fixes:** Apply urgent bug fixes without waiting for official updates.
* **Customization:** Modify third-party libraries to fit your specific needs.
* **Testing & Mocking:** Override behaviors to test edge cases or simulate external dependencies.
* **Fun & Mischief:** Sometimes, it's just fun to rewrite reality. 😈

However, with great power comes great responsibility. Monkey patching can make debugging **painful** and upgrades **risky** since patched code might break when the library updates.

***

## Common Uses of Monkey Patching

### 1. **Fixing Bugs in Libraries**

Sometimes, you find a bug in a library, but waiting for a fix isn't an option.

```python
import requests

def fixed_get(*args, **kwargs):
    print("Custom GET method")
    return "Patched Response"

requests.get = fixed_get  # Patching the get method

print(requests.get("http://example.com"))  # Custom GET method, Patched Response
```

### 2. **Modifying Behavior Dynamically**

```python
import time

def fast_sleep(seconds):
    print(f"Skipping {seconds} seconds of sleep")

time.sleep = fast_sleep  # Who needs sleep anyway?

time.sleep(5)  # Skipping 5 seconds of sleep
```

### 3. **Mocking for Testing**

```python
class API:
    def fetch_data(self):
        return "Real API Response"

def mock_fetch_data(self):
    return "Mocked Response"

API.fetch_data = mock_fetch_data

api = API()
print(api.fetch_data())  # Mocked Response
```

***

## Can You Do Monkey Patching in Other Languages?

| Language   | Can It Monkey Patch? | How?                                                                    |
| ---------- | -------------------- | ----------------------------------------------------------------------- |
| **Python** | ✅ Yes                | Directly modifying classes, modules, and functions at runtime.          |
| **Go**     | ❌ No                 | Go is strongly typed and compiled, preventing runtime modification.     |
| **C#**     | ⚠️ Kind of           | Reflection and IL code modification can achieve similar results.        |
| **C++**    | ⚠️ Sort of           | Function pointers, macros, and vtable modifications allow some hacking. |

### **C# Example (Using Reflection to Modify Methods)**

```csharp
using System;
using System.Reflection;

public class Greeter {
    public string SayHello() => "Hello!";
}

class Program {
    static void Main() {
        Greeter greeter = new Greeter();
        MethodInfo method = typeof(Greeter).GetMethod("SayHello");
        Console.WriteLine(method.Invoke(greeter, null)); // Hello!
    }
}
```

### **C++ Example (Overriding Methods via Function Pointers)**

```cpp
#include <iostream>

typedef void (*FunctionType)();
void original() { std::cout << "Original Function" << std::endl; }
void patched() { std::cout << "Patched Function" << std::endl; }

int main() {
    FunctionType func = original;
    func();  // Original Function
    func = patched;
    func();  // Patched Function
    return 0;
}
```

***

## Key Ideas

* **Monkey Patching** lets you modify code at runtime.
* **It’s powerful but dangerous**—use responsibly.
* **Common uses**: bug fixes, customization, testing, and pranks.
* **Not all languages allow monkey patching**—compiled languages make it harder.

***

## References

* https://docs.python.org/3/tutorial/classes.html
* https://en.wikipedia.org/wiki/Monkey\_patch
* https://realpython.com/python-mock-testing/
