---
title: "Generics vs Templates: Java vs C# vs C++"
description: "Generics vs Templates: Java vs C# vs C++"
slug: generics-vs-templates-java-vs-csharp-vs-cpp
date: 2019-07-15
image: post/Articles/IMAGES/jackrunning.png
categories:
  - CSharp
  - DotNet
  - Java
tags:
  - Java
  - CSharp
  - CPP
  - Generics
  - Templates
  - DesignPatterns
draft: false
weight: 26
categories_ref:
  - CSharp
  - DotNet
  - Java
lastmod: 2025-03-14T15:45:11.237Z
---
# Generics vs Templates: Java vs C# vs C++

<!-- 
## Introduction

Welcome to **"Generics vs Templates: The Battle of Type Safety!"** ⚔️

If you’ve ever wondered **why Java generics feel weird compared to C#**, or **why C++ templates seem magical yet terrifying**, this is the deep dive you never knew you needed. 😆

Let’s explore **how generics and templates work** in **Java, C#, and C++**, discuss **why Java did some weird stuff**, and take a look at **how these languages compile generics differently**.
-->

***

## Generics vs Templates: The Key Differences

### **How They Work at Compile-Time & Runtime**

One of the **biggest differences** between Java, C#, and C++ generics/templates is **how they compile**:

| Feature                       | C++ Templates                         | Java Generics                                | C# Generics                                     |
| ----------------------------- | ------------------------------------- | -------------------------------------------- | ----------------------------------------------- |
| **Compile-Time vs Runtime**   | **Compile-Time**                      | **Compile-Time (Erased at Runtime)**         | **Full Runtime Support**                        |
| **Output**                    | **Compiles directly to machine code** | **Compiles to Java Bytecode**                | **Compiles to .NET IL (Intermediate Language)** |
| **Performance**               | 🚀 Fast (No runtime overhead)         | 🐢 Slower (Type Erasure)                     | ⚡ Fast (Optimized in CLR)                       |
| **Reflection Support?**       | ❌ No                                  | ❌ No (Because erased)                        | ✅ Yes                                           |
| **Can Work with Primitives?** | ✅ Yes                                 | ❌ No (Must box to `Integer`, `Double`, etc.) | ✅ Yes                                           |
| **Metaprogramming?**          | ✅ Yes (Template Metaprogramming)      | ❌ No                                         | ❌ No                                            |

### **Java Generics: Backward Compatibility at the Cost of Performance**

Java generics were added in **Java 5 (2004)**, **but the JVM was designed in 1995**—before generics existed. To **maintain backward compatibility** with older JVMs, Java **removes generic type information at runtime** (aka **Type Erasure**).

This means **generic types don’t actually exist in the compiled Java bytecode**! So, while you write:

```java
List<String> list = new ArrayList<>();
```

At runtime, Java only sees:

```java
List list = new ArrayList();
```

Which means **type safety is only enforced at compile time**. Once compiled, it's **wild west time**.

***

### **C# Generics: Microsoft Didn’t Care About Old Code**

Unlike Java, **Microsoft designed .NET to support generics from the start** (2005), meaning **no type erasure**.

C# generics exist **at runtime**, are stored in **metadata**, and can be **reflected upon**:

```csharp
Console.WriteLine(typeof(List<string>)); // System.Collections.Generic.List`1[System.String]
```

This makes **C# generics more efficient** and **safer** than Java’s.

***

### **C++ Templates: The Ultimate Metaprogramming Beast**

C++ templates were introduced in **C++98**, and unlike Java/C#, they **don’t use a virtual machine**—they generate **actual assembly code** at compile-time.

This means:

1. **Each template instantiation generates new machine code**.
2. **There’s zero runtime overhead**.
3. **Templates can be used for metaprogramming** (yes, you can run code at compile time!).

For example, this C++ template:

```cpp
template <typename T>
T add(T a, T b) {
    return a + b;
}
```

Will **generate different assembly code** for `add<int>`, `add<double>`, etc.

***

## Generics in Action: Java vs C# vs C++

Let’s compare **how you’d write the same generic class** in **Java, C#, and C++**.

### **Java Generic Class (Type Erased)**

```java
class Box<T> {
    private T value;

    public Box(T value) {
        this.value = value;
    }

    public T getValue() {
        return value;
    }

    public static void main(String[] args) {
        Box<Integer> intBox = new Box<>(42);
        System.out.println("Java Box: " + intBox.getValue());
    }
}
```

### **C# Generic Class (Preserves Type at Runtime)**

```csharp
using System;

class Box<T> {
    private T value;

    public Box(T value) {
        this.value = value;
    }

    public T GetValue() {
        return value;
    }

    public static void Main() {
        Box<int> intBox = new Box<int>(42);
        Console.WriteLine("C# Box: " + intBox.GetValue());
    }
}
```

### **C++ Template Class (Compiles to Machine Code)**

```cpp
#include <iostream>

template <typename T>
class Box {
    private:
        T value;
    public:
        Box(T v) : value(v) {}
        T getValue() { return value; }
};

int main() {
    Box<int> intBox(42);
    std::cout << "C++ Box: " << intBox.getValue() << std::endl;
    return 0;
}
```

***

## **Assembly vs Bytecode vs IL: What Actually Gets Compiled?**

| Language | What it compiles to                                 |
| -------- | --------------------------------------------------- |
| **C++**  | **Machine Code (Assembly)**                         |
| **Java** | **Java Bytecode (Type Erased Generics)**            |
| **C#**   | **.NET Intermediate Language (Preserves Generics)** |

This means:

* **C++ is fastest**, but **generates tons of machine code**.
* **C# is efficient**, because **the CLR optimizes generics**.
* **Java is slower**, because **boxing happens for primitive types**, and generics **don’t exist at runtime**.

***

## **Final Thoughts: Which One is Best?**

| Language | Pros                                             | Cons                                                   |
| -------- | ------------------------------------------------ | ------------------------------------------------------ |
| **C++**  | Super fast, no runtime overhead, metaprogramming | Compiles to separate copies for each type (code bloat) |
| **Java** | Backward compatible, works everywhere            | Type erasure, can be slow due to boxing                |
| **C#**   | Preserves types, better performance than Java    | Needs .NET ecosystem                                   |

### **Concepts**

* **If you care about performance** → **C++**
* **If you need Java compatibility** → **Java**
* **If you want a good balance** → **C#**

***

## **Key Ideas Table**

| Concept                | Explanation                                                      |
| ---------------------- | ---------------------------------------------------------------- |
| Type Erasure           | Java removes generic types at runtime for backward compatibility |
| Runtime Generics       | C# stores generic type info, making reflection possible          |
| Compile-Time Templates | C++ generates machine code for each template instantiation       |
| Performance Impact     | C++ → Fastest, C# → Optimized, Java → Slower due to type erasure |

***

## **References**

* https://en.wikipedia.org/wiki/Generics\_in\_Java
* https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/generics/
* https://en.cppreference.com/w/cpp/language/templates
