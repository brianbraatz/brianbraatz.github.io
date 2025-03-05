---
title: SOLID in a Nutshell
description: Code examples in C++, C# ansd Python
slug: solid-in-a-nutshell-cpp-csharp-python
date: 2013-09-14
image: post/Articles/IMAGES/metalcloseup.png
categories:
  - Programming
  - Software Design
  - SOLID
  - Clean Code
  - CPP
  - Design Patterns
  - CSHarp
  - Python
tags:
  - Programming
  - Software
  - design
  - Solid
  - Clean
  - code
  - C++
  - C#
  - Python
draft: false
weight: 203
lastmod: 2025-03-05T22:16:34.121Z
---
# SOLID in a Nutshell: A Fun Dive into Clean Code

If you've ever looked at your own code a month after writing it and thought, *Who wrote this mess?*, then buddy, you might need some SOLID principles in your life.

<!-- SOLID is like the superhero squad of software design. It’s not just one thing—it’s five! These principles help you write maintainable, flexible, and non-hair-pulling code. -->

## The SOLID Principles

Each letter in SOLID stands for a design principle.

<!-- Let’s break it down without making your head explode. -->

### 1. **S**ingle Responsibility Principle (SRP)

*A class should have only one reason to change.*

Think of it this way: You wouldn’t want your toaster to also wash your dishes. (Although that would be amazing.)

#### **Bad Example (C++):**

```cpp
class Report {
public:
    void generateReport() {
        // Generates the report
    }
    void saveToFile() {
        // Saves the report to a file
    }
};
```

Why is this bad? Because generating and saving are two different responsibilities.

#### **Good Example (C++):**

```cpp
class ReportGenerator {
public:
    void generateReport() {
        // Generates the report
    }
};

class ReportSaver {
public:
    void saveToFile(ReportGenerator& report) {
        // Saves report
    }
};
```

Boom! Separation of concerns.

***

### 2. **O**pen/Closed Principle (OCP)

*A class should be open for extension, but closed for modification.*

Imagine if every time you wanted to add a new feature to your game, you had to rewrite the whole game. That would be awful.

#### **Bad Example (C#):**

```csharp
class DiscountService {
    public double ApplyDiscount(double price, string discountType) {
        if (discountType == "Christmas") return price * 0.9;
        if (discountType == "BlackFriday") return price * 0.8;
        return price;
    }
}
```

Every time we need a new discount, we have to change the method. Bad!

#### **Good Example (C#):**

```csharp
interface IDiscount {
    double Apply(double price);
}

class ChristmasDiscount : IDiscount {
    public double Apply(double price) => price * 0.9;
}

class BlackFridayDiscount : IDiscount {
    public double Apply(double price) => price * 0.8;
}

class DiscountService {
    public double ApplyDiscount(double price, IDiscount discount) => discount.Apply(price);
}
```

Now we can add new discounts without modifying existing code. Win!

***

### 3. **L**iskov Substitution Principle (LSP)

*If a class is a subclass of another, it should be able to replace it without causing issues.*

Basically, if it looks like a duck, quacks like a duck, and doesn’t break the program when used as a duck, it’s good.

#### **Bad Example (Python):**

```python
class Bird:
    def fly(self):
        print("Flap flap")

class Ostrich(Bird):
    def fly(self):
        raise Exception("Ostriches can't fly!")
```

An `Ostrich` should be a `Bird`, but it breaks the expectations. Not good!

#### **Good Example (Python):**

```python
class Bird:
    pass

class FlyingBird(Bird):
    def fly(self):
        print("Flap flap")

class Ostrich(Bird):
    def run(self):
        print("Running fast!")
```

Now we separate flying birds and non-flying birds. Problem solved!

***

### 4. **I**nterface Segregation Principle (ISP)

*Don’t force classes to implement methods they don’t need.*

If your fridge also required a `MakeCoffee()` method because of a general `Appliance` interface, you’d be in trouble.

#### **Bad Example (C++):**

```cpp
class Worker {
public:
    virtual void code() = 0;
    virtual void manage() = 0;
};
```

A `Manager` shouldn’t have to implement `code()`. Not everyone codes!

#### **Good Example (C++):**

```cpp
class Coder {
public:
    virtual void code() = 0;
};

class Manager {
public:
    virtual void manage() = 0;
};
```

Now we have separate concerns.

***

### 5. **D**ependency Inversion Principle (DIP)

*High-level modules should not depend on low-level modules. Both should depend on abstractions.*

#### **Bad Example (C#):**

```csharp
class SQLDatabase {
    public void Save(string data) { /* Save to database */ }
}

class DataManager {
    private SQLDatabase db = new SQLDatabase();
    public void SaveData(string data) {
        db.Save(data);
    }
}
```

If we change databases, we need to rewrite `DataManager`. Yikes!

#### **Good Example (C#):**

```csharp
interface IDatabase {
    void Save(string data);
}

class SQLDatabase : IDatabase {
    public void Save(string data) { /* Save to database */ }
}

class DataManager {
    private IDatabase db;
    public DataManager(IDatabase database) {
        db = database;
    }
    public void SaveData(string data) {
        db.Save(data);
    }
}
```

Now we can switch databases easily.

***

## Comparison Table

| Principle | C++ | C# | Python |
| --------- | --- | -- | ------ |
| SRP       | ✅   | ✅  | ✅      |
| OCP       | ✅   | ✅  | ✅      |
| LSP       | ✅   | ✅  | ✅      |
| ISP       | ✅   | ✅  | ✅      |
| DIP       | ✅   | ✅  | ✅      |

***

## Key Ideas

| Concept | Summary                                              |
| ------- | ---------------------------------------------------- |
| SOLID   | Five principles for maintainable code                |
| SRP     | One class, one job                                   |
| OCP     | Extend, don’t modify                                 |
| LSP     | Subclasses should behave like their parent class     |
| ISP     | Interfaces should be small and specific              |
| DIP     | Depend on abstractions, not concrete implementations |

***

## References

* [SOLID Principles on Wikipedia](https://en.wikipedia.org/wiki/SOLID)
* [Clean Code by Robert C. Martin](https://www.oreilly.com/library/view/clean-code/9780136083238/)
* [SOLID Principles Explained](https://stackify.com/solid-design-principles/)
