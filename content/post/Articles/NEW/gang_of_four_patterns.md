---
title: The Gang of Four Patterns Explained
description: The Gang of Four Patterns Explained
slug: the-gang-of-four-patterns-explained
date: 2023-12-15
image: post/Articles/IMAGES/gofwide.png
categories: 
tags:
  - Design
  - Patterns
  - Gang
  - Of
  - Four
  - Software
  - Engineering
  - Java
  - Object-Oriented
  - Programming
draft: false
weight: 243
lastmod: 2025-02-03T13:36:21.230Z
---
# The Gang of Four Patterns Explained

You ever feel like your code is a tangled mess of spaghetti, and not the good kind with marinara sauce?

Well, The **Gang of Four** (GoF) is here to save the day with their legendary **design patterns**.

These four brilliant minds tackled the chaos of object-oriented design and compiled a set of reusable, battle-tested patterns that developers still use today.

## Who Are the Gang of Four?

No, it's not a rock band (though that would be cool). The Gang of Four consists of **Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides**—four computer scientists who, back in 1994, wrote *Design Patterns: Elements of Reusable Object-Oriented Software*.

Basically, they looked at common software problems, went all Sherlock Holmes on them, and came up with solutions that developers could use over and over again.

Their motivation? Simple: **software was turning into a dumpster fire**. Large-scale applications were becoming increasingly complex, and developers kept reinventing the wheel (badly).

Instead of letting everyone struggle, they identified best practices and documented them into patterns that could be applied universally.

## The Gang of Four Design Patterns

The GoF patterns are categorized into **three groups**:

1. **Creational Patterns** – Focus on object creation mechanisms.
2. **Structural Patterns** – Deal with object composition and structure.
3. **Behavioral Patterns** – Address communication between objects.

Let's break them down, with **Java examples** for each.

### 1. Creational Patterns

These patterns help you create objects in a way that is **flexible and reusable**.

#### **Singleton Pattern**

Ensures that a class has only **one instance** and provides a global access point to it. Useful for things like configuration managers, logging, and database connections.

```java
public class Singleton {
    private static Singleton instance;
    
    private Singleton() {}  // Private constructor

    public static Singleton getInstance() {
        if (instance == null) {
            instance = new Singleton();
        }
        return instance;
    }
}
```

Reference: <https://en.wikipedia.org/wiki/Singleton_pattern>

### 2. Structural Patterns

These patterns define how objects and classes **interact and form larger structures**.

#### **Adapter Pattern**

Allows incompatible interfaces to work together. It’s like an electrical adapter—you plug in a European device into a US socket, and it just works.

```java
interface Target {
    void request();
}

class Adaptee {
    void specificRequest() {
        System.out.println("Specific request executed");
    }
}

class Adapter implements Target {
    private Adaptee adaptee = new Adaptee();

    public void request() {
        adaptee.specificRequest();
    }
}
```

Reference: <https://en.wikipedia.org/wiki/Adapter_pattern>

### 3. Behavioral Patterns

These patterns are all about how **objects communicate** with each other.

#### **Observer Pattern**

Allows multiple objects (observers) to **react to changes** in another object (subject). Think of it like YouTube subscriptions—when a new video is uploaded, subscribers are notified.

```java
import java.util.ArrayList;
import java.util.List;

interface Observer {
    void update(String message);
}

class Subject {
    private List<Observer> observers = new ArrayList<>();

    void addObserver(Observer observer) {
        observers.add(observer);
    }

    void notifyObservers(String message) {
        for (Observer observer : observers) {
            observer.update(message);
        }
    }
}

class ConcreteObserver implements Observer {
    private String name;

    public ConcreteObserver(String name) {
        this.name = name;
    }

    public void update(String message) {
        System.out.println(name + " received update: " + message);
    }
}
```

Reference: <https://en.wikipedia.org/wiki/Observer_pattern>

## Comparison Table of GoF Patterns

| Pattern   | Type       | Purpose                                              |
| --------- | ---------- | ---------------------------------------------------- |
| Singleton | Creational | Ensures a single instance of a class                 |
| Factory   | Creational | Creates objects without specifying exact class       |
| Adapter   | Structural | Bridges incompatible interfaces                      |
| Proxy     | Structural | Controls access to another object                    |
| Observer  | Behavioral | Allows objects to react to changes in another object |

## Summary of Key Ideas

* The Gang of Four wrote the **design patterns bible**.
* Their patterns solve **common software design problems**.
* Patterns are categorized into **Creational, Structural, and Behavioral**.
* Java examples make them **easy to understand and use**.

## References

* <https://en.wikipedia.org/wiki/Design_Patterns>
* [https://en.wikipedia.org/wiki/Gang\_of\_Four\_(design\_patterns)](https://en.wikipedia.org/wiki/Gang_of_Four_%28design_patterns%29)
* <https://en.wikipedia.org/wiki/Singleton_pattern>
* <https://en.wikipedia.org/wiki/Adapter_pattern>
* <https://en.wikipedia.org/wiki/Observer_pattern>
