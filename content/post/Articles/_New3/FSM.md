---
title: Finite State Machines-FSM Explained
description: Code Examples in Go, Python, C#, and C++
slug: finite-state-machines-go-python-c-and-cplusplus
date: 2022-10-30
image: post/Articles/IMAGES/fsm.png
categories:
  - Python
  - GoLang
  - CSharp
  - CPP
  - Design Patterns
  - Finite State Machine
tags:
  - Finite
  - State
  - Machine
  - Go
  - Python
  - C#
  - CPP
  - State
  - Transitions
  - CodeExamples
  - DesignPatterns
draft: false
weight: 152
categories_ref:
  - Python
  - GoLang
  - CSharp
  - CPP
  - Design Patterns
  - Finite State Machine
slug_calculated: https://brianbraatz.github.io/p/finite-state-machines-go-python-c-and-cplusplus
lastmod: 2025-03-14T16:40:26.897Z
---
# Finite State Machines, History, Understanding How They Work, and Code Examples in Go, Python, C#, and C++

<!-- 
## Introduction

Alright, folks, gather around! We're about to embark on a thrilling adventure into the wonderful world of **finite state machines (FSMs)**. 

Why should you care? Well, FSMs are used in everything from traffic lights to video game AI, and even vending machines (which, let’s be honest, are the true overlords of our snack-based economy). 

In this article, we'll cover:

- A brief history of FSMs (because knowing where things come from is cool)
- How FSMs work (because understanding things makes you look smart)
- Examples in **Go, Python, C#, and C++** (because code is life)

So grab a coffee, settle in, and let’s get started.
-->

***

## A Brief History of FSMs

Finite state machines have been around longer than your grandpa’s first calculator. The concept was formalized by **Warren McCulloch and Walter Pitts** in 1943 when they described neural networks using logic gates. Later, the idea of state machines was expanded by **John Myhill and Raymond Moore**, leading to what we now call **Mealy and Moore Machines**.

Smart people figured out that a system could have different states and transition between them based on inputs. And here we are, still using that idea to control robots, games, and—most importantly—vending machines.

***

## Understanding How FSMs Work

An FSM consists of:

1. **States**: The different conditions a system can be in.
2. **Transitions**: Rules for moving from one state to another.
3. **Inputs**: Events that trigger transitions.
4. **Outputs**: Actions performed when entering, exiting, or being in a state.

Imagine a **turnstile** at a subway station:

* **Locked state**: If you insert a coin, it transitions to **Unlocked**.
* **Unlocked state**: If you push, it transitions back to **Locked**.

Boom! That’s a finite state machine. Simple, right? Now let's look at some code!

***

## Code Examples

### Go Implementation

```go
package main

import (
    "fmt"
)

type State string

const (
    Locked   State = "Locked"
    Unlocked State = "Unlocked"
)

type Turnstile struct {
    state State
}

func (t *Turnstile) InsertCoin() {
    if t.state == Locked {
        fmt.Println("Coin inserted. Unlocking...")
        t.state = Unlocked
    } else {
        fmt.Println("Already unlocked! Just push!")
    }
}

func (t *Turnstile) Push() {
    if t.state == Unlocked {
        fmt.Println("Pushed. Locking...")
        t.state = Locked
    } else {
        fmt.Println("It's locked! Insert a coin first.")
    }
}

func main() {
    t := &Turnstile{state: Locked}
    t.InsertCoin()
    t.Push()
    t.Push()
}
```

***

### Python Implementation

```python
class Turnstile:
    def __init__(self):
        self.state = "Locked"

    def insert_coin(self):
        if self.state == "Locked":
            print("Coin inserted. Unlocking...")
            self.state = "Unlocked"
        else:
            print("Already unlocked! Just push!")

    def push(self):
        if self.state == "Unlocked":
            print("Pushed. Locking...")
            self.state = "Locked"
        else:
            print("It's locked! Insert a coin first.")

# Example usage
t = Turnstile()
t.insert_coin()
t.push()
t.push()
```

***

### C# Implementation

```csharp
using System;

class Turnstile
{
    private string state = "Locked";

    public void InsertCoin()
    {
        if (state == "Locked")
        {
            Console.WriteLine("Coin inserted. Unlocking...");
            state = "Unlocked";
        }
        else
        {
            Console.WriteLine("Already unlocked! Just push!");
        }
    }

    public void Push()
    {
        if (state == "Unlocked")
        {
            Console.WriteLine("Pushed. Locking...");
            state = "Locked";
        }
        else
        {
            Console.WriteLine("It's locked! Insert a coin first.");
        }
    }
}

class Program
{
    static void Main()
    {
        Turnstile t = new Turnstile();
        t.InsertCoin();
        t.Push();
        t.Push();
    }
}
```

***

### C++ Implementation

```cpp
#include <iostream>
#include <string>

class Turnstile {
private:
    std::string state = "Locked";

public:
    void insertCoin() {
        if (state == "Locked") {
            std::cout << "Coin inserted. Unlocking..." << std::endl;
            state = "Unlocked";
        } else {
            std::cout << "Already unlocked! Just push!" << std::endl;
        }
    }

    void push() {
        if (state == "Unlocked") {
            std::cout << "Pushed. Locking..." << std::endl;
            state = "Locked";
        } else {
            std::cout << "It's locked! Insert a coin first." << std::endl;
        }
    }
};

int main() {
    Turnstile t;
    t.insertCoin();
    t.push();
    t.push();
    return 0;
}
```

***

## Conclusion

Finite state machines are incredibly useful and appear in more places than you might think. Whether it’s controlling UI flows, video game characters, or vending machines (the true MVPs of life), FSMs keep things organized and predictable.

So go forth and implement FSMs in your projects! And if nothing else, you can now impress your friends by explaining why the turnstile at the subway works the way it does.

***

## Ideas for Further Exploration

* Implement FSMs for a simple game AI.
* Use FSMs in a web application to handle user states.
* Simulate a real-world vending machine with an FSM.
* Implement a state machine in Rust or Java.

***

## References

* [Finite-State Machine - Wikipedia](https://en.wikipedia.org/wiki/Finite-state_machine)
* [Moore and Mealy Machines](https://en.wikipedia.org/wiki/Mealy_machine)
* [State Pattern in Software Engineering](https://en.wikipedia.org/wiki/State_pattern)
