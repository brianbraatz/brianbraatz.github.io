---
title: Hierarchical State Machines-HSM, Explained
description: Code Examples in Go, Python, C#, and C++
slug: hsm-explained
date: 2019-05-20
image: post/Articles/IMAGES/hsm.png
categories:
  - Design Patterns
  - Hierarchical State Machine
  - GoLang
  - CSharp
  - CPP
  - Python
tags:
  - Hierarchical
  - State
  - Machine
  - Finite
  - State
  - Machine
  - History
  - Go
  - Python
  - C#
  - C++
  - Programming
  - State
  - Transitions
  - Code
  - Examples
draft: false
weight: 180
lastmod: 2025-03-03T17:22:47.775Z
---
<!-- 
# Hierarchical State Machines, History, Understanding How They Work, Comparison to Finite State Machines with Code Examples in Go, Python, C#, and C++
-->

<!-- 
## Introduction

Alright, buckle up! We're diving into **hierarchical state machines (HSMs)**—the fancy, structured cousin of the classic **finite state machine (FSM)**. If FSMs are like a simple to-do list, then HSMs are like organizing your tasks into neat folders. 

By the end of this, you'll know:

- The **history** of HSMs (so you can sound smart at parties)
- **How they work** (because understanding things is cool)
- **How they compare to FSMs** (spoiler: they add some structure)
- **Code examples in Go, Python, C#, and C++** (because coding is life)

Let’s go!

---
-->

## A Brief History of Hierarchical State Machines

Finite State Machines (FSMs) are great and all, but they can become a **spaghetti mess** when dealing with complex systems. This led to the **evolution** of FSMs into **hierarchical state machines** (HSMs), which were formalized in the 1980s by **David Harel**.

His work introduced **Statecharts**, which allowed states to have **substates**—kind of like folders within folders. This **reduced duplication** and made systems **more manageable**.

***

## How Hierarchical State Machines Work

An **HSM is an FSM but with superpowers**. Instead of having a **flat** list of states, HSMs **group states into parent-child relationships**.

Example:

* **Car State Machine**
  * **Moving State**
    * Accelerating
    * Cruising
    * Braking
  * **Stopped State**
    * Engine Off
    * Engine On

If you’re in "Braking," you’re also in "Moving." If you transition to "Engine Off," you also transition out of "Moving."

This **reduces redundancy** since common behaviors can be inherited.

***

## FSM vs. HSM: The Showdown

| Feature          | Finite State Machine (FSM) | Hierarchical State Machine (HSM) |
| ---------------- | -------------------------- | -------------------------------- |
| Structure        | Flat                       | Nested                           |
| Complexity       | Grows exponentially        | More manageable                  |
| Code Duplication | High                       | Low                              |
| Readability      | Messy for large systems    | Cleaner                          |
| Use Case         | Simple workflows           | Complex systems like robotics    |

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
    StoppedEngineOff State = "Stopped.EngineOff"
    StoppedEngineOn  State = "Stopped.EngineOn"
    MovingCruising   State = "Moving.Cruising"
)

type CarStateMachine struct {
    state State
}

func (c *CarStateMachine) TurnOnEngine() {
    if c.state == StoppedEngineOff {
        fmt.Println("Engine started!")
        c.state = StoppedEngineOn
    } else {
        fmt.Println("Engine is already on!")
    }
}

func (c *CarStateMachine) StartMoving() {
    if c.state == StoppedEngineOn {
        fmt.Println("Car is now moving!")
        c.state = MovingCruising
    } else {
        fmt.Println("Can't move unless the engine is on!")
    }
}

func (c *CarStateMachine) Brake() {
    if c.state == MovingCruising {
        fmt.Println("Braking...")
        c.state = StoppedEngineOn
    } else {
        fmt.Println("You're already stopped!")
    }
}

func main() {
    car := &CarStateMachine{state: StoppedEngineOff}
    car.TurnOnEngine()
    car.StartMoving()
    car.Brake()
}
```

***

### Python Implementation

```python
class CarStateMachine:
    def __init__(self):
        self.state = "Stopped.EngineOff"

    def turn_on_engine(self):
        if self.state == "Stopped.EngineOff":
            print("Engine started!")
            self.state = "Stopped.EngineOn"
        else:
            print("Engine is already on!")

    def start_moving(self):
        if self.state == "Stopped.EngineOn":
            print("Car is now moving!")
            self.state = "Moving.Cruising"
        else:
            print("Can't move unless the engine is on!")

    def brake(self):
        if self.state == "Moving.Cruising":
            print("Braking...")
            self.state = "Stopped.EngineOn"
        else:
            print("You're already stopped!")

# Example usage
car = CarStateMachine()
car.turn_on_engine()
car.start_moving()
car.brake()
```

***

### C# Implementation

```csharp
using System;

class CarStateMachine
{
    private string state = "Stopped.EngineOff";

    public void TurnOnEngine()
    {
        if (state == "Stopped.EngineOff")
        {
            Console.WriteLine("Engine started!");
            state = "Stopped.EngineOn";
        }
        else
        {
            Console.WriteLine("Engine is already on!");
        }
    }

    public void StartMoving()
    {
        if (state == "Stopped.EngineOn")
        {
            Console.WriteLine("Car is now moving!");
            state = "Moving.Cruising";
        }
        else
        {
            Console.WriteLine("Can't move unless the engine is on!");
        }
    }

    public void Brake()
    {
        if (state == "Moving.Cruising")
        {
            Console.WriteLine("Braking...");
            state = "Stopped.EngineOn";
        }
        else
        {
            Console.WriteLine("You're already stopped!");
        }
    }
}

class Program
{
    static void Main()
    {
        CarStateMachine car = new CarStateMachine();
        car.TurnOnEngine();
        car.StartMoving();
        car.Brake();
    }
}
```

***

### C++ Implementation

```cpp
#include <iostream>
#include <string>

class CarStateMachine {
private:
    std::string state = "Stopped.EngineOff";

public:
    void turnOnEngine() {
        if (state == "Stopped.EngineOff") {
            std::cout << "Engine started!" << std::endl;
            state = "Stopped.EngineOn";
        } else {
            std::cout << "Engine is already on!" << std::endl;
        }
    }

    void startMoving() {
        if (state == "Stopped.EngineOn") {
            std::cout << "Car is now moving!" << std::endl;
            state = "Moving.Cruising";
        } else {
            std::cout << "Can't move unless the engine is on!" << std::endl;
        }
    }

    void brake() {
        if (state == "Moving.Cruising") {
            std::cout << "Braking..." << std::endl;
            state = "Stopped.EngineOn";
        } else {
            std::cout << "You're already stopped!" << std::endl;
        }
    }
};

int main() {
    CarStateMachine car;
    car.turnOnEngine();
    car.startMoving();
    car.brake();
    return 0;
}
```

***

## Conclusion

Hierarchical State Machines are **better-organized** versions of FSMs. They help prevent **duplicate logic**, make **complex systems manageable**, and are used **in robotics, games, and UI frameworks**.

If FSMs were **basic notes**, HSMs would be **well-organized notebooks**.

Now, go forth and use HSMs in your projects!

***

## Ideas for Further Exploration

* Implement HSMs for **robot navigation**.
* Use HSMs in **video game AI**.
* Apply HSMs in **UI workflows**.
* Implement a **hierarchical vending machine**.

***

## References

* [Hierarchical State Machines - Wikipedia](https://en.wikipedia.org/wiki/UML_state_machine#Hierarchical_state_machines)
* [Statecharts by David Harel](https://www.sciencedirect.com/science/article/abs/pii/0167642387900359)
* [Finite vs Hierarchical State Machines](https://en.wikipedia.org/wiki/Finite-state_machine)
