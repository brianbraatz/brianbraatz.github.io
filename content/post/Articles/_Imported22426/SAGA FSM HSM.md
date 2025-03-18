---
title: Finite State Machines, Hierarchical State Machines, and SAGA
description: Code examples in C# and Python.
slug: finite-state-machines-and-saga
date: 2018-09-12
image: post/Articles/IMAGES/32.jpg
categories:
  - Distributed Systems
  - Microservices
  - Architecture
  - State Machines
  - Cloud
  - SAGA Pattern
tags:
  - Saga
  - Microservices
  - Finite
  - State
  - Machine
  - Hierarchical
  - State
  - Machine
  - Distributed
  - Systems
  - C#
  - Python
  - State
  - Management
draft: false
weight: 17
categories_ref:
  - Distributed Systems
  - Microservices
  - Architecture
  - State Machines
  - Cloud
  - SAGA Pattern
slug_calculated: https://brianbraatz.github.io/p/finite-state-machines-and-saga
lastmod: 2025-03-14T16:40:26.134Z
---
Used with Saga, **Finite State Machines (FSMs)** and **Hierarchical State Machines (HSMs)**—two powerful tools that can help structure SAGA workflows. 🚀

## What is a Finite State Machine (FSM)? 🤖

A **Finite State Machine** (FSM) is a model where a system can only be in **one state at a time** and transitions between states based on predefined rules.

### Example: Order Processing FSM

An order system typically has these states:

* **Pending** → **Paid** → **Shipped** → **Delivered**
* If payment fails, transition to **Failed**
* If the item is out of stock, transition to **Cancelled**

Here's a basic **FSM implementation in C#** using `enum`:

```csharp
enum OrderState { Pending, Paid, Shipped, Delivered, Failed, Cancelled }

class Order
{
    public OrderState State { get; private set; } = OrderState.Pending;

    public void Pay() { if (State == OrderState.Pending) State = OrderState.Paid; }
    public void Ship() { if (State == OrderState.Paid) State = OrderState.Shipped; }
    public void Deliver() { if (State == OrderState.Shipped) State = OrderState.Delivered; }
    public void Fail() { State = OrderState.Failed; }
    public void Cancel() { State = OrderState.Cancelled; }
}
```

### Python Example

```python
from enum import Enum

class OrderState(Enum):
    PENDING = "Pending"
    PAID = "Paid"
    SHIPPED = "Shipped"
    DELIVERED = "Delivered"
    FAILED = "Failed"
    CANCELLED = "Cancelled"

class Order:
    def __init__(self):
        self.state = OrderState.PENDING

    def pay(self):
        if self.state == OrderState.PENDING:
            self.state = OrderState.PAID
    
    def ship(self):
        if self.state == OrderState.PAID:
            self.state = OrderState.SHIPPED

    def deliver(self):
        if self.state == OrderState.SHIPPED:
            self.state = OrderState.DELIVERED
    
    def fail(self):
        self.state = OrderState.FAILED
    
    def cancel(self):
        self.state = OrderState.CANCELLED
```

## How FSMs Relate to SAGA 🌀

SAGA consists of **multiple services interacting asynchronously**, but each step in the SAGA workflow is essentially a **state transition**.

* Each microservice acts as an FSM, progressing through **defined states**.
* If a step fails, the system **transitions** into a failure state and triggers compensating actions.
* Using FSMs in SAGA ensures we always **know where the process is** and what action to take next.

***

## What is a Hierarchical State Machine (HSM)? 🏛️

A **Hierarchical State Machine** (HSM) is an FSM with **nested states**.

### Example: Expanding Order Processing with Substates

* **Order → Processing → Payment → Paid**
* **Order → Processing → Payment → Failed → Retry**
* **Order → Fulfillment → Shipped → Delivered**

With HSMs, we **group related states** and handle transitions within them.

### C# Example: HSM for Order Processing

```csharp
class OrderProcess
{
    public enum State { OrderPlaced, PaymentProcessing, PaymentFailed, Shipped, Delivered }
    private State _state = State.OrderPlaced;

    public void ProcessPayment(bool success)
    {
        if (_state == State.OrderPlaced)
            _state = success ? State.Shipped : State.PaymentFailed;
    }
    
    public void RetryPayment()
    {
        if (_state == State.PaymentFailed)
            _state = State.PaymentProcessing;
    }
}
```

### Python Example: HSM for Order Processing

```python
class OrderProcess:
    class State:
        ORDER_PLACED = "OrderPlaced"
        PAYMENT_PROCESSING = "PaymentProcessing"
        PAYMENT_FAILED = "PaymentFailed"
        SHIPPED = "Shipped"
        DELIVERED = "Delivered"

    def __init__(self):
        self.state = self.State.ORDER_PLACED

    def process_payment(self, success):
        if self.state == self.State.ORDER_PLACED:
            self.state = self.State.SHIPPED if success else self.State.PAYMENT_FAILED

    def retry_payment(self):
        if self.state == self.State.PAYMENT_FAILED:
            self.state = self.State.PAYMENT_PROCESSING
```

***

## How HSMs Relate to SAGA 🏗️

HSMs fit perfectly into **orchestrated SAGA workflows** where:

* **Substates** define smaller steps within a SAGA process.
* **Retry mechanisms** for failed steps can be modeled as state transitions.
  * **Nested states** help manage complex workflows like **order processing, refunds, and stock availability.**

***

## Final Thoughts 💡

FSMs and HSMs **provide structure and clarity** to microservices. **SAGA is already a state-driven workflow**, and using FSMs/HSMs makes it **easier to manage state transitions.**

If your **microservices are struggling with state tracking**, consider using **FSMs** or **HSMs** to keep them predictable and resilient. 🚀

***

## 🔑 Key Ideas Table

| Concept                          | Explanation                                            |
| -------------------------------- | ------------------------------------------------------ |
| Finite State Machine (FSM)       | System transitions between a limited number of states. |
| Hierarchical State Machine (HSM) | FSM with nested states for better organization.        |
| FSM in SAGA                      | Each service acts as a state machine.                  |
| HSM in SAGA                      | Nested states help manage complex workflows.           |
| C# and Python Examples           | Demonstrate FSM and HSM implementations.               |

***

## 📚 References

1. [State Machines in Microservices](https://martinfowler.com/articles/microservices-state.html)
2. [Finite State Machines Explained](https://dev.to/fsm-intro)
3. [SAGA and State Machines](https://microservices.io/patterns/data/saga.html)

***
