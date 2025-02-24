---
title: Understanding the SAGA architecture pattern
description: Understanding the SAGA architecture pattern
slug: understanding-saga
date: 2017-08-14
image: post/Articles/IMAGES/38.jpg
categories:
  - Distributed Systems
  - Microservices
  - Architecture
  - Cloud
  - SAGA Pattern
tags:
  - Saga
  - Microservices
  - Architecture
  - Distributed Systems
  - Event-driven
draft: false
weight: 573
lastmod: 2025-02-24T15:18:36.337Z
---
## What the Heck is SAGA? 🤔

SAGA is a **design pattern** used to manage distributed transactions in microservices. When you’ve got multiple services talking to each other, ensuring data consistency can be a nightmare. SAGA swoops in like a superhero to coordinate things smoothly.

Also: "saga" does not actually stand for anything as it's not an acronym; instead, the term is borrowed from storytelling, signifying a sequence of events that form a complete narrative,

### So What’s the Problem we need to solve?

In a **monolithic** system, transactions are simple. You wrap everything in a database transaction (`BEGIN TRANSACTION … COMMIT`), and if anything fails, you roll it back (`ROLLBACK`).

But in **microservices**, each service has its own database. So, if Service A calls Service B, which calls Service C, and something breaks halfway through, you can’t just roll back everything.

Enter **SAGA**. Instead of a single transaction, it breaks things into **a series of smaller transactions**, ensuring consistency through compensating actions (i.e., undo operations).

## How SAGA Works 🛠️

SAGA ensures that if something fails mid-way, the system doesn’t end up in a half-baked state. There are two main ways to implement it:

### 1. Choreography 🕺

Each service is like a well-trained dancer—it knows its moves and what to do next. There’s no central brain; each service reacts to events and triggers the next step.

* **Example**: You book a flight, which triggers a hotel booking, which triggers a car rental.
* If the hotel booking fails, an event is emitted to cancel the flight.

#### Pros:

✅ Simple to implement for small workflows\
✅ No central coordinator required

#### Cons:

❌ Can become messy with too many services\
❌ Harder to debug and track events

### 2. Orchestration 🎻

Here, a **central orchestrator** (think of a conductor in an orchestra) manages the entire flow. It calls each service in sequence and handles rollbacks if anything goes wrong.

* **Example**: A central **Order Service** coordinates payments, shipping, and inventory.
* If shipping fails, the orchestrator triggers a refund.

#### Pros:

✅ More control and visibility\
✅ Easier debugging

#### Cons:

❌ Adds complexity (you need an orchestrator service)\
❌ Can become a single point of failure (unless made resilient)

## Real-World Example: Ordering a Pizza 🍕

Let’s say you order a pizza. Here’s how SAGA keeps things sane:

1. **Order Service** → Receives order and initiates payment.
2. **Payment Service** → Charges your card.
3. **Kitchen Service** → Starts making the pizza.
4. **Delivery Service** → Assigns a delivery guy.
5. **Success!** 🎉 Your pizza arrives, and life is good.

**But what if something goes wrong?**

* If Payment fails → Cancel the order.
* If the Kitchen burns your pizza → Refund the payment.
* If no delivery driver is available → Offer pickup instead.

## Choreography Example: Order Processing (C#) 🚀

Here’s a **choreographed** SAGA for an order processing system in C#:

```csharp
using System;
using System.Threading.Tasks;

public class OrderService
{
    public async Task PlaceOrder()
    {
        Console.WriteLine("Order placed.");
        await PaymentService.ProcessPayment();
    }
}

public static class PaymentService
{
    public static async Task ProcessPayment()
    {
        Console.WriteLine("Payment processed.");
        await ShippingService.ShipOrder();
    }
}

public static class ShippingService
{
    public static async Task ShipOrder()
    {
        Console.WriteLine("Order shipped.");
    }
}

// Simulating the flow
await new OrderService().PlaceOrder();
```

### Breakdown:

✅ Each service does its part and calls the next step.\
✅ No central coordinator.\
❌ If something fails, rolling back is tricky! 😬

***

## Orchestration Example: Order Processing (Python) 🐍

Now, let’s do the same thing using a central **orchestrator** in Python:

```python
from fastapi import FastAPI

app = FastAPI()

class OrderSaga:
    def __init__(self):
        self.state = "START"

    async def place_order(self):
        print("Order placed.")
        self.state = "PAYMENT"
        return await self.process_payment()

    async def process_payment(self):
        print("Payment processed.")
        self.state = "SHIPPING"
        return await self.ship_order()

    async def ship_order(self):
        print("Order shipped.")
        self.state = "COMPLETED"
        return {"status": "success"}

order_saga = OrderSaga()

@app.post("/order")
async def order():
    return await order_saga.place_order()
```

### Breakdown:

✅ **Orchestrator (OrderSaga)** manages the flow.\
✅ Easier rollback handling.\
❌ Slightly more complex to set up.

***

## Which One Should You Use? 🤷

| Pattern       | Pros                              | Cons                                    |
| ------------- | --------------------------------- | --------------------------------------- |
| Choreography  | Simple, no central service needed | Hard to debug, complex failure handling |
| Orchestration | More control, easier rollback     | Adds complexity, requires extra service |
