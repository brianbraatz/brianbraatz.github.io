---
title: Multi-Threaded Lock-Free Programming in C#
description: A dive into this high performance techniuqe in C#
slug: lock-free-programming-csharp
date: 2024-12-15
image: post/Articles/IMAGES/csharpblue.png
categories:
  - CSharp
  - DotNet
  - Concurrency
tags:
  - Lock-Free
  - Programming
  - C#
  - Concurrency
  - Multithreading
  - Agile
  - SAFe
draft: false
weight: 30
lastmod: 2025-02-17T01:40:46.472Z
---
## What Is Lock-Free Programming?

Imagine you're at a party, and everyone wants to use the bathroom.

Traditional locking is like handing out keys—only one person can go at a time.

But lock-free programming?

It's like installing a bunch of bathrooms so everyone can go simultaneously without waiting.

Cool, right?

In technical terms, lock-free programming allows multiple threads to access shared data structures without traditional locks, ensuring that at least one thread makes progress at any given time.

This approach is crucial for high-performance applications where waiting is a no-go.

## Why Should You Care?

* **Performance Boost**: By avoiding the overhead of locks, your application can handle more operations in less time.
* **Scalability**: As your app grows and more threads join the party, lock-free structures help maintain smooth operations.
* **Reduced Deadlocks**: Without locks, the dreaded deadlock monster stays away.

## How to Do It in C# ?

## 1. Use Concurrent Collections

### Example: Using `ConcurrentDictionary`

Instead of using a regular `Dictionary` with locking, use `ConcurrentDictionary`:

```csharp
using System;
using System.Collections.Concurrent;

class Program
{
    static void Main()
    {
        var concurrentDict = new ConcurrentDictionary<int, string>();

        // Adding key-value pairs safely
        concurrentDict.TryAdd(1, "Value1");
        concurrentDict.TryAdd(2, "Value2");

        // Updating a value atomically
        concurrentDict.AddOrUpdate(1, oldValue => "UpdatedValue", (key, oldValue) => "UpdatedValue");

        // Retrieving a value safely
        if (concurrentDict.TryGetValue(1, out string value))
        {
            Console.WriteLine($"Key 1 has value: {value}");
        }
    }
}
```

### Example: Using `ConcurrentQueue`

`ConcurrentQueue<T>` is a thread-safe queue that allows multiple threads to enqueue and dequeue items safely.

```csharp
using System;
using System.Collections.Concurrent;
using System.Threading.Tasks;

class Program
{
    static void Main()
    {
        var queue = new ConcurrentQueue<int>();

        // Simulate multiple threads enqueuing items
        Parallel.For(0, 10, i => queue.Enqueue(i));

        // Dequeue items safely
        while (queue.TryDequeue(out int item))
        {
            Console.WriteLine($"Dequeued: {item}");
        }
    }
}
```

### Example: Using `ConcurrentBag`

A `ConcurrentBag<T>` is useful for scenarios where multiple threads are producing and consuming items in a non-deterministic order.

```csharp
using System;
using System.Collections.Concurrent;
using System.Threading.Tasks;

class Program
{
    static void Main()
    {
        var bag = new ConcurrentBag<int>();

        // Add items concurrently
        Parallel.For(0, 10, i => bag.Add(i));

        // Retrieve items (order is undefined)
        while (!bag.IsEmpty)
        {
            if (bag.TryTake(out int item))
            {
                Console.WriteLine($"Took: {item}");
            }
        }
    }
}
```

### Example: Using `ConcurrentStack`

If you need a thread-safe stack, `ConcurrentStack<T>` is your go-to choice.

```csharp
using System;
using System.Collections.Concurrent;
using System.Threading.Tasks;

class Program
{
    static void Main()
    {
        var stack = new ConcurrentStack<int>();

        // Push items concurrently
        Parallel.For(0, 10, i => stack.Push(i));

        // Pop items safely
        while (!stack.IsEmpty)
        {
            if (stack.TryPop(out int item))
            {
                Console.WriteLine($"Popped: {item}");
            }
        }
    }
}
```

These `Concurrent` collections allow you to avoid explicit locking while maintaining thread safety, making your C# applications more performant and scalable.

These collections handle the nitty-gritty of synchronization for you, so you can focus on the fun stuff. ([learn.microsoft.com](https://learn.microsoft.com/en-us/dotnet/standard/collections/thread-safe/when-to-use-a-thread-safe-collection))

### 2. Dive into Interlocked Operations

For simple types like integers or booleans, the `Interlocked` class is your friend. It provides atomic operations, ensuring that your threads don't step on each other's toes.

```csharp
int counter = 0;
Interlocked.Increment(ref counter); // Atomically increments counter
```

This ensures that even if multiple threads are incrementing the counter simultaneously, they won't cause a race condition.

### 3. Embrace the Power of Volatile

The `volatile` keyword tells the compiler and runtime that a field's value can be changed by multiple threads. It's like saying, "Hey, don't cache this value; always read it fresh."

```csharp
private volatile bool isRunning;
```

Use it wisely, though! It's not a silver bullet and doesn't replace proper synchronization.

### 4. Create Your Own Lock-Free Structures

Feeling adventurous?

You can build your own lock-free data structures using atomic operations.

It's like crafting your own superhero cape...

Here's a simple example of a lock-free stack using `Interlocked.CompareExchange`:

```csharp
public class LockFreeStack<T>
{
    private Node head;

    public void Push(T value)
    {
        Node newHead = new Node(value);
        do
        {
            newHead.Next = head;
        } while (Interlocked.CompareExchange(ref head, newHead, newHead.Next) != newHead.Next);
    }

    public T Pop()
    {
        Node oldHead = head;
        do
        {
            if (oldHead == null) throw new InvalidOperationException("Stack is empty.");
        } while (Interlocked.CompareExchange(ref head, oldHead.Next, oldHead) != oldHead);
        return oldHead.Value;
    }

    private class Node
    {
        public T Value;
        public Node Next;
        public Node(T value) { Value = value; }
    }
}
```

This stack allows multiple threads to push and pop items without traditional locks.

([codeproject.com](https://www.codeproject.com/articles/23317/lock-free-queue-implementation-in-cplusplus-and-cs))

## Key Ideas

| Concept                         | Description                                                                                        |
| ------------------------------- | -------------------------------------------------------------------------------------------------- |
| **Lock-Free Programming**       | Allows multiple threads to access shared data without traditional locks.                           |
| **Concurrent Collections**      | .NET's thread-safe collections like `ConcurrentQueue<T>` and `ConcurrentDictionary<TKey, TValue>`. |
| **Interlocked Operations**      | Atomic operations provided by the `Interlocked` class for simple types.                            |
| **Volatile Keyword**            | Indicates that a field's value can be changed by multiple threads.                                 |
| **Custom Lock-Free Structures** | Building your own lock-free data structures using atomic operations.                               |

## Further Reading

* [Locking-Free Synchronization - CodeProject](https://www.codeproject.com/Articles/190200/Locking-free-synchronization)
* [Creating High-Performance Locks and Lock-free Code (for .NET) - Adam Mil](https://adammil.net/blog/v111_Creating_High-Performance_Locks_and_Lock-free_Code_for_NET_.html)
* [Lock Free Queue Implementation in C++ and C# - CodeProject](https://www.codeproject.com/articles/23317/lock-free-queue-implementation-in-cplusplus-and-cs)
* [An Introduction to Lock-Free Programming - Preshing](https://preshing.com/20120612/an-introduction-to-lock-free-programming/)
