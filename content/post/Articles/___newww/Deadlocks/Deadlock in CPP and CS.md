---
title: Understanding Deadlocks and Race Conditions in C++ and C#
description: What They Are and How to Avoid Them
slug: understanding-deadlocks-and-race-conditions-in-cpp-and-csharp
date: 2019-07-14
image: post/Articles/IMAGES/locks.jpg
categories:
  - Concurrency
  - Multithreading
  - C++
  - C#
  - WinAPI
tags:
  - Concurrency
  - Multithreading
  - Deadlocks
  - Race
  - Conditions
  - Thread
  - Safety
  - CPP
  - CSharp
draft: false
weight: 298
lastmod: 2025-03-02T23:53:57.689Z
---
# Understanding Deadlocks and Race Conditions in C++ and C\#

Ah, concurrency. That magical realm where performance soars, or your program turns into a raging dumpster fire of synchronization nightmares.

Today, we’re diving into **deadlocks** and **race conditions**, two classic bugs that will make you question your career choices (or at least your debugging skills).

<!-- Let’s break them down, with **examples** and **solutions** to keep your code from going full chaos mode.
-->

## What is a Deadlock? 🤯

A **deadlock** occurs when two or more threads are waiting for each other to release a resource, but none of them ever does.

It's like two people standing in a doorway, each waiting for the other to move first. Spoiler: No one moves. Ever.

📖 [Wikipedia: Deadlock](https://en.wikipedia.org/wiki/Deadlock)

### Example 1: Classic Deadlock in C\#

```csharp
using System;
using System.Threading;

class DeadlockExample
{
    static readonly object lockA = new object();
    static readonly object lockB = new object();

    static void Method1()
    {
        lock (lockA)
        {
            Thread.Sleep(100);
            lock (lockB)
            {
                Console.WriteLine("Method1 acquired both locks.");
            }
        }
    }

    static void Method2()
    {
        lock (lockB)
        {
            Thread.Sleep(100);
            lock (lockA)
            {
                Console.WriteLine("Method2 acquired both locks.");
            }
        }
    }

    static void Main()
    {
        Thread t1 = new Thread(Method1);
        Thread t2 = new Thread(Method2);
        t1.Start();
        t2.Start();
        t1.Join();
        t2.Join();
    }
}
```

🔴 **Why it happens:** Thread 1 locks `lockA` and waits for `lockB`, while Thread 2 locks `lockB` and waits for `lockA`. Since neither releases their lock, they’re stuck in an eternal waiting game.

✅ **How to avoid it:** Always **lock resources in a consistent order**. In this case, ensure that both threads acquire locks in the same order (`lockA` then `lockB`).

### Example 2: Classic Deadlock in C++

```cpp
#include <iostream>
#include <thread>
#include <mutex>
using namespace std;

mutex mtxA, mtxB;

void thread1() {
    lock_guard<mutex> lock1(mtxA);
    this_thread::sleep_for(chrono::milliseconds(100));
    lock_guard<mutex> lock2(mtxB);
    cout << "Thread 1 acquired both locks" << endl;
}

void thread2() {
    lock_guard<mutex> lock1(mtxB);
    this_thread::sleep_for(chrono::milliseconds(100));
    lock_guard<mutex> lock2(mtxA);
    cout << "Thread 2 acquired both locks" << endl;
}

int main() {
    thread t1(thread1);
    thread t2(thread2);
    t1.join();
    t2.join();
    return 0;
}
```

🔴 **Why it happens:** Same problem as C#—locks are acquired in an inconsistent order.

✅ **How to avoid it:** Use `std::lock()` to lock multiple mutexes simultaneously and avoid deadlocks.

```cpp
lock(mtxA, mtxB);
lock_guard<mutex> lock1(mtxA, adopt_lock);
lock_guard<mutex> lock2(mtxB, adopt_lock);
```

## What is a Race Condition? 🏎️💨

A **race condition** happens when multiple threads access shared data and at least one modifies it without proper synchronization. The outcome? **Unpredictable behavior**.

📖 [Wikipedia: Race Condition](https://en.wikipedia.org/wiki/Race_condition)

### Example 3: Race Condition in C\#

```csharp
using System;
using System.Threading;

class RaceConditionExample
{
    static int counter = 0;

    static void Increment()
    {
        for (int i = 0; i < 1000000; i++)
        {
            counter++;
        }
    }

    static void Main()
    {
        Thread t1 = new Thread(Increment);
        Thread t2 = new Thread(Increment);
        t1.Start();
        t2.Start();
        t1.Join();
        t2.Join();
        Console.WriteLine($"Final counter value: {counter}");
    }
}
```

🔴 **Why it happens:** `counter++` isn’t atomic, so multiple threads can read, increment, and write **simultaneously**, leading to lost updates.

✅ **How to avoid it:** Use `Interlocked.Increment(ref counter);` or a lock.

### Example 4: Race Condition in C++

```cpp
#include <iostream>
#include <thread>
using namespace std;

int counter = 0;
void increment() {
    for (int i = 0; i < 1000000; i++) {
        counter++;
    }
}

int main() {
    thread t1(increment);
    thread t2(increment);
    t1.join();
    t2.join();
    cout << "Final counter value: " << counter << endl;
    return 0;
}
```

🔴 **Why it happens:** `counter++` isn’t atomic, causing race conditions.

✅ **How to avoid it:** Use `std::atomic<int> counter(0);` instead.

## Summary Table

| Issue          | Why It Happens                                        | How to Avoid It                      |
| -------------- | ----------------------------------------------------- | ------------------------------------ |
| Deadlock       | Threads waiting on each other forever                 | Lock resources in a consistent order |
| Race Condition | Threads modifying shared data without synchronization | Use locks or atomic variables        |

## References

* [Wikipedia: Deadlock](https://en.wikipedia.org/wiki/Deadlock)
* [Wikipedia: Race Condition](https://en.wikipedia.org/wiki/Race_condition)
