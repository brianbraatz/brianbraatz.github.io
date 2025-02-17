---
title: Understanding Deadlocks and Race Conditions in Golang and Python
description: What They Are and How to Avoid Them
slug: understanding-deadlocks-and-race-conditions-in-golang-and-python
date: 2018-12-05
image: post/Articles/IMAGES/racingbike.png
categories:
  - Concurrency
  - Multithreading
  - Golang
  - Python
tags:
  - Concurrency
  - Multithreading
  - Golang
  - Python
  - Deadlocks
  - Race
  - Conditions
  - Thread
  - Safety
draft: false
weight: 417
lastmod: 2025-02-17T14:47:40.897Z
---
# Understanding Deadlocks and Race Conditions in Golang and Python

Ah yes, concurrency—the magical force that either makes your program blazingly fast or turns it into an uncontrollable mess. If you’ve tackled C++ or C#, you know the pain. Now, let's dive into **Golang** and **Python**, and see how they handle (or mishandle) **deadlocks** and **race conditions**.

## What is a Deadlock? 🤯

A **deadlock** happens when two or more goroutines or threads wait for each other to release a resource, but none of them do. It’s like two polite people at a door, each insisting the other go first—forever.

📖 [Wikipedia: Deadlock](https://en.wikipedia.org/wiki/Deadlock)

### Example 1: Classic Deadlock in Golang

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

var lockA sync.Mutex
var lockB sync.Mutex

func task1() {
    lockA.Lock()
    time.Sleep(100 * time.Millisecond)
    lockB.Lock()
    fmt.Println("Task 1 completed")
    lockB.Unlock()
    lockA.Unlock()
}

func task2() {
    lockB.Lock()
    time.Sleep(100 * time.Millisecond)
    lockA.Lock()
    fmt.Println("Task 2 completed")
    lockA.Unlock()
    lockB.Unlock()
}

func main() {
    go task1()
    go task2()
    time.Sleep(1 * time.Second)
}
```

🔴 **Why it happens:** Task 1 locks `lockA` and waits for `lockB`, while Task 2 locks `lockB` and waits for `lockA`. Classic **deadlock**!

✅ **How to avoid it:** Always **acquire locks in the same order** to prevent circular waits.

### Example 2: Classic Deadlock in Python

```python
import threading
import time

lockA = threading.Lock()
lockB = threading.Lock()

def task1():
    with lockA:
        time.sleep(0.1)
        with lockB:
            print("Task 1 completed")

def task2():
    with lockB:
        time.sleep(0.1)
        with lockA:
            print("Task 2 completed")

thread1 = threading.Thread(target=task1)
thread2 = threading.Thread(target=task2)

thread1.start()
thread2.start()

thread1.join()
thread2.join()
```

🔴 **Why it happens:** Same issue as in Golang—locks are acquired in different orders.

✅ **How to avoid it:** Use a **consistent locking order** or try Python’s `threading.Condition` to avoid deadlocks.

## What is a Race Condition? 🏎️💨

A **race condition** occurs when multiple threads or goroutines access shared data simultaneously, leading to unpredictable results.

📖 [Wikipedia: Race Condition](https://en.wikipedia.org/wiki/Race_condition)

### Example 3: Race Condition in Golang

```go
package main

import (
    "fmt"
    "sync"
)

var counter = 0
var wg sync.WaitGroup

func increment() {
    for i := 0; i < 1000000; i++ {
        counter++
    }
    wg.Done()
}

func main() {
    wg.Add(2)
    go increment()
    go increment()
    wg.Wait()
    fmt.Println("Final counter value:", counter)
}
```

🔴 **Why it happens:** `counter++` isn’t atomic, so simultaneous writes lead to lost updates.

✅ **How to avoid it:** Use a **mutex** or **atomic operations** like `sync/atomic`.

```go
import "sync/atomic"
var counter int32
atomic.AddInt32(&counter, 1)
```

### Example 4: Race Condition in Python

```python
import threading

counter = 0

def increment():
    global counter
    for _ in range(1000000):
        counter += 1

thread1 = threading.Thread(target=increment)
thread2 = threading.Thread(target=increment)

thread1.start()
thread2.start()

thread1.join()
thread2.join()

print("Final counter value:", counter)
```

🔴 **Why it happens:** `counter += 1` isn't atomic in Python due to the **Global Interpreter Lock (GIL)**.

✅ **How to avoid it:** Use `threading.Lock()` or **multiprocessing** to bypass the GIL.

```python
lock = threading.Lock()

def increment():
    global counter
    for _ in range(1000000):
        with lock:
            counter += 1
```

## Summary Table

| Issue          | Why It Happens                                        | How to Avoid It                      |
| -------------- | ----------------------------------------------------- | ------------------------------------ |
| Deadlock       | Threads/goroutines waiting on each other forever      | Lock resources in a consistent order |
| Race Condition | Threads modifying shared data without synchronization | Use locks or atomic variables        |

## References

* [Wikipedia: Deadlock](https://en.wikipedia.org/wiki/Deadlock)
* [Wikipedia: Race Condition](https://en.wikipedia.org/wiki/Race_condition)

Now you know how to avoid these concurrency pitfalls in Golang and Python! Keep your threads happy and your locks in order. 🚀
