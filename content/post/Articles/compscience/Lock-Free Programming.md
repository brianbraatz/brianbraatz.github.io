---
title: Understanding Lock-Free Programming
slug: cs-lockfreeprog
date: 2015-12-15
image: post/Articles/IMAGES/lockfreewide.jpg
categories:
  - C
  - CPP
  - Windows
  - Linux
  - Unix
  - Mac OS
  - Concurrency
  - Assembly Language
tags:
  - Concurrency
  - Lock-Free
  - AssemblyLanguage
  - LockFreeThreading
  - CPP
  - Multithreading
draft: false
weight: 30
description: Dive into a Multi-Threading world with NO LOCKS!
categories_ref:
  - C
  - CPP
  - Windows
  - Linux
  - Unix
  - Mac OS
  - Concurrency
  - Assembly Language
slug_calculated: https://brianbraatz.github.io/p/cs-lockfreeprog
lastmod: 2025-03-31T23:24:08.160Z
---
<!-- # Understanding Lock-Free Programming

Hey there, fellow code nerds! 🧑‍💻 Ever found yourself tangled in the web of locks, mutexes, and semaphores, only to end up in a deadlock dance?????

 Fear not! Today, we're diving into the world of **lock-free programming**, with a sprinkle of wisdom from the legendary Andrei Alexandrescu. 
 
 > **(FAIR WARNING: I AM A SERIOUS FAN BOY of Andrei Alexandrescu. After I read his paper, I took time off to work and flew to Seattle to hang out with him and geek out over this idea... He was gracious and funny and smart (I told you i was a total fan boy) )**
 
 So, grab your favorite debugging snack, and let's get started!
-->

<!-- ![Lock-Free Programming](https://example.com/lock-free-programming-meme.jpg)
-->

> <!-- ![Lock-Free Programming](https://example.com/lock-free-programming-meme.jpg) -->

## A Brief History of Lock-Free Programming

Once upon a time in the land of computing, developers relied heavily on locks to manage concurrent processes.

While locks ensured data integrity, they often led to issues like deadlocks, priority inversion, and the dreaded performance bottlenecks.

Enter **lock-free programming**—a knight in shining armor promising a world where threads could work together without stepping on each other's toes.

The journey began with the introduction of non-blocking algorithms, which allowed threads to make progress without waiting for others to release locks.

This approach not only improved performance but also enhanced system reliability.

Over time, pioneers like Maurice Herlihy laid the groundwork for lock-free data structures, and our very own Andrei Alexandrescu further illuminated the path with his insightful writings.

For a deeper dive into the evolution of lock-free techniques, check out this [Introduction to Lock-Free Programming](https://preshing.com/20120612/an-introduction-to-lock-free-programming/).

## The Magic Behind Lock-Free: Compare-and-Swap (CAS)

At the heart of lock-free programming is the **Compare-and-Swap (CAS)** instruction.  It works like this:

1. **Compare**: Check if the current value at a memory location matches an expected value.
2. **Swap**: If it matches, swap it with a ( a list usually of )new value(s); if not, leave it be and try again.

## Basically - This is how it works In Practice:

1- Grab the address of the current list (pointer to first element)\
2- Make a copy of that list\
3- Do whatever operation you want on your copy of the list- add - delete - sort - whatever\
4- Now- do a CAS (compare and swap)-with the pointer for your NEW list- and the old list

* CAS- takes the old value (pointer to old list) and the new value (pointer to new list)
* The magic: IF the OLD value is still there- (compare) then SWAP the old value with the new value
* If the old value is NOT there-CAS fails-nothing is changed- so loop back to step one
  * Since we looped-we copy the whole list again- and do the whole operation AGAIN

**This atomic operation ensures that even if multiple threads attempt to modify the same data simultaneously, only one succeeds, while the others gracefully retry.**

<!-- 
It's like multiple chefs trying to grab the last donut—only one gets it, and the rest have to wait for the next batch.
-->

For a more technical deep dive into CAS, you can refer to the [Non-blocking algorithm](https://en.wikipedia.org/wiki/Non-blocking_algorithm) page on Wikipedia.

**This magic trick WORKS because CAS takes ONE cycle.. in Assembly . Preemptive Operation systems use timer interrupts -save off stack and registers. But since CAS runs in one cycle, you dont have to worry about being preempted!**

for more on how Preemption works - see my article here:\
[Write a Preemptive multi-threaded OS in 8051 Assembly Language](https://brianbraatz.github.io/p/multi-threaded-os-in-8051-assembly-language/)

The threads fighting over the list- never lock each other- they just loop until its their turn to modify the list.

Hence the name "Lock Free"

## Andrei Alexandrescu's Take on Lock-Free Data Structures

In his enlightening and global hunger ending paper, [Lock-Free Data Structures](https://erdani.org/publications/cuj-2004-10.pdf), Alexandrescu delves into the intricacies of designing data structures that operate without locks.

He goes into more detail about  the importance of atomic operations, like CAS, in building efficient and scalable concurrent systems.

By leveraging CAS, developers can create data structures where threads can add, remove, or modify elements without waiting for others to finish their operations.

This approach not only boosts performance but also eliminates common pitfalls associated with traditional locking mechanisms.

## Key Ideas

| Concept                                 | Description                                                                                                                                    |
| --------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| **Lock-Free Programming**               | A concurrency control mechanism that allows multiple threads to operate on shared data without using locks.                                    |
| **Compare-and-Swap (CAS)**              | An atomic instruction that compares the current value of a memory location to an expected value and, if they match, swaps it with a new value. |
| **Andrei Alexandrescu's Contributions** | Provided significant insights into designing lock-free data structures, emphasizing the role of atomic operations like CAS.                    |

## Further Reading and Resources

* [An Introduction to Lock-Free Programming](https://preshing.com/20120612/an-introduction-to-lock-free-programming/)
* [Non-blocking algorithm - Wikipedia](https://en.wikipedia.org/wiki/Non-blocking_algorithm)
* [Lock-Free Data Structures](https://erdani.org/publications/cuj-2004-10.pdf)

Remember, while lock-free programming offers numerous advantages, it's not a silver bullet.

It requires a deep understanding of concurrency and careful design to avoid pitfalls. But with the right approach (and maybe a bit of Alexandrescu's wisdom), you can unlock the full potential of your multi-threaded applications.

This blog article is just to get you wet.. er.. your feet wet.. You need to read more about this if you want to play in the playground..

The smarter ones of you reading this will figure out that Lock Free solves some problems in an awesome way, but also creates something of a garbage collection problem. Andrei talks about this in more depth in his paper..

The whole trick - really deeply depends on the CPU you are using having a CAS insruction that MUST execute in exactly one cycle..

if you dont have that , then no lock free party for you...
