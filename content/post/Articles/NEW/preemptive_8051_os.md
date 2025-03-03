---
title: Write a Preemptive multi-threaded OS in 8051 Assembly Language
description: For fun and profit...!!!!!!!!!!!!!!!!
slug: multi-threaded-os-in-8051-assembly-language
date: 2023-03-04
image: post/Articles/IMAGES/8051wide.jpg
categories:
  - Embedded
  - 8051 Embedded Processor
  - Concurrency
  - Assembly Language
tags:
  - Preemptive
  - Embedded
  - Firmware
  - Multithreading
  - 8051Assembly
  - 8051CPU
  - Performance
  - AssemblyLanguage
draft: false
weight: 15
lastmod: 2025-03-03T13:55:13.813Z
---
# How to write a Preemptive multi-threaded OS in 8051 Assembly Language

## The Legendary 8051: A Brief History

The 8051 microcontroller is like the Chuck Norris of the embedded world—old, but still packing a punch. Designed by Intel in 1980, this 8-bit microcontroller became the backbone of embedded systems due to its simplicity, robustness, and wide adoption.

While other CPUs went on to form fancy desktops and servers, the 8051 stayed true to its calling—controlling everything from microwave ovens to industrial machines.

It has built-in support for I/O ports, timers, and interrupts, making it perfect for real-time systems.

If you need a microcontroller that just *works*, with a well-documented instruction set, the 8051 is a solid choice.

## Why Preemptive Multithreading?

Most simple embedded systems run tasks in a cooperative fashion—meaning one task must finish before another runs.

But what if a high-priority task needs CPU time *right now*? Enter *preemptive multitasking*.

This allows tasks to be interrupted and switched dynamically, ensuring fairness and responsiveness.

## The Secret Sauce: Timer Interrupts

A preemptive system needs a way to stop a running task and switch to another.

**Without a CPU timer interrupt, you can't have true preemptive multitasking.**

Timers generate periodic interrupts that trigger our scheduler, which handles context switching between "threads."

Now, let’s be real. Me calling this an "Operating System", in the article title is just clickbait.

Real embedded engineers know that this is *firmware*, but OS just sounds cooler. 😎

## Understanding Preemptive Scheduling

1. We set up a hardware timer to fire at regular intervals.
2. When the timer interrupt occurs, the CPU jumps to our scheduler.
3. The scheduler:
   * Saves the current task's registers and stack.
   * Restores the next task's registers and stack.
   * Hands control to the next task.
4. This cycle repeats indefinitely.

Yes, even modern operating systems like Windows and Linux do something similar—just with more sophistication (and fewer `MOV` instructions).

## 8051 Assembly Code for a Preemptive Scheduler

Here's a simple implementation of a preemptive multitasking scheduler in 8051 assembly:

```assembly
ORG 0000H
    LJMP START

ORG 000BH  ; Timer 0 Interrupt Vector
    LJMP TIMER_ISR

ORG 0030H
START:
    MOV TMOD, #01H    ; Timer 0 Mode 1 (16-bit timer)
    MOV TH0, #HIGH(-50000)  ; Load timer high byte
    MOV TL0, #LOW(-50000)   ; Load timer low byte
    SETB TR0   ; Start Timer 0
    SETB ET0   ; Enable Timer 0 Interrupt
    SETB EA    ; Enable Global Interrupts

    MOV SP, #50H  ; Set stack pointer

TASK1:
    MOV A, #01H
    SJMP TASK1

TASK2:
    MOV A, #02H
    SJMP TASK2

TIMER_ISR:
    PUSH ACC
    PUSH PSW
    MOV A, SP
    PUSH A  ; Save current stack pointer

    MOV A, #TASK2_STACK  ; Switch to new task stack
    MOV SP, A
    POP A
    POP PSW
    POP ACC
    RETI  ; Return from interrupt
```

### Explanation:

* We set up *Timer 0* to generate periodic interrupts.
* The timer ISR (Interrupt Service Routine) **saves the current state** (registers, stack pointer).
* It **switches context** by loading another task's stack pointer.
* The process repeats, effectively multitasking between `TASK1` and `TASK2`.

## 8051 Assembly Cheat Sheet

| Instruction    | Description                |
| -------------- | -------------------------- |
| `MOV A, #data` | Load immediate data into A |
| `MOV Rn, A`    | Move A to register Rn      |
| `PUSH reg`     | Push register onto stack   |
| `POP reg`      | Pop value from stack       |
| `LJMP addr`    | Long jump to address       |
| `SETB bit`     | Set bit to 1               |
| `CLR bit`      | Clear bit                  |
| `RET`          | Return from subroutine     |
| `RETI`         | Return from interrupt      |

## Key Takeaways

| Concept                   | Summary                                     |
| ------------------------- | ------------------------------------------- |
| **Preemptive Scheduling** | Uses interrupts to switch tasks dynamically |
| **Timer Interrupts**      | Essential for preemptive multitasking       |
| **Context Switching**     | Saves and restores task states              |
| **8051 Simplicity**       | Great for embedded real-time applications   |

## References

* [8051 Instruction Set](https://www.keil.com/support/man/docs/is51/is51_opcodes.htm)
* [Intel 8051 Datasheet](https://www.intel.com/design/mcs51/manuals/272383.htm)
* [Preemptive Multitasking](https://en.wikipedia.org/wiki/Preemption_\(computing\))
