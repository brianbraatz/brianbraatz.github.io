---
title: Windows NT 3.51 to NT4
description: The Huge Kernel Change that Changed the (Windows) World
slug: windows-nt-3-51-to-nt4-huge-change
date: 2016-12-15
image: post/Articles/IMAGES/nt351_4.png
categories:
  - WinAPI
  - Windows
  - History
  - WinNT
  - CPP
  - Device Drivers
tags:
  - Windows
  - NT
  - Operating
  - Systems
  - NT
  - Kernel
  - VMS
  - Device
  - Drivers
  - Stability
  - vs
  - Speed
  - Dave
  - Cutler
  - Windows
  - History
draft: false
weight: 71
lastmod: 2025-03-03T14:52:27.574Z
---
Ah, Windows NT. The good ol' days of the 90s when Microsoft was figuring out how to build an operating system that wouldn’t crash when you sneezed on it.

NT 3.51 was like a tank — slow, heavy, but ridiculously hard to break.

Then came NT4, with a radical shift under the hood that made things faster… but also, well, a little more "fragile." So, what the heck happened?

Let’s dive into the wild, nerdy history of NT3.51 to NT4, the architectural decisions that changed Windows forever, and how Dave Cutler's VMS influence played a sneaky role.

## NT 3.51: The Uncrashable Tank (But Slow as a Snail on a Lazy Day)

Windows NT 3.51 was built like a fortress.

The design decision here was clear: stability over speed.

And boy, did it deliver.\
NT 3.51 could run for months without a hiccup. Why? Because of how device drivers interacted with the kernel.

Here’s the trick: when a device driver needed to make an API call, the operating system would put the driver to sleep.

Yep, like tucking it into bed with a warm cup of cocoa.

The call would then execute in a separate context (not in the driver's context), and once the operation completed, the device driver would get a gentle wake-up nudge.

This approach minimized the damage a buggy driver could cause.

You couldn’t crash the system easily because the driver's code never ran directly in kernel space.

This pattern is known as **Asynchronous Procedure Call (APC)**.

And yes, it’s deeply related to the design principles of **VMS** — the operating system Dave Cutler worked on before joining Microsoft.

VMS used a similar pattern to isolate kernel components, which heavily influenced NT’s architecture.

### Wait, What’s an Asynchronous Procedure Call (APC)?

An APC is like calling customer service and being told, “Please hold, your call is important to us.”

While you wait, the system does other stuff.

When the task is ready, it gives the driver a callback. In NT 3.51, this ensured that drivers didn’t get full access to kernel-mode operations, making the system incredibly stable.

But the trade-off? **Speed.**

This was like driving a tank through a school zone — slow and cautious, but super safe.

For a 1990s server, that was fine. But for desktop users who wanted things to move faster? Not so much.

## NT4: The Need for Speed (But Brace for Impact)

Enter **Windows NT4**.

Microsoft decided they needed more performance — particularly in the graphics subsystem, which was notoriously sluggish in NT 3.51.

So, they moved large parts of the Graphics Device Interface (GDI) from user mode to kernel mode.

This was like strapping a jet engine onto the tank. Suddenly, graphics were snappier, applications were more responsive, and users were… thrilled!

### But here’s the catch:

* Kernel mode code runs with higher privileges, so if a driver went rogue, it could take down the entire system.
* Graphics drivers, known for their "less-than-stellar" coding practices in the 90s, became the Achilles' heel of NT4.

Yep, moving GDI into kernel space made the system faster, but you also got the lovely Blue Screen of Death (BSOD) more often than you'd like. And if you remember NT4's days, you'd recall that **ATI and NVIDIA drivers were basically BSOD dispensers back then**.

## VMS: The Ghost in NT's Machine

So, what’s the deal with VMS? Dave Cutler, the architect behind NT, was also one of the main brains behind VMS at DEC (Digital Equipment Corporation).

VMS employed a similar asynchronous, context-switching pattern for device drivers, prioritizing reliability over speed.

When Cutler and his team joined Microsoft, they brought these design principles along.

NT 3.51’s driver model was basically VMS with a Windows badge slapped on it.

### Did VMS Have APCs?

Yes. VMS heavily relied on APC-like mechanisms for device driver operations. The philosophy: **“A driver should never be trusted.”** Cutler simply ported and enhanced these ideas to NT.

## The Aftermath: Stability Lost, Certification Gained

Windows NT4 was faster, sure.

But the newfound kernel-mode graphics drivers turned stability into a bit of a rollercoaster.

Over time, Microsoft had to get creative to keep things from turning into chaos.

### Enter: **Driver Certification and WHQL**

To curb the madness, Microsoft introduced stricter driver certification programs like **Windows Hardware Quality Labs (WHQL)**.

Vendors who wanted their drivers to be included with Windows had to go through a rigorous certification process.

This evolved into a robust ecosystem of certified drivers, culminating in **Driver Verifier** — a tool designed to stress-test drivers and ensure they followed the rules.

Microsoft learned that if drivers were going to live in kernel space, they needed to be squeaky clean.

## The Long-Term Impact

The NT4 gamble laid the foundation for the performance improvements that made Windows 2000, XP, and future versions much faster.

The NT kernel we know today is still haunted by the ghosts of these decisions.

### General Ideas

* NT 3.51: Slow but stable (like a Volvo from the 80s).
* NT4: Faster but prone to BSODs thanks to kernel-mode graphics.
* VMS: The spiritual ancestor of NT's kernel design.
* APCs: Asynchronous magic that made NT reliable — until Microsoft needed more speed.

## Key Ideas Table

| **Concept**                            | **Explanation**                                                             |
| -------------------------------------- | --------------------------------------------------------------------------- |
| **NT 3.51 Stability**                  | Device drivers ran in isolated contexts, ensuring stability.                |
| **Asynchronous Procedure Calls (APC)** | Device drivers were put to sleep while API calls ran elsewhere.             |
| **NT4 Performance Shift**              | Moved GDI to kernel space for better performance, at the cost of stability. |
| **VMS Influence**                      | NT's driver architecture borrowed heavily from VMS concepts.                |
| **Driver Certification**               | WHQL certification was introduced to tame driver-related crashes.           |

## Reference Links

1. [Windows NT Architecture Overview](https://en.wikipedia.org/wiki/Architecture_of_Windows_NT)
2. [The History of Windows NT](https://www.computerhistory.org/)
3. [Dave Cutler and VMS Influence](https://www.theregister.com/)
4. [APCs in Windows](https://docs.microsoft.com/en-us/windows/win32/sync/asynchronous-procedure-calls)
5. [WHQL Certification](https://learn.microsoft.com/en-us/windows-hardware/drivers/)

***
