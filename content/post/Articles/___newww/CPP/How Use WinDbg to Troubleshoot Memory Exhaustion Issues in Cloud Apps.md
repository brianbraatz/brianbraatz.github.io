---
title: Use WinDbg to Troubleshoot Memory Exhaustion in Cloud Apps
description: Yes. even C#!
slug: windbg-memory-fix
date: 2019-12-15
image: post/Articles/IMAGES/windbg.webp
categories:
  - DevOps
  - C++ Debugging
  - WinDbg
  - Cloud
tags:
  - PDB
  - Debugging
  - Release
  - Mode
  - Crash
  - Analysis
  - Windows
  - Symbols
  - Windbg
draft: false
weight: 46
lastmod: 2025-03-03T16:17:05.304Z
---
<!--
# How to Use WinDbg to Troubleshoot Memory Usage Problems in Cloud Applications
-->

So, your cloud application (or maybe windows service) is guzzling memory like a teenager at an all-you-can-eat buffet, and you have no idea why.

You’ve tried staring at Task Manager, whispering sweet nothings to your logs, and even turning it off and on again. Nothing’s working.

Time to pull out **WinDbg**, the terrifying-yet-powerful debugger from Microsoft that can help you figure out why your app is behaving like a memory-hungry monster.

Don’t worry if you’ve never used it before-that's the point of this article :)

***

## Step 1: Install WinDbg

First things first, you need **WinDbg (Preview)**, which you can install from the **Microsoft Store** like any normal Windows app.

Yes, we’re using the **Preview** version because it's modern, has a decent UI, and doesn’t make you feel like you’re debugging in the early 2000s.

Alternatively, if you’re a *true* Windows debugger veteran, you can grab the **Windows Debugging Tools** from the [Windows SDK](https://developer.microsoft.com/en-us/windows/downloads/windows-sdk/).

But fair warning—this is the "raw, unfiltered" WinDbg that will make you type out commands manually. Choose your pain wisely.

***

## Step 2: Capture a Memory Dump (Like a Pro)

Before you can analyze your app’s memory problems, you need a **memory dump**—essentially a snapshot of what your app was doing when it decided to eat all your RAM.

### Option 1: Using Task Manager (Easy Mode)

1. Open **Task Manager** (`Ctrl + Shift + Esc`).
2. Find your misbehaving process.
3. Right-click → **Create Dump File**.
4. Note where Windows saves the `.dmp` file (usually in `C:\Users\<YourUser>\AppData\Local\Temp`).

### Option 2: Using ProcDump (For Cool Kids)

Microsoft’s **ProcDump** tool lets you capture memory dumps when specific conditions occur (like insane memory usage). Get it from [Sysinternals](https://docs.microsoft.com/en-us/sysinternals/downloads/procdump).

Example:

```powershell
procdump -ma -w myapp.exe memorydump.dmp
```

This waits for `myapp.exe` to launch and captures a full memory dump. Fancy.

### Option 3: Using WinDbg Itself (Hardcore Mode)

If you want to attach WinDbg directly:

```cmd
windbg -p <ProcessID>
```

Then manually break execution and dump memory with:

```cmd
.dump /ma mydump.dmp
```

But let’s be honest, **Task Manager is easier**.

***

## Step 3: Open the Dump in WinDbg (Brace Yourself)

1. Open **WinDbg (Preview)**.
2. Click **File → Open Dump File**.
3. Select your `.dmp` file.
4. Wait while WinDbg loads symbols (this is where things get *fun*).

### If WinDbg Complains About Missing Symbols...

You'll need to set up **symbol paths** manually. Run this command:

```cmd
.sympath SRV*C:\Symbols*https://msdl.microsoft.com/download/symbols
.reload
```

This downloads debugging symbols from Microsoft, so you’re not staring at meaningless hex addresses.

***

## Step 4: Analyze Memory Usage (Find the Culprit)

### Find Out What’s Eating RAM

Once the dump loads, type:

```cmd
!address -summary
```

This gives you a breakdown of **heap usage**, **virtual memory**, and **allocated objects**.

Another useful command:

```cmd
!heap -stat
```

This lists memory allocations per heap. If a particular type of allocation is ridiculously high, **that’s your suspect**.

### Check for Leaky Objects

If your .NET app is leaking memory like a broken faucet, use:

```cmd
!dumpheap -stat
```

This shows **all allocated objects**, sorted by count and size.

If you see thousands of an object type sticking around longer than a bad habit, you might have a **memory leak**.

To investigate further:

```cmd
!dumpheap -mt <MethodTable>
```

Replace `<MethodTable>` with the object type's method table from the previous command to see **where those objects are being held**.

***

## Step 5: Find Who’s Holding Your Memory Hostage

If you suspect objects aren’t being garbage collected (because something’s still holding onto them), use:

```cmd
!gcroot <ObjectAddress>
```

This tells you **who’s keeping that object alive** like an overprotective parent.

***

## Step 6: Solve the Problem (A.K.A. Actually Fix Your Code)

By now, you’ve probably found:

* An object type with **way too many instances**.
* A **collection that never clears**.
* Some **evil static variables** holding onto memory forever.
* A **third-party library** that just won’t let go.

Once you know the culprit, it’s time to fix it:

* **Dispose of objects properly.**
* **Avoid unnecessary static references.**
* **Use weak references if needed.**
* **Investigate long-running tasks that might be holding memory.**
* **Consider tuning garbage collection settings.**

***

## Final Thoughts (And a Warning)

WinDbg is an insanely powerful tool, but it’s also **not for the faint of heart**.

Debugging memory issues can feel like searching for a single sock in a warehouse full of laundry.

But once you get the hang of it, **you’ll feel like a debugging Gandolph**.

If you ever get stuck, remember:

* **Google is your best friend.**
* **Microsoft Docs exist for a reason.**
* **Trial and error is part of the process.**
