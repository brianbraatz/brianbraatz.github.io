---
title: Who needs Visual Studio?? Write your OWN debugger....
description: For fun and profit
slug: write-your-own-debugger-windows
date: 2016-12-03
image: post/Articles/IMAGES/windbg.webp
categories:
  - C++ Debugging
  - WinDbg
  - Visual Studio
  - Performance Analysis
  - Memory Dump Analysis
tags:
  - Debugger
  - Windows API
  - Reverse Engineering
  - Windows Debugging
  - Fun Programming Projects
draft: false
weight: 637
lastmod: 2025-02-20T23:07:09.426Z
---
# Who Needs Visual Studio?? Write Your OWN Debugger Using the Windows API for Fun and Profit 🎯

Okay, let's get one thing straight: Visual Studio is great.

It's polished, powerful, and... well, it’s basically like driving a luxury car with all the assistive features.

But what if you could build your OWN debugger?

Imagine yourself wielding the raw power of the Windows API like a programming wizard.

Who needs luxury when you can build your own monster truck of debugging?

This article dives into writing a simple debugger from scratch using the Windows API.

Why? Because we can.

And because *debugging is fun* (and it sounds impressive at parties).

By doing this: you are also going to learn how Windbg does it as well as Visual Studio.

## 1. Why Write a Debugger?

* **Learning:** You’ll learn how debuggers actually work under the hood. Visual Studio hides all the cool stuff!
* **Control:** Build the debugger that does exactly what YOU want.
* **Bragging Rights:** Nothing beats telling people you built a debugger. Instant geek cred.

## 2. Windows API Debugging: The Secret Sauce

Windows provides a bunch of functions to create a debugger. Here's a quick overview of the heavy hitters:

* `CreateProcess`: Fire up your target process.
* `WaitForDebugEvent`: Sit back and wait for juicy debug events.
* `ContinueDebugEvent`: Tell Windows you’re done dealing with the current event.
* `ReadProcessMemory`/`WriteProcessMemory`: Peek and poke into the target process’s memory. Sneaky!
* `SetThreadContext`: Tinker with CPU registers like a digital surgeon.

## 3. Building the Debugger: Step-by-Step

### Step 1: Create the Process

```c
#include <windows.h>
#include <stdio.h>

int main() {
    STARTUPINFO si = {0};
    PROCESS_INFORMATION pi = {0};
    si.cb = sizeof(si);

    if (!CreateProcess("C:\\Windows\\System32\\notepad.exe", NULL, NULL, NULL, FALSE,
                       DEBUG_PROCESS, NULL, NULL, &si, &pi)) {
        printf("Failed to start process. Error: %lu\n", GetLastError());
        return 1;
    }

    printf("Debugger attached to Notepad!\n");
    return 0;
}
```

Boom! You've just launched Notepad with a debugger attached. Next step: intercept some events.

### Step 2: Wait for Debug Events

```c
DEBUG_EVENT debugEvent;
while (WaitForDebugEvent(&debugEvent, INFINITE)) {
    printf("Event code: %d\n", debugEvent.dwDebugEventCode);
    ContinueDebugEvent(debugEvent.dwProcessId, debugEvent.dwThreadId, DBG_CONTINUE);
}
```

Now your debugger can receive events like process creation, thread creation, and (our favorite) breakpoints.

### Step 3: Set a Breakpoint

Want to stop execution at a certain point? Let's inject a software breakpoint (INT 3).

```c
BYTE int3 = 0xCC;
SIZE_T bytesWritten;
DWORD address = 0x401000; // Replace with the address you want to breakpoint.
WriteProcessMemory(pi.hProcess, (LPVOID)address, &int3, 1, &bytesWritten);
```

That’s right—you just hijacked a process to insert a breakpoint. James Bond-level stuff.

### Step 4: Create a Target Program with an INT 3 Breakpoint

You can also add breakpoints inside your own code manually:

```c
#include <stdio.h>
#include <windows.h>

int main() {
    printf("Hello, Debugger!\n");
    __debugbreak(); // This triggers an INT 3 software breakpoint
    printf("After Breakpoint!\n");
    return 0;
}
```

When Visual Studio places breakpoints, it does exactly what we did above using `WriteProcessMemory()`. Of course, it also manages breakpoint tables, enabling/disabling breakpoints dynamically, and keeping everything neat and tidy.

### Step 5: Step Through Execution

To step through lines of code, we need to modify the `EIP/RIP` register manually. This is what debuggers do to implement stepping:

```c
CONTEXT context;
context.ContextFlags = CONTEXT_CONTROL;
GetThreadContext(pi.hThread, &context);
context.EFlags |= 0x100; // Set trap flag for single stepping
SetThreadContext(pi.hThread, &context);
```

This makes the CPU stop after executing a single instruction. Windbg does this MUCH better—but hey, you get the idea!

## More Information: Windbg Source Code

If you want to dig deeper into real-world debugger implementations, you can check out the Windbg source code:

* [Windbg Source Code](https://github.com/microsoft/WinDbg-Samples)

## Key Ideas

| Concept                | Explanation                                                                             |
| ---------------------- | --------------------------------------------------------------------------------------- |
| Debugging API          | Windows provides APIs like `WaitForDebugEvent` and `SetThreadContext` for debugging.    |
| Memory Manipulation    | `ReadProcessMemory` and `WriteProcessMemory` let you inspect and change process memory. |
| Breakpoints            | Insert `INT 3` instructions for software breakpoints.                                   |
| Event Handling         | Handle events like process/thread creation or termination.                              |
| Control Flow Hijacking | Modify registers and step through code manually.                                        |

## References

1. [Writing a Debugger from Scratch](https://www.timdbg.com/posts/writing-a-debugger-from-scratch-part-1/)
2. [Windows Debugging API Documentation](https://learn.microsoft.com/en-us/windows/win32/debug/)
3. [The Art of Debugging](https://en.wikipedia.org/wiki/Debugging)
4. [WinDbg Documentation](https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/)
5. [Windbg Source Code](https://github.com/microsoft/WinDbg-Samples)
