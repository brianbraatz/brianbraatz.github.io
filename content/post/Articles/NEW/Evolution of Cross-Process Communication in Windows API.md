---
title: Evolution of Cross-Process Communication in Windows
description: How the API evolved From Windows 1.0 to 11
slug: competing-methods-of-cross-process-communication-in-windows-api
date: 2024-12-15
image: post/Articles/IMAGES/microsoft-windows-title-screens-wide.png
categories: 
tags:
  - Windows
  - Win16
  - Win32
  - Win64
  - API
  - Cross-Process
  - Communication
  - Message
  - Passing
  - Named
  - Pipes
  - Signals
  - Slots
  - C
  - CPP
  - Programming
  - DesignPaterns
  - MessagingDesignPattern
  - Multithreading
draft: false
weight: 120
lastmod: 2025-02-05T14:51:38.463Z
---
**Design Evolution of the Microsoft Windows Welcome Screen**\
<https://www.versionmuseum.com/history-of/all-microsoft-windows-splash-title-screens>

# Evolution of Cross-Process Communication in Windows

## The History of Windows: From Single-Tasking to Modern Multi-Process Communication

Once upon a time, in the golden era of computing (cue nostalgic 8-bit music), Windows was a far cry from the multi-threaded, process-isolated behemoth it is today.

![](/post/Articles/NEW/microsoft-windows%5E1985%5Ewindows-1-title-screen.webp)

* **Windows 1.0 (1985)** – A GUI shell over MS-DOS. Nobody really cared.

![](/post/Articles/NEW/microsoft-windows%5E1987%5Ewindows-2-title-screen.webp)

* **Windows 2.0 (1987)** – Slightly better, but still awkward.

![](/post/Articles/NEW/microsoft-windows%5E1990%5Ewindows-3.0-title-screen.webp)

* **Windows 3.0 (1990)** – My personal favorite! This was a real game-changer.\
  ![](/post/Articles/NEW/Pasted%20image%2020250205064206.png)
* **Windows 3.11 for Workgroups (1993)** – The most popular version of early Windows. It introduced networking support, making it a staple for offices.

![](/post/Articles/NEW/microsoft-windows%5E1995%5Ewindows-95-title-screen.webp)

* **Windows 95 (1995)** – Goodbye, 16-bit era! Hello, true multi-tasking.

![](/post/Articles/NEW/Pasted%20image%2020250205064435.png)

* **Windows NT (1993 - today)** – True multi-user, multi-process OS with actual security (gasp!).\
  ![](/post/Articles/NEW/microsoft-windows%5E2001%5Ewindows-xp-home-title-screen.webp)\
  ![](/post/Articles/NEW/microsoft-windows%5E2006%5Ewindows-vista-title-screen.webp)

![](/post/Articles/NEW/microsoft-windows%5E2012%5Ewindows-8-title-screen%201.webp)

* **Windows XP, 7, 10, 11** – Evolution continues, and somewhere along the way, Microsoft decided people didn’t need to control their own computers anymore.

## Windows 3.x: The Era of Message Passing

Back in the 16-bit days, **everything** ran in the same address space. There was no such thing as process isolation; your app could poke around in another app’s memory like a nosy neighbor.

The operating system was **single-threaded**. The way we handled inter-application communication was through **message passing**.

Windows provided two main functions for message handling:

* **`GetMessage()`** – Waits for a message and retrieves it.
* **`PeekMessage()`** – Checks for a message but doesn’t wait.

### The Classic Windows Message Pump

All GUI applications had a **message pump** that looked like this:

```c
MSG msg;
while (GetMessage(&msg, NULL, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
}
```

This was the **lifeblood** of every Windows app.

### Sending Messages Between Windows Applications

Back in the Windows 3.x days, because **everything** was in the same address space, you could actually send a pointer to memory between "applications" (which were actually just different instances in the same memory space).

Here’s an example of sending a **registered Windows message** between two windows:

```c
#include <windows.h>

#define WM_MY_MESSAGE (WM_USER + 1)

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (msg == WM_MY_MESSAGE) {
        MessageBox(hwnd, "Received a custom message!", "Message", MB_OK);
        return 0;
    }
    return DefWindowProc(hwnd, msg, wParam, lParam);
}

void SendCustomMessage(HWND target) {
    SendMessage(target, WM_MY_MESSAGE, 0, 0);
}
```

This allowed communication between applications, but again, they were all **in the same process**.

## Modern Windows: Cross-Process Communication Methods

With the advent of **Windows NT** and true process isolation, sharing memory like a free-for-all became **a big no-no**. Instead, we got proper cross-process communication methods:

### 1. Named Pipes

Named pipes allow **bidirectional** communication between processes.

#### Example: Server

```c
#include <windows.h>
#include <stdio.h>

#define PIPE_NAME "\\\\.\\pipe\\MyPipe"

int main() {
    HANDLE hPipe = CreateNamedPipe(PIPE_NAME, PIPE_ACCESS_DUPLEX, PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE | PIPE_WAIT,
                                   1, 512, 512, 0, NULL);
    
    if (hPipe == INVALID_HANDLE_VALUE) {
        printf("Error creating named pipe\n");
        return 1;
    }

    printf("Waiting for client connection...\n");
    ConnectNamedPipe(hPipe, NULL);

    char buffer[512];
    DWORD bytesRead;
    ReadFile(hPipe, buffer, sizeof(buffer), &bytesRead, NULL);

    printf("Received: %s\n", buffer);

    CloseHandle(hPipe);
    return 0;
}
```

#### Example: Client

```c
#include <windows.h>
#include <stdio.h>

#define PIPE_NAME "\\\\.\\pipe\\MyPipe"

int main() {
    HANDLE hPipe = CreateFile(PIPE_NAME, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);

    if (hPipe == INVALID_HANDLE_VALUE) {
        printf("Error connecting to named pipe\n");
        return 1;
    }

    const char *message = "Hello from client!";
    DWORD bytesWritten;
    WriteFile(hPipe, message, strlen(message) + 1, &bytesWritten, NULL);

    CloseHandle(hPipe);
    return 0;
}
```

### 2. Signals (Event Objects)

Windows supports **event objects** for signaling between processes.

```c
HANDLE hEvent = CreateEvent(NULL, FALSE, FALSE, "Global\\MyEvent");

SetEvent(hEvent);
WaitForSingleObject(hEvent, INFINITE);
```

### 3. Slots (MailSlots)

Mail slots allow one-to-many communication.

**Sender:**

```c
HANDLE hSlot = CreateFile("\\\\.\\mailslot\\MySlot", GENERIC_WRITE, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
WriteFile(hSlot, "Hello", 6, NULL, NULL);
CloseHandle(hSlot);
```

**Receiver:**

```c
HANDLE hSlot = CreateMailslot("\\\\.\\mailslot\\MySlot", 0, MAILSLOT_WAIT_FOREVER, NULL);
ReadFile(hSlot, buffer, sizeof(buffer), NULL, NULL);
CloseHandle(hSlot);
```

## Conclusion

From the wild-west days of **direct memory sharing** to the **structured** world of named pipes, events, and mail slots, Windows cross-process communication has evolved significantly.

Sure, we lost the reckless fun of passing pointers between apps, but at least our programs no longer crash because some rogue app decided to overwrite half of our memory. Ah, progress.

***
