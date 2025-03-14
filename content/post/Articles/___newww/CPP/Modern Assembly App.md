---
title: Windows Hello World In Assembly??? in 2019?
description: Would you WANT TO? .. YES! I DO!
slug: windows-api-hello-world-in-assembly-in-2019
date: 2025-12-18
image: post/Articles/IMAGES/Motorola_6800_Assembly_Language.png
categories:
  - Windows
  - Assembly
  - WinAPI
  - Assembly Language
  - WinApi
tags:
  - Windows
  - Assembly
  - WinAPI
  - Programming
  - MessageBox
  - GUI
draft: false
weight: 567
categories_ref:
  - Windows
  - Assembly
  - WinAPI
  - Assembly Language
  - WinApi
lastmod: 2025-03-14T15:45:27.585Z
---
# Can you write a Windows API Hello World In Assembly in 2019?

Short answer?

Yes. Long answer?

BUT Why would you do this to yourself?

HAHA

Because its cool..

Actually In the wee days of Windows 3.1 we wrote mostly in C.. And occasionally dipped in to ASM as needed for performance.

Computers were slower back then, so we had to really push things sometimes..

Like not just for fun.. Our computers were much slower.. :)

So lets see if we can do some  real bare-metal, old-school, hair-pulling, Windows API madness.

***

## 🛠️ Tools You’ll Need

Before we get into the weeds, let’s make sure you have the right tools. You can't just yell at the computer in assembly and expect it to listen.

* **MASM (Microsoft Macro Assembler)** – Comes with Visual Studio, or you can grab it separately.
* **GoLink** – A free linker that plays well with assembly.
* **GoASM** – If you prefer it over MASM.
* **Windows SDK** – Because we’re working with Windows API.
* **A solid cup of coffee** – Trust me, you’ll need it.

***

## ✍️ The Assembly Code (MASM Style)

Here it is—the moment you've been waiting for. Our lovely **Windows GUI Hello World** in assembly:

```assembly
.386
.model flat, stdcall
option casemap:none

include windows.inc
include user32.inc
include kernel32.inc
include gdi32.inc

includelib user32.lib
includelib kernel32.lib
includelib gdi32.lib

.DATA
    AppName db "HelloWin",0
    ClassName db "WinClass",0
    WindowText db "WinAPI ASM",0
    LabelText db "Name:",0
    ButtonText db "Say Hello",0
    NameBuffer db 256 dup(0)

    hInstance dd ?
    hEdit dd ?
    hWnd dd ?

.CODE

start:
    invoke GetModuleHandle, NULL
    mov hInstance, eax

    invoke LoadIcon, 0, IDI_APPLICATION
    invoke LoadCursor, 0, IDC_ARROW

    ; Register Window Class
    invoke RegisterClass, addr wc
    invoke CreateWindowEx, 0, addr ClassName, addr WindowText, \
        WS_OVERLAPPEDWINDOW, 100, 100, 300, 200, NULL, NULL, hInstance, NULL
    mov hWnd, eax

    ; Create Label
    invoke CreateWindowEx, 0, addr LabelText, addr LabelText, WS_VISIBLE or WS_CHILD, \
        20, 20, 50, 20, hWnd, NULL, hInstance, NULL

    ; Create Edit Box
    invoke CreateWindowEx, WS_EX_CLIENTEDGE, addr LabelText, NULL, \
        WS_VISIBLE or WS_CHILD or WS_BORDER or ES_AUTOHSCROLL, \
        80, 20, 150, 20, hWnd, addr hEdit, hInstance, NULL

    ; Create Button
    invoke CreateWindowEx, 0, addr ButtonText, addr ButtonText, WS_VISIBLE or WS_CHILD, \
        80, 50, 100, 30, hWnd, 1, hInstance, NULL

    ; Show the Window
    invoke ShowWindow, hWnd, SW_SHOWNORMAL
    invoke UpdateWindow, hWnd

    ; Message loop
msg_loop:
    invoke GetMessage, addr msg, NULL, 0, 0
    cmp eax, 0
    je exit_program

    invoke TranslateMessage, addr msg
    invoke DispatchMessage, addr msg
    jmp msg_loop

exit_program:
    invoke ExitProcess, 0

; Window Procedure
WndProc proc hWnd:DWORD, uMsg:DWORD, wParam:DWORD, lParam:DWORD
    cmp uMsg, WM_COMMAND
    je process_command

    cmp uMsg, WM_DESTROY
    je close_window

    invoke DefWindowProc, hWnd, uMsg, wParam, lParam
    ret

process_command:
    cmp wParam, 1
    jne def_proc

    ; Get text from input field
    invoke GetWindowText, hEdit, addr NameBuffer, 255
    invoke MessageBox, hWnd, addr NameBuffer, addr WindowText, MB_OK
    ret

close_window:
    invoke PostQuitMessage, 0
    ret

def_proc:
    invoke DefWindowProc, hWnd, uMsg, wParam, lParam
    ret

WndProc endp

end start
```

***

## 🚀 What This Code Does

1. **Registers a window class** – Because Windows likes bureaucracy.
2. **Creates the main window** – Where all the magic happens.
3. **Creates a label, text field, and button** – The UI elements.
4. **Handles button clicks** – Reads text from the input field and pops up a `MessageBox`.
5. **Runs the Windows message loop** – Because Windows loves messages.

***

## 💡 Key Ideas

| Concept      | Explanation                                           |
| ------------ | ----------------------------------------------------- |
| Windows API  | We use the Windows API to create a GUI directly.      |
| MASM Syntax  | Assembly language specific to Windows programming.    |
| Message Loop | Windows apps run in an event loop.                    |
| UI Elements  | Created using `CreateWindowEx`.                       |
| MessageBox   | Displays the user's input after clicking "Say Hello". |

***

***

title: "Windows API Hello World: Assembly vs C"\
description: "A deep dive into Windows API Hello World in Assembly and C, complete with code explanations, instruction breakdowns, and comparisons."\
slug: "windows-api-hello-world-assembly-vs-c"\
date: 2025-12-03\
image: "post/Articles/IMAGES/45.jpg"\
categories: \[]\
tags: \["Windows", "Assembly", "WinAPI", "Programming", "C", "Low-Level Programming"]\
draft: false\
weight: 723
-----------

# Windows API Hello World: Assembly vs C

In our last adventure, we went full *mad scientist* and wrote a **Windows API Hello World GUI** *entirely in assembly language*. Now, if you actually *ran* that code, congratulations—you are braver than most.

But let’s be real. **What exactly is happening in that assembly code?** And how does it compare to the *same program written in C*?

Strap in! We’re about to dissect the assembly code, list the key **assembly instructions**, build a **Windows C version**, and **compare** the two.

***

## 🔍 Breaking Down the Assembly Code

If you didn’t run away the first time you saw the assembly code, you probably noticed it’s doing a few key things:

1. **Setting up the application** – Registering a window class and creating a main window.
2. **Creating UI elements** – Label, text field, and button.
3. **Handling Windows messages** – Because Windows apps are event-driven.
4. **Processing button clicks** – Fetching input text and displaying a message box.

Now, let’s break this beast down.

***

### 🏗️ Step 1: Setting Up the Window Class

```assembly
invoke GetModuleHandle, NULL
mov hInstance, eax
```

* `GetModuleHandle` fetches the instance handle of the application.
* `mov hInstance, eax` stores it for later use.

```assembly
invoke RegisterClass, addr wc
```

* Registers a **window class** with Windows, telling it what kind of window we want.

***

### 🖼️ Step 2: Creating the Window and UI Elements

```assembly
invoke CreateWindowEx, 0, addr ClassName, addr WindowText, \
    WS_OVERLAPPEDWINDOW, 100, 100, 300, 200, NULL, NULL, hInstance, NULL
mov hWnd, eax
```

* This **creates the main window** and stores the handle in `hWnd`.

We repeat this process for:

* **Label:** `"Name:"`
* **Text Field:** `ES_AUTOHSCROLL` (allows text input)
* **Button:** `"Say Hello"`

***

### 🔄 Step 3: The Windows Message Loop

```assembly
msg_loop:
    invoke GetMessage, addr msg, NULL, 0, 0
    cmp eax, 0
    je exit_program

    invoke TranslateMessage, addr msg
    invoke DispatchMessage, addr msg
    jmp msg_loop
```

* **`GetMessage`** retrieves messages from the Windows event queue.
* **`TranslateMessage`** and **`DispatchMessage`** send them to the correct window procedure.

***

### 🖱️ Step 4: Handling Button Clicks

```assembly
cmp wParam, 1
jne def_proc
invoke GetWindowText, hEdit, addr NameBuffer, 255
invoke MessageBox, hWnd, addr NameBuffer, addr WindowText, MB_OK
ret
```

* If the button is clicked (`wParam == 1`), we:
  * **Get text from the input field**
  * **Display it in a message box**

***

## 🔢 Assembly Instructions Used- What they do

| Instruction | Meaning                                                 |
| ----------- | ------------------------------------------------------- |
| `mov`       | Moves data from one register/memory location to another |
| `cmp`       | Compares two values                                     |
| `jne`       | Jumps to a label if values are **not equal**            |
| `je`        | Jumps to a label if values **are equal**                |
| `invoke`    | Calls a Windows API function                            |
| `jmp`       | Jumps unconditionally to a label                        |
| `ret`       | Returns from a function/procedure                       |

***

## 📝 The Same Program in C (WinAPI)

Now, let’s rewrite this in **C** to see how much easier our lives could have been.

```c
#include <windows.h>

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
HINSTANCE hInst;
HWND hEdit;

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow) {
    WNDCLASS wc = { 0 };
    wc.lpfnWndProc = WndProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = "WinClass";

    RegisterClass(&wc);
    HWND hWnd = CreateWindow("WinClass", "WinAPI C", WS_OVERLAPPEDWINDOW,
                             100, 100, 300, 200, NULL, NULL, hInstance, NULL);
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);

    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return msg.wParam;
}
```

***

## ⚖️ C vs Assembly: The Showdown

| Feature               | Assembly         | C                             |
| --------------------- | ---------------- | ----------------------------- |
| **Readability**       | Nightmare fuel   | Much easier to follow         |
| **Lines of Code**     | A ton            | Half as much                  |
| **Memory Control**    | Absolute control | Higher-level, but less hassle |
| **Performance**       | Slightly faster  | Still fast enough             |
| **Ease of Debugging** | Painful          | Debugging tools help a lot    |
| **Portability**       | Not portable     | C is far more portable        |

### 🏆 Verdict

* **Use Assembly** if you want **total control** over performance, memory, and execution.
* **Use C** if you want **readable, maintainable, and scalable** code.

***

## 🔗 References

* [MASM Documentation](https://docs.microsoft.com/en-us/cpp/assembler/masm/microsoft-macro-assembler-reference)
* [WinAPI Programming in C](https://docs.microsoft.com/en-us/windows/win32/api/)
* [GoLink and GoASM](https://www.godevtool.com/)
* [Windows API Functions](https://docs.microsoft.com/en-us/windows/win32/api/)
* [Assembly Programming on Windows](https://masm32.com/)

***

## 🎉 Wrapping Up

If you made it this far, congrats—you’re either a **mad genius** or you really love punishment.

Writing **Windows GUI apps in Assembly** is **painful but educational**.

C, on the other hand, is **way more practical**.

Would I recommend doing this in Assembly? **Nope.**

But is it *awesome*?

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\
**Absolutely**\
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

If you actually run this and it works, you deserve a **medal** (or at least a cookie).
