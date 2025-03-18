---
title: Buffer Overrun Attacks and How to Prevent Them
description: In Python, C#, and Go
slug: history-of-buffer-overrun-attacks
date: 2021-10-10
image: post/Articles/IMAGES/bufferoverflow.png
categories:
  - Cybersecurity
  - Programming
  - Python
  - C#
  - Go
  - Security
  - Assembly Language
  - WinApi
tags:
  - Cybersecurity
  - Buffer Overflow
  - Memory Safety
  - Python
  - C#
  - Go
  - Security Practices
draft: false
weight: 89
categories_ref:
  - Cybersecurity
  - Programming
  - Python
  - C#
  - Go
  - Security
  - Assembly Language
  - WinApi
slug_calculated: https://brianbraatz.github.io/p/history-of-buffer-overrun-attacks-and-how-to-prevent-them-in-python-csharp-and-go
lastmod: 2025-03-18T22:46:26.606Z
---
# History of Buffer Overrun Attacks and How to Prevent Them in Python, C#, and Go

Ah, buffer overruns—one of the oldest and most infamous programming blunders in the history of software development. If memory bugs had a "Most Wanted" list, buffer overruns would be at the top, right next to null pointer dereferences and accidentally deleting production databases.

## What is a Buffer Overrun?

A buffer overrun (or buffer overflow) happens when a program writes more data into a buffer than it can hold.

The excess data then spills over into adjacent memory, leading to corruption, crashes, and—if a hacker is feeling spicy—remote code execution. Sounds bad? It is. Real bad.

This type of vulnerability has been around since the dawn of programming, and it's responsible for some of the biggest cybersecurity disasters ever.

If you've ever wondered why your C program segfaults randomly, you might be looking at a baby buffer overflow.

[Wikipedia Buffer Overflow](https://en.wikipedia.org/wiki/Buffer_overflow).

## A Brief and Ridiculous History of Buffer Overruns

Buffer overruns have been around longer than your average internet meme. Let's go on a journey through time:

* **The 1980s**: Buffer overruns were just getting started. Early operating systems like UNIX were riddled with these vulnerabilities. Hackers were just beginning to understand how they could exploit them.

* **The 1990s**: The golden era of buffer overflows. Thanks to languages like C and C++, which basically tell programmers, "Do whatever you want, I trust you," buffer overruns became a hacker's best friend. The infamous 1988 **Morris Worm**, one of the first-ever worms, exploited a buffer overflow.

* **The 2000s**: The Internet boom meant more software, more security flaws, and more high-profile attacks. The 2001 **Code Red** and **Nimda** worms used buffer overflows to wreak havoc.

* **The 2010s**: Memory-safe languages like Python and Go started gaining popularity, helping prevent buffer overruns. But legacy code in C and C++ continued to haunt the world.

* **Today**: Buffer overruns still exist. Modern tools like AddressSanitizer and memory-safe languages have made them rarer, but in old-school systems, they're still a problem.

## How Buffer Overruns Work (For the Curious and Brave)

Imagine your program is a house, and your buffer is a glass of water. If you pour too much, it overflows onto the floor, ruining your carpet. In computing, that "spilled water" is extra data overwriting whatever is next in memory.

In C, this happens easily:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char buffer[10];
    strcpy(buffer, "This string is way too long!");
    printf("Buffer: %s\n", buffer);
    return 0;
}
```

Oops. `strcpy` doesn't care if the string fits. It just pours in all the data, even if it floods everything in its path.

## SO what ? IT ran over the buffer.. big deal or no?

Imagine that I put some chars in a string that overrun a buffer on a webserver... And those chars.. the bytes of them , will be interpreted as Assembly language.

Finding and leveraging these exploits was alot of work. But it paid off for the hacker because they can literally deliver executable assembly languge directly into someone elses sevrer. .

Usually that code would try to do something like make an admin account with a password the hacker knew.. Once the hacker could get "in" to the system with admin rights, thats where they could cause the real damage..

In the 2000s, (and still now - but systems were more vulnerable then ). Hackers would write scripts to automatically go out and "own" a bunch of servers.. and then install software on the server to sit and wait for instructions.

Then if the hacked wanted to Denial of Service attack someone, he would send instructions to his compromised systems telling them to attack.

All of sudden your grandmother's computer in the den is visciously attacking amazon.com...

## Preventing Buffer Overruns in Python, C#, and Go

### **Python: The Chill, Memory-Safe Language**

Python is mostly immune to buffer overruns because it manages memory automatically. You don’t manually allocate memory for strings or arrays; Python just handles it. But beware—Python code that interfaces with C (like using `ctypes`) can still be vulnerable.

If you're using C extensions or `ctypes`, **always** check buffer sizes before copying data.

```python
import ctypes

buffer = ctypes.create_string_buffer(10)
# Safe: Limiting string length
ctypes.memmove(buffer, b"Hello", 5)
```

### **C#: The Managed Middle Ground**

C# runs on the .NET runtime, which means memory management is mostly handled for you. But, if you decide to go full metal and use **unsafe code**, you're asking for trouble.

```csharp
unsafe {
    char* buffer = stackalloc char[10];
    for (int i = 0; i < 15; i++) // Whoops, buffer is only 10!
    {
        buffer[i] = 'A';
    }
}
```

**Prevention tips:**

* Stick to managed memory whenever possible.
* If you must use unsafe code, use **Span<T>** or **SafeHandle** for safety.

### **Go: The Modern Memory-Safe Champion**

Go has built-in memory safety features, so buffer overflows are rare. However, if you're using **cgo** (calling C code), you can still fall into this trap.

```go
package main

/*
#include <string.h>
#include <stdio.h>
*/
import "C"
import "unsafe"

func main() {
    buffer := make([]byte, 10)
    str := "This is too long!"
    
    C.strcpy((*C.char)(unsafe.Pointer(&buffer[0])), C.CString(str)) // Danger!
}
```

**Prevention tips:**

* Avoid **unsafe.Pointer** unless absolutely necessary.
* Use Go’s built-in slices and copy functions instead of C functions like `strcpy`.

## Conclusion: Stay Safe, Code Smart

Buffer overruns have a long history of causing chaos, but modern programming languages and tools make them easier to prevent. If you're writing in Python, C#, or Go, you’re already in a better spot than old-school C programmers. But if you're working with legacy C or C++ code, always be mindful of memory safety!

### Key Ideas Table

| Concept        | Explanation                                                         |
| -------------- | ------------------------------------------------------------------- |
| Buffer Overrun | When data overflows a buffer, overwriting adjacent memory.          |
| Memory Safety  | Ensuring a program does not accidentally access memory incorrectly. |
| Python         | Memory-safe but can still have issues when interfacing with C.      |
| C#             | Managed memory but can still have unsafe operations.                |
| Go             | Strong memory safety unless using `unsafe.Pointer`.                 |

### References

* [Buffer Overflow - Wikipedia](https://en.wikipedia.org/wiki/Buffer_overflow)
* [Morris Worm](https://en.wikipedia.org/wiki/Morris_worm)
* [Code Red Worm](https://en.wikipedia.org/wiki/Code_Red_\(computer_worm\))
* [Go unsafe package](https://pkg.go.dev/unsafe)
