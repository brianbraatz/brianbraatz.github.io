---
title: C++ Static vs Dynamic Linking on Windows and Linux
description: C++ Static vs Dynamic Linking on Windows and Linux
slug: cpp-static-vs-dynamic-linking-on-windows-and-linux
date: 2019-06-08
image: post/Articles/IMAGES/downydryer.png
categories:
  - C++
  - Static Linking
  - Dynamic Linking
  - Windows
  - Linux
  - DLL Hell
  - Shared Libraries
  - GPL
  - Versioning
  - Build Systems
tags:
  - C++
  - Static
  - Linking
  - Dynamic
  - Linking
  - Windows
  - Linux
  - DLL
  - Hell
  - Shared
  - Libraries
  - GPL
  - Versioning
  - Build
  - Systems
draft: false
weight: 48
categories_ref:
  - C++
  - Static Linking
  - Dynamic Linking
  - Windows
  - Linux
  - DLL Hell
  - Shared Libraries
  - GPL
  - Versioning
  - Build Systems
lastmod: 2025-03-14T15:45:18.898Z
---
[How To prevent Static Cling :) ](https://downy.com/en-us/fabric-softener-tips/how-to-use-downy/how-to-use-dryer-sheets)

## Static vs Dynamic Linking: The Basics

### Static Linking

Static linking means that all required libraries are bundled directly into your executable at **compile-time**. This makes deployment easier since your program doesn’t rely on external dependencies at runtime.

**Pros:**

* No missing DLLs or shared libraries—your program has everything it needs!
* Faster load times because no external lookups are needed.
* Less risk of versioning issues since everything is baked in.

**Cons:**

* Bigger executable sizes (since it contains all the library code).
* More memory usage because each process loads its own copy of the library.
* Updating the library means **recompiling** everything that depends on it.

### Dynamic Linking

Dynamic linking, on the other hand, means that your program relies on **shared libraries** (`.so` on Linux, `.dll` on Windows) that are loaded at **runtime**.

**Pros:**

* Smaller executable size.
* Memory savings—multiple programs can share the same library in RAM.
* Libraries can be updated without recompiling dependent applications.

**Cons:**

* If a required shared library is missing, your program **won’t run**.
* Version mismatches can lead to **runtime crashes**.
* Debugging can be a nightmare.

Now that we’ve covered the basics, let’s compare how Windows and Linux handle this.

***

## Windows vs. Linux: The Linker Showdown

| Feature                   | Windows (`.dll`)                                     | Linux (`.so`)                           |
| ------------------------- | ---------------------------------------------------- | --------------------------------------- |
| Default Linking           | Dynamic (`.dll`)                                     | Dynamic (`.so`)                         |
| Static Library Extension  | `.lib`                                               | `.a`                                    |
| Dynamic Library Extension | `.dll`                                               | `.so`                                   |
| Symbol Resolution         | **Explicitly** via `LoadLibrary` or `GetProcAddress` | **Automatically** via `ld.so`           |
| Versioning                | **Manual** (e.g., DLLs with version numbers)         | **Automatic** (`soname` and `ldconfig`) |
| Common Issue              | **DLL Hell**                                         | **Missing `.so` files**                 |

### Windows: Living with DLLs

Windows relies heavily on **DLLs (Dynamic Link Libraries)**. The big issue here is **DLL Hell**, which happens when:

* Different applications need different versions of the same DLL.
* A system update replaces a DLL, breaking older programs.
* Multiple copies of the same DLL exist, leading to chaos.

Windows developers have fought back with:

* **Side-by-side (SxS) assemblies** (basically, "versioned" DLLs).
* **Redistributable packages** (e.g., the Visual C++ Redistributable).
* **Static linking** (when you don’t want to deal with DLLs at all).

### Linux: Shared Libraries and `.so` Files

Linux handles shared libraries using `.so` (Shared Object) files. Instead of dumping them in a system folder like Windows, Linux uses:

* `/usr/lib` and `/usr/local/lib` for system-wide libraries.
* `LD_LIBRARY_PATH` for user-defined locations.
* **Versioning with `soname`** (e.g., `libmylib.so.1` ensures compatibility).

Linux users still face missing `.so` issues, but **package managers** (`apt`, `dnf`, etc.) make installing dependencies easier.

***

## DLL Hell vs. Shared Library Chaos

Windows developers suffer from **DLL Hell**, while Linux users encounter **dependency nightmares**.

**DLL Hell** happens when:

* A DLL version change breaks compatibility.
* A missing DLL prevents an app from running.
* Multiple apps need different versions of the same DLL.

**Shared Library Chaos** happens on Linux when:

* A `.so` file is missing or outdated.
* The system linker (`ld.so`) can't find the correct version.
* A program requires a very specific library version.

Both problems are why **static linking** is sometimes preferred, even if it makes binaries larger.

***

## How the GPL Ties into All of This

If you think linking is just a technical issue, think again! The **GNU General Public License (GPL)** has some strong opinions about it.

### Static Linking and the GPL

* If you **statically link** to a GPL-licensed library, your entire program is now considered a **derivative work** and **must** also be GPL.
* This means you must **release your source code** under the GPL.

### Dynamic Linking and the GPL

* If you **dynamically link** to a GPL library, things get a little **murky**.
* Some argue that since the library is a separate file, your code **isn’t** a derivative work.
* The **LGPL (Lesser GPL)** was created to allow dynamic linking **without** forcing your entire app to be GPL.

### How Companies Work Around This

* They use **MIT/BSD-licensed alternatives** to avoid GPL headaches.
* They provide **plugin-based architectures** where users can load libraries dynamically.
* They **release open-source versions** to comply with the GPL but sell commercial versions with different licenses.

This is why some proprietary software **avoids GPL libraries entirely**—they don’t want to risk their code becoming **"infected"** by the GPL.

***

## Key Takeaways

| Topic               | Summary                                                                   |
| ------------------- | ------------------------------------------------------------------------- |
| **Static Linking**  | Bundles everything into the executable (bigger size, fewer dependencies). |
| **Dynamic Linking** | Uses shared libraries at runtime (smaller size, version issues).          |
| **Windows Linking** | Uses `.dll` files, but suffers from **DLL Hell**.                         |
| **Linux Linking**   | Uses `.so` files, with **automatic versioning** but dependency issues.    |
| **GPL & Linking**   | Statically linking to a GPL library means **your app must be GPL too**.   |

***

## References

* [Windows DLL Documentation](https://learn.microsoft.com/en-us/windows/win32/dlls/dynamic-link-libraries)
* [Linux Shared Library Docs](https://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html)
* [GNU GPL and Linking](https://www.gnu.org/licenses/gpl-faq.en.html#GPLStaticVsDynamic)
