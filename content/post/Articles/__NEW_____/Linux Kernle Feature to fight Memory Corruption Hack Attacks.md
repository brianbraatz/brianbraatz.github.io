---
title: Linux Kernel-Using mseal to fight Memory Corruption  Attacks
description: mseal explained with examples and limits explored
slug: linux-kernel-mseal
date: 2024-12-15
image: post/Articles/IMAGES/linux.png
categories:
  - Linux
  - Security
tags:
  - Linux
  - Security
  - Mseal
  - Memory Corruption
  - Syscall
  - Kernel
  - Cybersecurity
draft: false
weight: 287
lastmod: 2025-02-17T03:09:01.760Z
---
# How Hackers Used Memory Corruption to Break Into Systems

Memory corruption happens when software mistakenly writes data where it shouldn’t.

Attackers love this because they can exploit it to hijack programs and make them do things they weren’t supposed to.

Some of the classic ways they do this include buffer overflows (where they overwrite important parts of memory) and return-oriented programming (ROP) (where they trick a program into executing malicious code without directly injecting new code).

These techniques let hackers take control of systems, steal data, or even run arbitrary commands.

# What mseal Does to Stop This

To shut down these kinds of attacks, the Linux kernel has introduced mseal, a new system call that lets developers "seal" memory regions, making them impossible to modify later.

The mseal system call was introduced in the Linux kernel version 6.10, released in October 2024.

The primary goal of mseal is to enhance security by mitigating memory corruption vulnerabilities that could be exploited to alter critical memory areas.

in Linux, you can change how memory behaves using things like mprotect (which changes permissions) or munmap (which removes memory mappings).

But if a memory region is "sealed" with mseal, none of these changes can happen.

This means even if an attacker gains control of a program, they can’t mess with its critical memory regions to launch an exploit.

### How Does `mseal` Work?

When you allocate memory in a program (using functions like `malloc`), the operating system maps that memory into your process's address space.

Normally, you can change the properties of this memory—like making it executable or writable—using functions like `mprotect`.

But with `mseal`, once you've sealed a memory region, any attempt to change its properties or unmap it will be met with a stern "Access Denied."

### Intro to Using `mseal`

```c
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

int main() {
    // Allocate a page of memory
    size_t page_size = getpagesize();
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    // Write some data to the memory
    snprintf((char *)mem, page_size, "Sealing this memory region!");

    // Seal the memory region
    if (mseal(mem, page_size) == -1) {
        perror("mseal");
        munmap(mem, page_size);
        exit(EXIT_FAILURE);
    }

    // Attempt to change memory protection after sealing
    if (mprotect(mem, page_size, PROT_READ) == -1) {
        perror("mprotect");
    } else {
        printf("mprotect succeeded (unexpected)\n");
    }

    // Clean up
    munmap(mem, page_size);
    return 0;
}
```

## How It Works in Practice

When a developer seals a memory region using mseal, it locks that memory in place.

The operating system refuses any future changes—you can’t modify its permissions, you can’t remove it, and you can’t remap it.

This is a game-changer because a lot of common exploits rely on being able to tweak memory settings after gaining control of a program.

If they can’t do that, their attack fails.

## Ia This Is a Big Deal for Linux Security?

With mseal, Linux is adding another strong layer of defense against memory corruption exploits.

It doesn’t solve all security issues, but it makes it much harder for attackers to pull off certain types of hacks.

Developers now have a straightforward way to lock down sensitive memory regions, preventing them from being tampered with.

This makes it significantly harder for hackers to exploit these vulnerabilities, improving overall system security.

In short, mseal is like putting a deadbolt on your memory—once it's locked, no one's getting in.

**But it does have limitations.. read on for more..**

## Details of How `mseal` Works

`mseal` works by **locking down** specific memory regions in a process so that their properties **cannot** be changed after they have been sealed.

Normally, a program (or an attacker who has gained some control over a program) can modify memory regions using system calls like:

* **`mprotect`** – Changes memory permissions (e.g., making a memory region executable or writable).
* **`munmap`** – Unmaps memory, effectively removing it.
* **`madvise`** – Can be used to discard or change how memory is handled.

With `mseal`, once a region of memory is **sealed**, the kernel **blocks any attempt to modify or unmap it**. Even if an attacker gains access to a process, they **cannot** use these system calls to manipulate memory, which is a common way exploits work.

***

## What If Low-Level Code (Assembly) Tries to Access or Modify the Memory?

Attackers often use **assembly language** or other **low-level tricks** to bypass security features, sometimes interacting directly with memory and registers.

But `mseal` is implemented **at the kernel level**, which means that even if an attacker writes assembly code or attempts to manipulate memory manually, the kernel **enforces the seal and blocks any modification attempts**.

Here’s what happens if a program (or attacker) tries to mess with sealed memory:

### 1. Direct Memory Writes (`mov` in Assembly)

* If an attacker already has access to writable memory, they could try to write into it using assembly instructions like `mov [address], value`.
* **`mseal` does not block normal memory writes**, but it ensures that **memory protection cannot be changed**. If the memory was **read-only**, an attacker cannot make it writable.

### 2. Modifying Permissions (`mprotect` in Assembly)

* Some exploits involve changing memory permissions using syscalls. Attackers could try to call `mprotect` using inline assembly or direct system call instructions.
* **`mseal` blocks `mprotect` from changing sealed memory regions**, so even if an attacker tries to make **read-only memory writable** (or vice versa), it won’t work.

### 3. Trying to Unmap Memory (`munmap` in Assembly)

* An attacker might try to remove (unmap) memory to overwrite it later.
* **Sealed memory cannot be unmapped**, so this attack method is also blocked.

### 4. Changing Memory Mappings (`mmap`, `remap` in Assembly)

* Some attacks involve **remapping** existing memory regions. For example, an attacker could try to replace a function’s code by remapping memory.
* **Sealed memory cannot be remapped**, stopping this kind of attack.

***

## How Does the Kernel Enforce `mseal`?

When a memory region is sealed, the **kernel sets a special flag (`VM_SEALED`)** on that memory region. This flag tells the Linux memory management system to **deny any future modification attempts**. Whenever a process tries to modify, unmap, or change protection on that memory, the kernel checks the flag and **immediately rejects the request**.

If an attacker (or a legitimate process) tries to bypass `mseal` by making direct system calls or using low-level assembly tricks, they **will receive an error from the kernel**. The process will either fail **silently** (the memory won’t change) or, in some cases, **trigger a segmentation fault (`SIGSEGV`)**, crashing the program.

***

## Why Is This Effective?

Many modern exploits **depend on modifying memory permissions** to execute arbitrary code. `mseal` **cuts off this path entirely** by making memory **immutable after sealing**. Even if an attacker has found a way to execute some code, they **cannot** use standard tricks to escalate their control over the system.

## `mseal`: Why You Can’t Use It Everywhere

While `mseal` is a powerful tool that **prevents memory modifications** and **shuts down common hacking techniques**, it has an important limitation: **you cannot expand or shrink a memory allocation after sealing it**.

This means that once you call `mseal`, you **lose the ability to resize memory using `mremap`**.

This restriction makes `mseal` unsuitable for many **dynamic memory allocations**, such as those used by **heap managers, JIT compilers, or applications that frequently resize memory**. Let’s break this down in detail.

***

## The Problem: `mseal` and `mremap` Are Incompatible

### **What Does `mremap` Do?**

Normally, when you allocate memory using `mmap`, you get a fixed-size region.

But sometimes, you need **more memory** or want to **shrink the allocation**. This is where `mremap` comes in.

* `mremap` lets you **resize an existing memory allocation** without needing to allocate new memory and copy data manually.
* You can **expand or shrink** an existing memory region.

**Example: Expanding a Memory Allocation Using `mremap`**

```c
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

int main() {
    size_t page_size = getpagesize();
    size_t initial_size = page_size;
    size_t expanded_size = page_size * 2;

    // Allocate 1 page of memory
    void *mem = mmap(NULL, initial_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    strcpy((char *)mem, "Hello, mmap!");

    // Expand the memory region
    void *new_mem = mremap(mem, initial_size, expanded_size, MREMAP_MAYMOVE);
    if (new_mem == MAP_FAILED) {
        perror("mremap");
        munmap(mem, initial_size);
        exit(EXIT_FAILURE);
    }

    printf("Expanded memory contains: %s\n", (char *)new_mem);

    // Clean up
    munmap(new_mem, expanded_size);
    return 0;
}
```

**What happens?**\
The memory starts as one page and expands to two pages, keeping the original data intact.

## mseal Breaks mremap

Now, what happens if we seal the memory region before calling mremap?

Sealing Memory Before Trying to Expand It

```c++
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

int main() {
    size_t page_size = getpagesize();
    size_t initial_size = page_size;
    size_t expanded_size = page_size * 2;

    // Allocate 1 page of memory
    void *mem = mmap(NULL, initial_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    strcpy((char *)mem, "Sealed memory!");

    // Seal the memory region
    if (mseal(mem, initial_size) == -1) {
        perror("mseal");
        munmap(mem, initial_size);
        exit(EXIT_FAILURE);
    }

    // Try to expand the memory region
    void *new_mem = mremap(mem, initial_size, expanded_size, MREMAP_MAYMOVE);
    if (new_mem == MAP_FAILED) {
        perror("mremap failed due to mseal");
    } else {
        printf("Unexpectedly expanded memory: %s\n", (char *)new_mem);
        munmap(new_mem, expanded_size);
    }

    return 0;
}
```

## What happens?

The mremap call fails because mseal has locked the memory, preventing any changes to its mapping.

## Why Does This Happen?

Once a memory region is sealed with mseal, the kernel marks it as immutable.

**This means:**

* No mprotect changes – You cannot change the memory’s read/write/execute permissions.
* No munmap calls – You cannot unmap the memory.
* No mremap resizing – You cannot expand or shrink it.

**mremap requires changing the mapping of the memory by either:**

* Extending the current mapping (if there’s free space).
* Moving the memory elsewhere (if it needs more space).

But since mseal locks the memory region, mremap cannot perform its operations.

The kernel enforces this restriction to prevent exploits that rely on modifying memory regions after a process has started.

## Example: Trying to Shrink Memory After Sealing

If you try to reduce a memory region after sealing it, it fails in the same way:

```c++
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

int main() {
    size_t page_size = getpagesize();
    size_t initial_size = page_size * 2;
    size_t reduced_size = page_size;

    // Allocate 2 pages of memory
    void *mem = mmap(NULL, initial_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    strcpy((char *)mem, "Memory before shrinking!");

    // Seal the memory
    if (mseal(mem, initial_size) == -1) {
        perror("mseal");
        munmap(mem, initial_size);
        exit(EXIT_FAILURE);
    }

    // Try to shrink the memory
    void *new_mem = mremap(mem, initial_size, reduced_size, MREMAP_MAYMOVE);
    if (new_mem == MAP_FAILED) {
        perror("mremap failed due to mseal");
    } else {
        printf("Unexpectedly shrunk memory: %s\n", (char *)new_mem);
        munmap(new_mem, reduced_size);
    }

    return 0;
}
```

❌ What happens? The mremap call fails because shrinking a sealed memory region is also not allowed.

## When Should You Not Use mseal?

**Since mseal makes memory completely immutable, you should avoid using it in:**

* Dynamic memory allocations – Programs that frequently resize memory will break.
* Heap management – Memory allocators like malloc rely on resizing or reusing memory.
* JIT compilers – Just-in-time compilers need to generate and modify memory.
* Shared memory regions – If memory needs to be dynamically changed by multiple processes.

## When Is mseal Useful?

**mseal is great when:**

* You want to lock down critical memory (e.g., security-sensitive structures).
* Prevent attackers from modifying memory permissions (stopping many exploits).
* Protect executable code regions from being tampered with.

<!-- 
============
## Introduction

Ah, Linux—the operating system that makes you feel like a wizard typing spells into a terminal. 

But even wizards need protection from dark forces, and in the digital realm, those dark forces are hackers exploiting memory corruption vulnerabilities. 

Fear not, for the Linux kernel developers have donned their armor and introduced a shiny new syscall: `mseal`. This bad boy is here to slam the door on a whopping 70% of hacks that have plagued systems over the past two decades. 

he video talks about a new Linux security feature called mseal, which is designed to stop a huge chunk of hacks—around 70% of them—that have been happening for the last 20 years. These hacks mostly come from something called memory corruption, which is when a program accidentally messes up its own memory in a way that attackers can take advantage of.


What mseal Does to Stop This
To shut down these kinds of attacks, the Linux kernel has introduced mseal, a new system call that lets developers "seal" memory regions, making them impossible to modify later. Normally, in Linux, you can change how memory behaves using things like mprotect (which changes permissions) or munmap (which removes memory mappings). But if a memory region is "sealed" with mseal, none of these changes can happen. This means even if an attacker gains control of a program, they can’t mess with its critical memory regions to launch an exploit.

How It Works in Practice
When a developer seals a memory region using mseal, it locks that memory in place. The operating system refuses any future changes—you can’t modify its permissions, you can’t remove it, and you can’t remap it. This is a game-changer because a lot of common exploits rely on being able to tweak memory settings after gaining control of a program. If they can’t do that, their attack fails.

Why This Is a Big Deal for Linux Security
With mseal, Linux is adding another strong layer of defense against memory corruption exploits. It doesn’t solve all security issues, but it makes it much harder for attackers to pull off certain types of hacks. Developers now have a straightforward way to lock down sensitive memory regions, preventing them from being tampered with. This makes it significantly harder for hackers to exploit these vulnerabilities, improving overall system security.

In short, mseal is like putting a deadbolt on your memory—once it's locked, no one's getting in.

===

## Key Points on 'mseal' 
* mseal completely locks memory regions, making them unmodifiable.
* Once you seal a memory region, you cannot use mremap to expand or shrink it.
* This makes mseal unsuitable for dynamic memory but great for security-critical memory.
* If your application relies on resizing memory, do not use mseal. 
    * But if you need absolute protection against memory modifications, it’s one of the strongest defenses available in Linux today.


# Should You Use `mseal` in a Device Drivers?

The short answer: **probably not in most cases, but it depends on what the driver does**.

---

## **Why Would You Consider Using `mseal` in a Device Driver?**
Device drivers interact directly with hardware and often **manage critical memory regions** that should not be modified during runtime. 

If a driver **allocates memory that should remain static and untouchable**, `mseal` **could** be useful to ensure its integrity.

Some potential **use cases for `mseal` in a driver** include:

1. **Securing DMA Buffers**  
   - If your driver sets up **Direct Memory Access (DMA)** buffers, you might want to **seal them** after allocation to prevent any unauthorized modifications.
   - Example: A **network card driver** may allocate a buffer for packet processing and want to prevent user space from tampering with it.

2. **Protecting Memory-Mapped I/O (MMIO) Regions**  
   - Some devices map registers into memory. If a driver has a **fixed mapping for hardware registers**, `mseal` could be used to **ensure the mapping is never changed** after initialization.
   - Example: A GPU driver setting up **memory-mapped command buffers** for rendering tasks.

3. **Preventing Kernel Exploits**  
   - Drivers are common attack surfaces. If a driver **allocates security-sensitive memory** (e.g., kernel metadata, cryptographic keys), `mseal` could **make sure it never gets tampered with**.

---

## **Why `mseal` Is Usually Not Used in Drivers**
Despite its potential benefits, **device drivers rarely use `mseal`** for several reasons:

### **1. Device Drivers Often Need to Modify Memory**
- Many drivers **allocate memory dynamically** and **adjust permissions** during operation.
- `mseal` **prevents all modifications**, meaning:
  - You **cannot** change memory protection (`mprotect`).
  - You **cannot** remap or resize memory (`mremap`).
  - You **cannot** unmap or replace memory (`munmap`).

➡ **Example Problem**:  
A **graphics driver** dynamically allocates and resizes video memory for different workloads. If `mseal` were applied, the driver **could not expand or free memory**, leading to **failures or performance issues**.

### **2. Drivers Often Need to Free and Reuse Memory**
- Device drivers **frequently allocate and release memory** as needed.
- If you seal memory with `mseal`, **you can never free it**.
- This can **cause memory leaks** and **waste resources**.

➡ **Example Problem**:  
A **WiFi driver** allocates temporary buffers for packets. If `mseal` were used, those buffers **could not be freed or reused**, leading to unnecessary memory consumption.

### **3. Hardware Often Needs Access to Memory**
- Some devices require **modifying memory regions dynamically**.
- `mseal` **locks down memory**, which could interfere with **hardware operations**.

➡ **Example Problem**:  
A **sound card driver** allocates a ring buffer for audio playback. Since the buffer needs to be **updated constantly**, sealing it with `mseal` would **break playback**.

### **4. The Kernel Already Has Stronger Protections**
- Linux **already has** memory protection mechanisms like:
  - **Memory barriers** (to enforce order of memory accesses).
  - **DMA memory protection** (to prevent unauthorized access).
  - **SELinux & AppArmor** (to restrict memory access at the OS level).
- These mechanisms **provide more flexibility than `mseal`** while still ensuring security.

---

## **Example: When You Might Use `mseal` in a Driver**
If you **must** use `mseal` in a driver, it would likely be for **memory regions that should never be modified after initialization**.

Here’s an example:

```c
#include <linux/module.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/mm.h>
#include <linux/mman.h>

static char *secure_buffer;

static int __init my_driver_init(void) {
    // Allocate memory
    secure_buffer = kmalloc(4096, GFP_KERNEL);
    if (!secure_buffer) {
        pr_err("Failed to allocate memory\n");
        return -ENOMEM;
    }

    // Fill memory with sensitive data
    strcpy(secure_buffer, "Critical data that must never be modified!");

    // Seal the memory to prevent any modifications
    if (mseal(secure_buffer, 4096) == -1) {
        pr_err("mseal failed\n");
        kfree(secure_buffer);
        return -EFAULT;
    }

    pr_info("Memory sealed successfully!\n");
    return 0;
}

static void __exit my_driver_exit(void) {
    // Memory cannot be freed after sealing, so this is pointless:
    kfree(secure_buffer);
    pr_info("Driver exiting...\n");
}

module_init(my_driver_init);
module_exit(my_driver_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Linux Dev");
MODULE_DESCRIPTION("Example of using mseal in a device driver");

```

## Why This Is a Bad Idea?
*  After sealing the memory, kfree(secure_buffer) will not work, leading to a memory leak.
* If the driver needs to modify the data later, it won’t be able to.
* If the system runs out of memory, it cannot reclaim the sealed region, which reduces efficiency.

## Should You Use `mseal` in a Driver?

| **Use Case**                        | **Should You Use `mseal`?** | **Why?**                                        |
|--------------------------------------|----------------------------|-------------------------------------------------|
| **Static security-sensitive memory** | ✅ Yes (Rare Cases)        | Protects data like cryptographic keys          |
| **Memory-mapped I/O (MMIO)**         | ✅ Yes (With Caution)      | Can prevent changes to mapped registers        |
| **Dynamically allocated buffers**    | ❌ No                      | Buffers often need resizing & freeing          |
| **Memory that must be accessed by hardware** | ❌ No               | Hardware may need to modify the memory        |
| **Heap or dynamically growing structures** | ❌ No               | `mseal` prevents `mremap` and resizing        |

## When Is `mseal` Useful?

`mseal` is great when:

- You want to **lock down critical memory** (e.g., security-sensitive structures).
- Prevent **attackers from modifying memory permissions** (stopping many exploits).
- Protect **executable code regions** from being tampered with.

## Final-FINAL!!! (final?) Thoughts

- **`mseal` can be useful in rare cases** where memory **must never change**.
- Most **device drivers need flexibility** to allocate, resize, and free memory—**which `mseal` prevents**.
- **Linux already provides other security mechanisms** that are **better suited** for protecting memory in drivers.

-->
