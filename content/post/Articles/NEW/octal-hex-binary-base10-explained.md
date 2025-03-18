---
title: Octal-Hex-Binary-Base10? Numbering Systems Explained
description: The wheres and whys of it..
slug: octal-hex-binary-base10-numbering-systems-explained
date: 2024-02-11
image: post/Articles/IMAGES/30.jpg
categories:
  - Algorithms
  - History
tags:
  - Binary
  - Hexadecimal
  - Octal
  - Number
  - Systems
  - Computing
  - History
draft: false
weight: 477
categories_ref:
  - Algorithms
  - History
slug_calculated: https://brianbraatz.github.io/p/octal-hex-binary-base10?-numbering-systems-explained
lastmod: 2025-03-18T18:10:03.172Z
---
# Octal-Hex-Binary-Base10? Numbering Systems Explained

## A Brief History of Computers and Number Systems

Computers, at their core, are just a bunch of tiny switches flipping on and off at incredible speeds.

Before we had the **fancy** machines of today, early computers like the **PDP-8**, **PDP-11**, and other legendary machines used primitive yet powerful techniques to store and manipulate data.

These machines had **address buses** that were only 8-bit, 12-bit, or 16-bit, meaning they could only address a limited amount of memory.

[More on the PDP-11](https://en.wikipedia.org/wiki/PDP-11)

### Wires and Binary: Why Computers Think in 1s and 0s

Inside a computer, everything is just **electricity**—either the voltage is there (`1`), or it isn’t (`0`).

Since we can't have three, four, or infinite voltage states, early engineers had to use **base 2 (binary)** to express numbers using **wires and transistors**.

For example, the number `5` in binary is:

```
5 in decimal  = 101 in binary
```

Each column represents **a power of two**:

```
(1 × 2²) + (0 × 2¹) + (1 × 2⁰) = 4 + 0 + 1 = 5
```

[More on Binary](https://en.wikipedia.org/wiki/Binary_number)

## Why Hexadecimal?

As computers got **more powerful**, engineers realized that writing everything in binary was a **nightmare**. So, they switched to **hexadecimal (base 16)** to represent numbers **in a more compact way**.

Instead of:

```
1010101010101010 (binary)
```

You could write:

```
AAAA (hex)
```

**Hexadecimal uses 16 symbols:**

```
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
```

where `A = 10`, `B = 11`, `C = 12`, and so on.

[More on Hexadecimal](https://en.wikipedia.org/wiki/Hexadecimal)

## Base 10: Because We Have 10 Fingers

Humans use **base 10** because we have **10 fingers** (unless you’re an alien from *Rendezvous with Rama*—more on that later). In decimal (base 10), each place value increases by a power of **10**:

```
234 in decimal = (2 × 10²) + (3 × 10¹) + (4 × 10⁰)
```

[More on Decimal](https://en.wikipedia.org/wiki/Decimal)

## Octal: The Forgotten Base

Octal (base **8**) was heavily used in **early computing**, especially with machines like the **PDP-8**, because it neatly fit into **3-bit chunks**. Each octal digit represents **three binary bits**:

```
Binary:  101 110
Octal:    5   6
```

That made things easier for engineers working with older computer hardware.

[More on Octal](https://en.wikipedia.org/wiki/Octal)

## Rendezvous with Rama: Aliens Who Hate Base 10?

In *Rendezvous with Rama*, Arthur C. Clarke describes a spaceship where the aliens use **base 8 (octal)** instead of base 10. Why? Because they likely had **8 fingers**, and their whole numbering system evolved around that. This makes us wonder: If humans had 12 fingers, would we be using base **12** today?

[More on Rendezvous with Rama](https://en.wikipedia.org/wiki/Rendezvous_with_Rama)

## Number Systems in Computer Graphics

Early computer graphics relied on **binary and hexadecimal** to define pixel colors, memory addresses, and image data.

For example, colors in **RGB (Red-Green-Blue)** are often represented in **hexadecimal**:

```
#FF5733  →  (255, 87, 51) in decimal
```

Each pair represents a **byte (8-bit)** value for red, green, and blue.

## Why Does This Matter in Modern Programming?

* **Hexadecimal** is widely used in memory addressing and debugging.
* **Octal** is still seen in **file permissions in Unix/Linux** (`chmod 755`).
* **Binary** is essential for **bitwise operations, encryption, and low-level programming**.

## How Does This Relate to TCP/IP Subnet Masking?

Subnet masks use **binary place values** to define network portions:

```
255.255.255.0 → 11111111.11111111.11111111.00000000 (binary)
```

Understanding binary makes **networking and IP addressing** much easier.

<!-- 

## Conclusion

Understanding numbering systems is essential for **programming, networking, graphics, and system architecture**. Whether you're **debugging code**, **configuring a network**, or **reading an old sci-fi book**, number bases are everywhere.
-->

***

## Key Ideas

| Concept               | Explanation                                             |
| --------------------- | ------------------------------------------------------- |
| Binary (Base 2)       | Used by computers, based on 1s and 0s                   |
| Hexadecimal (Base 16) | Compact way to represent binary values                  |
| Octal (Base 8)        | Used in early computers, still in UNIX file permissions |
| Decimal (Base 10)     | The system we use daily, based on our fingers           |
| Alien Math            | *Rendezvous with Rama* aliens used base 8               |
| Subnet Masking        | Uses binary place values for network segmentation       |
| Computer Graphics     | Uses hexadecimal for colors and pixel data              |

***

## Reference Links

* [Binary Numbers](https://en.wikipedia.org/wiki/Binary_number)
* [Hexadecimal](https://en.wikipedia.org/wiki/Hexadecimal)
* [Octal](https://en.wikipedia.org/wiki/Octal)
* [Decimal](https://en.wikipedia.org/wiki/Decimal)
* [PDP-11](https://en.wikipedia.org/wiki/PDP-11)
* [Rendezvous with Rama](https://en.wikipedia.org/wiki/Rendezvous_with_Rama)
* [Subnetting](https://en.wikipedia.org/wiki/Subnetwork)
