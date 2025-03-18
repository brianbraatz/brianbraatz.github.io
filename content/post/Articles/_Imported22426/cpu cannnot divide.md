---
title: Your CPU Cannot Divide! At Least the Old Ones
description: Your CPU Cannot Divide! At Least the Old Ones
slug: your-cpu-cannot-divide-at-least-the-old-ones
date: 2016-11-09
image: post/Articles/IMAGES/42.jpg
categories:
  - CPU
  - Binary
  - Bitshifting
  - Division
  - Programming
  - C++
  - Python
  - Computing
  - ALU
  - Assembly Language
  - History
tags:
  - CPU
  - Binary
  - Bitshifting
  - Division
  - Programming
  - C++
  - Python
  - Computing
  - ALU
draft: false
weight: 637
categories_ref:
  - CPU
  - Binary
  - Bitshifting
  - Division
  - Programming
  - C++
  - Python
  - Computing
  - ALU
  - Assembly Language
  - History
slug_calculated: https://brianbraatz.github.io/p/your-cpu-cannot-divide-at-least-the-old-ones
lastmod: 2025-03-14T16:40:24.560Z
---
# Your CPU Cannot Divide! At Least the Old Ones

## Wait, What? My CPU Can't Divide?

Yes! At least, if we’re talking about the **really** old ones.

Back in the day, CPUs didn’t have a fancy division instruction. Instead, they had to **fake it** using binary shifts and subtraction.

Think of it like trying to cut a pizza with a spoon—possible, but far from efficient.

Modern CPUs, however, have **hardware division units** that do the job properly. But before we dive into those, let’s rewind the clock and see how the **ancient silicon warriors** handled division.

***

## How Early CPUs Used Binary Shifting to Divide

Before CPUs had division instructions, they relied on **bit shifting** and **subtraction** to approximate division.

Since computers use **binary numbers** (0s and 1s), shifting a number to the **right (`>>`) is equivalent to dividing by two**, while shifting **left (`<<`) multiplies by two**.

### Example: Shifting Right to Divide

Let’s take `8` in binary (`1000`):

```
1000  (8 in binary)
↓ shift right by 1
0100  (4 in binary, which is 8 / 2)
```

Shift it again:

```
0100  (4 in binary)
↓ shift right by 1
0010  (2 in binary, which is 4 / 2)
```

And just like that, **we divided by two using just bit shifts!**

But what if we want to divide by a number that **isn’t** a power of two? Well, CPUs had to get creative, using **subtraction loops** and bit-shifting tricks.

Imagine trying to share 23 apples between 5 people **without using division**. You’d keep handing out apples one-by-one or in chunks. Early CPUs did something similar—subtracting multiple times while adjusting the divisor using bit shifts.

This method worked, but it was **slow**—like downloading a movie on dial-up.

***

## How Modern CPUs Handle Division

Today’s CPUs don’t have to play the bit-shifting game (well, mostly). Instead, they have **dedicated division circuits** inside the **Arithmetic Logic Unit (ALU)**.

These circuits perform division in **hardware**, making it significantly faster.

However, **division is still one of the slowest operations in modern CPUs** compared to addition and multiplication. Some processors even **approximate division** using reciprocal multiplication (`x * (1/y)`) because it’s faster.

Moral of the story? **Even modern CPUs aren’t thrilled about division.** They just put up with it.

***

## How to Divide Using Bit Shifting in C++ and Python

Let’s simulate how old CPUs performed division **without using the division operator**.

### C++ Example: Division with Bit Shifting

```cpp
#include <iostream>
using namespace std;

int divide(int dividend, int divisor) {
    int quotient = 0;
    int temp = 1;

    while (divisor <= dividend) {
        divisor <<= 1;  // Multiply divisor by 2
        temp <<= 1;     // Keep track of power of two
    }

    while (temp > 1) {
        divisor >>= 1;  // Move divisor back
        temp >>= 1;     // Move tracking power back

        if (dividend >= divisor) {
            dividend -= divisor;
            quotient += temp;
        }
    }

    return quotient;
}

int main() {
    int dividend = 27, divisor = 3;
    cout << "27 / 3 = " << divide(dividend, divisor) << endl;
    return 0;
}
```

### Python Example: Division with Bit Shifting

```python
def divide(dividend, divisor):
    quotient = 0
    temp = 1

    while divisor <= dividend:
        divisor <<= 1  # Multiply divisor by 2
        temp <<= 1     # Keep track of power of two

    while temp > 1:
        divisor >>= 1  # Move divisor back
        temp >>= 1     # Move tracking power back

        if dividend >= divisor:
            dividend -= divisor
            quotient += temp

    return quotient

# Example usage
print("27 / 3 =", divide(27, 3))
```

This method **mimics what early CPUs did**—scaling the divisor up using shifts and then subtracting back down.

It works, but compared to modern hardware division, **it’s like running Windows 95 on a potato**.

***

## Key Ideas

| Concept                             | Explanation                                                        |
| ----------------------------------- | ------------------------------------------------------------------ |
| **Early CPU Division**              | Used bit shifting and subtraction instead of direct division       |
| **Bit Shifting**                    | Right shift (`>>`) divides by 2, left shift (`<<`) multiplies by 2 |
| **Modern CPUs**                     | Have dedicated division units for faster calculations              |
| **Binary Division in C++ & Python** | Implemented using shifting and subtraction                         |

***

## References

1. [Bitwise Operations in CPUs](https://en.wikipedia.org/wiki/Bitwise_operation)
2. [How Division Works in Modern CPUs](https://en.wikipedia.org/wiki/Division_algorithm)
3. [Introduction to Binary Arithmetic](https://www.geeksforgeeks.org/binary-arithmetic/)

```


```
