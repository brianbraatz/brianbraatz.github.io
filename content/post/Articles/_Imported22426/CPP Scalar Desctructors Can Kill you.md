---
title: In C++, (Lack of) Scalar Destructors Can Kill You...
description: 
slug: in-c++-scalar-destructors-can-kill-you
date: 2015-07-14
image: post/Articles/IMAGES/cppblue.png
categories:
  - C++
  - Memory Management
  - Delete
  - Destructors
  - CPP
tags:
  - Memory
  - Management
  - Delete
  - Destructors
  - Arrays
  - Bugs
draft: false
weight: 242
categories_ref:
  - C++
  - Memory Management
  - Delete
  - Destructors
  - CPP
lastmod: 2025-03-14T15:45:16.157Z
---
# In C++, Scalar Destructors Can Kill You...

## The Tale of `delete` vs. `delete[]`

C++ is a wonderful language—if you like living on the edge of chaos.

It gives you the power to manage memory manually, and with great power comes... great foot-shooting potential. One of the biggest pitfalls in C++ memory management is the difference between `delete` and `delete[]`, and how forgetting those little square brackets can leave your program bleeding out on the floor.

## Why Do We Have `delete[]`?

Imagine you need a bunch of characters, so you allocate an array like this:

```cpp
char* pChar = new char[10];
```

At some point, you realize you've had enough of these characters and want to clean up. So, like a good programmer, you call:

```cpp
delete pChar; // Uh-oh...
```

And that's where the trouble begins.

Since `pChar` points to an *array*, not a single object, the correct way to delete it is:

```cpp
delete[] pChar;
```

If you use plain old `delete`, C++ will only call the destructor for the first element of the array (or worse, just release part of the allocated memory), leading to undefined behavior, memory leaks, and possibly the C++ gods smiting your application.

## The Root of the Problem

Unlike some more "forgiving" languages, C++ doesn’t store array sizes when you allocate memory with `new[]`. It doesn’t secretly track how many elements you asked for—you're expected to remember that yourself.

This means `delete pChar;` has no idea it was supposed to delete an array. It just looks at the first object, calls its destructor (if applicable), and moves on, leaving the rest of the array's memory floating in limbo.

## A Bug Factory in the Making

This little distinction has caused *countless* bugs in real-world software. Here are just a few of the horrors you might encounter:

1. **Memory Leaks** – If you're lucky, the memory for the rest of the array is never released, and you "just" have a memory leak.

2. **Corrupt Memory** – If the array elements have destructors that manage resources (like file handles or dynamic memory), then only the first element's destructor gets called, leaving the rest of the resources dangling.

3. **Heap Corruption** – If the runtime is particularly unhappy with you, the memory manager may completely break down, leading to *undefined behavior*. This includes crashes, gibberish output, or even silent data corruption.

## An Example with Destructors

Let's take a class that does something in its destructor:

```cpp
#include <iostream>

class Test {
public:
    Test() { std::cout << "Constructed\n"; }
    ~Test() { std::cout << "Destroyed\n"; }
};

int main() {
    Test* arr = new Test[3];

    delete arr; // WRONG! Only calls ~Test() for the first object

    return 0;
}
```

This will output:

```
Constructed
Constructed
Constructed
Destroyed  // Only one destructor gets called!
```

Now let’s fix it:

```cpp
delete[] arr; // CORRECT!
```

And now, every `Test` object gets its destructor called properly.

## How Can You Avoid This?

There are a few ways to dodge this bullet:

* **Use `std::vector<T>` instead of raw arrays.** Seriously, just use it. It handles all the memory management for you.
* **Use smart pointers like `std::unique_ptr<T[]>` if you must use raw arrays.**
* **If you manually allocate an array with `new[]`, always deallocate with `delete[]`.** Write it down, tattoo it on your arm, do whatever it takes to remember.

## Wrapping Up

C++ lets you allocate memory however you like, but it won’t stop you from making a mess. The distinction between `delete` and `delete[]` is one of those quirky, dangerous corners of the language that have tripped up even experienced developers.

So, the next time you see `new[]`, make sure you also see `delete[]`, or be prepared for chaos.

***

## Key Ideas

| Concept                | Explanation                                                                       |
| ---------------------- | --------------------------------------------------------------------------------- |
| `delete` vs `delete[]` | `delete` is for single objects, `delete[]` is for arrays                          |
| Why It Matters         | Using `delete` on an array causes memory leaks, corruption, or undefined behavior |
| Memory Tracking        | C++ doesn’t track array sizes, so you must tell it explicitly                     |
| Destructor Problems    | Only the first object's destructor gets called if you use `delete` on an array    |
| Safer Alternatives     | Use `std::vector` or smart pointers instead of raw arrays                         |

***

## References

* [cppreference.com on delete](https://en.cppreference.com/w/cpp/language/delete)
* [Effective C++ by Scott Meyers](https://www.oreilly.com/library/view/effective-c-55/9780134570085/)
* [The C++ Programming Language by Bjarne Stroustrup](https://www.stroustrup.com/)
