---
title: In C++, Virtual Destructors (or Lack Thereof) Can Kill You
description: 
slug: in-cpp-virtual-destructors
date: 2018-05-05
image: post/Articles/IMAGES/cppblue.png
categories:
  - C++
  - Memory Management
  - Virtual Destructors
  - Polymorphism
  - CPP
tags:
  - Memory
  - Management
  - Virtual
  - Destructors
  - Polymorphism
  - Inheritance
  - Bugs
draft: false
weight: 56
categories_ref:
  - C++
  - Memory Management
  - Virtual Destructors
  - Polymorphism
  - CPP
lastmod: 2025-03-14T15:45:16.306Z
---
# In C++, Virtual Destructors (or Lack Thereof) Can Kill You

## The Problem: Inheritance and Non-Virtual Destructors

If you’ve been writing C++ for any length of time, you’ve probably heard the golden rule: *If a class has virtual functions, its destructor should be virtual.*

If you haven't heard that yet, well, now you have. And if you ignore it, your code may descend into the abyss of **undefined behavior** faster than you can say "segmentation fault."

## The Classic Trap: Deleting a Derived Object Through a Base Pointer

Let’s start with an innocent-looking class hierarchy:

```cpp
#include <iostream>

class Base {
public:
    Base() { std::cout << "Base constructed\n"; }
    ~Base() { std::cout << "Base destroyed\n"; } // Oops! No 'virtual' here!
};

class Derived : public Base {
public:
    Derived() { std::cout << "Derived constructed\n"; }
    ~Derived() { std::cout << "Derived destroyed\n"; }
};

int main() {
    Base* obj = new Derived();
    delete obj; // Uh-oh...
    return 0;
}
```

Expected output?

```
Base constructed
Derived constructed
Base destroyed
```

Wait, where’s `Derived destroyed`?

## Why Did This Happen?

Because `Base`’s destructor **is not virtual**, when `delete obj;` is called, **only `Base`'s destructor runs**. C++ doesn’t know that `obj` actually points to a `Derived` object, so it doesn’t call `Derived`’s destructor.

This means:

1. Any resources allocated in `Derived` are never freed.
2. Memory leaks occur.
3. If `Derived` has dynamically allocated memory or file handles, they **never get released**.

## The Solution: Virtual Destructors to the Rescue

A simple fix:

```cpp
class Base {
public:
    Base() { std::cout << "Base constructed\n"; }
    virtual ~Base() { std::cout << "Base destroyed\n"; } // Now it's virtual!
};
```

Now the output is:

```
Base constructed
Derived constructed
Derived destroyed
Base destroyed
```

Much better.

## When Should You Use Virtual Destructors?

A good rule of thumb:

* **If your class is intended to be a base class (i.e., it has virtual functions), make the destructor virtual.**
* **If your class is not meant to be inherited from, no need for a virtual destructor.**

## Performance Concerns

You might have heard that virtual destructors add overhead. Technically, that’s true—adding a virtual destructor means an extra entry in the vtable.

But unless you’re writing **low-latency, high-performance systems code**, the trade-off is well worth it for safety.

## What If You Forget?

If you forget to make your destructor virtual and delete a derived object through a base pointer, **bad things happen**:

* **Memory leaks** (best case scenario)
* **Undefined behavior** (worst case scenario)
* **Developers spending hours debugging weird crashes** (inevitable scenario)

## Wrapping Up

C++ gives you a lot of power, but forgetting to make destructors virtual when necessary is a **classic mistake** that leads to serious memory issues.

So, remember: *If your class is polymorphic, its destructor should be virtual.*

Now go forth and free your memory correctly.

***

## Key Ideas

| Concept                         | Explanation                                                                                   |
| ------------------------------- | --------------------------------------------------------------------------------------------- |
| Virtual Destructors             | Required for polymorphic base classes to ensure proper destruction of derived objects         |
| Non-Virtual Destructors         | Can cause memory leaks and undefined behavior when deleting derived objects via base pointers |
| vtable Overhead                 | Slight performance impact, but usually worth it for safety                                    |
| When to Use Virtual Destructors | Only when your class is meant to be a base class with polymorphic behavior                    |
| Common Bug                      | Forgetting to make destructors virtual leads to incomplete destruction of objects             |

***

## References

* [cppreference.com on virtual destructors](https://en.cppreference.com/w/cpp/language/destructor)
* [Effective C++ by Scott Meyers](https://www.oreilly.com/library/view/effective-c-55/9780134570085/)
* [The C++ Programming Language by Bjarne Stroustrup](https://www.stroustrup.com/)
