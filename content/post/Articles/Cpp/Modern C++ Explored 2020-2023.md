---
title: Exploring C++ 2020 and 2023
description: Exploring the new Features & comparing to C#, Python, GO, and Java
slug: exploring-cpp-20-and-23
date: 2024-08-13
image: post/Articles/IMAGES/cpppsycha.png
categories:
  - CPP
  - CPP 20
  - CPP 23
  - Embedded
  - C++
  - Languages
tags:
  - Programming
  - Comparison
  - Python
  - Go
  - Java
  - Code
  - Examples
  - CPP
  - GoLang
draft: false
weight: 87
lastmod: 2025-03-03T17:14:58.396Z
---
C++ has been around for so long that dinosaurs probably wrote their own game engines in it before the asteroid hit.

But here we are in the era of C++ 20 and C++ 23, and boy, things have changed.

From its humble beginnings in the 1980s, where C++ was basically "C but with more pain," to today, where it's "C but with even more features (and also more pain)," it's been quite the ride.

***

## 🎞️ A Brief History of C++

* **C++ 1.0 (1985):** It was like C, but object-oriented. Nobody knew what that meant, but they were excited anyway.
* **C++ 98/03:** Introduced the STL (Standard Template Library), which was great but also made error messages longer than a Tolstoy novel.
* **C++ 11:** Auto, smart pointers, lambdas, and "modern C++" were born. Devs cheered, then promptly forgot half the features.
* **C++ 14/17:** Refinements and some cool new features like structured bindings. Not revolutionary, but nice.
* **C++ 20:** Concepts, coroutines, ranges, and the infamous spaceship operator. 🚀
* **C++ 23:** More improvements, std::expected, and better constexpr support. At this point, C++ can probably write its own code.

***

## 1️⃣ "Hello, World!" (Classic vs. Modern)

```cpp
// Classic C++ 1.0 Style
#include <iostream>
int main() {
    std::cout << "Hello, world!\n";
    return 0;
}
```

```cpp
// Modern C++ 23
#include <iostream>
int main() {
    std::cout << "Hello, world!\n";  // Still the same, but cooler
}
```

👉 **Comparison:** Syntax hasn't changed, but modern C++ drops unnecessary boilerplate like `return 0;`.

***

## 2️⃣ Auto Magic

```cpp
// C++ 98
int x = 10;
```

```cpp
// C++ 11+ (Much Cooler)
auto x = 10;  // The compiler figures it out
```

👉 **Comparison:** Auto removes the headache of typing variable types.

***

## 3️⃣ Ranges in C++ 20

```cpp
#include <ranges>
#include <vector>
#include <iostream>

int main() {
    std::vector<int> nums {1, 2, 3, 4, 5};
    auto squares = nums | std::views::transform([](int n) { return n * n; });
    for (int n : squares) std::cout << n << " ";
}
```

👉 **Comparison:** This makes C++ feel more like Python!

***

## 4️⃣ Coroutines in C++ 20

```cpp
#include <coroutine>
#include <iostream>

struct Task {
    struct promise_type {
        Task get_return_object() { return {}; }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() {}
    };
};

Task example() { co_return; }

int main() { example(); }
```

👉 **Comparison:** Python had async/await for years, now C++ has it. Better late than never!

***

## 5️⃣ Spaceship Operator (<=>) in C++ 20

```cpp
#include <iostream>

struct MyClass {
    int value;
    auto operator<=>(const MyClass&) const = default;
};

int main() {
    MyClass a{10}, b{20};
    std::cout << (a < b) << "\n";
}
```

👉 **Comparison:** It makes comparisons easier than in Java and Go.

***

## 🥊 C++ vs. C#, Python, Go, and Java

| Feature       | C++ 23   | C#  | Python | Go | Java |
| ------------- | -------- | --- | ------ | -- | ---- |
| Performance   | ⚡⚡⚡⚡⚡    | ⚡⚡⚡ | ⚡      | ⚡⚡ | ⚡⚡⚡  |
| Ease of Use   | 😭       | 😃  | 😍     | 😃 | 🙂   |
| Memory Safety | ❌        | ✅   | ✅      | ✅  | ✅    |
| Coroutines    | ✅ (new!) | ✅   | ✅      | ✅  | ✅    |
| Compile Time  | 🐢       | 🏎️ | 🚀     | 🚀 | 🏎️  |

👉 **Takeaway:** If you need raw speed, C++ still wins. But if you value your sanity, maybe check out C# or Python.

***

## 🏁Thoughts

C++ has come a long way, from a clunky extension of C to a modern language with powerful features.

C++ 20 and 23 introduce features that make it feel more high-level, but it still retains its "you better know what you're doing" philosophy.

If you’re a C++ dev like me (since 1987 :) ), embrace these new features. If you’re new to C++, well… good luck.

***

<!-- 
## 📌 Key Takeaways

| Key Idea | Summary |
|----------|---------|
| C++ has evolved | From C++ 1.0 to 23, things have improved drastically. |
| Modern features | Ranges, coroutines, and spaceship operators make life easier. |
| C++ vs. Other Languages | C++ is fast but complex; C# is friendly, Python is simple, Go is efficient, Java is robust. |

---
-->

## 📚 References

* [C++ Wikipedia](https://en.wikipedia.org/wiki/C%2B%2B)
* [C++ 20 Features](https://en.cppreference.com/w/cpp/20)
* [C++ 23 Features](https://en.cppreference.com/w/cpp/23)
* [C++ vs Python](https://realpython.com/python-vs-cpp/)

***
