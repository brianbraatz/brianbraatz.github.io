---
title: C++ (11+) Lambda and bind in a Nutshell
description: With comparison to std::bind and history
slug: cpp-bind-lambda
date: 2019-03-18
image: post/Articles/IMAGES/cppblue.png
categories:
  - CPP
  - Template Programming
  - CPP-STL
  - CPP 11
tags:
  - Bind
  - Boost
  - Bind
  - Functional
  - Programming
  - Modern
  - Lambda
  - Functions
  - Syntax
  - Comparison
draft: false
weight: 85
categories_ref:
  - CPP
  - Template Programming
  - CPP-STL
  - CPP 11
slug_calculated: https://brianbraatz.github.io/p/cpp-bind-lambda
lastmod: 2025-03-14T16:40:31.691Z
---
# Comparing Lambdas and `std::bind` in Modern C++

In modern C++ development, both `std::bind` and lambda expressions offer ways to create function objects, but they differ in syntax, flexibility, and performance.

This article explores these differences, providing code examples to illustrate when to prefer one over the other.

***

## Understanding `std::bind`

Introduced in C++11, `std::bind` allows the creation of function objects by binding specific arguments to a function, enabling partial function application.

This is particularly useful for adapting functions to interfaces that require a certain signature.

### Example: Binding a Free Function

```cpp
#include <iostream>
#include <functional>

void greet(const std::string& name) {
    std::cout << "Hello, " << name << "!" << std::endl;
}

int main() {
    auto greetAlice = std::bind(greet, "Alice");
    greetAlice(); // Output: Hello, Alice!
    return 0;
}
```

In this example, `std::bind` creates a new function object `greetAlice` with the `name` parameter bound to "Alice".

***

## Introducing Lambda Expressions

Lambda expressions, also introduced in C++11, provide a (more flexible?)  way to create anonymous function objects directly in the code.

Lambda expressions are usually clearer and have less syntax clutter compared to `std::bind`.

### Example: Equivalent Functionality Using a Lambda

```cpp
#include <iostream>

void greet(const std::string& name) {
    std::cout << "Hello, " << name << "!" << std::endl;
}

int main() {
    auto greetAlice = []() { greet("Alice"); };
    greetAlice(); // Output: Hello, Alice!
    return 0;
}
```

Here, the lambda expression `[]() { greet("Alice"); }` achieves the same result as the previous `std::bind` example but with more straightforward syntax.

***

## Comparing `std::bind` and Lambdas

While both `std::bind` and lambdas can create function objects, lambdas offer several advantages:

1. **Readability**: Lambdas provide clearer and more intuitive syntax, making the code easier to understand.
2. **Performance**: Lambdas are more likely to be inlined by the compiler, potentially leading to better performance. In contrast, `std::bind` may involve additional overhead due to its implementation.
3. **Flexibility**: Lambdas can capture variables from the surrounding scope by value or reference, offering greater flexibility. `std::bind` requires explicit use of `std::ref` to achieve similar behavior.

### Example: Capturing Variables

```cpp
#include <iostream>

int main() {
    int factor = 2;
    auto multiply = [factor](int value) { return value * factor; };
    std::cout << multiply(5) << std::endl; // Output: 10
    return 0;
}
```

In this example, the lambda captures the `factor` variable by value, allowing it to be used within the function body.

***

## Handling Function Overloads

One area where lambdas have a distinct advantage is in handling overloaded functions.

`std::bind` can struggle with overloaded functions due to ambiguity.

Lambdas can explicitly specify which function to call.

For any C++ geeks who follow the history, there has always been a long standing split in most discussions about "change the compiler?" or "write a library?" whenever C++ addition has been proposed to the committee.

std::bind is a library .\
Lambdas are a part of the compiler..

So in this case, both sides won it seems.. :)

If you have code that has legacy boost::bind, thats usually MUCH easier to port to std::bind.

Since std::bind was inspired by boost::bind.

### Example: Binding an Overloaded Function

```cpp
#include <iostream>
#include <functional>

void print(int value) {
    std::cout << "Integer: " << value << std::endl;
}

void print(const std::string& value) {
    std::cout << "String: " << value << std::endl;
}

int main() {
    // Using a lambda to specify the int version of print
    auto printInt = [](int value) { print(value); };
    printInt(42); // Output: Integer: 42

    // Using std::bind requires casting to resolve the overload
    auto printString = std::bind(static_cast<void(*)(const std::string&)>(&print), std::placeholders::_1);
    printString("Hello"); // Output: String: Hello
    return 0;
}
```

In this case, the lambda provides a more straightforward approach to handling the overloaded `print` function.

***

## Conclusion

While `std::bind` offers capabilities for function binding, lambda expressions generally provide a more readable, flexible, and efficient alternative in modern C++ development.

Lambdas simplify the syntax and enhance code clarity, making them the preferred choice in most scenarios.

If you have code that has legacy boost::bind, thats usually MUCH easier to port to std::bind.
