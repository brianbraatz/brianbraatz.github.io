---
title: Kotlin in a Nutshell with 10 Code Examples
description: Kotlin in a Nutshell with 10 Code Examples
slug: kotlin-in-a-nutshell-with-10-code-examples
date: 2017-03-22
image: post/Articles/IMAGES/41.jpg
categories:
  - Programming
  - Kotlin
  - Code Examples
tags:
  - Programming
  - Kotlin
  - Examples
  - JVM
  - Android
  - Syntax
draft: false
weight: 678
lastmod: 2025-02-24T14:15:34.515Z
---
# Kotlin in a Nutshell with 10 Code Examples

Kotlin is a modern, concise, and powerful programming language that runs on the Java Virtual Machine (JVM). It’s designed to be fully interoperable with Java while improving upon its verbosity and complexity. Whether you’re developing Android apps, backend services, or even multi-platform applications, Kotlin makes your code more expressive and safer.

## 1. Hello, World!

```kotlin
fun main() {
    println("Hello, World!")
}
```

This is the most basic Kotlin program. The `main` function is the entry point, and `println` outputs text to the console.

## 2. Variables and Constants

```kotlin
val name = "Kotlin"
var age = 10
age += 1
```

`val` declares a read-only variable (constant), while `var` allows modification.

## 3. Functions

```kotlin
fun greet(name: String): String {
    return "Hello, $name!"
}
```

Kotlin functions are concise and support string interpolation with `$name`.

## 4. Null Safety

```kotlin
var nullableString: String? = null
nullableString?.let { println(it.length) }
```

Using `?` prevents `NullPointerException` by enabling safe access.

## 5. Conditional Expressions

```kotlin
val number = 10
val result = if (number > 0) "Positive" else "Negative"
```

`if` in Kotlin is an expression that returns a value.

## 6. Loops

```kotlin
for (i in 1..5) {
    println(i)
}
```

Kotlin supports range-based loops with `..`.

## 7. When Expression

```kotlin
val grade = "A"
val message = when (grade) {
    "A" -> "Excellent!"
    "B" -> "Good job!"
    else -> "Keep trying!"
}
```

`when` replaces `switch` statements with a more expressive syntax.

## 8. Classes and Objects

```kotlin
class Person(val name: String, var age: Int)

val person = Person("Alice", 25)
println(person.name)
```

Kotlin simplifies class definitions with primary constructors.

## 9. Extension Functions

```kotlin
fun String.shout() = this.uppercase()
println("hello".shout())
```

You can add new functions to existing classes without modifying them.

## 10. Coroutines

```kotlin
import kotlinx.coroutines.*

fun main() = runBlocking {
    launch {
        delay(1000L)
        println("Coroutine Example")
    }
    println("Start")
}
```

Coroutines provide a lightweight way to handle asynchronous operations.

***

## Key Ideas Table

| Key Idea            | Summary                                                                |
| ------------------- | ---------------------------------------------------------------------- |
| Kotlin Overview     | Kotlin is a modern and expressive language for JVM, Android, and more  |
| Hello World         | A simple program to print text to the console                          |
| Variables           | `val` for constants, `var` for mutable variables                       |
| Functions           | Kotlin functions are concise and support string interpolation          |
| Null Safety         | Prevents `NullPointerException` with safe calls and the Elvis operator |
| Conditionals        | `if` expressions return values, `when` replaces `switch` statements    |
| Loops               | Kotlin supports range-based loops                                      |
| Classes             | Kotlin simplifies class definitions with primary constructors          |
| Extension Functions | Add functions to existing classes without modifying them               |
| Coroutines          | Lightweight threading with structured concurrency                      |

***

## References

1. [Official Kotlin Website](https://kotlinlang.org/)
2. [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
3. [JetBrains Kotlin Blog](https://blog.jetbrains.com/kotlin/)
4. [Google Developers: Kotlin for Android](https://developer.android.com/kotlin)
5. [Coroutines Guide](https://kotlinlang.org/docs/coroutines-guide.html)

***
