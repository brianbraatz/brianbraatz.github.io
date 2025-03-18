---
title: Java in a Nutshell
slug: java-in-a-nutshell
date: 2013-12-15
image: post/Articles/IMAGES/java.png
categories:
  - Java
  - Concurrency
  - CPP
  - Unit Testing
tags:
  - Java
  - Programming
  - Software
  - Development
  - Concurrency
  - GUI
  - Testing
  - WebDevelopment
  - JUnit
  - UnitTest
draft: false
weight: 30
categories_ref:
  - Java
  - Concurrency
  - CPP
  - Unit Testing
slug_calculated: https://brianbraatz.github.io/p/java-in-a-nutshell
lastmod: 2025-03-14T16:40:19.859Z
---
## History and Motivation

Once upon a time, in a land far far away...  in the early 1990s, a group of Sun Microsystems engineers, led by James Gosling, embarked on a quest to develop a language for interactive television.

They named it Oak, after the tree outside Gosling's office.

However, Oak was already taken (darn you, trademark laws!), so they settled on Java, inspired by their love for coffee.

Java was designed with the mantra "Write Once, Run Anywhere" (WORA), aiming for platform independence and security.

It quickly gained popularity, especially with the rise of the internet, and became a staple in software development. ([en.wikipedia.org](https://en.wikipedia.org/wiki/Java_%28programming_language%29))

## Personal Memories...

I can attest to the popularity of Java. When it was becoming fashionable in the mid 1990s, I was a C++ nerd.

The Java nerds would wave the Java flag in my face in arrogance..

Oh so many thoguhts I had at the time of what they could do with that flag..

BUTT.. (get it?)... this blog needs to remain kid friendly.. so we move on ..

If you were writing software around this time. Java was "the thing" ...

My sister in law (who is not an engineer) , even asked me about it..

It was crazy popular and crazy slow.. but the zealouts didnt care...

Now days Java is all grown up.

I dont hate it anymore. and after working on several larger Java projects I have grown a deeper respect for its consistency, especially in the library design... .

C++ organically grew up.. so things like the design of the libraries can be all over the map and not always consistent..

A nice thing about Java (and one could argue the things that came after ).. is the consistency of patterns and design in the common libraries..

These days I tend to prefer C#, but without Java - we would all still be coding things in DOS Bat files or something.....

## Common Operations in Java

Let's dive into some basic Java operations. Remember, every Java program starts with a `main` method. Here's a simple "Hello, World!" program:

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

Want to perform some arithmetic? Java's got you covered:

```java
public class Arithmetic {
    public static void main(String[] args) {
        int a = 5;
        int b = 3;
        System.out.println("Sum: " + (a + b));
        System.out.println("Difference: " + (a - b));
        System.out.println("Product: " + (a * b));
        System.out.println("Quotient: " + (a / b));
    }
}
```

(amazing ! no?)

## Advanced Techniques

Feeling adventurous? Let's explore some advanced Java features.

**Generics** allow for type-safe data structures:

```java
import java.util.ArrayList;

public class GenericsExample {
    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<>();
        list.add("Java");
        list.add("Coffee");
        for (String item : list) {
            System.out.println(item);
        }
    }
}
```

Simliar to Templates in C++ and C#.

**Lambda Expressions** (introduced in Java 8) enable functional programming:

```java
import java.util.Arrays;
import java.util.List;

public class LambdaExample {
    public static void main(String[] args) {
        List<String> list = Arrays.asList("Java", "is", "fun");
        list.forEach(item -> System.out.println(item));
    }
}
```

## Web Support

Can you build web services or websites with Java?

YES!

Java offers mature frameworks for web development.

**Spring Boot** simplifies the creation of stand-alone, production-grade applications:

**Heres a dandy little web service:**

```java
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
public class DemoApplication {
    public static void main(String[] args) {
        SpringApplication.run(DemoApplication.class, args);
    }
}

@RestController
class HelloController {
    @GetMapping("/")
    public String hello() {
        return "Hello, World!";
    }
}
```

More examples on building web services with Java:

* [Spring Boot Tutorials](https://spring.io/guides)
* [Java EE Documentation](https://javaee.github.io/tutorial/)

## Concurrency Support

Java has built-in support for multithreading, allowing you to perform multiple tasks simultaneously (which is like totally multi-threading...).

You can create threads by extending the `Thread` class or implementing the `Runnable` interface:

```java
public class MyThread extends Thread {
    public void run() {
        System.out.println("Thread is running");
    }

    public static void main(String[] args) {
        MyThread thread = new MyThread();
        thread.start();
    }
}
```

For asynchronous programming, Java provides the `CompletableFuture` class:

```java
import java.util.concurrent.CompletableFuture;

public class AsyncExample {
    public static void main(String[] args) {
        CompletableFuture.supplyAsync(() -> "Hello, Async World!")
            .thenAccept(System.out::println);
    }
}
```

More on Java Concurrency:

* [Java Concurrency Tutorial](https://docs.oracle.com/javase/tutorial/essential/concurrency/)
* [Guide to CompletableFuture](https://www.baeldung.com/java-completablefuture)

## User Interfaces

Yes, you can create GUIs in Java!

The **Swing** framework allows for building window-based applications:

```java
import javax.swing.JButton;
import javax.swing.JFrame;

public class SwingExample {
    public static void main(String[] args) {
        JFrame frame = new JFrame("My GUI");
        JButton button = new JButton("Click me!");
        frame.add(button);
        frame.setSize(300, 200);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}
```

I wrote my first Swing UI around 1993... And it was buggy, and difficult and I hated it..

But after learning WPF \ XAML I have a new appreciation for the declaritive UI pattern.

More on GUIs with Java:

* [Java Swing Tutorial](https://www.javatpoint.com/java-swing)
* [Creating a GUI With JFC/Swing](https://docs.oracle.com/javase/tutorial/uiswing/)

## Unit Testing

The meat and potatoes unit testing library is **JUnit**:

```java
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class CalculatorTest {
    @Test
    public void testAddition() {
        Calculator calc = new Calculator();
        assertEquals(5, calc.add(2, 3));
    }
}
```

Unit testing in Java:

* [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
* [Mockito Tutorial](https://www.baeldung.com/mockito-series)

## Key Ideas

| Topic               | Description                                                                      |
| ------------------- | -------------------------------------------------------------------------------- |
| History             | Developed by James Gosling at Sun Microsystems; aimed for platform independence. |
| Common Operations   | Basic syntax includes classes, methods, and standard operations.                 |
| Advanced Techniques | Features like Generics and Lambda Expressions enhance functionality.             |
| Web Support         | Frameworks like Spring Boot facilitate web service development.                  |
| Concurrency         | Built-in support for multithreading and asynchronous programming.                |
| User Interfaces     | Swing framework enables GUI application development.                             |
| Unit Testing        | Frameworks like JUnit support effective unit testing.                            |

## Related Links

* [Java Programming Language - Wikipedia](https://en.wikipedia.org/wiki/Java_%28programming_language%29)
* [James Gosling - Wikipedia](https://en.wikipedia.org/wiki/James_Gosling)
* [Java Platform, Standard Edition - Oracle](https://www.oracle.com/java/technologies/javase-downloads.html)
