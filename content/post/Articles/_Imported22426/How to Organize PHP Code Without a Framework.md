---
title: Understanding PHP Classes and Good Practices for OO and Isolation in PHP
description: Understanding PHP Classes and Good Practices for OO and Isolation in PHP
slug: php-classes-oo-isolation
date: 2018-07-15
image: post/Articles/IMAGES/42.jpg
categories:
  - PHP
  - Object-Oriented Programming
  - Best Practices
  - Code Organization
  - SQL
  - Cloud
tags:
  - PHP
  - Classes
  - OOP
  - Dependency
  - Injection
  - IoC
  - Code
  - Structure
  - SOLID
  - Principles
draft: false
weight: 547
categories_ref:
  - PHP
  - Object-Oriented Programming
  - Best Practices
  - Code Organization
  - SQL
  - Cloud
slug_calculated: https://brianbraatz.github.io/p/understanding-php-classes-and-good-practices-for-oo-and-isolation-in-php
lastmod: 2025-03-18T22:42:10.296Z
---
w

# Understanding PHP Classes and Good Practices for OO and Isolation in PHP

## Introduction

PHP is like that one friend who insists on being everywhere.\
It powers websites, APIs, and even that janky CMS your boss won’t let go of.\
But if you don’t organize your PHP code properly, it turns into a tangled mess of spaghetti.

This is where **Object-Oriented Programming (OOP)** and **good coding practices** come in.\
In this article, we'll take a deep dive into PHP classes, **best practices for isolation**, and **how to structure PHP code**—even when you're flying solo without a framework.

## What Are PHP Classes?

A PHP class is basically a blueprint for creating objects.\
Think of it like a **cookie cutter**: the class defines the shape, and the objects are the delicious cookies.

### Basic PHP Class Example

```php
<?php

class Dog {
    public $name;
    
    public function __construct($name) {
        $this->name = $name;
    }

    public function bark() {
        return "{$this->name} says Woof!";
    }
}

$dog = new Dog("Buddy");
echo $dog->bark(); // Output: Buddy says Woof!

?>
```

## Best Practices for OOP in PHP

### 1. Use Encapsulation (Keep Your Stuff Private)

```php
<?php

class BankAccount {
    private $balance;

    public function __construct($balance) {
        $this->balance = $balance;
    }

    public function getBalance() {
        return $this->balance;
    }

    public function deposit($amount) {
        if ($amount > 0) {
            $this->balance += $amount;
        }
    }
}

$account = new BankAccount(100);
$account->deposit(50);
echo $account->getBalance(); // Output: 150

?>
```

### 2. Follow the SOLID Principles

* **S**ingle Responsibility Principle (SRP) – A class should do one thing well.
* **O**pen/Closed Principle (OCP) – Classes should be **open for extension**, but **closed for modification**.
* **L**iskov Substitution Principle (LSP) – Subclasses should be replaceable with their parent classes.
* **I**nterface Segregation Principle (ISP) – Don't force classes to implement methods they don’t need.
* **D**ependency Inversion Principle (DIP) – Depend on **abstractions**, not **concrete implementations**.

### 3. Use Dependency Injection (DI)

```php
<?php

class Logger {
    public function log($message) {
        echo "[LOG]: $message";
    }
}

class UserService {
    private $logger;

    public function __construct(Logger $logger) {
        $this->logger = $logger;
    }

    public function createUser($name) {
        $this->logger->log("User '$name' created.");
    }
}

$logger = new Logger();
$userService = new UserService($logger);
$userService->createUser("Alice");

?>
```

## What is IoC (Inversion of Control)?

IoC lets external sources manage dependencies, reducing tight coupling.

```php
<?php

class Container {
    private $bindings = [];

    public function bind($key, $resolver) {
        $this->bindings[$key] = $resolver;
    }

    public function make($key) {
        return call_user_func($this->bindings[$key]);
    }
}

$container = new Container();
$container->bind("logger", function() {
    return new Logger();
});

$logger = $container->make("logger");
$logger->log("Hello from IoC!");

?>
```

## How to Organize PHP Code Without a Framework

### 1. Use a Proper Directory Structure

```
/myapp
  /src
    /Controllers
    /Models
    /Services
    /Repositories
  /config
  /public
  /vendor
```

### 2. Use an Autoloader

```json
{
  "autoload": {
    "psr-4": {
      "App\\": "src/"
    }
  }
}
```

Run `composer dump-autoload` to apply it.

### 3. Separate Business Logic from Controllers

Controllers should only **handle requests and responses**—the heavy lifting should be done in services or repositories.

### 4. Use Environment Variables for Configurations

```ini
DB_HOST=localhost
DB_USER=root
DB_PASS=supersecurepassword
```

Use `getenv('DB_HOST')` in PHP to access them.

## Conclusion

Writing maintainable PHP is like keeping a **toddler entertained**—if you don’t do it right, things **get messy fast**.

Follow **OOP principles**, **use DI and IoC**, and **organize your code properly**, and you’ll have a **clean, testable, and scalable PHP application**.

***

## Key Ideas

| Concept              | Explanation                                       |
| -------------------- | ------------------------------------------------- |
| Classes              | Blueprints for objects                            |
| Encapsulation        | Keeping properties private                        |
| SOLID                | Five principles for better OOP                    |
| Dependency Injection | Inject dependencies instead of hardcoding         |
| IoC                  | Letting an external container manage dependencies |
| Autoloading          | Use Composer’s PSR-4 autoloader                   |
| Directory Structure  | Keep controllers, models, and services separate   |

## References

* [PHP Official Documentation](https://www.php.net/docs.php)
* [SOLID Principles](https://en.wikipedia.org/wiki/SOLID)
* [Dependency Injection in PHP](https://phptherightway.com/#dependency_injection)
