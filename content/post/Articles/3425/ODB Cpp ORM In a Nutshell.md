---
title: ODB ORM In a Nutshell
description: C++ ORM
slug: odb-cpp-orm-in-a-nutshell
date: 2017-06-14
image: post/Articles/IMAGES/cppblue.png
categories:
  - Cpp
  - Orm
  - Database
  - Persistence
tags:
  - Cpp
  - Orm
  - Database
  - Persistence
draft: false
weight: 453
categories_ref:
  - Cpp
  - Orm
  - Database
  - Persistence
lastmod: 2025-03-14T15:45:05.984Z
---
# ODB C++ ORM In a Nutshell

If you've ever tried wrangling databases in C++ manually, you probably have some war stories. Raw SQL? Pain. Hand-written CRUD operations? Even more pain.

But fear not! Enter **ODB**, the C++ ORM (Object-Relational Mapping) library that takes away the pain and makes dealing with databases much less of a headache.

<!-- So grab a coffee ☕, and let's dive into **ODB**—the magical tool that lets you work with databases in C++ like a sane person. -->

## What is ODB?

ODB is an open-source, cross-platform ORM for C++. It **automatically** generates database access code from your C++ classes, so you can write object-oriented code without constantly worrying about SQL queries.

It supports multiple databases, including:

* **SQLite** 🏡 (for small projects)
* **MySQL** 🐬 (for when you need a little more power)
* **PostgreSQL** 🐘 (for when you need a lot more power)
* **Oracle** ☀️ (if you're feeling corporate)
* **Microsoft SQL Server** 🏢 (also corporate, but Windows-y)

## Why Use ODB?

### The Good Stuff:

* **Automatic Code Generation** – Say goodbye to writing repetitive SQL statements.
* **Type-Safe Queries** – No more stringly-typed nonsense.
* **Object-Oriented** – Use real C++ classes instead of writing SQL spaghetti.
* **Portable** – Works with multiple databases.
* **Integration with Boost and Qt** – Play nice with your favorite C++ frameworks.

### The "Meh" Stuff:

* **Requires a Preprocessing Step** – You need to run the ODB compiler on your headers.
* **Not as Popular as Hibernate or Entity Framework** – But hey, it’s C++.

## How ODB Works

ODB works by processing your C++ headers and generating the necessary database code behind the scenes. Here’s a quick breakdown:

1. You write a **normal C++ class** and annotate it with `#pragma db` directives.
2. ODB generates database code automatically.
3. You compile your project like usual, and everything just works.

## A Simple Example

Let’s see how to define a simple `Person` class and persist it using ODB.

### 1. Define the Class

```cpp
#pragma once
#include <string>

#pragma db object
class Person {
public:
    Person() = default;
    Person(const std::string& name, int age) : name_(name), age_(age) {}

    int getId() const { return id_; }
    const std::string& getName() const { return name_; }
    int getAge() const { return age_; }

private:
    #pragma db id auto
    int id_;
    std::string name_;
    int age_;
};
```

### 2. Generate the Database Code

Run the ODB compiler on the header file:

```sh
odb -d sqlite --generate-query --generate-schema person.hxx
```

### 3. Using ODB in C++ Code

Now, let's store and retrieve a `Person` object.

```cpp
#include <odb/sqlite/database.hxx>
#include <odb/core.hxx>
#include "person-odb.hxx"

int main() {
    auto db = std::make_shared<odb::sqlite::database>("people.db");
    odb::transaction t(db->begin());
    
    std::shared_ptr<Person> p = std::make_shared<Person>("Alice", 30);
    db->persist(p);
    
    t.commit();
    return 0;
}
```

## ODB vs. Other C++ ORM Solutions

| Feature           | ODB                             | SOCI       | sqlpp11               | Handmade SQL       |
| ----------------- | ------------------------------- | ---------- | --------------------- | ------------------ |
| Code Generation   | ✅ Yes                           | ❌ No       | ❌ No                  | ❌ No               |
| Type-Safe Queries | ✅ Yes                           | ✅ Yes      | ✅ Yes                 | ❌ No               |
| Database Support  | SQLite, MySQL, PostgreSQL, etc. | Many       | Mostly SQLite & MySQL | Whatever you write |
| Ease of Use       | 🚀 Easy                         | 🛠️ Medium | 🏗️ Hard              | 😭 Painful         |

## When Should You Use ODB?

Use **ODB** if:

* You want an **object-oriented** way to handle database persistence.
* You love **type safety** and dislike raw SQL strings.
* You don’t mind running an **extra compiler step**.
* You need **multiple database support** without major code changes.

## When Should You NOT Use ODB?

Maybe skip **ODB** if:

* You need an **ultra-lightweight solution** (e.g., raw SQLite queries).
* You don’t like **code generation tools**.
* You prefer a **header-only library** (try `sqlpp11` instead).

<!-- ## Wrapping Up

ODB is a fantastic **C++ ORM** that brings modern database handling to your C++ applications. It’s got **type safety**, **code generation**, and **multi-database support**, making it a great alternative to hand-rolling your own persistence layer.

Sure, it has a few quirks (like needing a preprocessor step), but overall, it's one of the best ways to **avoid writing painful SQL queries** in C++.

If you’re working on a **database-heavy C++ application**, give ODB a shot. Your future self will thank you! 🚀 -->

***

## 🔑 Key Ideas

| Topic               | Summary                                                     |
| ------------------- | ----------------------------------------------------------- |
| What is ODB?        | A powerful ORM for C++ that automates database persistence. |
| Supported Databases | Works with SQLite, MySQL, PostgreSQL, and more.             |
| How it Works        | Uses a compiler to generate database access code.           |
| Example Usage       | Simple `Person` class stored in an SQLite database.         |
| Pros and Cons       | Great for type safety, but requires an extra compiler step. |
| Alternatives        | SOCI, sqlpp11, raw SQL.                                     |

***

## 📚 References

1. [ODB Official Documentation](https://www.codesynthesis.com/products/odb/)
2. [ODB GitHub Repository](https://github.com/CodeSynthesis/odb)
3. [Using ODB with SQLite](https://www.codesynthesis.com/products/odb/doc/sqlite/)
