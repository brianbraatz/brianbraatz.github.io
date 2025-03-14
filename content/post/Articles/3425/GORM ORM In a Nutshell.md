---
title: GORM ORM In a Nutshell
description: GORM ORM In a Nutshell
slug: gorm-orm-in-a-nutshell
date: 2017-08-14
image: post/Articles/IMAGES/Go-Logo_Blue.png
categories:
  - Golang
  - Database
  - ORM
tags:
  - Golang
  - Gorm
  - Orm
  - Database
  - Mysql
  - Postgresql
  - Sqlite
draft: false
weight: 523
categories_ref:
  - Golang
  - Database
  - ORM
lastmod: 2025-03-14T15:45:05.785Z
---
# GORM ORM In a Nutshell

<!-- So, you're writing Go, and you need to talk to a database. You could use raw SQL like a caveman... or you could use **GORM**, a powerful and flexible ORM (Object-Relational Mapper) for Go that makes dealing with databases much less painful. -->

## Why GORM?

Because raw SQL is like writing assembly when you don’t need to. GORM makes database operations much easier while still letting you drop down to raw SQL when you need to.

GORM supports multiple databases, including **PostgreSQL, MySQL, SQLite, and SQL Server**. It comes with great features like:

* **Auto Migrations** – No more writing boring migration scripts.
* **Relations** – Handle associations easily.
* **Hooks** – Execute logic before/after actions.
* **Transactions** – Wrap operations in transactions effortlessly.
* **Eager Loading** – Fetch related records with ease.
* **Raw SQL Support** – Sometimes you just need that custom SQL.

Let's dive in and see some code!

## Getting Started

### Installation

First, install GORM and the appropriate database driver:

```sh
go get -u gorm.io/gorm

go get -u gorm.io/driver/sqlite   # SQLite
go get -u gorm.io/driver/mysql    # MySQL
go get -u gorm.io/driver/postgres # PostgreSQL
```

### Connecting to a Database

Here’s how you connect to different databases:

#### SQLite Example

```go
package main

import (
    "gorm.io/driver/sqlite"
    "gorm.io/gorm"
    "log"
)

func main() {
    db, err := gorm.Open(sqlite.Open("test.db"), &gorm.Config{})
    if err != nil {
        log.Fatal("Failed to connect to database")
    }
    log.Println("Connected to SQLite!")
}
```

#### MySQL Example

```go
import "gorm.io/driver/mysql"

dsn := "user:password@tcp(127.0.0.1:3306)/dbname?charset=utf8mb4&parseTime=True&loc=Local"
db, err := gorm.Open(mysql.Open(dsn), &gorm.Config{})
```

#### PostgreSQL Example

```go
import "gorm.io/driver/postgres"

dsn := "host=localhost user=youruser password=yourpassword dbname=yourdb port=5432 sslmode=disable"
db, err := gorm.Open(postgres.Open(dsn), &gorm.Config{})
```

## Defining Models

Now, let’s define a model and let GORM do the heavy lifting.

```go
type User struct {
    ID    uint   `gorm:"primaryKey"`
    Name  string `gorm:"size:255"`
    Email string `gorm:"unique"`
}
```

## Auto Migration

You don’t need to write `CREATE TABLE` statements manually. GORM can do it for you.

```go
db.AutoMigrate(&User{})
```

## CRUD Operations

### Creating a Record

```go
user := User{Name: "John Doe", Email: "john@example.com"}
db.Create(&user)
```

### Retrieving Records

```go
var user User
db.First(&user, 1) // Get user with ID 1
db.First(&user, "email = ?", "john@example.com")
```

### Updating a Record

```go
db.Model(&user).Update("Name", "Johnny")
```

### Deleting a Record

```go
db.Delete(&user)
```

## Relationships

### One-to-Many

```go
type Order struct {
    ID     uint
    UserID uint
    User   User
}

db.AutoMigrate(&Order{})
```

### Many-to-Many

```go
type Product struct {
    ID    uint
    Name  string
    Users []User `gorm:"many2many:user_products"`
}

db.AutoMigrate(&Product{})
```

## Raw SQL

If GORM gets in your way, you can always write raw SQL:

```go
rows, err := db.Raw("SELECT * FROM users WHERE name = ?", "John").Rows()
```

## Comparing GORM with Other ORMs

| Feature        | GORM | Ent (Go) | SQLBoiler | xORM (Python) |
| -------------- | ---- | -------- | --------- | ------------- |
| Auto Migration | ✅    | ✅        | ❌         | ✅             |
| Relations      | ✅    | ✅        | ✅         | ✅             |
| Hooks          | ✅    | ✅        | ✅         | ✅             |
| Raw SQL        | ✅    | ✅        | ✅         | ✅             |
| Transactions   | ✅    | ✅        | ✅         | ✅             |
| Ease of Use    | ✅    | ❌        | ❌         | ✅             |

<!-- ## Final Thoughts

GORM is a powerful ORM that can save you a ton of time and boilerplate code when working with databases in Go.

It’s not perfect, and sometimes it’s better to write raw SQL, but for most use cases, it’s an excellent choice.

So, next time you find yourself writing `SELECT * FROM users` for the 100th time, give GORM a shot! -->

***

## Key Ideas

| Concept             | Summary                                                                    |
| ------------------- | -------------------------------------------------------------------------- |
| GORM Overview       | GORM is a powerful ORM for Golang, simplifying database interactions.      |
| Features            | Supports auto migrations, relationships, hooks, transactions, and raw SQL. |
| Supported Databases | Works with PostgreSQL, MySQL, SQLite, and SQL Server.                      |
| CRUD Operations     | Easily create, read, update, and delete records.                           |
| Relationships       | Supports one-to-many and many-to-many relationships.                       |
| Comparisons         | GORM stacks up well against other ORMs with strong feature support.        |

***

## References

* [GORM Official Documentation](https://gorm.io/)
* [Go Database/SQL Package](https://golang.org/pkg/database/sql/)
* [GORM GitHub Repository](https://github.com/go-gorm/gorm)
