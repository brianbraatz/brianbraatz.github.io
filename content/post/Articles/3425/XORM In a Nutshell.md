---
title: XORM In a Nutshell
description: XORM In a Nutshell
slug: xorm-in-a-nutshell
date: 2017-04-15
image: post/Articles/IMAGES/Go-Logo_Blue.png
categories:
  - Go
  - Xorm
  - ORM
  - Database
tags:
  - Go
  - Xorm
  - Orm
  - Database
draft: false
weight: 482
categories_ref:
  - Go
  - Xorm
  - ORM
  - Database
lastmod: 2025-03-14T15:45:06.426Z
---
<!-- 
# XORM In a Nutshell

If you've ever wrestled with raw SQL queries in Go and wished for something more elegant, welcome to **XORM**—the ORM (Object-Relational Mapper) that makes dealing with databases in Go less painful.

It's fast, it's easy, and it lets you interact with databases like a civilized human being instead of a caveman smashing SQL statements together.

Let's dive in! -->

***

## What the Heck is XORM?

XORM is a Go-based ORM that provides a simple way to interact with SQL databases like MySQL, PostgreSQL, and SQLite.

It maps Go structs to database tables, so you don’t have to write raw SQL for every little thing.

With XORM, you can:

* Define Go structs as database models.
* Query, insert, update, and delete records easily.
* Handle transactions and migrations without losing your sanity.

Sounds good? Let's see it in action!

***

## Installing XORM

First things first, install XORM:

```sh
go get xorm.io/xorm
go get -u github.com/mattn/go-sqlite3  # Example for SQLite support
```

You might need different drivers depending on your database:

* MySQL: `go get -u github.com/go-sql-driver/mysql`
* PostgreSQL: `go get -u github.com/lib/pq`
* SQLite: `go get -u github.com/mattn/go-sqlite3`

Once that’s done, let's play with some code!

***

## Defining a Model

In XORM, you define your database tables as Go structs:

```go
package main

import (
	"fmt"
	"xorm.io/xorm"
	_ "github.com/mattn/go-sqlite3"
)

type User struct {
	ID    int64  `xorm:"pk autoincr"`
	Name  string `xorm:"unique"`
	Email string
}

func main() {
	db, err := xorm.NewEngine("sqlite3", "test.db")
	if err != nil {
		panic(err)
	}

	db.Sync(new(User)) // Create the table if it doesn’t exist

	fmt.Println("Database ready!")
}
```

Boom! You now have a `User` table in your SQLite database.

***

## CRUD Operations with XORM

Now let's do some basic operations.

### Insert a User

```go
user := User{Name: "Alice", Email: "alice@example.com"}
_, err := db.Insert(&user)
if err != nil {
	fmt.Println("Insert failed:", err)
}
```

### Fetch a User

```go
var user User
db.Where("name = ?", "Alice").Get(&user)
fmt.Println("Fetched user:", user)
```

### Update a User

```go
user.Email = "newalice@example.com"
db.ID(user.ID).Update(&user)
```

### Delete a User

```go
db.ID(user.ID).Delete(&user)
```

***

## XORM vs Other ORMs

How does XORM stack up against the competition? Here's a handy comparison:

| Feature         | XORM | GORM      | SQLBoiler |
| --------------- | ---- | --------- | --------- |
| Ease of Use     | ⭐⭐⭐⭐ | ⭐⭐⭐       | ⭐⭐        |
| Performance     | ⭐⭐⭐⭐ | ⭐⭐⭐       | ⭐⭐⭐⭐⭐     |
| Auto Migration  | Yes  | Yes       | No        |
| Raw SQL Support | Yes  | Limited   | Yes       |
| Documentation   | Good | Excellent | Average   |

***

## Transactions in XORM

Transactions are easy in XORM. Here's how you do it:

```go
session := db.NewSession()
defer session.Close()

if err := session.Begin(); err != nil {
	panic(err)
}

if _, err := session.Insert(&User{Name: "Bob", Email: "bob@example.com"}); err != nil {
	session.Rollback()
	panic(err)
}

session.Commit()
```

If anything goes wrong, `session.Rollback()` will undo the changes.

<!-- 
---

## Wrapping Up

XORM is a powerful, easy-to-use ORM for Go that makes database interactions a breeze.

It’s great for quick projects, and while it may not be as feature-rich as GORM, it’s a solid choice if you want simplicity and performance.

N -->
