---
title: SQLBoiler In a Nutshell
description: SQLBoiler In a Nutshell
slug: sqlboiler-in-a-nutshell
date: 2017-09-14
image: post/Articles/IMAGES/Go-Logo_Blue.png
categories:
  - Go
  - SQLBoiler
  - ORM
  - Code Generation
tags:
  - Go
  - Sqlboiler
  - Orm
  - Code generation
  - Database
draft: false
weight: 457
categories_ref:
  - Go
  - SQLBoiler
  - ORM
  - Code Generation
lastmod: 2025-03-14T15:45:06.242Z
---
# SQLBoiler In a Nutshell

## What the Heck is SQLBoiler? 🤔

Alright, so you're tired of writing boring, repetitive SQL queries, and you want something to do the heavy lifting for you. But at the same time, you don’t want a clunky ORM that hides SQL from you like a secret family recipe.

Enter **SQLBoiler**! 🎉

SQLBoiler is a **Go ORM (Object-Relational Mapper)** that **generates Go structs and methods based on your database schema**. The cool part? It doesn’t force you into a magical, invisible ORM world. Instead, it **reads your database schema and generates strongly-typed Go code**. Think of it as an ORM with training wheels—you still control the bike, but it helps you ride smoothly.

## Why Should You Care? 😎

* **It generates code** – No more writing boilerplate CRUD operations!
* **It’s database-first** – If you’re the type who likes designing your database schema first and then working on the Go code, SQLBoiler is for you.
* **Performance is solid** – Since it generates native Go code, there's no runtime magic slowing things down.
* **Cross-database support** – Works with PostgreSQL, MySQL, MSSQL, and SQLite.

## Installing SQLBoiler 🚀

You need to install **SQLBoiler** and a database driver for your specific database.

```sh
# Install SQLBoiler
go install github.com/volatiletech/sqlboiler/v4@latest

# Install a database driver (choose one based on your DB)
go install github.com/volatiletech/sqlboiler/v4/drivers/sqlboiler-psql@latest  # PostgreSQL
go install github.com/volatiletech/sqlboiler/v4/drivers/sqlboiler-mysql@latest  # MySQL
go install github.com/volatiletech/sqlboiler/v4/drivers/sqlboiler-sqlite3@latest  # SQLite
```

## Setting Up SQLBoiler ⚙️

You'll need a `sqlboiler.toml` file to configure SQLBoiler. Here’s an example for PostgreSQL:

```toml
dbname = "mydatabase"
host = "localhost"
port = 5432
user = "myuser"
pass = "mypassword"
schema = "public"
dialect = "psql"
out_dir = "models"
```

Run the generator:

```sh
sqlboiler psql
```

BOOM! 💥 SQLBoiler will scan your database and generate models in the `models/` directory.

## SQLBoiler in Action 🎬

Let’s say we have a **users** table:

```sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

SQLBoiler will generate a `User` struct like this:

```go
type User struct {
    ID        int       `boil:"id" json:"id"`
    Name      string    `boil:"name" json:"name"`
    Email     string    `boil:"email" json:"email"`
    CreatedAt time.Time `boil:"created_at" json:"created_at"`
}
```

Now, let's use SQLBoiler to do some actual work.

### Insert a New User

```go
import (
    "context"
    "database/sql"
    "fmt"
    "log"
    "models"
    _ "github.com/lib/pq"
)

func main() {
    db, err := sql.Open("postgres", "your_connection_string")
    if err != nil {
        log.Fatal(err)
    }
    defer db.Close()

    ctx := context.Background()
    user := models.User{
        Name:  "John Doe",
        Email: "john.doe@example.com",
    }

    err = user.Insert(ctx, db, boil.Infer())
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println("User inserted successfully!")
}
```

### Query a User

```go
user, err := models.Users(qm.Where("email = ?", "john.doe@example.com")).One(ctx, db)
if err != nil {
    log.Fatal(err)
}
fmt.Println("User found:", user.Name)
```

### Update a User

```go
user.Name = "Johnny Doe"
_, err = user.Update(ctx, db, boil.Infer())
if err != nil {
    log.Fatal(err)
}
fmt.Println("User updated successfully!")
```

### Delete a User

```go
_, err = user.Delete(ctx, db)
if err != nil {
    log.Fatal(err)
}
fmt.Println("User deleted successfully!")
```

## SQLBoiler vs Other ORMs 🥊

| Feature         | SQLBoiler | GORM      | Ent    | XORM      |
| --------------- | --------- | --------- | ------ | --------- |
| Code Generation | ✅         | ❌         | ✅      | ❌         |
| Performance     | 🔥 Fast   | 🐢 Slower | ⚡ Fast | 🐢 Slower |
| Database First  | ✅         | ❌         | ✅      | ✅         |
| Active Record   | ❌         | ✅         | ✅      | ✅         |
| Strict Typing   | ✅         | ❌         | ✅      | ❌         |
| Learning Curve  | Medium    | Easy      | Medium | Easy      |

<!-- ## Final Thoughts 🤔

SQLBoiler is awesome if you love **generated, strongly-typed code** that plays well with Go's philosophy. It’s not an ORM in the traditional sense, but more of a **Go code generator for databases**.

If you want total control over SQL while still reducing the pain of writing repetitive queries, SQLBoiler is **a solid choice**. 🚀 -->

## Key Ideas

| Concept         | Summary                                              |
| --------------- | ---------------------------------------------------- |
| SQLBoiler       | Go ORM that generates code from a database schema    |
| Installation    | Install SQLBoiler and the required database driver   |
| Code Generation | Generates Go structs and methods for CRUD operations |
| Querying Data   | Uses query modifiers to filter and fetch records     |
| Comparison      | Compared with GORM, Ent, and XORM                    |

## References

* [SQLBoiler GitHub](https://github.com/volatiletech/sqlboiler)
* [SQLBoiler Documentation](https://github.com/volatiletech/sqlboiler/tree/master/docs)
* [Go Database/SQL Package](https://pkg.go.dev/database/sql)
