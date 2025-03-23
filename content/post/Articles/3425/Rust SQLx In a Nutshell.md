---
title: SQLx In a Nutshell
description: ""
slug: rust-sqlx-in-a-nutshell
date: 2016-08-17
image: post/Articles/IMAGES/rust.png
categories:
  - Rust
  - Database
  - SQLx
  - Async
tags:
  - Rust
  - Database
  - SQLx
  - Postgres
  - MySQL
  - SQLite
draft: false
weight: 513
categories_ref:
  - Rust
  - Database
  - SQLx
slug_calculated: https://brianbraatz.github.io/p/rust-sqlx-in-a-nutshell
lastmod: 2025-03-23T16:17:36.225Z
---
# Rust SQLx In a Nutshell

Rust and databases? Oh yeah, they go together like peanut butter and jelly—if peanut butter was a highly performant systems programming language and jelly was a blazing-fast async database library. That’s right, we’re talking about **SQLx**!

<!-- So, buckle up, grab a coffee (or a Rust-themed energy drink if that exists), and let’s dive deep into **Rust SQLx**, the async database toolkit that will make you question why you ever tolerated slow and unsafe database interactions. -->

***

## What is SQLx?

SQLx is an asynchronous, compile-time checked SQL library for Rust. Think of it as Diesel’s cool cousin who doesn’t need macros to do their job. Unlike Diesel, SQLx doesn’t generate Rust types for your queries at compile time—it just **directly executes SQL** while ensuring at compile-time that your queries are valid.

Key features of SQLx:

* **Async-first** – Leverages Rust’s async ecosystem (Tokio, async-std, etc.).
* **Compile-time checked SQL** – No more runtime SQL errors!
* **Supports PostgreSQL, MySQL, SQLite, and MSSQL** – Pick your poison.
* **Minimal boilerplate** – No need to define Rust structs for everything (but you can if you want!).
* **Connection pooling** – Because nobody wants to wait for a fresh connection every time.
* **Row mappings** – Map query results into Rust structs seamlessly.

***

## Installing SQLx

To get started, add SQLx to your `Cargo.toml` file. You'll need to specify the database features you want.

```toml
[dependencies]
sqlx = { version = "0.7", features = ["postgres", "runtime-tokio"] }
tokio = { version = "1", features = ["full"] }
```

If you're using MySQL, SQLite, or MSSQL, replace `"postgres"` with `"mysql"`, `"sqlite"`, or `"mssql"`.

***

## Connecting to a Database

Alright, let’s get our hands dirty. First, we need to connect to a database. Here’s how you do it with PostgreSQL:

```rust
use sqlx::{PgPool};
use std::env;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let pool = PgPool::connect(&database_url).await?;
    
    println!("Connected to database!");
    Ok(())
}
```

Boom! You’re connected.

Make sure you set your `DATABASE_URL` environment variable before running this.

For MySQL, just replace `PgPool` with `MySqlPool` and for SQLite, use `SqlitePool`.

***

## Running Queries

SQLx lets you execute queries using **raw SQL** while still being safe and ergonomic.

### Simple Query Execution

Let’s fetch some users from a `users` table:

```rust
use sqlx::Row;

async fn fetch_users(pool: &sqlx::PgPool) -> Result<(), sqlx::Error> {
    let rows = sqlx::query("SELECT id, name FROM users")
        .fetch_all(pool)
        .await?;
    
    for row in rows {
        let id: i32 = row.get("id");
        let name: String = row.get("name");
        println!("User: {} - {}", id, name);
    }
    
    Ok(())
}
```

### Query with Parameters

SQLx supports query parameters like a boss:

```rust
async fn insert_user(pool: &sqlx::PgPool, name: &str) -> Result<(), sqlx::Error> {
    sqlx::query("INSERT INTO users (name) VALUES ($1)")
        .bind(name)
        .execute(pool)
        .await?;
    
    println!("User added successfully!");
    Ok(())
}
```

***

## Async Connection Pooling

SQLx provides built-in connection pooling. Instead of creating a new connection every time, we can use a **connection pool**:

```rust
use sqlx::PgPool;

async fn create_pool() -> Result<PgPool, sqlx::Error> {
    let pool = PgPool::connect("postgres://user:password@localhost/db_name").await?;
    Ok(pool)
}
```

This helps avoid the dreaded **"too many connections"** error!

***

## Comparison: SQLx vs Diesel vs SeaORM

| Feature                   | SQLx                           | Diesel                  | SeaORM                  |
| ------------------------- | ------------------------------ | ----------------------- | ----------------------- |
| Compile-time SQL Checking | ✅                              | ✅                       | ❌                       |
| Async Support             | ✅                              | ❌                       | ✅                       |
| ORM Features              | ❌ (Raw SQL)                    | ✅                       | ✅                       |
| Macros Required           | ❌                              | ✅                       | ✅                       |
| Connection Pooling        | ✅                              | ✅                       | ✅                       |
| Database Support          | Postgres, MySQL, SQLite, MSSQL | Postgres, MySQL, SQLite | Postgres, MySQL, SQLite |

***

<!-- 
## Conclusion

SQLx is **blazing fast, async-friendly, and safer than a SQL-injection-riddled application**. If you love writing raw SQL but hate runtime errors, SQLx is **your best bet**.

So, go forth, **query confidently**, and build amazing Rust applications! -->

***

## Key Ideas

| Concept            | Summary                                                                            |
| ------------------ | ---------------------------------------------------------------------------------- |
| SQLx               | Async, compile-time checked SQL library for Rust                                   |
| Features           | Supports PostgreSQL, MySQL, SQLite, MSSQL, connection pooling, and async execution |
| Install            | Add `sqlx` to `Cargo.toml` with feature flags for your database                    |
| Queries            | Use raw SQL with `.query()` and `.bind()` for parameters                           |
| Connection Pooling | Use `PgPool`, `MySqlPool`, or `SqlitePool` to manage DB connections efficiently    |
| Comparison         | SQLx vs Diesel vs SeaORM in terms of features and async support                    |

***

## References

* [SQLx GitHub Repo](https://github.com/launchbadge/sqlx)
* [SQLx Documentation](https://docs.rs/sqlx/latest/sqlx/)
* [Rust Async Book](https://rust-lang.github.io/async-book/)
