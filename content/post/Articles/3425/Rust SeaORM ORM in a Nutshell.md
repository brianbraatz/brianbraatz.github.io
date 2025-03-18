---
title: SeaORM in a Nutshell
description: ""
slug: rust-seaorm-orm-in-a-nutshell
date: 2017-09-15
image: post/Articles/IMAGES/rust.png
categories:
  - Rust
  - SeaORM
  - Database
  - ORM
tags:
  - Rust
  - Seaorm
  - Database
  - Orm
  - Async
  - Postgresql
  - Mysql
  - Sqlite
draft: false
weight: 537
categories_ref:
  - Rust
  - SeaORM
  - Database
  - ORM
slug_calculated: https://brianbraatz.github.io/p/rust-seaorm-orm-in-a-nutshell
lastmod: 2025-03-14T16:40:14.277Z
---
# Rust SeaORM: ORM in a Nutshell

## Introduction

Rust is fast, safe, and full of surprises. But writing database queries by hand? Not fun.

That's where **SeaORM** comes in! If you've ever suffered through the pain of manually managing SQL queries, let me introduce you to a Rust ORM that’s async-friendly, powerful, and won’t make you cry at night.

<!-- 
In this article, we’ll take a casual but in-depth look at **SeaORM**, with plenty of examples and a sprinkle of humor to keep things interesting. -->

## What is SeaORM?

SeaORM is an **async ORM (Object-Relational Mapper) for Rust**. It makes working with databases in Rust much easier, supporting MySQL, PostgreSQL, and SQLite. It’s built on top of **SQLx**, an async database driver.

In short: **SQL but make it Rusty.**

### Why Use SeaORM?

* Fully **asynchronous** (great for modern Rust apps)
* Supports **multiple databases** (SQLite, MySQL, PostgreSQL)
* **Compile-time query validation** (because who likes runtime errors?)
* Built-in **migration support**
* **Entity-based API** (so you can stop writing raw SQL like a caveman)

## Setting Up SeaORM

Let's dive right in and set up SeaORM in a Rust project.

### 1. Add Dependencies

Open your `Cargo.toml` and add the required dependencies:

```toml
[dependencies]
sea-orm = { version = "0.12", features = ["sqlx-sqlite", "runtime-tokio-rustls"] }
tokio = { version = "1", features = ["full"] }
```

### 2. Define Your Database Connection

```rust
use sea_orm::{Database, DatabaseConnection};
use tokio;

#[tokio::main]
async fn main() {
    let db_url = "sqlite::memory:"; // In-memory SQLite database
    let db: DatabaseConnection = Database::connect(db_url).await.unwrap();
    println!("Database connected!");
}
```

Congrats! You now have a database connection. 🎉

## Defining Entities (a.k.a. Tables, But Fancy)

In SeaORM, database tables are represented as **entities**.

### Example: Define a `User` Entity

```rust
use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, DeriveEntityModel)]
#[sea_orm(table_name = "users")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i32,
    pub name: String,
    pub email: String,
}

#[derive(Copy, Clone, Debug, EnumIter)]
pub enum Relation {}

impl Related<()> for Entity {}
impl ActiveModelBehavior for ActiveModel {}
```

This defines a `users` table with three columns: `id`, `name`, and `email`.

## Querying the Database

### Insert Data

```rust
use sea_orm::ActiveModelTrait;

let user = user::ActiveModel {
    name: Set("Alice".to_string()),
    email: Set("alice@example.com".to_string()),
    ..Default::default()
};

let inserted = user.insert(&db).await.unwrap();
println!("Inserted user with ID: {}", inserted.id);
```

### Retrieve Data

```rust
use sea_orm::EntityTrait;

let user = user::Entity::find_by_id(1).one(&db).await.unwrap();
if let Some(user) = user {
    println!("User: {} - {}", user.name, user.email);
} else {
    println!("User not found!");
}
```

### Update Data

```rust
let mut user: user::ActiveModel = user.unwrap().into();
user.name = Set("Alice Wonderland".to_string());
let updated_user = user.update(&db).await.unwrap();
println!("Updated user: {}", updated_user.name);
```

### Delete Data

```rust
user::Entity::delete_by_id(1).exec(&db).await.unwrap();
println!("User deleted!");
```

## Comparison: SeaORM vs Other Rust ORMs

| Feature                       | SeaORM | Diesel             | SQLx              |
| ----------------------------- | ------ | ------------------ | ----------------- |
| Async Support                 | ✅ Yes  | ❌ No               | ✅ Yes             |
| Compile-time Query Validation | ✅ Yes  | ✅ Yes              | ✅ Yes             |
| Built-in Migrations           | ✅ Yes  | ✅ Yes              | ❌ No              |
| Multiple Database Support     | ✅ Yes  | ✅ Yes              | ✅ Yes             |
| Easy to Use                   | ✅ Yes  | ❌ No (macro-heavy) | ❌ No (manual SQL) |

<!-- ## Conclusion

SeaORM is a powerful, async-friendly ORM for Rust that makes database interactions a breeze.

If you're building a Rust web app or a data-heavy service, SeaORM can save you time and effort while keeping your code clean and readable.

Now go forth and write some Rusty database code! -->

## Key Ideas

| Key Idea          | Summary                                                 |
| ----------------- | ------------------------------------------------------- |
| What is SeaORM?   | An async ORM for Rust, built on SQLx.                   |
| Why use SeaORM?   | Asynchronous, multiple DB support, compile-time safety. |
| Setting up SeaORM | Add dependencies, connect to the database.              |
| Defining Entities | Use `DeriveEntityModel` to define tables.               |
| Querying Data     | CRUD operations with async queries.                     |
| Comparison Table  | SeaORM vs Diesel vs SQLx.                               |

## References

* [SeaORM Documentation](https://www.sea-ql.org/SeaORM/)
* [Rust Async Programming](https://rust-lang.github.io/async-book/)
* [SQLx](https://github.com/launchbadge/sqlx)
