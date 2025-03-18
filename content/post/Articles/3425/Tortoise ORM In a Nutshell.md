---
title: Tortoise ORM In a Nutshell
description: ""
slug: python-tortoise-orm-in-a-nutshell
date: 2017-06-15
image: post/Articles/IMAGES/pythonlogo.png
categories:
  - Python
  - ORM
  - Tortoise
tags:
  - Python
  - Orm
  - Tortoise
  - Async
  - Database
draft: false
weight: 653
categories_ref:
  - Python
  - ORM
  - Tortoise
slug_calculated: https://brianbraatz.github.io/p/python-tortoise-orm-in-a-nutshell
lastmod: 2025-03-14T16:40:14.938Z
---
# Python Tortoise ORM In a Nutshell

## Introduction

Tortoise ORM is like that chill friend who handles all your database interactions while you focus on writing awesome Python code.

It’s an **asynchronous** ORM (Object-Relational Mapper) that plays nicely with **async frameworks** like FastAPI, Starlette, and even the rebel asyncio.

So if you’ve ever dreamed of making database queries without blocking your app’s event loop, Tortoise ORM is here to make that dream come true. 🐢✨

## Why Use Tortoise ORM?

* **Asynchronous Support** – No more blocking operations. Your app stays fast and responsive.
* **Simple API** – Tortoise ORM keeps things clean and Pythonic.
* **Built-in Schema Generation** – No need to manually create tables.
* **Works with Pydantic** – Plays well with FastAPI and data validation.
* **Supports Multiple Databases** – PostgreSQL, MySQL, SQLite, and even MariaDB.

## Setting Up Tortoise ORM

Before you go full ninja with Tortoise ORM, you need to install it. So fire up your terminal and run:

```sh
pip install tortoise-orm
```

If you’re planning to use it with **PostgreSQL** or **MySQL**, don’t forget to install the corresponding database driver:

```sh
pip install asyncpg  # For PostgreSQL
pip install aiomysql  # For MySQL
```

## Basic Usage

Let’s build a **simple model** with Tortoise ORM. Imagine we have a blog, and we need a `Post` model.

### Defining a Model

```python
from tortoise import fields, Tortoise, models

class Post(models.Model):
    id = fields.IntField(pk=True)
    title = fields.CharField(max_length=255)
    content = fields.TextField()
    created_at = fields.DatetimeField(auto_now_add=True)
```

### Initializing Tortoise ORM

Before using the ORM, we need to initialize it with a database configuration.

```python
import asyncio

async def init():
    await Tortoise.init(
        db_url="sqlite://db.sqlite3",
        modules={"models": ["__main__"]}  # Points to the module where our models are defined
    )
    await Tortoise.generate_schemas()
```

### Creating and Fetching Data

```python
async def create_post():
    post = await Post.create(title="Hello World", content="This is my first post!")
    print(f"Post Created: {post.title}")

async def get_posts():
    posts = await Post.all()
    for post in posts:
        print(post.title)

async def main():
    await init()
    await create_post()
    await get_posts()

asyncio.run(main())
```

Boom! 🎉 You just created a database record **asynchronously**.

## Querying Data

### Filtering Records

```python
post = await Post.filter(title="Hello World").first()
print(post.content)
```

### Updating Records

```python
post = await Post.get(id=1)
post.title = "Updated Title"
await post.save()
```

### Deleting Records

```python
post = await Post.get(id=1)
await post.delete()
```

## Comparison Table: Tortoise ORM vs Other ORMs

| Feature           | Tortoise ORM              | SQLAlchemy           | Django ORM                |
| ----------------- | ------------------------- | -------------------- | ------------------------- |
| Async Support     | ✅ Yes                     | ✅ Yes (with asyncio) | ❌ No                      |
| Schema Generation | ✅ Yes                     | ❌ No                 | ✅ Yes                     |
| Database Support  | PostgreSQL, MySQL, SQLite | Many                 | PostgreSQL, MySQL, SQLite |
| Ease of Use       | ✅ Simple API              | 🔧 Complex           | 🔥 Easy                   |
| Pydantic Support  | ✅ Yes                     | ❌ No                 | ❌ No                      |

## Running Migrations

Tortoise ORM doesn’t have built-in migrations (yet), but you can use **Aerich**, which is a migration tool built for Tortoise.

Install it with:

```sh
pip install aerich
```

Initialize it:

```sh
aerich init -t config.TORTOISE_ORM
```

Create and apply migrations:

```sh
aerich migrate
aerich upgrade
```

<!-- ## Conclusion

Tortoise ORM is a fantastic choice if you’re working on an **async project** and need a **lightweight, easy-to-use** ORM.

It’s perfect for **FastAPI, Starlette, and any asyncio-based app**.

If you love async programming and hate blocking calls, Tortoise ORM might just become your new best friend. 🐢🚀 -->

## Key Ideas

| Key Idea                  | Summary                                      |
| ------------------------- | -------------------------------------------- |
| What is Tortoise ORM?     | An async ORM for Python with simple API      |
| Why use it?               | Asynchronous, lightweight, schema generation |
| How to install?           | `pip install tortoise-orm`                   |
| Basic usage?              | Define models, init ORM, create/fetch data   |
| How to query?             | Use `.filter()`, `.get()`, `.all()` methods  |
| How to update/delete?     | Modify and call `.save()` / `.delete()`      |
| Comparison to other ORMs? | Async support, schema gen, ease of use       |
| Migrations?               | Use Aerich for migrations                    |

## References

* [Tortoise ORM Docs](https://tortoise-orm.readthedocs.io/)
* [FastAPI Docs](https://fastapi.tiangolo.com/)
* [Aerich Migration Tool](https://github.com/tortoise/aerich)

***
