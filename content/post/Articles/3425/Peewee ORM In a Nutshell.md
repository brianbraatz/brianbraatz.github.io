---
title: Peewee ORM In a Nutshell
description: ""
slug: python-peewee-orm-in-a-nutshell
date: 2018-09-15
image: post/Articles/IMAGES/pythonlogo.png
categories:
  - Python
  - Database
  - ORM
  - Peewee
tags:
  - Python
  - Database
  - Orm
  - Peewee
  - Sqlite
  - Mysql
  - Postgresql
draft: false
weight: 524
categories_ref:
  - Python
  - Database
  - ORM
  - Peewee
slug_calculated: https://brianbraatz.github.io/p/python-peewee-orm-in-a-nutshell
lastmod: 2025-03-14T16:40:14.153Z
---
<!-- 
# Python Peewee ORM In a Nutshell

Ah, ORMs. You either love 'em or you hate 'em. But let's be real—nobody loves writing raw SQL all day. So, enter **Peewee**! It's like SQLAlchemy’s chill younger sibling—lightweight, simple, and doesn’t make you question your life choices.

Let’s dive into this bad boy and see what makes it tick! -->

***

## What the Heck is Peewee?

Peewee is a tiny but powerful **Object-Relational Mapping (ORM)** library for Python. It simplifies database interactions by letting you write Pythonic code instead of raw SQL queries.

Unlike heavyweight ORMs like SQLAlchemy, Peewee is **small, fast, and easy to learn**, making it perfect for small to medium-sized applications.

### Why Should You Use Peewee?

* **Lightweight** – Less bloat, faster performance.
* **Easy to Use** – Simple, intuitive API.
* **Supports Multiple Databases** – SQLite, MySQL, PostgreSQL, you name it!
* **Built-in Migrations** – No more manual schema changes.
* **Great for Beginners** – Minimal setup and easy to grasp.

***

## Installing Peewee

First things first—let’s get this thing installed. If you don’t have Peewee yet, just run:

```bash
pip install peewee
```

If you're feeling fancy and need MySQL or PostgreSQL support, install with:

```bash
pip install peewee psycopg2-binary mysql-connector-python
```

***

## Setting Up a Simple Database

Let’s start with **SQLite** (because it’s the easiest to set up).

### Define Your Database and Models

```python
from peewee import *

# Create a database instance (SQLite for now)
db = SqliteDatabase('my_database.db')

# Define a model
class BaseModel(Model):
    class Meta:
        database = db  # This model uses the "db" database

class User(BaseModel):
    username = CharField(unique=True)
    email = CharField(unique=True)
    joined_at = DateTimeField(default=datetime.datetime.now)

# Create the tables
db.connect()
db.create_tables([User])
```

Boom! We just defined a SQLite database and a **User** model.

***

## Adding Some Data

Let’s toss in some users:

```python
# Insert data
User.create(username='john_doe', email='john@example.com')
User.create(username='jane_doe', email='jane@example.com')
```

Now we have users in our database! That was easy, right?

***

## Querying Data

Peewee makes querying **stupidly simple**:

```python
# Fetch all users
users = User.select()
for user in users:
    print(user.username, user.email)
```

Want a single user?

```python
user = User.get(User.username == 'john_doe')
print(user.email)  # Output: john@example.com
```

Need filtering? No problem!

```python
users = User.select().where(User.username.contains('doe'))
```

***

## Updating and Deleting Data

Updating is also a breeze:

```python
query = User.update(email='new_email@example.com').where(User.username == 'john_doe')
query.execute()
```

And when it's time to say goodbye:

```python
user = User.get(User.username == 'john_doe')
user.delete_instance()
```

***

## Peewee vs Other ORMs

Let’s compare Peewee with some other popular ORMs:

| Feature         | Peewee       | SQLAlchemy | Django ORM  |
| --------------- | ------------ | ---------- | ----------- |
| **Complexity**  | Low          | High       | Medium      |
| **Setup Time**  | Fast         | Slow       | Medium      |
| **Performance** | Fast         | Slower     | Medium      |
| **Query Power** | Good         | Excellent  | Good        |
| **Best for**    | Small/Medium | Large Apps | Django Apps |

If you want a **lightweight, simple ORM**, Peewee is a solid choice!

***

## Using Peewee with MySQL or PostgreSQL

Switching databases is easy—just change the database instance:

```python
# For MySQL
db = MySQLDatabase('my_database', user='root', password='password', host='localhost')

# For PostgreSQL
db = PostgresqlDatabase('my_database', user='postgres', password='password', host='localhost')
```

Then, just update the `database` in your models and you’re good to go!

***

<!-- ## Conclusion

Peewee is an awesome, lightweight ORM that makes working with databases **way easier** than raw SQL. It’s perfect for small to medium-sized projects, and its simple syntax makes it a joy to use.

So, if you’re tired of writing SQL queries by hand but don’t want the overhead of SQLAlchemy, **give Peewee a shot!**

Happy coding! 🚀 -->

***

## Key Ideas

| Key Idea              | Summary                                  |
| --------------------- | ---------------------------------------- |
| What is Peewee?       | A lightweight ORM for Python             |
| Installation          | Install with `pip install peewee`        |
| Setting up SQLite     | Define models and create tables          |
| Querying data         | Use `.select()`, `.get()`, `.where()`    |
| Updating and Deleting | Use `.update()` and `.delete_instance()` |
| Comparison with ORMs  | Lightweight alternative to SQLAlchemy    |
| Multi-DB Support      | Supports SQLite, MySQL, PostgreSQL       |

***

## References

* [Peewee Official Documentation](https://peewee.readthedocs.io/)
* [Python MySQL Connector](https://dev.mysql.com/doc/connector-python/en/)
* [PostgreSQL Psycopg2](https://www.psycopg.org/docs/)
