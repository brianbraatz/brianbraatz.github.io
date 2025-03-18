---
title: SQLAlchemy In a Nutshell
description: ""
slug: python-sqlalchemy-in-a-nutshell
date: 2016-05-14
image: post/Articles/IMAGES/pythonlogo.png
categories:
  - Python
  - SQLAlchemy
  - Database
  - ORM
  - SQL
tags:
  - Python
  - Sqlalchemy
  - Database
  - Orm
  - Sql
  - Postgresql
  - Mysql
  - Sqlite
draft: false
weight: 482
categories_ref:
  - Python
  - SQLAlchemy
  - Database
  - ORM
  - SQL
slug_calculated: https://brianbraatz.github.io/p/python-sqlalchemy-in-a-nutshell
lastmod: 2025-03-14T16:40:14.531Z
---
# Python SQLAlchemy In a Nutshell

So, you’ve decided to mess with databases in Python, huh? Good choice! You could write raw SQL like a caveman, but why do that when SQLAlchemy exists?

SQLAlchemy is like the cool best friend who does all the hard work for you while making sure you don’t break anything.

It’s Python’s most popular **Object-Relational Mapper (ORM)**, and it makes working with databases feel like handling normal Python objects.

Let’s dive in and see why SQLAlchemy is awesome.

***

## Why Use SQLAlchemy?

1. **It Works with Multiple Databases** – PostgreSQL, MySQL, SQLite, you name it!
2. **It Lets You Write Python Instead of SQL** – Because, let's be honest, SQL can be a pain.
3. **It Handles Connections for You** – No more worrying about opening and closing database connections manually.
4. **It’s Powerful and Flexible** – You can go full ORM or write raw SQL if you’re feeling adventurous.

***

## SQLAlchemy vs Raw SQL vs Django ORM

Let’s start with a quick comparison of SQLAlchemy, raw SQL, and Django’s ORM.

| Feature             | SQLAlchemy ORM | Raw SQL          | Django ORM       |
| ------------------- | -------------- | ---------------- | ---------------- |
| Cross-DB Support    | ✅ Yes          | ⚠️ Manual effort | ✅ Yes            |
| Write Pythonic Code | ✅ Yes          | ❌ Nope           | ✅ Yes            |
| Connection Handling | ✅ Yes          | ❌ No             | ✅ Yes            |
| Full Control        | ✅ Yes          | ✅ Yes            | ❌ Limited        |
| Query Performance   | ✅ Optimized    | ✅ Fast           | ⚠️ Can be slower |
| Learning Curve      | 🟡 Medium      | 🟢 Easy          | 🟡 Medium        |

***

## Setting Up SQLAlchemy

First things first, install it:

```bash
pip install sqlalchemy
```

If you need database drivers, install them too:

```bash
pip install psycopg2  # PostgreSQL
pip install pymysql  # MySQL
pip install sqlite3  # SQLite (built-in)
```

Now, let's create a simple SQLite database using SQLAlchemy.

***

## Creating a Simple Database with SQLAlchemy ORM

### Step 1: Import and Set Up the Database Engine

```python
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, declarative_base

# Create an SQLite database
engine = create_engine("sqlite:///example.db")
Session = sessionmaker(bind=engine)
session = Session()
Base = declarative_base()
```

### Step 2: Define a Model (Aka Your Fancy Table)

```python
from sqlalchemy import Column, Integer, String

class User(Base):
    __tablename__ = "users"
    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    email = Column(String, unique=True, nullable=False)
```

### Step 3: Create the Table

```python
Base.metadata.create_all(engine)
```

Boom! You’ve got yourself a table.

***

## Adding, Querying, and Deleting Data

### Adding Data

```python
new_user = User(name="Alice", email="alice@example.com")
session.add(new_user)
session.commit()
```

### Querying Data

```python
user = session.query(User).filter_by(name="Alice").first()
print(user.email)  # alice@example.com
```

### Deleting Data

```python
session.delete(user)
session.commit()
```

***

## Writing Raw SQL in SQLAlchemy

Sometimes, you just need good old-fashioned SQL.

```python
result = session.execute("SELECT * FROM users")
for row in result:
    print(row)
```

***

## Relationships (One-to-Many & Many-to-Many)

### One-to-Many Relationship (User -> Posts)

```python
from sqlalchemy import ForeignKey
from sqlalchemy.orm import relationship

class Post(Base):
    __tablename__ = "posts"
    id = Column(Integer, primary_key=True)
    title = Column(String, nullable=False)
    user_id = Column(Integer, ForeignKey("users.id"))
    user = relationship("User", back_populates="posts")

User.posts = relationship("Post", order_by=Post.id, back_populates="user")
```

### Many-to-Many Relationship (Users <-> Groups)

```python
from sqlalchemy import Table

user_group = Table(
    "user_group",
    Base.metadata,
    Column("user_id", Integer, ForeignKey("users.id")),
    Column("group_id", Integer, ForeignKey("groups.id"))
)

class Group(Base):
    __tablename__ = "groups"
    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    users = relationship("User", secondary=user_group, back_populates="groups")

User.groups = relationship("Group", secondary=user_group, back_populates="users")
```

<!-- ---

## Conclusion

SQLAlchemy is an absolute powerhouse for working with databases in Python. Whether you love ORMs or prefer raw SQL, it has something for everyone.

So go forth, install SQLAlchemy, and start managing databases like a pro (or at least pretend to). -->

***

## Key Ideas

| Key Idea        | Description                                               |
| --------------- | --------------------------------------------------------- |
| SQLAlchemy      | A Python ORM that simplifies database management          |
| ORM vs Raw SQL  | SQLAlchemy allows both ORM and raw SQL usage              |
| Creating Tables | Use `Base.metadata.create_all(engine)` to generate tables |
| Querying Data   | Use `session.query(User).filter_by(name="Alice").first()` |
| Relationships   | Supports One-to-Many and Many-to-Many relationships       |

***

## References

* [SQLAlchemy Documentation](https://www.sqlalchemy.org/)
* [Python Official Site](https://www.python.org/)
* [SQLite Official Site](https://www.sqlite.org/index.html)
* [PostgreSQL Official Site](https://www.postgresql.org/)
* [MySQL Official Site](https://www.mysql.com/)
