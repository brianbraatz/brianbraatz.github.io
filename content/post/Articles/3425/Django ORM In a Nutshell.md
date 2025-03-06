---
title: Django ORM In a Nutshell
description: Python ORM
slug: python-django-orm-in-a-nutshell
date: 2017-06-14
image: post/Articles/IMAGES/pythonlogo.png
categories:
  - Django
  - Python
  - ORM
tags:
  - Django
  - Python
  - Orm
  - Database
  - Query
  - Model
  - Migration
  - Sqlalchemy
draft: false
weight: 592
lastmod: 2025-03-06T12:31:18.374Z
---
# Python Django ORM In a Nutshell

<!-- ## Introduction

Alright, buckle up, fellow code wranglers! If you've ever wrangled SQL queries by hand, you'll appreciate Django's ORM like a cold drink on a hot day.

The Django ORM (Object-Relational Mapper) is like a personal assistant for your database. Instead of writing soul-draining SQL, you write Python, and Django does the heavy lifting.

No more forgetting that weird JOIN syntax or debugging mysterious SQL errors at 3 AM. Instead, you get to work with Python classes and objects, and Django makes sure everything talks smoothly to your database. -->

***

## Setting Up Django ORM

Before we dive in, let's set up Django in a project. If you haven't installed Django yet, do this:

```sh
pip install django
```

Then, create a new Django project:

```sh
django-admin startproject myproject
cd myproject
python manage.py startapp myapp
```

Now, go to `settings.py` and configure your database. By default, Django uses SQLite, but you can change it to PostgreSQL, MySQL, or whatever suits your fancy.

Example for PostgreSQL:

```python
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql',
        'NAME': 'mydatabase',
        'USER': 'myuser',
        'PASSWORD': 'mypassword',
        'HOST': 'localhost',
        'PORT': '5432',
    }
}
```

***

## Defining Models (A.K.A Your Database Tables)

In Django, a **model** is a Python class that represents a database table. You define your models in `models.py` inside your app.

```python
from django.db import models

class Author(models.Model):
    name = models.CharField(max_length=100)
    email = models.EmailField(unique=True)
    
    def __str__(self):
        return self.name

class Book(models.Model):
    title = models.CharField(max_length=200)
    author = models.ForeignKey(Author, on_delete=models.CASCADE)
    published_date = models.DateField()
    
    def __str__(self):
        return self.title
```

What’s happening here?

* `Author` and `Book` are two tables.
* `CharField`, `EmailField`, and `DateField` define the types of data these fields hold.
* `ForeignKey` sets up a relationship between books and authors.
* `__str__()` makes Django display readable names in the admin panel.

Run migrations to apply these models to your database:

```sh
python manage.py makemigrations
python manage.py migrate
```

Boom! Your database is now ready to store authors and books.

***

## Querying Data (The Fun Part)

Alright, let’s talk about CRUD—Create, Read, Update, and Delete. This is where Django ORM really shines.

### Creating Data

Adding new records is as easy as pie:

```python
>>> from myapp.models import Author, Book
>>> author = Author.objects.create(name="J.K. Rowling", email="jk@example.com")
>>> book = Book.objects.create(title="Harry Potter", author=author, published_date="1997-06-26")
```

### Reading Data

Fetching data? Piece of cake.

```python
>>> books = Book.objects.all()  # Get all books
>>> first_book = Book.objects.first()  # Get first book
>>> books_by_rowling = Book.objects.filter(author__name="J.K. Rowling")  # Filter books
```

### Updating Data

Want to update a book’s title? No problem.

```python
>>> book = Book.objects.get(id=1)
>>> book.title = "Harry Potter and the Sorcerer’s Stone"
>>> book.save()
```

### Deleting Data

Deleting is just as easy:

```python
>>> book.delete()
```

Just like that, no more Harry Potter in the database. (Oops!)

***

## Django ORM vs Raw SQL

If you’re wondering how Django ORM compares to raw SQL, check out this handy table:

| Feature     | Django ORM                                | Raw SQL                        |
| ----------- | ----------------------------------------- | ------------------------------ |
| Readability | Super clean and Pythonic                  | SQL can be verbose and complex |
| Performance | Optimized but slightly abstracted         | Fine-tuned control             |
| Security    | Built-in protection against SQL injection | Requires manual handling       |
| Portability | Works across multiple databases           | Tied to a specific SQL dialect |
| Flexibility | Limited to ORM’s capabilities             | Total control                  |

Django ORM is great for most cases, but if you need super complex queries, raw SQL might still be your best bet.

***

## Running Raw SQL in Django ORM

Sometimes, ORM doesn’t cut it, and you need to write raw SQL. You can do that like this:

```python
from django.db import connection

with connection.cursor() as cursor:
    cursor.execute("SELECT * FROM myapp_book")
    books = cursor.fetchall()
```

Use with caution—raw SQL is powerful but also dangerous if you’re not careful!

***

<!-- ## Conclusion

Django ORM is like a magic wand for your database. It makes querying easy, migrations painless, and life as a developer a whole lot better.

So go forth, embrace the ORM life, and may your queries always be efficient!

--- -->

## Key Ideas Table

| Concept    | Summary                                                                  |
| ---------- | ------------------------------------------------------------------------ |
| Django ORM | A way to interact with databases using Python classes instead of raw SQL |
| Models     | Represent database tables using Python classes                           |
| Queries    | CRUD operations (Create, Read, Update, Delete) using Pythonic syntax     |
| Migrations | Django’s way of applying database changes automatically                  |
| Raw SQL    | Can be used when ORM isn't enough, but should be used carefully          |

***

## References

* [Django ORM Official Docs](https://docs.djangoproject.com/en/stable/topics/db/models/)
* [Django QuerySet API](https://docs.djangoproject.com/en/stable/ref/models/querysets/)
* [Django Database Migrations](https://docs.djangoproject.com/en/stable/topics/migrations/)
* [SQL vs ORM](https://stackoverflow.com/questions/6579061/sql-vs-orm-performance)
