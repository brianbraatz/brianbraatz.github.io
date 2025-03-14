---
title: Build a Python CLI Todo App with Click and SQLite
description: Track your todos on the console..
slug: build-cli-app-click-sqlite
date: 2022-08-30
image: post/Articles/IMAGES/pythonclicklogo.png
categories:
  - Python
  - SQLite
  - SQL
  - Python-Click
  - Python-Gui
tags:
  - Python
  - CLI
  - Click
  - SQLite
  - Database
  - Automation
draft: true
weight: 333
categories_ref:
  - Python
  - SQLite
  - SQL
  - Python-Click
  - Python-Gui
lastmod: 2025-03-14T15:45:19.689Z
---
# How to Build a Full CLI App with Click and SQLite

So, you want to build a **command-line app** that actually does something useful? Maybe a **task manager, a note-taking app, or even a simple inventory system**?

Well, good news: **Click** makes handling command-line input **painless**, and **SQLite** lets you store data **without setting up a whole database server**.

Today, we’re going to **build a full CLI app** that **stores data persistently** in an SQLite database. By the end, you’ll have:\
✅ A **fully functional CLI app**\
✅ **Command-line argument handling** (without `sys.argv` nightmares)\
✅ A **SQLite database** to store and retrieve data\
✅ A **cool hacking-movie feel** while you type commands

***

## 🛠 Setting Up Click and SQLite

First, install **Click** and **SQLite** (though SQLite comes with Python).

```sh
pip install click
```

Now, let’s create two Python files:

1. **`db.py`** → Handles database operations.
2. **`cli.py`** → Handles user input via Click.

***

## 📦 Step 1: Setting Up SQLite (db.py)

Let’s handle the **database first**.

Create `db.py`:

```python
import sqlite3

DB_FILE = "tasks.db"

def init_db():
    """Creates the tasks table if it doesn't exist."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS tasks (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                description TEXT NOT NULL,
                completed BOOLEAN NOT NULL DEFAULT 0
            )
        """)
        conn.commit()

def add_task(description):
    """Adds a new task to the database."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("INSERT INTO tasks (description, completed) VALUES (?, 0)", (description,))
        conn.commit()

def list_tasks():
    """Returns all tasks."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("SELECT id, description, completed FROM tasks")
        return cursor.fetchall()

def mark_done(task_id):
    """Marks a task as completed."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("UPDATE tasks SET completed = 1 WHERE id = ?", (task_id,))
        conn.commit()

def delete_task(task_id):
    """Deletes a task."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("DELETE FROM tasks WHERE id = ?", (task_id,))
        conn.commit()
```

***

## 🎮 Step 2: Building the CLI with Click (cli.py)

Now, let’s make a **command-line interface**.

Create `cli.py`:

```python
import click
import db

# Initialize the database
db.init_db()

@click.group()
def cli():
    """A simple CLI Task Manager."""
    pass

@click.command()
@click.argument("description")
def add(description):
    """Adds a new task."""
    db.add_task(description)
    click.secho(f"✅ Task added: {description}", fg="green")

@click.command()
def list():
    """Lists all tasks."""
    tasks = db.list_tasks()
    if not tasks:
        click.secho("No tasks found!", fg="red")
        return
    
    for task in tasks:
        status = "✅" if task[2] else "❌"
        click.echo(f"{task[0]}. {task[1]} {status}")

@click.command()
@click.argument("task_id", type=int)
def done(task_id):
    """Marks a task as completed."""
    db.mark_done(task_id)
    click.secho(f"🎉 Task {task_id} marked as done!", fg="yellow")

@click.command()
@click.argument("task_id", type=int)
def delete(task_id):
    """Deletes a task."""
    db.delete_task(task_id)
    click.secho(f"🗑️ Task {task_id} deleted!", fg="red")

# Add commands to CLI
cli.add_command(add)
cli.add_command(list)
cli.add_command(done)
cli.add_command(delete)

if __name__ == "__main__":
    cli()
```

***

## 🚀 Conclusion

With **Click and SQLite**, you can create **powerful CLI apps** without breaking a sweat. Today, we:\
✅ **Built a task manager CLI app**\
✅ **Handled user commands easily with Click**\
✅ **Used SQLite for persistent storage**

You now have **the foundation for building larger CLI projects**

<!-- 

---

## 💡 Ideas for Future Articles

- **"Adding User Authentication to a CLI App"**  
- **"How to Encrypt an SQLite Database"**  
- **"Building a CLI Note-Taking App with Click"**  
-->

***

## 📚 References

1. [Click Official Docs](https://click.palletsprojects.com/)
2. [SQLite Python Documentation](https://docs.python.org/3/library/sqlite3.html)
