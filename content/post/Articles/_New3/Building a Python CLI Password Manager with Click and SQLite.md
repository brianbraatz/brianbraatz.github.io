---
title: Building a Python CLI Password Manager with Click and SQLite
description: "Because command line apps that use sql are fun :) "
slug: cli-password-manager-click-sqlite
date: 2023-07-22
image: post/Articles/IMAGES/pythonclicklogo.png
categories:
  - Python
  - Python-Click
  - SQL
  - SQLite
tags:
  - Python
  - CLI
  - Click
  - SQLite
  - Security
  - Automation
draft: false
weight: 346
lastmod: 2025-02-09T20:25:34.631Z
---
# Building a CLI Password Manager with Click and SQLite

Let’s be real—**password management is a nightmare.** You either:

1. Use the same weak password for everything (*bad idea*).
2. Store them in a **Notepad file called "passwords.txt"** (*even worse*).
3. Forget them and reset them every time (*annoying*).

But what if you could **build your own password manager** in **Python**? **One that runs from the command line** and securely stores your passwords in a **SQLite database**?

That’s what we’re doing today! **We’ll use Click** (for a clean CLI interface) and **SQLite** (for safe storage) to build a **simple but functional password manager**.

By the end of this, you’ll have:

* **A CLI tool** to save and retrieve passwords securely.
* **A simple database** that stores credentials.
* **A cool hacking movie feel** when you type commands.

***

## 🛠 What You’ll Need

First, install Click and SQLite3 (though SQLite is built into Python):

```sh
pip install click
```

***

## 🔑 Step 1: Setting Up SQLite

We need a **database** to store our passwords. SQLite is perfect for this because:

* It’s **lightweight** and **built into Python** (no setup needed).
* It’s **secure** (as long as nobody gets access to the DB file).
* It’s **easy to use** (because who wants complex SQL?).

Let’s create a simple **database handler** in a file called `db.py`:

```python
import sqlite3

DB_FILE = "passwords.db"

def init_db():
    """Creates the passwords table if it doesn't exist."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS passwords (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                service TEXT NOT NULL,
                username TEXT NOT NULL,
                password TEXT NOT NULL
            )
        """)
        conn.commit()

def add_password(service, username, password):
    """Stores a new password in the database."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("INSERT INTO passwords (service, username, password) VALUES (?, ?, ?)", 
                       (service, username, password))
        conn.commit()

def get_password(service):
    """Retrieves a password from the database."""
    with sqlite3.connect(DB_FILE) as conn:
        cursor = conn.cursor()
        cursor.execute("SELECT username, password FROM passwords WHERE service = ?", (service,))
        result = cursor.fetchone()
        return result
```

Now, we have:\
✅ A **database**\
✅ A function to **add passwords**\
✅ A function to **retrieve passwords**

Let’s move to the **CLI part**.

***

## 🎮 Step 2: Building the CLI with Click

Now, let’s create `password_manager.py` and **hook it up with Click**.

```python
import click
import db

# Initialize the database
db.init_db()

@click.group()
def cli():
    """Simple CLI Password Manager"""
    pass

@click.command()
@click.argument("service")
@click.argument("username")
@click.argument("password")
def add(service, username, password):
    """Adds a new password entry."""
    db.add_password(service, username, password)
    click.secho(f"✅ Password for {service} saved!", fg="green")

@click.command()
@click.argument("service")
def get(service):
    """Retrieves a stored password."""
    result = db.get_password(service)
    if result:
        username, password = result
        click.secho(f"🔑 Service: {service}", fg="cyan")
        click.secho(f"👤 Username: {username}", fg="yellow")
        click.secho(f"🔒 Password: {password}", fg="red")
    else:
        click.secho("❌ No password found!", fg="red")

# Add commands to the CLI group
cli.add_command(add)
cli.add_command(get)

if __name__ == "__main__":
    cli()
```

### **How it Works:**

* `add <service> <username> <password>` → **Stores a password.**
* `get <service>` → **Retrieves a stored password.**
* Colors make it **easier to read**!

***

## 🏃‍♂️ Step 3: Running Your Password Manager

### **1. Add a New Password**

```sh
python password_manager.py add twitter myuser supersecure123
```

```
✅ Password for twitter saved!
```

### **2. Retrieve a Password**

```sh
python password_manager.py get twitter
```

```
🔑 Service: twitter
👤 Username: myuser
🔒 Password: supersecure123
```

✨ **Boom!** You now have a fully functional password manager in Python.

***

## 🔐 Step 4: Improving Security (Optional)

Right now, our passwords are **stored in plain text**—which is **bad**.

### **1. Hashing Passwords**

Instead of storing raw passwords, let’s hash them using **bcrypt**:

```sh
pip install bcrypt
```

Modify `db.py` to hash passwords before saving:

```python
import bcrypt

def hash_password(password):
    """Hashes a password before storing it."""
    return bcrypt.hashpw(password.encode(), bcrypt.gensalt()).decode()

def verify_password(stored_hash, password):
    """Checks if a password matches the stored hash."""
    return bcrypt.checkpw(password.encode(), stored_hash.encode())
```

Then, change `add_password()` to **store the hash** instead of plain text.

Now, even if someone steals your database, **they won’t be able to read the passwords!**

***

## 🚀 Bonus: Making It Even Cooler

* **🔑 Auto-generate strong passwords** using `secrets`
* **📦 Add categories (e.g., work, personal, banking)**
* **📁 Store passwords in an encrypted file** instead of SQLite
* **🔐 Require a master password to access stored passwords**

If you’re feeling fancy, **turn this into a full GUI app** later using `Tkinter` or `Rich`.

***

## 🎉 Wrapping Up

Today, you learned how to **build your own CLI password manager** using:\
✅ **Click** for an easy-to-use command-line interface\
✅ **SQLite** for lightweight, persistent storage\
✅ **bcrypt** for password hashing (security matters!)

Now, you never have to forget your passwords again—unless you forget the command to retrieve them. 😅

<!-- 
---


## 💡 Ideas for Future Articles

- **"How to Encrypt a SQLite Database for Extra Security"**
- **"Building a Flask Web App for Your Password Manager"**
- **"Creating a GUI Password Manager with Python and Tkinter"**
-->

***

## 📚 References

4. [Click Official Docs](https://click.palletsprojects.com/)
5. [SQLite Python Documentation](https://docs.python.org/3/library/sqlite3.html)
6. [bcrypt Python Library](https://pypi.org/project/bcrypt/)
7. [Python Secrets Module](https://docs.python.org/3/library/secrets.html)
