---
title: "Web Security: How SQL Injection Attacks Work"
description: Putting Uninvited SQL... where its not supposed to go..
slug: web-security-how-sql-injection-attacks-work
date: 2017-08-17
image: post/Articles/IMAGES/hacker.webp
categories:
  - Web Development
  - SQL
  - MySql
  - SQLite
  - Microsoft Sql Server
  - Postgres Sql
  - PHP
  - Python
  - Security
  - ORM
tags:
  - Web
  - Security
  - SQL
  - Injection
  - Cybersecurity
  - Hacking
  - Database
  - Security
  - Vulnerabilities
  - EntityFramework
  - SQLAlchemy
  - Python
  - CSharp
  - Dapper
draft: false
weight: 29
categories_ref:
  - Web Development
  - SQL
  - MySql
  - SQLite
  - Microsoft Sql Server
  - Postgres Sql
  - PHP
  - Python
  - Security
  - ORM
slug_calculated: https://brianbraatz.github.io/p/web-security-how-sql-injection-attacks-work
lastmod: 2025-03-14T16:40:29.392Z
---
<!-- 
# Web Security: How SQL Injection Attacks Work

## A Brief History of SQL Injection (or: How Hackers Got Free Pizza)
-->

Once upon a time, in the dark and mysterious lands of the early 2000s, developers were happily building websites, blissfully unaware of the chaos that awaited them.

Enter SQL Injection (SQLi), the mischievous trick that allowed attackers to sneak into databases like a ninja in a tracksuit.

The first known SQLi attack was reported around **1998**, and since then, it has caused billions of dollars in damage.

<!-- Fun fact: Some hackers even used SQLi to order **free pizza** by modifying payment databases! 🍕
-->

The early web was full of MySpace pages with tutorials on how to hand code your own website..

AND many of these original tutorials would have people **dynamically** make a **SQL** statement from the **unfiltered input text** to make a SQL statement..

**Here is why that was bad:**

<!-- 
## Why Hackers Love SQL Injection (And Why You Shouldn’t)

Imagine leaving your front door open, thinking, “Who would actually just walk in?” Spoiler alert: **Hackers would**. SQL Injection is basically that, but for databases. The motivation behind SQLi attacks includes:

- **Data Theft** – Stealing credit card numbers, user credentials, and other sensitive info.
- **Website Defacement** – Because changing your homepage to “Hacked by Mr. Robot” is *so much fun*.
- **Bypassing Authentication** – “Forget your password? No worries, just SQL inject your way in!”
- **Destruction** – Some people just want to watch the world burn... or at least drop your database.

## How SQL Injection Works (With Code Examples!)
-->

Let's say you have a simple login form that checks a user's credentials against a database:

```sql
SELECT * FROM users WHERE username = 'admin' AND password = 'password123';
```

Nothing suspicious, right? But here’s where the magic (read: disaster) happens. What if an attacker enters this as the username?

```sql
admin' --
```

The query now looks like this:

```sql
SELECT * FROM users WHERE username = 'admin' --' AND password = 'password123';
```

Since `--` is a comment in SQL, everything after it is ignored. **Boom! Instant admin access.**

### The Classic `' OR '1'='1'` Attack

Another famous trick is using **always-true conditions**:

```sql
SELECT * FROM users WHERE username = '' OR '1'='1' AND password = '';
```

Since `'1'='1'` always evaluates to `true`, the database happily logs in **anyone**. Congratulations, you're now an admin! (Just kidding, **please don't do this**.)

## How to Prevent SQL Injection (a.k.a. Don’t Be That Developer)

Now that you’re thoroughly terrified, let's talk about fixing this mess.

### 1. Use Prepared Statements (aka The Silver Bullet)

Instead of embedding user input directly in SQL queries, use **prepared statements**.

**Python Example (Using SQLite3):**

```python
import sqlite3

db = sqlite3.connect("users.db")
cursor = db.cursor()
username = input("Enter username: ")
password = input("Enter password: ")

cursor.execute("SELECT * FROM users WHERE username=? AND password=?", (username, password))
result = cursor.fetchone()
```

Notice how the input is handled separately? That’s because prepared statements **prevent SQLi attacks by design.**

### 2. Sanitize User Input (No Funny Business Allowed)

If you absolutely must work with raw queries, **validate and escape** input.

### 3. Use ORM Frameworks

ORMs like SQLAlchemy (Python) or Entity Framework (C#) automatically handle input safely.

### 4. Restrict Database Privileges

Don't give every application full database access. Use **least privilege** principles.

### 5. Monitor and Log Suspicious Activity

Use Web Application Firewalls (WAFs) and logging systems to **detect and block** attacks in real time.

## SQL Injection vs Other Attacks (A Quick Comparison)

| Attack Type                          | What It Does                            | How Bad Is It?                 |
| ------------------------------------ | --------------------------------------- | ------------------------------ |
| SQL Injection                        | Manipulates database queries            | 🚨🚨🚨 (Very bad!)             |
| XSS (Cross-Site Scripting)           | Injects malicious scripts into webpages | 😱 (Bad, but fixable)          |
| CSRF (Cross-Site Request Forgery)    | Forces users to perform actions         | 🤨 (Annoying, but preventable) |
| DDoS (Distributed Denial of Service) | Overloads servers                       | 😤 (Painful, but survivable)   |

## Final Thoughts

SQL Injection is **one of the oldest and deadliest** web vulnerabilities.

If you’re a developer, **always** use prepared statements, validate input, and secure your database.

<!--
If you’re a hacker, well… go hack ethically instead. The world needs **more white hats** and fewer data breaches.

Stay safe out there, and keep your queries **hacker-proof**! 💻🔒
-->

***

## References

* [OWASP SQL Injection Guide](https://owasp.org/www-community/attacks/SQL_Injection)
* [SQL Injection Explained (SQLMap)](https://sqlmap.org/)
* [A Hilarious Look at SQL Injection Fails](https://www.exploit-db.com/)

***
