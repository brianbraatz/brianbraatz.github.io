---
title: Best Practices for PHP with Databases
description: 
slug: hp-with-databases
date: 2017-08-14
image: post/Articles/IMAGES/php.png
categories:
  - PHP
  - Database
  - Best Practices
  - SQL Security
  - Cloud
  - SQL
tags:
  - PHP
  - Databases
  - SQL
  - Security
  - PDO
  - ORM
  - Optimization
  - SQL
  - Injection
  - Prevention
draft: false
weight: 523
categories_ref:
  - PHP
  - Database
  - Best Practices
  - SQL Security
  - Cloud
  - SQL
slug_calculated: https://brianbraatz.github.io/p/hp-with-databases
lastmod: 2025-03-14T16:40:25.681Z
---
# Best Practices for PHP with Databases

## Introduction

PHP and databases go together like peanut butter and jelly, except sometimes the peanut butter is expired, and the jelly is SQL injection waiting to happen.

If you’ve ever written PHP code that interacts with a database, you know that things can get messy fast. Queries start sprawling across your code like spaghetti, performance issues sneak up like a ninja, and security holes appear like plot holes in a bad movie.

Fear not! In this guide, we’re going to cover **best practices for PHP with databases**—how to write **secure, maintainable, and optimized** database interactions that won’t haunt your future self.

***

## 1. Always Use Prepared Statements (No, Seriously, Always)

### The Bad Way (Don't do this!)

```php
<?php
$pdo = new PDO('mysql:host=localhost;dbname=mydb', 'user', 'password');
$username = $_GET['username'];
$password = $_GET['password'];
$query = "SELECT * FROM users WHERE username = '$username' AND password = '$password'";
$result = $pdo->query($query);
?>
```

This is **bad** because if someone enters `admin' --` as their username, they bypass authentication.

### The Right Way: Use Prepared Statements

```php
<?php
$pdo = new PDO('mysql:host=localhost;dbname=mydb', 'user', 'password');
$query = "SELECT * FROM users WHERE username = :username AND password = :password";
$stmt = $pdo->prepare($query);
$stmt->execute(['username' => $_GET['username'], 'password' => $_GET['password']]);
$result = $stmt->fetchAll();
?>
```

This ensures user input is safely **escaped** and **sanitized**, making SQL injection virtually impossible.

***

## 2. Use PDO Instead of `mysqli_*`

`mysqli_*` functions are like that outdated library in your project—still around, but **PDO is just better**.

### Why Use PDO?

* Supports multiple databases (MySQL, PostgreSQL, SQLite, etc.).
* Uses **prepared statements by default**.
* Cleaner, more readable code.

***

## 3. Optimize Queries (Don't Be a Performance Sloth)

### Index Your Database

Indexes are like **bookmarks for databases**. Without them, MySQL will have to scan **every** row, which is painfully slow.

```sql
CREATE INDEX idx_username ON users(username);
```

### Avoid `SELECT *` (Be Specific)

Instead of this:

```sql
SELECT * FROM users;
```

Do this:

```sql
SELECT id, username FROM users;
```

Only query what you **need**.

***

## 4. Keep Database Credentials Secure

Hardcoding credentials is a rookie mistake. Instead, use **environment variables**.

```ini
DB_HOST=localhost
DB_USER=root
DB_PASS=supersecurepassword
```

Then access them in PHP:

```php
<?php
$dsn = "mysql:host=" . getenv('DB_HOST') . ";dbname=mydb";
$user = getenv('DB_USER');
$pass = getenv('DB_PASS');
$pdo = new PDO($dsn, $user, $pass);
?>
```

This keeps credentials out of your codebase.

***

## 5. Consider an ORM (But Don’t Overdo It)

ORMs (like Doctrine, Eloquent) **abstract away SQL** and let you work with objects.

Instead of writing:

```sql
SELECT * FROM users WHERE id = 1;
```

You do:

```php
$user = User::find(1);
```

Great for maintainability, but remember **ORMS can be slow**. Sometimes, **writing raw SQL is better**.

***

## 6. Use Database Transactions for Multiple Queries

When performing multiple dependent queries, use **transactions** to prevent partial updates.

```php
<?php
$pdo->beginTransaction();
try {
    $pdo->exec("UPDATE accounts SET balance = balance - 100 WHERE id = 1");
    $pdo->exec("UPDATE accounts SET balance = balance + 100 WHERE id = 2");
    $pdo->commit();
} catch (Exception $e) {
    $pdo->rollBack();
    echo "Transaction failed: " . $e->getMessage();
}
?>
```

If something fails, `ROLLBACK` ensures **no money disappears into the void**.

***

## Conclusion

Working with databases in PHP doesn’t have to be a nightmare.

Follow these **best practices** to ensure your queries are **secure, optimized, and maintainable**. Future-you will thank you!

***

## Key Ideas

| Best Practice              | Why It’s Important                       |
| -------------------------- | ---------------------------------------- |
| Use Prepared Statements    | Prevents SQL Injection                   |
| Use PDO                    | More flexible and secure than `mysqli_*` |
| Optimize Queries           | Improves performance                     |
| Store Credentials Securely | Prevents security leaks                  |
| Use ORMs Wisely            | Improves maintainability                 |
| Use Transactions           | Ensures data consistency                 |

## References

* [PHP PDO Documentation](https://www.php.net/manual/en/book.pdo.php)
* [SQL Injection Prevention](https://www.owasp.org/index.php/SQL_Injection)
* [MySQL Indexing](https://dev.mysql.com/doc/refman/8.0/en/optimization-indexes.html)
