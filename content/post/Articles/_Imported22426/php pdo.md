---
title: PHP PDO in a Nutshell
description: A concise yet comprehensive guide to PHP PDO (PHP Data Objects) for database interactions.
slug: php-pdo-in-a-nutshell
date: 2016-04-22
image: post/Articles/IMAGES/php.png
categories:
  - PHP
  - Database
  - PDO
  - SQL
  - Cloud
tags:
  - PHP
  - PDO
  - Databases
  - SQL
  - Security
  - Prepared
  - Statements
draft: false
weight: 437
lastmod: 2025-03-03T03:11:19.403Z
---
<!-- 
# PHP PDO in a Nutshell

## Introduction
-->

PHP Data Objects (PDO) is like the **Swiss Army knife** of database interaction in PHP.

It’s flexible, secure, and supports multiple database systems without requiring you to rewrite queries for every new database engine.

Yet, many PHP developers either underuse it or misuse it. Let’s fix that!

***

## Why Use PDO?

### 1. **Database Agnostic**

Unlike `mysqli_*`, PDO supports multiple databases (MySQL, PostgreSQL, SQLite, etc.) with the same API.

### 2. **Prepared Statements by Default**

Prevents SQL injection **out of the box**.

### 3. **Cleaner and More Readable Code**

PDO simplifies database interactions with fewer function calls.

***

## Setting Up PDO

### Connecting to a Database

```php
<?php
$dsn = "mysql:host=localhost;dbname=testdb;charset=utf8mb4";
$username = "root";
$password = "password";
$options = [
    PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
    PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC
];

try {
    $pdo = new PDO($dsn, $username, $password, $options);
    echo "Connected successfully!";
} catch (PDOException $e) {
    die("Connection failed: " . $e->getMessage());
}
?>
```

* **DSN (Data Source Name):** Specifies the database type, host, and name.
* **Options:** Enforce error handling and clean fetch modes.

***

## Running Queries with PDO

### 1. **Executing a Simple Query**

```php
<?php
$sql = "SELECT * FROM users";
$statement = $pdo->query($sql);
$users = $statement->fetchAll();
print_r($users);
?>
```

### 2. **Using Prepared Statements (Prevents SQL Injection!)**

```php
<?php
$sql = "SELECT * FROM users WHERE email = :email";
$stmt = $pdo->prepare($sql);
$stmt->execute(['email' => 'user@example.com']);
$user = $stmt->fetch();
print_r($user);
?>
```

Using placeholders (`:email`) ensures user input **never** gets directly injected into queries.

***

## Inserting Data with PDO

```php
<?php
$sql = "INSERT INTO users (name, email) VALUES (:name, :email)";
$stmt = $pdo->prepare($sql);
$stmt->execute([
    'name' => 'John Doe',
    'email' => 'john@example.com'
]);
echo "User inserted!";
?>
```

***

## Updating and Deleting Data

### **Update a Record**

```php
<?php
$sql = "UPDATE users SET name = :name WHERE id = :id";
$stmt = $pdo->prepare($sql);
$stmt->execute([
    'name' => 'Jane Doe',
    'id' => 1
]);
echo "User updated!";
?>
```

### **Delete a Record**

```php
<?php
$sql = "DELETE FROM users WHERE id = :id";
$stmt = $pdo->prepare($sql);
$stmt->execute(['id' => 2]);
echo "User deleted!";
?>
```

***

## Transactions: Ensuring Data Integrity

Use transactions when executing multiple queries that **must** succeed together.

```php
<?php
try {
    $pdo->beginTransaction();
    
    $pdo->exec("UPDATE accounts SET balance = balance - 100 WHERE id = 1");
    $pdo->exec("UPDATE accounts SET balance = balance + 100 WHERE id = 2");
    
    $pdo->commit();
    echo "Transaction successful!";
} catch (Exception $e) {
    $pdo->rollBack();
    echo "Transaction failed: " . $e->getMessage();
}
?>
```

***

## Error Handling with PDO

By default, PDO will **fail silently**, so make sure you enable `PDO::ERRMODE_EXCEPTION` to catch errors properly.

```php
<?php
try {
    $pdo = new PDO($dsn, $username, $password, [
        PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION
    ]);
} catch (PDOException $e) {
    die("Error: " . $e->getMessage());
}
?>
```

***

## Conclusion

PDO makes database interactions **safer, more flexible, and easier to read** than raw SQL queries.

By following best practices like **using prepared statements, handling errors, and leveraging transactions**, you can write **secure, maintainable** database-driven PHP applications.

So go forth, ditch `mysqli_*`, and embrace the **PDO way**! 🚀

***

## Key Ideas

| Concept             | Explanation                                                       |
| ------------------- | ----------------------------------------------------------------- |
| Database Agnostic   | PDO supports multiple databases (MySQL, PostgreSQL, SQLite, etc.) |
| Prepared Statements | Prevents SQL injection by safely handling user input              |
| Error Handling      | Use `PDO::ERRMODE_EXCEPTION` to catch database errors             |
| Transactions        | Ensures multiple queries succeed together                         |
| Cleaner Code        | PDO provides a simple and flexible way to interact with databases |

## References

* [PHP PDO Documentation](https://www.php.net/manual/en/book.pdo.php)
* [SQL Injection Prevention](https://www.owasp.org/index.php/SQL_Injection)
* [MySQL Prepared Statements](https://dev.mysql.com/doc/refman/8.0/en/sql-prepared-statements.html)
