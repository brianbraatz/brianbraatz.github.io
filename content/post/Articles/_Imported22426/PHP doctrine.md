---
title: PHP Doctrine in a Nutshell
description: A concise guide to PHP Doctrine, an Object-Relational Mapper (ORM) for database interactions.
slug: php-doctrine-in-a-nutshell
date: 2018-02-11
image: post/Articles/IMAGES/php.png
categories:
  - PHP
  - Database
  - ORM
  - Doctrine
  - Cloud
  - SQL
tags:
  - PHP
  - Doctrine
  - ORM
  - Databases
  - Entities
  - Repositories
  - Doctrine
  - Migrations
draft: false
weight: 487
lastmod: 2025-03-03T13:11:31.957Z
---
# PHP Doctrine in a Nutshell

## Introduction

Doctrine is like the **Lamborghini of PHP ORMs**—powerful, sophisticated, and sometimes a little too much if all you need is a bicycle.\
If you've ever struggled with writing raw SQL queries and keeping your database interactions clean, Doctrine is here to save the day.

Doctrine allows you to work with databases using **objects** instead of SQL, which means fewer headaches, cleaner code, and a happier you.\
Let’s dive into how Doctrine works and why it’s an essential tool for serious PHP developers.

***

## What is Doctrine?

Doctrine is an **Object-Relational Mapper (ORM)** for PHP that abstracts database interactions.\
Instead of writing SQL, you interact with database records using **PHP objects** (a.k.a. entities).

It consists of several components, but the two most commonly used are:

* **Doctrine ORM** (maps database tables to PHP objects)
* **Doctrine DBAL** (a powerful database abstraction layer)

***

## Setting Up Doctrine

First, install Doctrine using Composer:

```sh
composer require doctrine/orm
```

Then, generate the necessary configuration files:

```sh
vendor/bin/doctrine orm:setup
```

This will create a default configuration for connecting to your database.

***

## Defining an Entity

An **entity** is a PHP class that represents a database table.

```php
<?php
use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity
 * @ORM\Table(name="users")
 */
class User
{
    /** @ORM\Id @ORM\GeneratedValue @ORM\Column(type="integer") */
    private $id;

    /** @ORM\Column(type="string", length=100) */
    private $name;

    /** @ORM\Column(type="string", unique=true) */
    private $email;

    public function getId() { return $this->id; }
    public function getName() { return $this->name; }
    public function setName($name) { $this->name = $name; }
    public function getEmail() { return $this->email; }
    public function setEmail($email) { $this->email = $email; }
}
?>
```

Doctrine maps this class to a **users** table in the database.

***

## Creating the Database Schema

Once your entities are set up, generate the database schema:

```sh
vendor/bin/doctrine orm:schema-tool:update --force
```

Doctrine will automatically create the required tables based on your entity definitions.

***

## Inserting Data with Doctrine

To insert a new record, use Doctrine’s **EntityManager**:

```php
<?php
$entityManager = require 'bootstrap.php';

$user = new User();
$user->setName("John Doe");
$user->setEmail("john@example.com");

$entityManager->persist($user);
$entityManager->flush();

echo "User created with ID " . $user->getId();
?>
```

***

## Fetching Data

### Fetch a Single Record by ID

```php
<?php
$user = $entityManager->find(User::class, 1);
echo $user->getName();
?>
```

### Fetching All Users

```php
<?php
$users = $entityManager->getRepository(User::class)->findAll();
foreach ($users as $user) {
    echo $user->getName() . "\n";
}
?>
```

***

## Updating Data

```php
<?php
$user = $entityManager->find(User::class, 1);
$user->setName("Jane Doe");
$entityManager->flush();
?>
```

Doctrine tracks changes to objects and automatically updates the database.

***

## Deleting Data

```php
<?php
$user = $entityManager->find(User::class, 1);
$entityManager->remove($user);
$entityManager->flush();
?>
```

***

## Doctrine Migrations

If your database schema changes, you don’t want to manually modify tables.\
Doctrine Migrations automate this process:

1. Generate a migration file:

   ```sh
   vendor/bin/doctrine migrations:diff
   ```

2. Run the migration:

   ```sh
   vendor/bin/doctrine migrations:migrate
   ```

Doctrine will safely apply database changes.

***

## Conclusion

Doctrine is a **powerful** ORM that makes working with databases in PHP much more enjoyable.\
Instead of dealing with raw SQL queries, you can **focus on business logic** using PHP objects.

By leveraging Doctrine's **entities, repositories, and migrations**, you can build scalable and maintainable applications **without the headache of writing SQL manually**.

So if you want cleaner code and an easier time managing databases, **Doctrine is the way to go**! 🚀

***

## Key Ideas

| Concept       | Explanation                                          |
| ------------- | ---------------------------------------------------- |
| Doctrine ORM  | Maps PHP objects to database tables                  |
| Entities      | PHP classes that represent database tables           |
| EntityManager | Handles database operations (insert, update, delete) |
| Migrations    | Automates database schema updates                    |
| Repositories  | Helps fetch data without writing queries             |

## References

* [Doctrine ORM Official Docs](https://www.doctrine-project.org/projects/orm.html)
* [Doctrine Migrations](https://www.doctrine-project.org/projects/migrations.html)
* [PHP Doctrine Tutorial](https://www.tutorialspoint.com/doctrine/index.htm)
