---
title: SQL Transactions In a Nutshell
description: Code Examples in MSSQL, MySql and Postgres
slug: sql-transactions-nutshell
date: 2016-11-28
image: post/Articles/IMAGES/sqlquerysyntax.png
categories:
  - SQL
  - Microsoft Sql Server
  - MySql
  - Postgres Sql
tags:
  - SQL
  - Transactions
  - Databases
  - Stored
  - Procedures
  - MSSQL
  - MySQL
  - PostgreSQL
  - Journal
  - Log
  - Distributed
  - Transactions
draft: false
weight: 72
lastmod: 2025-03-03T15:02:52.253Z
---
# SQL Transactions: The History, How They Work, and Why You Should Care

## Introduction

Ah, **SQL transactions**. The unsung guardians of database integrity. Without them, databases would be like a **Jenga tower made of spaghetti**—a single bad query and *everything collapses*.

From **bank transfers** to **e-commerce orders**, transactions ensure that things either **fully happen** or **don’t happen at all**—because nobody wants their paycheck **half-processed**.

In this article, we’ll explore:

* The **history** and **motivation** behind SQL transactions.
* How transactions actually **work** (with **code examples** in **MSSQL, MySQL, and PostgreSQL**).
* The **journal log** and how it keeps your database from becoming a black hole.
* **Distributed transactions**—because sometimes one database just isn't enough.

***

## The History and Motivation Behind Transactions

Before transactions, databases were **wild**. Imagine:

* Buying an **airplane ticket**, but the system crashes **after** deducting your money but **before** confirming your seat.
* A bank transfer **half-executing**, with money vanishing into the void.
* An e-commerce site **selling the same PlayStation 5 to 10 people at once**.

Clearly, **something had to be done**.

Enter **ACID transactions**, the saviors of database sanity.

### The ACID Properties

**A**tomicity → *All or nothing!*\
**C**onsistency → *No corruption allowed!*\
**I**solation → *No peeking into other transactions!*\
**D**urability → *Data stays even if the server explodes!*

Now, let’s see **how transactions actually work**.

***

## How Transactions Work (With Code Examples)

### MSSQL Example

```sql
BEGIN TRANSACTION;

UPDATE Accounts SET Balance = Balance - 100 WHERE AccountID = 1;
UPDATE Accounts SET Balance = Balance + 100 WHERE AccountID = 2;

IF @@ERROR = 0
    COMMIT TRANSACTION;
ELSE
    ROLLBACK TRANSACTION;
```

If something **goes wrong**, everything **reverts** to its original state.

### MySQL Example

```sql
START TRANSACTION;

UPDATE Accounts SET Balance = Balance - 100 WHERE AccountID = 1;
UPDATE Accounts SET Balance = Balance + 100 WHERE AccountID = 2;

COMMIT;
```

MySQL also allows **ROLLBACK** if things go south.

### PostgreSQL Example

```sql
BEGIN;

UPDATE Accounts SET Balance = Balance - 100 WHERE AccountID = 1;
UPDATE Accounts SET Balance = Balance + 100 WHERE AccountID = 2;

COMMIT;
```

Similar logic across all databases—**either everything happens, or nothing does**.

***

## The Journal Log: Keeping Transactions Safe

### What is the Journal Log?

A **journal log** (or **write-ahead log**) is the **database’s insurance policy**. Before making any actual changes, transactions are **logged first**.

**Why?**

* If the database crashes, it can **replay the log** to restore the last known state.
* If a transaction **fails**, the log ensures the rollback happens properly.

### How It Works in Different Databases

| Database   | Journal Log Mechanism      |
| ---------- | -------------------------- |
| MSSQL      | **Transaction Log (.ldf)** |
| MySQL      | **Binary Log (binlog)**    |
| PostgreSQL | **Write-Ahead Log (WAL)**  |

Whenever a transaction **commits**, the log is **flushed to disk**, ensuring that **no data is lost** even in the event of a **power failure or crash**.

***

## Distributed Transactions: When One Database Isn't Enough

### MSSQL: Distributed Transactions with MSDTC

MSSQL supports **Distributed Transactions** via the **Microsoft Distributed Transaction Coordinator (MSDTC)**.

```sql
BEGIN DISTRIBUTED TRANSACTION;

UPDATE RemoteDB.dbo.Accounts SET Balance = Balance - 100 WHERE AccountID = 1;
UPDATE LocalDB.dbo.Accounts SET Balance = Balance + 100 WHERE AccountID = 2;

COMMIT TRANSACTION;
```

MSDTC ensures **both databases commit together**, preventing data inconsistencies.

### MySQL: XA Transactions

MySQL supports **XA Transactions**, which allow **distributed commits** across multiple databases.

```sql
XA START 'txn1';
UPDATE RemoteDB.Accounts SET Balance = Balance - 100 WHERE AccountID = 1;
UPDATE LocalDB.Accounts SET Balance = Balance + 100 WHERE AccountID = 2;
XA END 'txn1';

XA PREPARE 'txn1';
XA COMMIT 'txn1';
```

### PostgreSQL: Two-Phase Commit

PostgreSQL **natively** supports two-phase commits.

```sql
BEGIN;

PREPARE TRANSACTION 'txn1';
UPDATE RemoteDB.Accounts SET Balance = Balance - 100 WHERE AccountID = 1;
UPDATE LocalDB.Accounts SET Balance = Balance + 100 WHERE AccountID = 2;

COMMIT PREPARED 'txn1';
```

These techniques **ensure** that transactions spanning multiple databases **either succeed fully or fail completely**.

***

## Key Ideas

* **SQL Transactions prevent disasters** by following ACID rules.
* **The Journal Log ensures no data loss** in case of crashes.
* **MSSQL, MySQL, and PostgreSQL all support transactions**, but syntax varies.
* **Distributed Transactions ensure consistency across multiple databases** using **MSDTC (MSSQL), XA Transactions (MySQL), and Two-Phase Commit (PostgreSQL)**.

***

## References

1. [MSSQL Transactions](https://learn.microsoft.com/en-us/sql/t-sql/statements/transactions-transact-sql)
2. [MySQL XA Transactions](https://dev.mysql.com/doc/refman/8.0/en/xa.html)
3. [PostgreSQL Two-Phase Commit](https://www.postgresql.org/docs/current/sql-prepare-transaction.html)
4. [Write-Ahead Logging](https://en.wikipedia.org/wiki/Write-ahead_logging)
