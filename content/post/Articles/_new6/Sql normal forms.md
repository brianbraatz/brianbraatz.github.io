---
title: SQL Levels of Normalization
description: Understanding the Levels of Normalization in SQL
slug: understanding-the-levels-of-normalization-in-sql
date: 2024-02-03
image: post/Articles/IMAGES/sql.png
categories:
  - SQL
  - Design Patterns
  - Software Architecture
tags:
  - Sql
  - Database
  - Normalization
  - First
  - Normal
  - Form
  - Second
  - Normal
  - Form
  - Third
  - Normal
  - Form
  - Boyce-Codd
  - Normal
  - Form
  - Denormalization
  - Performance
  - Relational
  - Databases
  - Database
  - Optimization
draft: false
weight: 250
lastmod: 2025-02-10T18:04:18.524Z
---
<!--

# Understanding the Levels of Normalization in SQL
-->

## Introduction

So, you’ve heard about **normalization** in SQL and wondered, *"Do I really need all these fancy normal forms?"*\
Short answer: **It depends.**

Long answer? Let's take a **deep dive into database normalization**—its history, its **pros and cons**, and most importantly, **how far you should go** when designing your database.

***

## What Came Before SQL? A Quick History

Before relational databases and SQL became the standard, **data was stored in hierarchical and network databases**. These databases required **manual data structuring**, leading to:

* **Data redundancy** (storing the same data in multiple places).
* **Update anomalies** (changing one record meant manually updating others).
* **Difficult queries** (retrieving data required complex programming).

Then came **E.F. Codd** in **1970**, who introduced the **relational database model** and the **concept of normalization**. And just like that, SQL databases became the norm.

> **Further Reading:** [Relational Model on Wikipedia](https://en.wikipedia.org/wiki/Relational_model)

***

## What is Normalization?

Normalization is the **process of structuring a relational database** to **reduce redundancy and improve integrity**. It does this by:

* Organizing data **into separate tables**.
* Eliminating **duplicate** data.
* Ensuring **data dependencies** are correctly managed.

### **Why Normalize?**

| Benefit                | Description                               |
| ---------------------- | ----------------------------------------- |
| **Reduces Redundancy** | No duplicate data across tables.          |
| **Improves Integrity** | Changes in one place reflect everywhere.  |
| **Prevents Anomalies** | No update, insertion, or deletion issues. |
| **Optimizes Storage**  | Uses disk space more efficiently.         |

***

## The Levels of Normalization (With Examples!)

### **1st Normal Form (1NF) – The Basics**

**Rule:** Eliminate **duplicate rows** and ensure **atomic** values (no lists or arrays in a single column).

#### **Bad Example (Not in 1NF):**

| OrderID | Customer | Products       |
| ------- | -------- | -------------- |
| 1       | Alice    | Laptop, Mouse  |
| 2       | Bob      | Keyboard       |
| 3       | Alice    | Monitor, Cable |

💀 **Issues:**

* **Multiple values in a single column ("Laptop, Mouse")**
* **Difficult to query individual products**

#### **Fixed (1NF Compliant):**

| OrderID | Customer | Product  |
| ------- | -------- | -------- |
| 1       | Alice    | Laptop   |
| 1       | Alice    | Mouse    |
| 2       | Bob      | Keyboard |
| 3       | Alice    | Monitor  |
| 3       | Alice    | Cable    |

🎉 **Now every field has only atomic values!**

***

### **2nd Normal Form (2NF) – No Partial Dependencies**

**Rule:** **Every non-key column must depend on the entire primary key**.

#### **Bad Example (Not in 2NF):**

| OrderID | Product  | Customer | CustomerPhone |
| ------- | -------- | -------- | ------------- |
| 1       | Laptop   | Alice    | 123-4567      |
| 2       | Mouse    | Bob      | 987-6543      |
| 3       | Keyboard | Alice    | 123-4567      |

💀 **Issues:**

* **CustomerPhone depends only on Customer, not OrderID.**
* **If Alice changes her phone number, we update multiple rows.**

#### **Fixed (2NF Compliant - Splitting Customer Info Into Its Own Table)**

| OrderID | Product  |
| ------- | -------- |
| 1       | Laptop   |
| 2       | Mouse    |
| 3       | Keyboard |

| CustomerID | Name  | Phone    |
| ---------- | ----- | -------- |
| 1          | Alice | 123-4567 |
| 2          | Bob   | 987-6543 |

🎉 **Now each column depends on the entire primary key.**

***

### **3rd Normal Form (3NF) – No Transitive Dependencies**

**Rule:** **Every column should depend only on the primary key.**

#### **Bad Example (Not in 3NF):**

| OrderID | Product  | CustomerID | CustomerCity |
| ------- | -------- | ---------- | ------------ |
| 1       | Laptop   | 1          | New York     |
| 2       | Mouse    | 2          | Chicago      |
| 3       | Keyboard | 1          | New York     |

💀 **Issues:**

* **CustomerCity depends on CustomerID, not OrderID.**
* **If we update CustomerCity, multiple rows need changes.**

#### **Fixed (3NF Compliant - Splitting Customer Data Further)**

| OrderID | Product  | CustomerID |
| ------- | -------- | ---------- |
| 1       | Laptop   | 1          |
| 2       | Mouse    | 2          |
| 3       | Keyboard | 1          |

| CustomerID | Name  | City     |
| ---------- | ----- | -------- |
| 1          | Alice | New York |
| 2          | Bob   | Chicago  |

🎉 **Now every column depends only on its table’s primary key.**

***

## Should You Normalize Everything? **Trade-offs with Performance**

| Approach                          | Pros                                   | Cons                              |
| --------------------------------- | -------------------------------------- | --------------------------------- |
| **Fully Normalized (3NF, BCNF)**  | Reduces redundancy, improves integrity | Slower queries, more joins        |
| **Denormalized**                  | Faster reads, better performance       | More redundancy, update anomalies |
| **Hybrid (Partially Normalized)** | Best of both worlds                    | Needs careful planning            |

👉 **Best Practice:**

* **OLTP systems (e.g., Banking, CRM)** → Normalize for integrity.
* **OLAP/Analytics (e.g., Reporting, Data Warehouses)** → Denormalize for speed.

***

## Key Ideas

* **Normalization improves data integrity but can slow down queries.**
* **1NF removes duplicate columns, 2NF eliminates partial dependencies, and 3NF eliminates transitive dependencies.**
* **Denormalization can improve performance for read-heavy applications.**
* **Most real-world databases use a hybrid approach.**

***

## References

1. [Database Normalization (Wikipedia)](https://en.wikipedia.org/wiki/Database_normalization)
2. [E.F. Codd and the Relational Model](https://en.wikipedia.org/wiki/Edgar_F._Codd)
3. [Normalization Forms Explained](https://www.geeksforgeeks.org/normalization-in-dbms/)
4. [Denormalization vs Normalization](https://www.sqlshack.com/database-normalization-vs-denormalization/)

K
