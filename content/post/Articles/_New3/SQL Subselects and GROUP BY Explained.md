---
title: SQL Subselects and GROUP BY Explained
description: Code Examples in MSSQL, MySql and Postgres
slug: sql-subselects-group-by-explained
date: 2024-12-08
image: post/Articles/IMAGES/sql.png
categories:
  - SQL
  - Microsoft Sql Server
  - MySql
  - Postgres Sql
tags:
  - SQL
  - Subselects
  - Group
  - By
  - Databases
  - MSSQL
  - MySQL
  - PostgreSQL
  - Aggregation
  - Query
  - Optimization
draft: false
weight: 62
categories_ref:
  - SQL
  - Microsoft Sql Server
  - MySql
  - Postgres Sql
slug_calculated: https://brianbraatz.github.io/p/sql-subselects-group-by-explained
lastmod: 2025-03-14T16:40:27.192Z
---
<!-- >
# SQL Subselects and GROUP BY Explained: The History, Motivation, and How They Work
-->

## Introduction

SQL subselects (or **subqueries**) and **GROUP BY**—two of the most misunderstood yet **incredibly powerful** tools in SQL.

One lets you **nest queries inside queries** like SQL-ception, and the other allows you to **aggregate data** like a boss.

***

## A Brief History of Subselects and GROUP BY

### The Dark Ages of SQL (Before Subqueries)

Once upon a time, in the early days of relational databases, queries were **simple**—you could only SELECT, INSERT, UPDATE, and DELETE. But what if you wanted to:

* Find **customers who placed an order last month**?
* Retrieve **the highest salary per department**?
* Get **the total revenue per year**?

Without **subqueries and GROUP BY**, SQL was **like a caveman with a rock**, limited to basic operations.

### The Enlightenment: The Birth of Subqueries

At some point, database wizards thought, *"What if we could put a query INSIDE another query?"* Boom. **Subqueries** were born. Now, SQL could **filter data dynamically**, using results from one query inside another.

### The Rise of GROUP BY

But what if you wanted **totals, averages, or counts**? The solution: **GROUP BY**, which allowed SQL to **aggregate data** neatly into summary rows.

And thus, the world of SQL became **far more powerful**—and slightly more confusing.

***

## How Subselects Work

A **subselect (or subquery)** is a **query inside another query**. It can be used in:

* **SELECT** statements (to fetch calculated values).
* **WHERE** clauses (to filter dynamically).
* **FROM** clauses (treating a subquery as a table).

### Example 1: Subselect in WHERE Clause

**Find employees who earn more than the company’s average salary.**

#### MSSQL / MySQL / PostgreSQL

```sql
SELECT name, salary 
FROM employees 
WHERE salary > (SELECT AVG(salary) FROM employees);
```

**How it works:**

* The **inner query** `(SELECT AVG(salary) FROM employees)` calculates the **average salary**.
* The **outer query** retrieves **employees earning above that average**.

### Example 2: Subselect in FROM Clause

**Find the highest salary per department.**

#### MSSQL / MySQL / PostgreSQL

```sql
SELECT department, MAX(salary) AS highest_salary 
FROM employees 
GROUP BY department;
```

But what if you need **more details**? That’s where **subqueries in the FROM clause** help.

```sql
SELECT e.name, e.department, e.salary 
FROM employees e
JOIN (SELECT department, MAX(salary) AS highest_salary FROM employees GROUP BY department) sub
ON e.department = sub.department AND e.salary = sub.highest_salary;
```

Here, the **subquery calculates the max salary per department**, and the **main query finds employees matching those salaries**.

***

## How GROUP BY Works

GROUP BY is used to **aggregate data into groups**, typically with functions like:

* `COUNT()` – Counts the number of rows.
* `SUM()` – Adds up values.
* `AVG()` – Calculates averages.
* `MAX()` / `MIN()` – Find the highest or lowest values.

### Example 1: Counting Orders per Customer

```sql
SELECT customer_id, COUNT(order_id) AS total_orders 
FROM orders 
GROUP BY customer_id;
```

This gives you **one row per customer**, showing how many orders they placed.

### Example 2: Sales Revenue Per Year

```sql
SELECT YEAR(order_date) AS year, SUM(total_price) AS revenue 
FROM orders 
GROUP BY YEAR(order_date);
```

Now we have **total revenue per year**.

***

## Common Mistakes and How to Avoid Them

### 1. Using GROUP BY Without Aggregation

❌ **Wrong:**

```sql
SELECT name, department, salary 
FROM employees 
GROUP BY department;
```

SQL will complain because **name and salary** aren't aggregated.

✅ **Right:**

```sql
SELECT department, COUNT(*) AS total_employees 
FROM employees 
GROUP BY department;
```

### 2. Using a Subquery When a JOIN is Faster

❌ **Inefficient Subquery:**

```sql
SELECT name, department, salary 
FROM employees 
WHERE salary = (SELECT MAX(salary) FROM employees);
```

✅ **Better Approach Using JOIN:**

```sql
SELECT e.name, e.department, e.salary 
FROM employees e
JOIN (SELECT MAX(salary) AS max_salary FROM employees) sub
ON e.salary = sub.max_salary;
```

**Joins are usually faster than subqueries, so use them when possible!**

***

## Key Ideas

* **Subqueries allow nested queries**, useful for filtering, calculations, and table-like operations.
* **GROUP BY aggregates data**, making it easier to summarize information.
* **Use JOINs instead of subqueries** when performance matters.
* **SQL became much more powerful** when subqueries and GROUP BY were introduced.

***

## References

1. [MSSQL Subqueries](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql)
2. [MySQL Subqueries](https://dev.mysql.com/doc/refman/8.0/en/subqueries.html)
3. [PostgreSQL GROUP BY](https://www.postgresql.org/docs/current/queries-table-expressions.html#QUERIES-GROUPING)
