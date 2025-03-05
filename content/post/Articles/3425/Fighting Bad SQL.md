---
title: Fighting Bad SQL
description: Tonight, tonight, won't be just any night.....
slug: fighting-bad-sql
date: 2023-12-15
image: post/Articles/IMAGES/WestSideStoryRumble.png
categories:
  - SQL
  - Performance Optimization
tags:
  - SQL
  - Performance
  - Bad
  - Practices
  - Optimization
  - Database
  - Indexing
draft: false
weight: 15
lastmod: 2025-03-05T17:41:52.924Z
---
**Yo, you writin' SQL, huh? That's cool, that's cool. But hold up...**

**You makin' those rookie mistakes that gonna slow your queries down and have your database straight-up bawlin', like, "why you do me like that?"**

(yes... why WOULD you "DO ME" like that?.... so sayeth the SQL engine.. (if the SQL Engine were a character in West Side Story...))

[West Side Story](https://en.wikipedia.org/wiki/West_Side_Story)

***

## 1. Using `SELECT *` in Queries

### ❌ Bad Practice Example:

```sql
SELECT * FROM employees WHERE department = 'Sales';
```

### ❗ Why It’s Bad:

Look, I get it—`SELECT *` is tempting because it’s easy. But it’s like ordering every item on a restaurant menu when you just want a burger.

Your database ends up working overtime to fetch unnecessary data, which means slower queries and unhappy servers.

### ✅ Good Practice:

Specify only the columns you need:

```sql
SELECT employee_id, first_name, last_name FROM employees WHERE department = 'Sales';
```

### 📈 Performance Boost:

This can speed up queries by **up to 50%** since you're only grabbing what you actually need.

### 🔗 References:

* [Dev.to - SQL Bad Practices](https://dev.to/abdelrahmanallam/7-bad-practices-to-avoid-when-writing-sql-queries-for-better-performance-c87)
* [Wikipedia - SELECT](https://en.wikipedia.org/wiki/SELECT_%28SQL%29)

***

## 2. Not Using Indexes Properly

### ❌ Bad Practice Example:

```sql
SELECT * FROM orders WHERE customer_id = 12345;
```

### ❗ Why It’s Bad:

If you don’t have an index on `customer_id`, your database has to scan **every single row** in your `orders` table to find a match. That’s like flipping through a 1,000-page book to find a single word.

### ✅ Good Practice:

Create an index so your database can find data faster:

```sql
CREATE INDEX idx_customer_id ON orders(customer_id);
```

### 📈 Performance Boost:

Proper indexing can make queries **several orders of magnitude faster**. No more full table scans!

### 🔗 References:

* [Red9 - SQL Performance Tuning](https://red9.com/blog/top-10-sql-server-performance-tuning-mistakes/)
* [Wikipedia - Index (Database)](https://en.wikipedia.org/wiki/Database_index)

***

## 3. Using Functions in WHERE Clauses

### ❌ Bad Practice Example:

```sql
SELECT * FROM employees WHERE YEAR(hire_date) = 2020;
```

### ❗ Why It’s Bad:

Applying functions to columns in `WHERE` clauses forces your database to process **every row** before filtering results. It’s like asking someone to calculate their age before checking if they were born in 2020.

### ✅ Good Practice:

Rewrite your query to avoid functions:

```sql
SELECT * FROM employees WHERE hire_date BETWEEN '2020-01-01' AND '2020-12-31';
```

### 📈 Performance Boost:

Avoiding functions in `WHERE` can **improve performance by 90%** since indexes can be used properly.

### 🔗 References:

* [Wikipedia - SQL WHERE Clause](https://en.wikipedia.org/wiki/SQL_where_clause)

***

## 4. Using Leading Wildcards in `LIKE` Clauses

### ❌ Bad Practice Example:

```sql
SELECT * FROM products WHERE name LIKE '%phone';
```

### ❗ Why It’s Bad:

Starting a `LIKE` search with `%` means your database has **no clue** where to start, so it has to check **every single row**. It’s like searching for a word in a book without an index.

### ✅ Good Practice:

Only use wildcards at the **end**:

```sql
SELECT * FROM products WHERE name LIKE 'phone%';
```

### 📈 Performance Boost:

This can improve performance by **95%**, because now the database can use indexing.

### 🔗 References:

* [Wikipedia - SQL LIKE](https://en.wikipedia.org/wiki/LIKE_%28SQL%29)

***

## 5. Using `OR` in WHERE Clauses Without Indexing

### ❌ Bad Practice Example:

```sql
SELECT * FROM products WHERE category = 'Electronics' OR category = 'Appliances';
```

### ❗ Why It’s Bad:

Without proper indexing, `OR` forces your database to perform multiple searches instead of a single optimized one.

### ✅ Good Practice:

Use `IN` instead:

```sql
SELECT * FROM products WHERE category IN ('Electronics', 'Appliances');
```

### 📈 Performance Boost:

Using `IN` properly can **speed up queries by 80%**.

### 🔗 References:

* [Wikipedia - SQL IN](https://en.wikipedia.org/wiki/SQL_IN)

***

## Summary Table of SQL Bad Practices and Solutions

| **Bad Practice**                | **Why It’s Bad**                                              | **Good Practice**                                      | **Performance Boost**                  |
| ------------------------------- | ------------------------------------------------------------- | ------------------------------------------------------ | -------------------------------------- |
| **Using `SELECT *`**            | Fetches unnecessary data, increasing I/O and slowing queries. | Specify only the columns you need.                     | **Up to 50% faster**                   |
| **Not Using Indexes**           | Causes full table scans, making queries slow.                 | Create indexes on frequently searched columns.         | **Several orders of magnitude faster** |
| **Functions in WHERE**          | Prevents indexes from being used, causing full table scans.   | Rewrite queries to avoid functions on indexed columns. | **Up to 90% faster**                   |
| **Leading Wildcards in `LIKE`** | Prevents indexes from being used, causing full scans.         | Use wildcards only at the end (`'phone%'`).            | **Up to 95% faster**                   |
| **Using `OR` Without Indexing** | Causes multiple scans instead of one optimized search.        | Use `IN` instead of multiple `OR` conditions.          | **Up to 80% faster**                   |

***

## Check These Links:

* [Dev.to - SQL Optimization](https://dev.to/abhay_yt_52a8e72b213be229/10-bad-practices-to-avoid-when-writing-sql-queries-for-better-performance-3doe)
* [Red9 - SQL Performance Tuning](https://red9.com/blog/top-10-sql-server-performance-tuning-mistakes/)
* [Wikipedia - SQL Indexing](https://en.wikipedia.org/wiki/Database_index)

***
