---
title: SQL Paging Strategies
description: Load Only What You Need
slug: sql-paging-strategies
date: 2017-06-22
image: post/Articles/IMAGES/sql.png
categories:
  - Sql
  - Paging
  - Database
  - Mssql
  - Mysql
  - Postgres
tags:
  - Sql
  - Paging
  - Database
  - Mssql
  - Mysql
  - Postgres
draft: "False"
weight: "432"
lastmod: 2025-03-02T23:06:10.102Z
---
# SQL Paging Strategies: How to Load Only What You Need

So, you've got a database full of emails, and you need to load them for your users.

But here's the thing: **you do not want to load a million emails at once**.

Unless you want your database to melt like a popsicle in the sun, you need paging.

Paging is just a fancy way of saying, *"Give me a few rows at a time, and I'll ask for more when I need them."* This way, your app stays snappy, your database doesn’t cry itself to sleep, and your users don’t get hit with a tsunami of data they didn’t ask for.

So, how do we do paging in SQL?

Let's look at **simple, medium, and advanced** methods in **MSSQL, MySQL, and PostgreSQL**.

***

## 🥤 Simple Paging (LIMIT & OFFSET)

This is the "I just need something quick" method.

It’s simple, works everywhere, and is perfect for small datasets.

Here’s how it works:

* `LIMIT` tells SQL how many rows to return.
* `OFFSET` tells SQL where to start.

### **MySQL & PostgreSQL:**

```sql
SELECT * FROM emails
ORDER BY created_at DESC
LIMIT 10 OFFSET 0; -- First page (10 emails)
```

```sql
SELECT * FROM emails
ORDER BY created_at DESC
LIMIT 10 OFFSET 10; -- Second page (next 10 emails)
```

### **SQL Server (MSSQL):**

```sql
SELECT * FROM emails
ORDER BY created_at DESC
OFFSET 0 ROWS FETCH NEXT 10 ROWS ONLY; -- First page
```

```sql
SELECT * FROM emails
ORDER BY created_at DESC
OFFSET 10 ROWS FETCH NEXT 10 ROWS ONLY; -- Second page
```

📌 **Downside:** The further you go, the slower it gets!

Why?

Because the database still reads all the previous rows before returning your page.

At page 100, that’s 1000 rows read just to skip them.

Yikes.

***

## 🍔 Medium Paging (Using a WHERE Condition)

If you’re fetching data based on a column like `id` or `created_at`, you can use a **WHERE condition** to make things faster.

Instead of `OFFSET`, just **remember the last item from the previous page** and use that to filter the next batch.

### **MySQL & PostgreSQL:**

```sql
SELECT * FROM emails
WHERE created_at < '2024-01-01 12:00:00'
ORDER BY created_at DESC
LIMIT 10;
```

### **MSSQL:**

```sql
SELECT TOP 10 * FROM emails
WHERE created_at < '2024-01-01 12:00:00'
ORDER BY created_at DESC;
```

📌 **Why this is better:** It doesn’t need to scan and skip a ton of rows.

It just finds where to start and grabs the next batch.

👎 **Downside:** This only works if you’re paging based on a column that keeps increasing (like an `id` or a `timestamp`).

If you need flexible pagination, keep reading.

***

## 🚀 Advanced Paging (Keyset Pagination)

Now we’re talking **serious performance**.

Keyset pagination is the king of paging when performance matters.

It avoids `OFFSET` entirely and uses **indexed columns** to fetch the next batch efficiently.

### **MySQL & PostgreSQL:**

```sql
SELECT * FROM emails
WHERE id > 1000
ORDER BY id ASC
LIMIT 10;
```

### **MSSQL:**

```sql
SELECT TOP 10 * FROM emails
WHERE id > 1000
ORDER BY id ASC;
```

📌 **Why this is the best:** It **only fetches the rows you need**, doesn’t scan unnecessary data, and performs well even with **millions of rows**.

👎 **Downside:** You need an **indexed column** (like `id` or `created_at`) that lets you filter efficiently.

***

## 🧐 Which One Should You Use?

| Method              | Pros                           | Cons                             |
| ------------------- | ------------------------------ | -------------------------------- |
| `LIMIT OFFSET`      | Simple, easy to use            | Slow on large datasets           |
| `WHERE condition`   | Faster, avoids offset overhead | Needs a reliable ordering column |
| `Keyset pagination` | Super-fast, efficient          | Requires indexed column          |

If you’re just starting out, use `LIMIT OFFSET`.

If performance starts getting bad, switch to a **WHERE condition** or **keyset pagination**.

If you expect **millions** of records, just go straight for **keyset pagination** and save yourself some future headaches.

***

## 🎯 Key Ideas

| Concept         | Summary                                                                     |
| --------------- | --------------------------------------------------------------------------- |
| Simple Paging   | Uses `LIMIT OFFSET`, works but gets slow at high page numbers               |
| Medium Paging   | Uses `WHERE` with timestamps or IDs to fetch rows efficiently               |
| Advanced Paging | Uses **Keyset pagination**, skipping OFFSET for max speed                   |
| Best Practice   | Start with `LIMIT OFFSET`, move to **keyset pagination** for large datasets |

***

## 🔗 References

* [MySQL LIMIT Documentation](https://dev.mysql.com/doc/refman/8.0/en/select.html#id4651996)
* [PostgreSQL LIMIT Documentation](https://www.postgresql.org/docs/current/queries-limit.html)
* [MSSQL OFFSET-FETCH Documentation](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-order-by-clause-transact-sql?view=sql-server-ver16)

***
