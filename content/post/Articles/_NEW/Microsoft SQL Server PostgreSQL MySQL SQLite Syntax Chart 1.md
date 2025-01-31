---
title: " Microsoft SQL Server PostgreSQL MySQL SQLite Syntax Chart"
description: Comparison of the SQL DIalects for MSSQL, Postgres, MySql SQLLite
slug: mssql-postgres-mysql-sqllite-dialect
date: 2023-04-06
image: post/Articles/IMAGES/21.jpg
categories: 
tags:
  - Cheatsheet
  - SQL
  - PostgresSql
  - MySql
  - SqlLite
weight: 30
draft: false
lastmod: 2025-01-31T14:36:24.814Z
---
Dialect Chart -**Microsoft SQL Server (MSSQL)**, **PostgreSQL**, **MySQL**, and **SQLite**

| Operation                     | Microsoft SQL Server (MSSQL)    | PostgreSQL                                   | MySQL                                                   | SQLite                                               |
| ----------------------------- | ------------------------------- | -------------------------------------------- | ------------------------------------------------------- | ---------------------------------------------------- |
| **String Concatenation**      | `CONCAT`                        | `CONCAT`                                     | `CONCAT`                                                | `CONCAT`                                             |
| **Auto Increment**            | `IDENTITY(1,1)`                 | `SERIAL`                                     | `AUTO_INCREMENT`                                        | `AUTOINCREMENT`                                      |
| **Limit Rows**                | `SELECT TOP 10 * FROM table;`   | `SELECT * FROM table LIMIT 10;`              | `SELECT * FROM table LIMIT 10;`                         | `SELECT * FROM table LIMIT 10;`                      |
| **Boolean Data Type**         | `BIT` (`1` = true, `0` = false) | `BOOLEAN` (`TRUE`, `FALSE`)                  | `BOOLEAN` (`1` = true, `0` = false)                     | `BOOLEAN` (Stored as `1` or `0`)                     |
| **Current Date & Time**       | `GETDATE()`                     | `NOW()`                                      | `NOW()`                                                 | `DATETIME('now')`                                    |
| **String Length**             | `LEN(string)`                   | `LENGTH(string)`                             | `CHAR_LENGTH(string)`                                   | `LENGTH(string)`                                     |
| **Case Sensitivity**          | Case-insensitive by default     | Case-sensitive                               | Case-insensitive by default                             | Case-insensitive by default                          |
| **IF Conditional**            | `IF...ELSE`                     | `CASE...WHEN`                                | `IF()` function                                         | `CASE...WHEN`                                        |
| **Stored Procedures**         | `CREATE PROCEDURE`              | `CREATE FUNCTION` / `CREATE PROCEDURE`       | `CREATE PROCEDURE`                                      | Not supported                                        |
| **Full Outer Join**           | `FULL OUTER JOIN`               | `FULL OUTER JOIN`                            | Not supported (use `UNION` of `LEFT` and `RIGHT` joins) | Not supported                                        |
| **Upsert (Insert or Update)** | `MERGE`                         | `INSERT INTO... ON CONFLICT DO UPDATE`       | `INSERT INTO... ON DUPLICATE KEY UPDATE`                | `INSERT INTO... ON CONFLICT DO UPDATE` (since v3.24) |
| **Delete with Limit**         | `DELETE TOP (n) FROM table`     | `DELETE FROM table WHERE condition LIMIT n;` | `DELETE FROM table LIMIT n;`                            | Not supported                                        |
| **Select Random Rows**        | `ORDER BY NEWID()`              | `ORDER BY RANDOM()`                          | `ORDER BY RAND()`                                       | `ORDER BY RANDOM()`                                  |
| **Default Port**              | `1433`                          | `5432`                                       | `3306`                                                  | No default port (file-based)                         |
