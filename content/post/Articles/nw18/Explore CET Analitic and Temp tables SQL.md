---
title: SQL-Exploring CET Analytic(Window) Functions, Common Table Expressions (CTEs)
description: Explored in Oracle, MSSQL , MySql and Postgres
slug: sql-win-funcs-cte-temptable
date: 2020-07-21
image: post/Articles/IMAGES/sql.png
categories:
  - CSharp
  - Database
  - Design Patterns
  - SQL
  - Oracle
tags:
  - C#
  - Database
  - Abstraction
  - Stored
  - Procedures
  - Design
  - Patterns
  - Multi-DB
  - Support
draft: false
weight: 542
lastmod: 2025-03-19T01:44:38.218Z
---
<!-- SQL-Exploring CET Analytic(Window) Functions, Common Table Expressions (CTEs), and Temp tables in Oracle, MSSQL , MySql and Postgres -->

<!-- ## Analytic Functions and Common Table Expressions (CTEs) in SQL

SQL is packed with powerful features that allow for advanced querying and data manipulation. Two such features are **Analytic Functions** and **Common Table Expressions (CTEs)**. 

Let’s break them down with simple explanations and practical examples.

--- -->

## **1. Analytic Functions in SQL**

Analytic functions (also called **window functions**) perform calculations across a specific range of rows (**a window**) related to the current row. Unlike **aggregate functions** (which collapse multiple rows into one), analytic functions retain individual row results while still providing aggregated insights.

### 🔹 **Key Features of Analytic Functions**

* They operate **over a defined window of rows**.
* They **do not group** the result set into a single row.
* They require an **OVER() clause**.

### 🔹 **Common Analytic Functions**

| Function          | Description                                                             |
| ----------------- | ----------------------------------------------------------------------- |
| `ROW_NUMBER()`    | Assigns a unique row number to each row in a partition.                 |
| `RANK()`          | Assigns a rank to each row, allowing for ties (skips numbers for ties). |
| `DENSE_RANK()`    | Similar to `RANK()`, but without skipping numbers for ties.             |
| `NTILE(n)`        | Distributes rows into `n` roughly equal buckets.                        |
| `LAG(column, n)`  | Returns the value of the column from `n` rows before the current row.   |
| `LEAD(column, n)` | Returns the value of the column from `n` rows after the current row.    |
| `SUM() OVER()`    | Calculates a running total.                                             |
| `AVG() OVER()`    | Computes a moving average.                                              |

### 🔹 **Example: Using ROW\_NUMBER()**

Let’s say we have a `sales` table:

| id | customer | amount |
| -- | -------- | ------ |
| 1  | Alice    | 100    |
| 2  | Bob      | 200    |
| 3  | Alice    | 150    |
| 4  | Bob      | 250    |
| 5  | Charlie  | 300    |

To rank each customer’s purchases:

```sql
SELECT 
    customer, 
    amount, 
    ROW_NUMBER() OVER(PARTITION BY customer ORDER BY amount DESC) AS row_num
FROM sales;
```

**Breakdown**:

* `PARTITION BY customer`: Resets the row numbering for each customer.
* `ORDER BY amount DESC`: Orders within each partition by highest amount.
* `ROW_NUMBER()`: Assigns a unique row number per partition.

**Result:**

| customer | amount | row\_num |
| -------- | ------ | -------- |
| Alice    | 150    | 1        |
| Alice    | 100    | 2        |
| Bob      | 250    | 1        |
| Bob      | 200    | 2        |
| Charlie  | 300    | 1        |

***

## **2. Common Table Expressions (CTEs)**

A **Common Table Expression (CTE)** is a temporary result set that exists only within the execution of a single query. It helps improve **readability** and **modularity**.

### 🔹 **Why Use CTEs?**

* Makes complex queries **easier to read**.
* Can be **referenced multiple times** within the same query.
* Helps **avoid subquery repetition**.

### 🔹 **Basic Syntax**

```sql
WITH cte_name AS (
    -- Your query here
)
SELECT * FROM cte_name;
```

### 🔹 **Example: Using a CTE**

Consider a `sales` table where we want to calculate the **total sales per customer**, and then filter those with sales above \$200.

```sql
WITH total_sales AS (
    SELECT customer, SUM(amount) AS total_amount
    FROM sales
    GROUP BY customer
)
SELECT * 
FROM total_sales
WHERE total_amount > 200;
```

**Breakdown**:

1. The **CTE (`total_sales`)** first computes `SUM(amount)` per customer.
2. The **main query** filters results where `total_amount > 200`.

💡 **Alternative (Without CTEs)**: We’d have to repeat the aggregation:

```sql
SELECT * FROM (
    SELECT customer, SUM(amount) AS total_amount
    FROM sales
    GROUP BY customer
) t
WHERE total_amount > 200;
```

CTEs make queries much cleaner! 😎

***

## **Combining Analytic Functions and CTEs**

Sometimes, **CTEs and analytic functions** work well together. Let's rank sales using a CTE:

```sql
WITH ranked_sales AS (
    SELECT 
        customer, 
        amount, 
        ROW_NUMBER() OVER(PARTITION BY customer ORDER BY amount DESC) AS rank
    FROM sales
)
SELECT * FROM ranked_sales WHERE rank = 1;
```

🔹 **Purpose**: This finds the **highest sale per customer**.

***

<!-- ##  **Final Thoughts**
- **Analytic functions** give you powerful row-wise calculations **without collapsing results**.
- **CTEs** make queries **more readable** and **reusable**.
- Combining **both** lets you handle **advanced SQL problems elegantly**.

Now go forth and write some killer SQL! 🏆🔥

## **Difference Between a CTE and a Temporary Table in SQL**
Both **Common Table Expressions (CTEs)** and **Temporary Tables** help store intermediate results in SQL, but they serve different purposes and have distinct characteristics. -->

***

<!-- 
## **1. CTE (Common Table Expression)**
A **CTE** is a temporary result set that **only exists during the execution of a single query**. It improves query readability and helps break down complex queries into manageable parts.

### ✅ **Characteristics of CTEs**
- **Scope**: Exists **only within** the statement where it is defined.
- **Performance**: Acts like an inline view or subquery; it does **not persist** in memory or disk.
- **Reusability**: Cannot be modified after creation; used only within the query.
- **Syntax**:
  ```sql
  WITH cte_name AS (
      SELECT column1, column2 FROM some_table WHERE condition
  )
  SELECT * FROM cte_name;
  ```

### 📌 **Example: Using a CTE to Calculate Total Sales Per Customer**
```sql
WITH total_sales AS (
    SELECT customer, SUM(amount) AS total_amount
    FROM sales
    GROUP BY customer
)
SELECT * 
FROM total_sales
WHERE total_amount > 500;
```
🔹 **What Happens?**  
- `total_sales` exists only for the duration of this query.
- Once the query executes, it **disappears**.

--- -->

## **3. Temporary Table**

A **Temporary Table** is a physical table stored in `tempdb` (SQL Server) or a temporary schema (MySQL, PostgreSQL). It can store intermediate results and be referenced **multiple times** within a session.

### **Characteristics of Temporary Tables**

* **Scope**:
  * `#TempTable` (local) exists **only in the current session**.
  * `##GlobalTempTable` (global) can be accessed by multiple sessions.
* **Performance**: **Stored in memory/disk**, better for large data sets.
* **Reusability**: Can be modified after creation (INSERT, UPDATE, DELETE).
* **Indexing**: Can have **indexes** for performance optimization.
* **Syntax**:
  ```sql
  CREATE TABLE #temp_table (
      id INT,
      name VARCHAR(50)
  );

  INSERT INTO #temp_table VALUES (1, 'Alice');

  SELECT * FROM #temp_table;
  ```

### **Example: Using a Temporary Table to Store Sales Data**

```sql
-- Create a temporary table
CREATE TABLE #TempSales (
    customer VARCHAR(50),
    total_amount DECIMAL(10,2)
);

-- Insert aggregated data into temp table
INSERT INTO #TempSales
SELECT customer, SUM(amount) 
FROM sales
GROUP BY customer;

-- Query from temp table multiple times
SELECT * FROM #TempSales WHERE total_amount > 500;
SELECT COUNT(*) FROM #TempSales;

-- Drop the temp table when done
DROP TABLE #TempSales;
```

🔹 **What Happens?**

* `#TempSales` is **physically created** in memory (or disk if large).
* The data persists **until the session ends or the table is dropped**.
* You can run multiple queries against it, unlike a CTE.

***

## \*\* Key Differences Between CTEs and Temp Tables\*\*

| Feature           | CTE                                       | Temporary Table                                     |
| ----------------- | ----------------------------------------- | --------------------------------------------------- |
| **Scope**         | Exists only for the duration of the query | Exists for the session or until explicitly dropped  |
| **Persistence**   | Not stored physically                     | Stored in `tempdb` (SQL Server) or temporary schema |
| **Performance**   | Best for small, in-memory calculations    | Better for large datasets that need indexing        |
| **Reusability**   | Cannot be reused outside the query        | Can be queried multiple times                       |
| **Modifiability** | Read-only                                 | Can be modified (INSERT, UPDATE, DELETE)            |
| **Indexing**      | No indexes                                | Can have indexes                                    |

***

## \*\* When to Use What?\*\*

| Scenario                                                 | Best Choice      |
| -------------------------------------------------------- | ---------------- |
| **Breaking down complex queries** for better readability | ✅ **CTE**        |
| **One-time use within a single query**                   | ✅ **CTE**        |
| **Need to reuse data in multiple queries**               | ✅ **Temp Table** |
| **Working with large datasets that need indexing**       | ✅ **Temp Table** |
| **Needing to modify data after creation**                | ✅ **Temp Table** |

<!-- ---

## **Final Thoughts**
- **Use a CTE** when you need **a quick, readable, one-time temporary result set** within a query.
- **Use a Temporary Table** when you **need to store and manipulate data across multiple queries**.

If your data is small and does **not** need multiple modifications, **CTEs are faster**. For **larger datasets** or when you need to perform **multiple operations**, **Temporary Tables are more efficient**.
 -->

<!-- # **Analytic (Window) Functions in Oracle, SQL Server, PostgreSQL, and MySQL** -->

<!-- ---

## ** 4. What are Analytic (Window) Functions?**
Analytic functions, also known as **window functions**, are powerful SQL functions that allow **row-wise** calculations while maintaining individual row values.

All four databases (**Oracle, SQL Server, PostgreSQL, and MySQL**) support analytic functions, but with differences in implementation, performance, and additional features.

Analytic functions:
- Perform calculations **across a set of rows (a window) without collapsing them**.
- Use the `OVER()` clause to define the **partitioning and ordering** of data.
- Include functions like **`RANK()`, `DENSE_RANK()`, `ROW_NUMBER()`, `LAG()`, `LEAD()`, `SUM() OVER()`, etc.**.

---

# **🔥 Analytic Function Support in Different Databases**
| Feature | **Oracle** | **SQL Server (MSSQL)** | **PostgreSQL** | **MySQL** |
|---------|-----------|-----------------------|---------------|-----------|
| **Basic Window Functions** (`RANK()`, `ROW_NUMBER()`, `SUM() OVER()`, etc.) | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes (MySQL 8.0+) |
| **Advanced Functions** (`NTILE()`, `LAG()`, `LEAD()`) | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes (MySQL 8.0+) |
| **Frame Specification (`ROWS BETWEEN` & `RANGE BETWEEN`)** | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes |
| **Statistical Functions (e.g., `CUME_DIST()`, `PERCENT_RANK()`)** | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No |
| **Custom Aggregate Window Functions** | ❌ No | ❌ No | ✅ Yes | ❌ No | -->

***

# \*\* 4. Analytic Function Syntax in Different Databases\*\*

## \*\* Oracle\*\*

Oracle has **the most mature** support for analytic functions, available **since Oracle 8i**.

### **Basic Example: Ranking Orders by Amount**

```sql
SELECT 
    customer_id, order_id, amount,
    RANK() OVER (PARTITION BY customer_id ORDER BY amount DESC) AS order_rank
FROM orders;
```

### **Running Total Example**

```sql
SELECT 
    customer_id, order_id, amount,
    SUM(amount) OVER (PARTITION BY customer_id ORDER BY order_id) AS running_total
FROM orders;
```

### 🔹 **Oracle-Specific Features**

* **Supports advanced functions** like `CUME_DIST()` and `PERCENT_RANK()`.
* **Performance-optimized window functions** with Oracle's query optimizer.
* **Can use RANGE and ROWS** in the `OVER()` clause.

***

## \*\* SQL Server (MSSQL)\*\*

SQL Server **introduced analytic functions in SQL Server 2012**, making it one of the later adopters.

### **Basic Example: Ranking Employees by Salary**

```sql
SELECT 
    employee_id, department_id, salary,
    RANK() OVER (PARTITION BY department_id ORDER BY salary DESC) AS rank_in_department
FROM employees;
```

### **Lag and Lead Functions Example**

```sql
SELECT 
    employee_id, salary, 
    LAG(salary, 1, 0) OVER (ORDER BY employee_id) AS previous_salary,
    LEAD(salary, 1, 0) OVER (ORDER BY employee_id) AS next_salary
FROM employees;
```

### 🔹 **SQL Server-Specific Features**

* **Full support for analytic functions** (since 2012).
* **Performance tuning required** for large datasets.
* **Limited statistical functions** (compared to Oracle/PostgreSQL).

***

## \*\* PostgreSQL\*\*

PostgreSQL **introduced full analytic function support in version 8.4** and provides **more flexibility than SQL Server**.

### **Basic Example: Ranking Products by Sales**

```sql
SELECT 
    product_id, category_id, sales,
    RANK() OVER (PARTITION BY category_id ORDER BY sales DESC) AS rank
FROM sales;
```

### **Running Sum Example**

```sql
SELECT 
    customer_id, amount,
    SUM(amount) OVER (PARTITION BY customer_id ORDER BY transaction_date) AS running_sum
FROM transactions;
```

### 🔹 **PostgreSQL-Specific Features**

* **Supports advanced statistical window functions** (`CUME_DIST()`, `PERCENT_RANK()`).
* **Supports custom aggregate window functions** (not available in Oracle or SQL Server).
* **Better performance for large datasets** (compared to SQL Server).

***

## \*\* MySQL (8.0+)\*\*

MySQL **introduced window functions in MySQL 8.0** (before that, they were unsupported).

### **Basic Example: Ranking Customers**

```sql
SELECT 
    customer_id, order_id, amount,
    RANK() OVER (PARTITION BY customer_id ORDER BY amount DESC) AS rank
FROM orders;
```

### **Running Total Example**

```sql
SELECT 
    customer_id, order_id, amount,
    SUM(amount) OVER (PARTITION BY customer_id ORDER BY order_id) AS running_total
FROM orders;
```

### 🔹 **MySQL-Specific Limitations**

* **Lacks statistical functions** (`CUME_DIST()`, `PERCENT_RANK()`).
* **Performance is not optimized** for large datasets.
* **No custom aggregate window functions** (unlike PostgreSQL).

***

# **🚀 Key Differences Across Databases**

| Feature                                                          | **Oracle**        | **SQL Server (MSSQL)** | **PostgreSQL**         | **MySQL (8.0+)** |
| ---------------------------------------------------------------- | ----------------- | ---------------------- | ---------------------- | ---------------- |
| **First to support window functions?**                           | ✅ Yes (Oracle 8i) | ❌ No (SQL Server 2012) | ✅ Yes (PostgreSQL 8.4) | ❌ No (MySQL 8.0) |
| **Ranking functions (`RANK()`, `DENSE_RANK()`, `ROW_NUMBER()`)** | ✅ Yes             | ✅ Yes                  | ✅ Yes                  | ✅ Yes            |
| **Lag/Lead functions (`LAG()`, `LEAD()`)**                       | ✅ Yes             | ✅ Yes                  | ✅ Yes                  | ✅ Yes            |
| **Running totals (`SUM() OVER()`, `AVG() OVER()`)**              | ✅ Yes             | ✅ Yes                  | ✅ Yes                  | ✅ Yes            |
| **Statistical functions (`CUME_DIST()`, `PERCENT_RANK()`)**      | ✅ Yes             | ✅ Yes                  | ✅ Yes                  | ❌ No             |
| **Custom aggregate window functions**                            | ❌ No              | ❌ No                   | ✅ Yes                  | ❌ No             |

***

<!-- 
# **🏆 When to Use Which Database for Analytic Functions?**
| Use Case | Best Choice |
|----------|------------|
| **Enterprise applications requiring high-performance analytics** | ✅ **Oracle** |
| **General business applications with analytics support** | ✅ **SQL Server** |
| **Large-scale data analysis, flexible window functions** | ✅ **PostgreSQL** |
| **Basic analytic needs with MySQL 8.0+** | ✅ **MySQL** |

---

# **🎯 Conclusion**
- **Oracle and PostgreSQL** offer **the most advanced analytic function support**.
- **SQL Server** has **strong** but **less flexible** support.
- **MySQL 8.0+** finally introduced **basic** analytic function support but **lacks statistical functions**.
 -->
