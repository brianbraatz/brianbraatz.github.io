---
title: "Comparing SQL Triggers : MSSQL, MySQL, Postgres"
description: Side by Side compare Triggers in multiple engines
slug: comparing-sql-triggers
date: 2021-04-10
image: post/Articles/IMAGES/roy-trig-wide.png
categories: []
tags:
  - SQL
  - Triggers
  - Sql
  - MSSQL
  - MySQL
  - PostgreSQL
  - Database
  - Comparison
draft: false
weight: 108
lastmod: 2025-02-04T00:01:33.187Z
---
<https://en.wikipedia.org/wiki/Trigger_(horse)>

# Comparing SQL Triggers & Functions: MSSQL, MySQL, Postgres

SQL databases are like flavors of ice cream: they all look similar on the outside, but under the hood, they have their quirks. Today, we're diving into **triggers and functions** across **MSSQL**, **MySQL**, and **PostgreSQL** to see which one has the best, the worst, and the "what the heck were they thinking?" implementations.

## A Quick History of Triggers & Functions in SQL

### MSSQL (Microsoft SQL Server)

MSSQL has supported **triggers** since the **early days of SQL Server 6.5 (1996)**. Microsoft wanted to make SQL Server competitive with Oracle, so they introduced **AFTER triggers** and later added **INSTEAD OF triggers**.

### MySQL

MySQL didn’t support triggers until **version 5.0 (2005)**—yes, really! Before that, developers had to **simulate triggers with stored procedures** and lots of duct tape.

### PostgreSQL

PostgreSQL has had **trigger support since version 7.0 (2000)** and, like everything else in Postgres, it’s **insanely flexible**. Not only does it support **BEFORE and AFTER triggers**, but you can also use **procedural languages (PL/pgSQL, Python, etc.)** inside triggers.

***

***

## Types of Triggers Explained

### BEFORE Triggers

BEFORE triggers execute **before** the actual operation (INSERT, UPDATE, DELETE) happens. They are great for **data validation and modification before it’s committed**.

### AFTER Triggers

AFTER triggers execute **after** the operation completes. These are commonly used for **logging changes, auditing, and enforcing business rules**.

### INSTEAD OF Triggers

INSTEAD OF triggers allow you to **replace the normal execution of an operation**. These are useful when dealing with **views that are not directly updatable**.

### Row-Level Triggers

Row-level triggers fire **once per affected row** in an operation. If a query affects 10 rows, the trigger will execute 10 times.

### Statement-Level Triggers

Statement-level triggers fire **once per SQL statement**, regardless of how many rows it affects.

### Multiple Triggers per Event

Most modern databases allow multiple triggers to be defined for the same event, and they execute in **an undefined order unless explicitly controlled**.

### Functions Inside Triggers

Triggers can call stored functions to keep logic modular and **reduce code duplication**.

***

## Feature Comparison: Triggers & Functions

| Feature                     | MSSQL           | MySQL           | PostgreSQL |
| --------------------------- | --------------- | --------------- | ---------- |
| BEFORE Triggers             | ❌ Not supported | ✅ Yes           | ✅ Yes      |
| AFTER Triggers              | ✅ Yes           | ✅ Yes           | ✅ Yes      |
| INSTEAD OF Triggers         | ✅ Yes           | ❌ Not supported | ✅ Yes      |
| Row-Level Triggers          | ✅ Yes           | ✅ Yes           | ✅ Yes      |
| Statement-Level Triggers    | ✅ Yes           | ✅ Yes           | ✅ Yes      |
| Multiple Triggers per Event | ✅ Yes           | ✅ Yes           | ✅ Yes      |
| Functions Inside Triggers   | ✅ Yes           | ✅ Yes           | ✅ Yes      |

***

## Code Examples

<!-- 
### MSSQL - Creating a Trigger
~~~sql
CREATE TRIGGER trg_AfterInsert
ON Employees
AFTER INSERT
AS
BEGIN
    PRINT 'A new employee has been added!'
END;
~~~

### MySQL - Creating a Trigger
~~~sql
CREATE TRIGGER trg_AfterInsert
AFTER INSERT ON Employees
FOR EACH ROW
BEGIN
    INSERT INTO LogTable (Message) VALUES ('New employee added');
END;
~~~

### PostgreSQL - Creating a Trigger
~~~sql
CREATE FUNCTION trg_function() RETURNS trigger AS $$
BEGIN
    INSERT INTO LogTable (Message) VALUES ('New employee added');
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_AfterInsert
AFTER INSERT ON Employees
FOR EACH ROW EXECUTE FUNCTION trg_function();
~~~
-->

### MSSQL - Creating a BEFORE Trigger (Simulated with INSTEAD OF)

```sql
CREATE TRIGGER trg_InsteadOfInsert
ON Employees
INSTEAD OF INSERT
AS
BEGIN
    PRINT 'Insert operation intercepted!'
END;
```

### MySQL - Creating a BEFORE Trigger

```sql
CREATE TRIGGER trg_BeforeInsert
BEFORE INSERT ON Employees
FOR EACH ROW
BEGIN
    SET NEW.salary = NEW.salary * 1.10; -- Auto-increase salary by 10%
END;
```

### PostgreSQL - Creating a BEFORE Trigger

```sql
CREATE FUNCTION trg_function() RETURNS trigger AS $$
BEGIN
    NEW.salary := NEW.salary * 1.10;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_BeforeInsert
BEFORE INSERT ON Employees
FOR EACH ROW EXECUTE FUNCTION trg_function();
```

### MSSQL - AFTER Trigger

```sql
CREATE TRIGGER trg_AfterInsert ON Employees
AFTER INSERT
AS
BEGIN
    PRINT 'A new employee has been added!'
END;
```

### MySQL - AFTER Trigger

```sql
CREATE TRIGGER trg_AfterInsert
AFTER INSERT ON Employees
FOR EACH ROW
BEGIN
    INSERT INTO LogTable (Message) VALUES ('New employee added');
END;
```

### PostgreSQL - AFTER Trigger

```sql
CREATE FUNCTION trg_function() RETURNS trigger AS $$
BEGIN
    INSERT INTO LogTable (Message) VALUES ('New employee added');
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_AfterInsert
AFTER INSERT ON Employees
FOR EACH ROW EXECUTE FUNCTION trg_function();
```

### MSSQL - INSTEAD OF Trigger

```sql
CREATE TRIGGER trg_InsteadOfInsert ON Employees
INSTEAD OF INSERT
AS
BEGIN
    PRINT 'Insert operation intercepted!'
END;
```

### MySQL - No Support for INSTEAD OF Triggers

### PostgreSQL - INSTEAD OF Trigger

```sql
CREATE TRIGGER trg_InsteadOfInsert
INSTEAD OF INSERT ON EmployeeView
FOR EACH ROW EXECUTE FUNCTION trg_function();
```

### MSSQL - Row-Level Trigger

```sql
CREATE TRIGGER trg_RowLevel ON Employees
AFTER INSERT
AS
BEGIN
    PRINT 'Row-level trigger executed!'
END;
```

### MySQL - Row-Level Trigger

```sql
CREATE TRIGGER trg_RowLevel
BEFORE UPDATE ON Employees
FOR EACH ROW
BEGIN
    SET NEW.salary = NEW.salary * 1.10;
END;
```

### PostgreSQL - Row-Level Trigger

```sql
CREATE TRIGGER trg_RowLevel
BEFORE UPDATE ON Employees
FOR EACH ROW EXECUTE FUNCTION trg_function();
```

### MSSQL - Statement-Level Trigger

```sql
CREATE TRIGGER trg_StatementLevel
ON Employees
AFTER INSERT
AS
BEGIN
    PRINT 'Statement-level trigger executed!'
END;
```

### MySQL - Statement-Level Trigger

```sql
CREATE TRIGGER trg_StatementLevel
AFTER INSERT ON Employees
FOR EACH STATEMENT
BEGIN
    INSERT INTO LogTable (Message) VALUES ('Statement-level trigger fired');
END;
```

### PostgreSQL - Statement-Level Trigger

```sql
CREATE TRIGGER trg_StatementLevel
AFTER INSERT ON Employees
FOR EACH STATEMENT EXECUTE FUNCTION trg_function();
```

### MSSQL - Multiple Triggers per Event

```sql
CREATE TRIGGER trg_Multiple1 ON Employees
AFTER INSERT AS PRINT 'Trigger 1';

CREATE TRIGGER trg_Multiple2 ON Employees
AFTER INSERT AS PRINT 'Trigger 2';
```

### MySQL - Multiple Triggers per Event

```sql
CREATE TRIGGER trg_Multiple1 AFTER INSERT ON Employees FOR EACH ROW
BEGIN
    INSERT INTO LogTable (Message) VALUES ('Trigger 1 fired');
END;

CREATE TRIGGER trg_Multiple2 AFTER INSERT ON Employees FOR EACH ROW
BEGIN
    INSERT INTO LogTable (Message) VALUES ('Trigger 2 fired');
END;
```

### PostgreSQL - Multiple Triggers per Event

```sql
CREATE TRIGGER trg_Multiple1 AFTER INSERT ON Employees FOR EACH ROW EXECUTE FUNCTION trg_function();
CREATE TRIGGER trg_Multiple2 AFTER INSERT ON Employees FOR EACH ROW EXECUTE FUNCTION trg_function();
```

***

## Pros & Cons of Triggers in Each Database

| Database   | Pros                                             | Cons                       |
| ---------- | ------------------------------------------------ | -------------------------- |
| MSSQL      | **Good support for AFTER & INSTEAD OF triggers** | **No BEFORE triggers**     |
| MySQL      | **Simple and easy to use**                       | **No INSTEAD OF triggers** |
| PostgreSQL | **Most flexible and feature-rich**               | **More complex syntax**    |

***

## Key Takeaways

| Topic            | Summary                                                                                                       |
| ---------------- | ------------------------------------------------------------------------------------------------------------- |
| Trigger Support  | PostgreSQL has the most flexibility, MSSQL is solid, and MySQL is decent but lacks INSTEAD OF triggers.       |
| Function Support | All three databases allow functions inside triggers, but PostgreSQL allows more customization.                |
| Ease of Use      | MySQL is the easiest to use, MSSQL is well-documented, and PostgreSQL is powerful but requires more learning. |

***

## References

* https://learn.microsoft.com/en-us/sql/t-sql/statements/create-trigger-transact-sql
* https://dev.mysql.com/doc/refman/8.0/en/create-trigger.html
* https://www.postgresql.org/docs/current/sql-createtrigger.html

<!-- 
============================================


---
title: "Comparing SQL Triggers & Functions: MSSQL, MySQL, Postgres"
description: "Comparing SQL Triggers & Functions: MSSQL, MySQL, Postgres"
slug: "comparing-sql-triggers-and-functions-mssql-mysql-postgres"
date: "2021-04-10"
image: "post/Articles/IMAGES/4.jpg"
categories: []
tags: ["SQL", "Triggers", "Functions", "MSSQL", "MySQL", "PostgreSQL", "Database", "Comparison"]
draft: false
weight: 108
---

# Comparing SQL Triggers & Functions: MSSQL, MySQL, Postgres

SQL databases are like flavors of ice cream: they all look similar on the outside, but under the hood, they have their quirks. Today, we're diving into **triggers and functions** across **MSSQL**, **MySQL**, and **PostgreSQL** to see which one has the best, the worst, and the "what the heck were they thinking?" implementations.

## A Quick History of Triggers & Functions in SQL

### MSSQL (Microsoft SQL Server)
MSSQL has supported **triggers** since the **early days of SQL Server 6.5 (1996)**. Microsoft wanted to make SQL Server competitive with Oracle, so they introduced **AFTER triggers** and later added **INSTEAD OF triggers**. 

### MySQL
MySQL didn’t support triggers until **version 5.0 (2005)**—yes, really! Before that, developers had to **simulate triggers with stored procedures** and lots of duct tape. 

### PostgreSQL
PostgreSQL has had **trigger support since version 7.0 (2000)** and, like everything else in Postgres, it’s **insanely flexible**. Not only does it support **BEFORE and AFTER triggers**, but you can also use **procedural languages (PL/pgSQL, Python, etc.)** inside triggers. 

---

## Types of Triggers Explained

### AFTER Triggers
AFTER triggers execute **after** the actual operation (INSERT, UPDATE, DELETE) happens. These are commonly used for **logging changes, auditing, and enforcing business rules**.

### INSTEAD OF Triggers
INSTEAD OF triggers allow you to **replace the normal execution of an operation**. These are useful when dealing with **views that are not directly updatable**.

### Row-Level Triggers
Row-level triggers fire **once per affected row** in an operation. If a query affects 10 rows, the trigger will execute 10 times.

### Statement-Level Triggers
Statement-level triggers fire **once per SQL statement**, regardless of how many rows it affects.

### Multiple Triggers per Event
Most modern databases allow multiple triggers to be defined for the same event, and they execute in **an undefined order unless explicitly controlled**.

### Functions Inside Triggers
Triggers can call stored functions to keep logic modular and **reduce code duplication**.

---

## Code Examples

### MSSQL - AFTER Trigger
~~~sql
CREATE TRIGGER trg_AfterInsert ON Employees
AFTER INSERT
AS
BEGIN
    PRINT 'A new employee has been added!'
END;
~~~

### MySQL - AFTER Trigger
~~~sql
CREATE TRIGGER trg_AfterInsert
AFTER INSERT ON Employees
FOR EACH ROW
BEGIN
    INSERT INTO LogTable (Message) VALUES ('New employee added');
END;
~~~

### PostgreSQL - AFTER Trigger
~~~sql
CREATE FUNCTION trg_function() RETURNS trigger AS $$
BEGIN
    INSERT INTO LogTable (Message) VALUES ('New employee added');
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_AfterInsert
AFTER INSERT ON Employees
FOR EACH ROW EXECUTE FUNCTION trg_function();
~~~

### MSSQL - INSTEAD OF Trigger
~~~sql
CREATE TRIGGER trg_InsteadOfInsert ON Employees
INSTEAD OF INSERT
AS
BEGIN
    PRINT 'Insert operation intercepted!'
END;
~~~

### MySQL - No Support for INSTEAD OF Triggers

### PostgreSQL - INSTEAD OF Trigger
~~~sql
CREATE TRIGGER trg_InsteadOfInsert
INSTEAD OF INSERT ON EmployeeView
FOR EACH ROW EXECUTE FUNCTION trg_function();
~~~

### MSSQL - Row-Level Trigger
~~~sql
CREATE TRIGGER trg_RowLevel ON Employees
AFTER INSERT
AS
BEGIN
    PRINT 'Row-level trigger executed!'
END;
~~~

### MySQL - Row-Level Trigger
~~~sql
CREATE TRIGGER trg_RowLevel
BEFORE UPDATE ON Employees
FOR EACH ROW
BEGIN
    SET NEW.salary = NEW.salary * 1.10;
END;
~~~

### PostgreSQL - Row-Level Trigger
~~~sql
CREATE TRIGGER trg_RowLevel
BEFORE UPDATE ON Employees
FOR EACH ROW EXECUTE FUNCTION trg_function();
~~~

### MSSQL - Statement-Level Trigger
~~~sql
CREATE TRIGGER trg_StatementLevel
ON Employees
AFTER INSERT
AS
BEGIN
    PRINT 'Statement-level trigger executed!'
END;
~~~

### MySQL - Statement-Level Trigger
~~~sql
CREATE TRIGGER trg_StatementLevel
AFTER INSERT ON Employees
FOR EACH STATEMENT
BEGIN
    INSERT INTO LogTable (Message) VALUES ('Statement-level trigger fired');
END;
~~~

### PostgreSQL - Statement-Level Trigger
~~~sql
CREATE TRIGGER trg_StatementLevel
AFTER INSERT ON Employees
FOR EACH STATEMENT EXECUTE FUNCTION trg_function();
~~~

### MSSQL - Multiple Triggers per Event
~~~sql
CREATE TRIGGER trg_Multiple1 ON Employees
AFTER INSERT AS PRINT 'Trigger 1';

CREATE TRIGGER trg_Multiple2 ON Employees
AFTER INSERT AS PRINT 'Trigger 2';
~~~

### MySQL - Multiple Triggers per Event
~~~sql
CREATE TRIGGER trg_Multiple1 AFTER INSERT ON Employees FOR EACH ROW
BEGIN
    INSERT INTO LogTable (Message) VALUES ('Trigger 1 fired');
END;

CREATE TRIGGER trg_Multiple2 AFTER INSERT ON Employees FOR EACH ROW
BEGIN
    INSERT INTO LogTable (Message) VALUES ('Trigger 2 fired');
END;
~~~

### PostgreSQL - Multiple Triggers per Event
~~~sql
CREATE TRIGGER trg_Multiple1 AFTER INSERT ON Employees FOR EACH ROW EXECUTE FUNCTION trg_function();
CREATE TRIGGER trg_Multiple2 AFTER INSERT ON Employees FOR EACH ROW EXECUTE FUNCTION trg_function();
~~~

---

## References

- https://learn.microsoft.com/en-us/sql/t-sql/statements/create-trigger-transact-sql
- https://dev.mysql.com/doc/refman/8.0/en/create-trigger.html
- https://www.postgresql.org/docs/current/sql-createtrigger.html


=============================================
---
title: "Comparing SQL Triggers & Functions: MSSQL, MySQL, Postgres"
description: "Comparing SQL Triggers & Functions: MSSQL, MySQL, Postgres"
slug: "comparing-sql-triggers-and-functions-mssql-mysql-postgres"
date: "2021-04-10"
image: "post/Articles/IMAGES/4.jpg"
categories: []
tags: ["SQL", "Triggers", "Functions", "MSSQL", "MySQL", "PostgreSQL", "Database", "Comparison"]
draft: false
weight: 108
---

# Comparing SQL Triggers & Functions: MSSQL, MySQL, Postgres

SQL databases are like flavors of ice cream: they all look similar on the outside, but under the hood, they have their quirks. Today, we're diving into **triggers and functions** across **MSSQL**, **MySQL**, and **PostgreSQL** to see which one has the best, the worst, and the "what the heck were they thinking?" implementations.

## A Quick History of Triggers & Functions in SQL

### MSSQL (Microsoft SQL Server)
MSSQL has supported **triggers** since the **early days of SQL Server 6.5 (1996)**. Microsoft wanted to make SQL Server competitive with Oracle, so they introduced **AFTER triggers** and later added **INSTEAD OF triggers**. 

### MySQL
MySQL didn’t support triggers until **version 5.0 (2005)**—yes, really! Before that, developers had to **simulate triggers with stored procedures** and lots of duct tape. 

### PostgreSQL
PostgreSQL has had **trigger support since version 7.0 (2000)** and, like everything else in Postgres, it’s **insanely flexible**. Not only does it support **BEFORE and AFTER triggers**, but you can also use **procedural languages (PL/pgSQL, Python, etc.)** inside triggers. 

---

## Types of Triggers Explained

### BEFORE Triggers
BEFORE triggers execute **before** the actual operation (INSERT, UPDATE, DELETE) happens. They are great for **data validation and modification before it’s committed**.

### AFTER Triggers
AFTER triggers execute **after** the operation completes. These are commonly used for **logging changes, auditing, and enforcing business rules**.

### INSTEAD OF Triggers
INSTEAD OF triggers allow you to **replace the normal execution of an operation**. These are useful when dealing with **views that are not directly updatable**.

### Row-Level Triggers
Row-level triggers fire **once per affected row** in an operation. If a query affects 10 rows, the trigger will execute 10 times.

### Statement-Level Triggers
Statement-level triggers fire **once per SQL statement**, regardless of how many rows it affects.

### Multiple Triggers per Event
Most modern databases allow multiple triggers to be defined for the same event, and they execute in **an undefined order unless explicitly controlled**.

### Functions Inside Triggers
Triggers can call stored functions to keep logic modular and **reduce code duplication**.

---

## Feature Comparison: Triggers & Functions

| Feature                 | MSSQL                        | MySQL                        | PostgreSQL                  |
|-------------------------|-----------------------------|-----------------------------|-----------------------------|
| BEFORE Triggers        | ❌ Not supported            | ✅ Yes                        | ✅ Yes                        |
| AFTER Triggers         | ✅ Yes                        | ✅ Yes                        | ✅ Yes                        |
| INSTEAD OF Triggers    | ✅ Yes                        | ❌ Not supported            | ✅ Yes                        |
| Row-Level Triggers     | ✅ Yes                        | ✅ Yes                        | ✅ Yes                        |
| Statement-Level Triggers | ✅ Yes                      | ✅ Yes                        | ✅ Yes                        |
| Multiple Triggers per Event | ✅ Yes                | ✅ Yes                        | ✅ Yes                        |
| Functions Inside Triggers | ✅ Yes                     | ✅ Yes                        | ✅ Yes                        |

---

## Code Examples

### MSSQL - Creating a BEFORE Trigger (Simulated with INSTEAD OF)
~~~sql
CREATE TRIGGER trg_InsteadOfInsert
ON Employees
INSTEAD OF INSERT
AS
BEGIN
    PRINT 'Insert operation intercepted!'
END;
~~~

### MySQL - Creating a BEFORE Trigger
~~~sql
CREATE TRIGGER trg_BeforeInsert
BEFORE INSERT ON Employees
FOR EACH ROW
BEGIN
    SET NEW.salary = NEW.salary * 1.10; -- Auto-increase salary by 10%
END;
~~~

### PostgreSQL - Creating a BEFORE Trigger
~~~sql
CREATE FUNCTION trg_function() RETURNS trigger AS $$
BEGIN
    NEW.salary := NEW.salary * 1.10;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_BeforeInsert
BEFORE INSERT ON Employees
FOR EACH ROW EXECUTE FUNCTION trg_function();
~~~

---

## Pros & Cons of Triggers in Each Database

| Database  | Pros                                  | Cons                            |
|-----------|--------------------------------------|--------------------------------|
| MSSQL     | **Good support for AFTER & INSTEAD OF triggers** | **No BEFORE triggers**       |
| MySQL     | **Simple and easy to use**           | **No INSTEAD OF triggers**    |
| PostgreSQL | **Most flexible and feature-rich**  | **More complex syntax**       |

---

## Key Takeaways

| Topic            | Summary |
|-----------------|---------|
| Trigger Support | PostgreSQL has the most flexibility, MSSQL is solid, and MySQL is decent but lacks INSTEAD OF triggers. |
| Function Support | All three databases allow functions inside triggers, but PostgreSQL allows more customization. |
| Ease of Use | MySQL is the easiest to use, MSSQL is well-documented, and PostgreSQL is powerful but requires more learning. |

---

## References

- https://learn.microsoft.com/en-us/sql/t-sql/statements/create-trigger-transact-sql
- https://dev.mysql.com/doc/refman/8.0/en/create-trigger.html
- https://www.postgresql.org/docs/current/sql-createtrigger.html


-->
