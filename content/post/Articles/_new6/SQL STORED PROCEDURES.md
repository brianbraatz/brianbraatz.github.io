---
title: "Enterprise Design Pattern: SQL CRUD Explained"
description: Explains this Pattern and How to Generate CRUD for MySQL, SQL Server, and PostgreSQL Using Python
slug: sql-crud-design-pattern-with-stored-procedures:-history-usage-pros-and-cons-with-alternatives-and-how-to-generate-crud-stored-procedures-with-mysql-sql-server-and-postgresql-using-python
date: 2020-10-03
image: post/Articles/IMAGES/sql.png
categories: 
tags:
  - Sql
  - Stored
  - Procedures
  - Crud
  - Database
  - Architecture
  - Mysql
  - Postgresql
  - Sql
  - Server
  - Database
  - Performance
  - Enterprise
  - Systems
  - Python
  - Automation
  - Database
  - Schema
draft: false
weight: 106
lastmod: 2025-02-10T13:39:34.912Z
---
<!--

# SQL CRUD Design Pattern with Stored Procedures: History, Usage, Pros and Cons with Alternatives, and How to Generate CRUD Stored Procedures with MySQL, SQL Server, and PostgreSQL Using Python
-->

## Introduction

Back in the **1990s and early 2000s**, enterprise applications followed a **structured approach**:

📌 **"Keep all SQL logic centralized in a stored procedure layer."**

### Why was this done?

1. **Performance** – Stored procedures run **inside the database**, reducing network overhead.
2. **Security** – Direct table access was restricted, preventing unauthorized data manipulation.
3. **Maintainability** – Business logic was centralized, making updates easier.

This article explores:

* The **history** of stored procedures.
* **Pros and cons** of this approach.
* **Alternative methods** for handling CRUD operations.
* **How to generate CRUD stored procedures for MySQL, SQL Server, and PostgreSQL using Python.**

***

## The History of Stored Procedures

Before modern **ORMs (Object-Relational Mappers)** like **Entity Framework, SQLAlchemy, and Hibernate**, database interactions were done using **raw SQL queries**.

### **Pre-Stored Procedures Era**

* Developers embedded **raw SQL statements** in application code.
* Code and database logic were **tightly coupled**.
* Query execution was **slow** because every request required **parsing and optimization**.

### **The Stored Procedure Revolution (1990s - Early 2000s)**

* SQL vendors introduced **stored procedures** as **precompiled database scripts**.
* These scripts were stored inside the database itself, leading to **faster execution and better security**.
* Many **enterprise applications** relied **heavily** on stored procedures for CRUD operations.

> **Further Reading:** [Stored Procedure - Wikipedia](https://en.wikipedia.org/wiki/Stored_procedure)

***

## Why Use Stored Procedures for CRUD?

A **typical CRUD operation** using stored procedures follows this structure:

| CRUD Operation | Example Stored Procedure |
| -------------- | ------------------------ |
| **Create**     | `usp_CreateUser`         |
| **Read**       | `usp_GetUserById`        |
| **Update**     | `usp_UpdateUser`         |
| **Delete**     | `usp_DeleteUser`         |

### **Advantages of Using Stored Procedures**

| Feature                     | Benefit                                                      |
| --------------------------- | ------------------------------------------------------------ |
| **Performance**             | Precompiled execution improves query speed.                  |
| **Security**                | Restricts direct table access, reducing SQL injection risks. |
| **Centralized Logic**       | Business logic stays in the database.                        |
| **Reduced Network Traffic** | Only procedure calls are sent to the database.               |

### **Disadvantages of Using Stored Procedures**

| Issue                      | Drawback                                                |
| -------------------------- | ------------------------------------------------------- |
| **Hard to Debug**          | Debugging SQL inside the database is challenging.       |
| **Version Control Issues** | Harder to track changes compared to application code.   |
| **Limited Portability**    | Vendor-specific SQL syntax makes migration difficult.   |
| **Slower Development**     | Writing and maintaining stored procedures takes effort. |

***

## Automating CRUD Stored Procedure Generation with Python

To simplify CRUD procedure creation, we can **automatically generate** stored procedures for **MySQL, SQL Server, and PostgreSQL** using Python.

## How we used to do this

We would usually have a gen directory and an edited .

Every time you changed the schema you regenerate.

If you tweak or edit a stored procedure, you copy it to the edited directory  first.

Then when you apply the stored procedures to to the database , apply the generated first , then the edited.

Sometimes we would generate LIST stored procedures and sometimes views , it just depended on the needs pod the application we were building.

### **Project Structure**

```plaintext
/sql-crud-generator
  ├── gen (Generated stored procedures)
  ├── edited (Manually edited procedures)
  ├── scripts
  │   ├── generate_procedures.py
  │   ├── apply_procedures.py
  ├── config.yaml (Database connection settings)
```

### **Python Script to Generate CRUD Stored Procedures**

```python
import os

def generate_crud_procedures(table_name, columns):
    primary_key = columns[0][0]  # Assume first column is PK
    column_list = ", ".join([col[0] for col in columns])
    values_list = ", ".join([f"@{col[0]}" for col in columns])

    return f"""
-- CREATE PROCEDURE FOR {table_name}
CREATE PROCEDURE usp_Create{table_name} ({values_list})
AS
BEGIN
    INSERT INTO {table_name} ({column_list}) VALUES ({values_list});
END;

-- READ PROCEDURE
CREATE PROCEDURE usp_Get{table_name}ById (@id INT)
AS
BEGIN
    SELECT * FROM {table_name} WHERE {primary_key} = @id;
END;

-- UPDATE PROCEDURE
CREATE PROCEDURE usp_Update{table_name} ({values_list})
AS
BEGIN
    UPDATE {table_name} SET {', '.join([f'{col[0]} = @{col[0]}' for col in columns])} WHERE {primary_key} = @{primary_key};
END;

-- DELETE PROCEDURE
CREATE PROCEDURE usp_Delete{table_name} (@id INT)
AS
BEGIN
    DELETE FROM {table_name} WHERE {primary_key} = @id;
END;
"""

def save_procedures(schema):
    os.makedirs("gen", exist_ok=True)
    for table, columns in schema.items():
        with open(f"gen/{table}.sql", "w") as f:
            f.write(generate_crud_procedures(table, columns))

# Example usage
schema = {"users": [("id", "int"), ("name", "varchar(255)"), ("email", "varchar(255)")]}
save_procedures(schema)
```

***

## Key Takeaways

* Stored procedures **enhance performance and security**, but **increase maintenance complexity**.
* **Python automation** can simplify stored procedure creation.
* **Alternative methods like ORMs** (Entity Framework, SQLAlchemy) provide **more flexibility**.
* **Views and indexing** can improve stored procedure efficiency.

***

## References

4. [Stored Procedures - Wikipedia](https://en.wikipedia.org/wiki/Stored_procedure)
5. [SQL CRUD Operations](https://www.sqlshack.com/sql-crud-operations/)
6. [Best Practices for Stored Procedures](https://www.sqlservercentral.com/articles/best-practices-for-stored-procedures)
