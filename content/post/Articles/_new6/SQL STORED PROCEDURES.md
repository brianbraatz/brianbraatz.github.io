---
title: "Enterprise Design Pattern: SQL CRUD Explained"
description: Explains this Pattern and How to Generate CRUD With Python
slug: sql-crud--with-stored-procedures
date: 2020-10-03
image: post/Articles/IMAGES/sql.png
categories:
  - SQL
  - Python
  - Scripting
  - Unit Testing
  - MySql
  - Microsoft Sql Server
  - Postgres Sql
  - Cloud
  - ORM
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
weight: 22
categories_ref:
  - SQL
  - Python
  - Scripting
  - Unit Testing
  - MySql
  - Microsoft Sql Server
  - Postgres Sql
  - Cloud
  - ORM
lastmod: 2025-03-14T15:45:23.437Z
---
<!--

# SQL CRUD Design Pattern with Stored Procedures: History, Usage, Pros and Cons with Alternatives, and How to Generate CRUD Stored Procedures with MySQL, SQL Server, and PostgreSQL Using Python
-->

## Introduction

Back in the **1990s and early 2000s**, many enterprise applications followed a **structured approach**:

📌 **"Keep all SQL logic centralized in a stored procedure layer."**

**(At least many of the projects I worked on between mid 1990s and Mid 2000s, used some form of this pattern)**

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

### **Project Structure- The General idea**

```plaintext
/sql-crud-generator
  ├── gen (Generated stored procedures)
  ├── edited (Manually edited procedures)
  ├── scripts
  │   ├── generate_procedures.py
  │   ├── apply_procedures.py
  ├── config.yaml (Database connection settings)
```

### Example: Auto-Generate CRUD Stored Procedures from Live MySQL Database

This Python script connects to a live MySQL database, retrieves the schema, and generates CRUD (Create, Read, Update, Delete) stored procedures for each table found in the database.

#### Prerequisites

1. **Install Python Dependencies:**

   ```bash
   pip install mysql-connector-python
   ```

2. **Update Database Credentials** in the script (`your_username`, `your_password`, `your_host`, `your_database`). (and then email the script to me with the public ip of your server......)

***

#### Python Script to Generate CRUD Procedures

```python
import mysql.connector
from mysql.connector import errorcode

# Database connection configuration
config = {
    'user': 'your_username',
    'password': 'your_password',
    'host': 'your_host',
    'database': 'your_database',
}

# Function to generate CRUD stored procedures for a given table
def generate_crud_procedures(cursor, table_name):
    # Fetch columns and primary key information
    cursor.execute(f"SHOW COLUMNS FROM {table_name}")
    columns = cursor.fetchall()
    primary_key = None
    column_definitions = []
    for column in columns:
        column_name = column[0]
        column_type = column[1]
        key = column[3]
        if key == 'PRI':
            primary_key = column_name
        column_definitions.append((column_name, column_type))

    if not primary_key:
        print(f"Table {table_name} does not have a primary key. Skipping CRUD generation.")
        return

    # Generate stored procedures
    procedures = []

    # CREATE Procedure
    create_columns = ', '.join([col[0] for col in column_definitions if col[0] != primary_key])
    create_values = ', '.join([f"p_{col[0]}" for col in column_definitions if col[0] != primary_key])
    create_params = ', '.join([f"IN p_{col[0]} {col[1]}" for col in column_definitions if col[0] != primary_key])
    create_procedure = f"""
    DELIMITER //
    CREATE PROCEDURE sp_create_{table_name} ({create_params})
    BEGIN
        INSERT INTO {table_name} ({create_columns})
        VALUES ({create_values});
    END //
    DELIMITER ;
    """
    procedures.append(create_procedure)

    # READ Procedure
    read_procedure = f"""
    DELIMITER //
    CREATE PROCEDURE sp_read_{table_name} (IN p_{primary_key} {next(col[1] for col in column_definitions if col[0] == primary_key)})
    BEGIN
        SELECT * FROM {table_name} WHERE {primary_key} = p_{primary_key};
    END //
    DELIMITER ;
    """
    procedures.append(read_procedure)

    # UPDATE Procedure
    update_set_clause = ', '.join([f"{col[0]} = p_{col[0]}" for col in column_definitions if col[0] != primary_key])
    update_params = ', '.join([f"IN p_{col[0]} {col[1]}" for col in column_definitions])
    update_procedure = f"""
    DELIMITER //
    CREATE PROCEDURE sp_update_{table_name} ({update_params})
    BEGIN
        UPDATE {table_name}
        SET {update_set_clause}
        WHERE {primary_key} = p_{primary_key};
    END //
    DELIMITER ;
    """
    procedures.append(update_procedure)

    # DELETE Procedure
    delete_procedure = f"""
    DELIMITER //
    CREATE PROCEDURE sp_delete_{table_name} (IN p_{primary_key} {next(col[1] for col in column_definitions if col[0] == primary_key)})
    BEGIN
        DELETE FROM {table_name} WHERE {primary_key} = p_{primary_key};
    END //
    DELIMITER ;
    """
    procedures.append(delete_procedure)

    # Output the procedures
    for procedure in procedures:
        print(procedure)

# Main function
def main():
    try:
        # Establish the database connection
        cnx = mysql.connector.connect(**config)
        cursor = cnx.cursor()

        # Fetch all tables in the database
        cursor.execute("SHOW TABLES")
        tables = cursor.fetchall()

        for (table_name,) in tables:
            print(f"Generating CRUD procedures for table: {table_name}")
            generate_crud_procedures(cursor, table_name)
            print("\n")

        cursor.close()
        cnx.close()

    except mysql.connector.Error as err:
        if err.errno == errorcode.ER_ACCESS_DENIED_ERROR:
            print("Something is wrong with your user name or password")
        elif err.errno == errorcode.ER_BAD_DB_ERROR:
            print("Database does not exist")
        else:
            print(err)

if __name__ == "__main__":
    main()
```

## Explanation

* The script connects to a MySQL database and retrieves all table names.
* It then fetches the column details, identifying the primary key for each table.
* It generates four stored procedures for each table:
  * `sp_create_{table_name}`: Inserts a new record.
  * `sp_read_{table_name}`: Retrieves a record by primary key.
  * `sp_update_{table_name}`: Updates a record based on primary key.
  * `sp_delete_{table_name}`: Deletes a record by primary key.
* The generated stored procedures are printed to the console.
  * We used to generate one file per operation per table, but you can do it however you like
  * See the project structure diagram above for ideas and how to structure

#### Notes On The Python Script

* **Primary Key Requirement:** The script requires that each table has a primary key. If a table lacks one, it is skipped.
* **Customization:** You may probably should modify the script to include additional constraints, logging, or transaction management.
* **Execution:** Redirect the script's output to a SQL file or copy-paste it into a MySQL console to execute the stored procedures.

<!--
### **Python Script to Generate CRUD Stored Procedures**  

~~~python
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
~~~
-->

***

## Key Takeaways

* Stored procedures **enhance performance and security**, but **increase maintenance complexity**.
* **Python automation** can simplify stored procedure creation.
* **Alternative methods like ORMs** (Entity Framework, SQLAlchemy) provide **more flexibility**.
* **Views and indexing** can improve stored procedure efficiency.

***

## References

3. [Stored Procedures - Wikipedia](https://en.wikipedia.org/wiki/Stored_procedure)
4. [SQL CRUD Operations](https://www.sqlshack.com/sql-crud-operations/)
5. [Best Practices for Stored Procedures](https://www.sqlservercentral.com/articles/best-practices-for-stored-procedures)
