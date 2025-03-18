---
title: How we supported Multiple DB Engines in the 2000s
description: Examples of how we managed multiple database backends in C# before Entity Framework existed.
slug: csharp-multi-db-2000s
date: 2020-07-21
image: post/Articles/IMAGES/SQL.png
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
lastmod: 2025-03-18T13:35:13.144Z
---
See Related Article here:\
[Enterprise Design Pattern: SQL CRUD Explained](https://brianbraatz.github.io/p/sql-crud--with-stored-procedures/)\
csharp-multi-db-2000s

## How We Wrote C# Code to Support Multiple Database Backends in 2006

Back in 2006, before **Entity Framework** or even LINQ to SQL, writing **C# applications** that needed to support multiple database backends was a challenging but fun problem.

There were no modern ORMs, so we had to get creative with **design patterns, stored procedures, and abstraction layers** to make our applications database-agnostic.

Entity Framework, LINQ Etc... these things did not come into common use until late 2000s...

In this article, I'll walk you through how we handled this using **Factory Pattern, Repository Pattern, and CRUD stored procedures**, and I'll even show a code example that worked across **Oracle, Microsoft SQL Server, and MySQL**.

***

## The Problem: No ORM, Only Raw ADO.NET

In 2000's, when you wanted to interact with a database in C#, you used **ADO.NET**. This meant dealing with:

* **SqlConnection, OracleConnection, MySqlConnection** (each DBMS had its own connection object).
* **SqlCommand, OracleCommand, MySqlCommand** (different syntax for executing queries).
* **DataReaders, DataTables, and DataSets**.
* **Raw SQL and stored procedures**.

There was no clean way to **switch databases** without rewriting a ton of code. So, we had to **abstract the database layer**.

**MySql Notes:** We could not do this approch below with MySql until around 2005, since Stored Procedures didnt exist in MySql until then.

***

## The Solution: Abstracting the Database Layer

To make our C# application work across different databases, we used a **combination of design patterns**:

1. **Factory Pattern** - To create the appropriate database objects dynamically.
2. **Repository Pattern** - To provide a common interface for data access.
3. **Stored Procedures for CRUD Operations** - To encapsulate logic inside the database and optimize performance.

Let's break it down.

***

## Step 1: The Factory Pattern for Database Independence

The **Factory Pattern** allowed us to instantiate the right database objects dynamically based on configuration settings.

### C# Example: Database Factory

```csharp
public interface IDatabase
{
    IDbConnection GetConnection();
    IDbCommand CreateCommand(string query, IDbConnection connection);
}

public class SqlServerDatabase : IDatabase
{
    public IDbConnection GetConnection() => new SqlConnection("your-sqlserver-connection-string");
    public IDbCommand CreateCommand(string query, IDbConnection connection) => new SqlCommand(query, (SqlConnection)connection);
}

public class MySqlDatabase : IDatabase
{
    public IDbConnection GetConnection() => new MySqlConnection("your-mysql-connection-string");
    public IDbCommand CreateCommand(string query, IDbConnection connection) => new MySqlCommand(query, (MySqlConnection)connection);
}

public class OracleDatabase : IDatabase
{
    public IDbConnection GetConnection() => new OracleConnection("your-oracle-connection-string");
    public IDbCommand CreateCommand(string query, IDbConnection connection) => new OracleCommand(query, (OracleConnection)connection);
}

public static class DatabaseFactory
{
    public static IDatabase GetDatabase(string dbType)
    {
        return dbType switch
        {
            "MSSQL" => new SqlServerDatabase(),
            "MySQL" => new MySqlDatabase(),
            "Oracle" => new OracleDatabase(),
            _ => throw new NotImplementedException($"Database type {dbType} is not supported")
        };
    }
}
```

Now, in our app, we could instantiate the correct database at runtime:

```csharp
IDatabase database = DatabaseFactory.GetDatabase("MySQL");
using var connection = database.GetConnection();
connection.Open();
```

***

## Step 2: The Repository Pattern for Data Access

The **Repository Pattern** provided a **common interface** for interacting with the database, independent of the actual backend.

### C# Example: Repository for Users

```csharp
public interface IUserRepository
{
    void AddUser(string username);
    List<string> GetAllUsers();
}

public class UserRepository : IUserRepository
{
    private readonly IDatabase _database;

    public UserRepository(IDatabase database)
    {
        _database = database;
    }

    public void AddUser(string username)
    {
        using var connection = _database.GetConnection();
        connection.Open();
        using var command = _database.CreateCommand("usp_AddUser", connection);
        command.CommandType = CommandType.StoredProcedure;
        command.Parameters.Add(new SqlParameter("@Username", username));
        command.ExecuteNonQuery();
    }

    public List<string> GetAllUsers()
    {
        var users = new List<string>();
        using var connection = _database.GetConnection();
        connection.Open();
        using var command = _database.CreateCommand("usp_GetAllUsers", connection);
        command.CommandType = CommandType.StoredProcedure;
        using var reader = command.ExecuteReader();
        while (reader.Read())
        {
            users.Add(reader.GetString(0));
        }
        return users;
    }
}
```

This ensured that **our application code never had to change** regardless of which database we used.

***

## Step 3: CRUD Stored Procedures for Different Databases

Each database backend had different syntax, so we wrote **custom stored procedures** for **Create, Read, Update, and Delete (CRUD)** operations.

### SQL Server Stored Procedure (MSSQL)

```sql
CREATE PROCEDURE usp_AddUser @Username NVARCHAR(50)
AS
BEGIN
    INSERT INTO Users (Username) VALUES (@Username);
END
```

### MySQL Stored Procedure

```sql
DELIMITER $$
CREATE PROCEDURE usp_AddUser(IN p_Username VARCHAR(50))
BEGIN
    INSERT INTO Users (Username) VALUES (p_Username);
END $$
DELIMITER ;
```

### Oracle Stored Procedure

```sql
CREATE PROCEDURE usp_AddUser(p_Username IN VARCHAR2) AS
BEGIN
    INSERT INTO Users (Username) VALUES (p_Username);
END;
```

By keeping the stored procedures **consistent in naming**, the C# code remained unchanged.

***

## Conclusion

Before ORMs like Entity Framework, we **rolled our own** database abstraction using:

* **Factory Pattern** to create database-specific objects.
* **Repository Pattern** to abstract data access.
* **Stored Procedures** to encapsulate database-specific logic.

This approach allowed our application to run on **Oracle, Microsoft SQL Server, and MySQL without modifying the core business logic**.

While modern ORMs have made things easier, sometimes you still need **raw control over database operations**, and this approach remains relevant today!

***

## Key Ideas

| Concept            | Summary                                                  |
| ------------------ | -------------------------------------------------------- |
| Factory Pattern    | Created database-specific connection objects dynamically |
| Repository Pattern | Provided a unified interface for data access             |
| Stored Procedures  | Standardized CRUD operations across databases            |
| Multi-DB Support   | Enabled switching between MSSQL, MySQL, and Oracle       |

This was how we built **multi-database support in C# back in 2006**!
