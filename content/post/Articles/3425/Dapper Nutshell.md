---
title: Dapper in a Nutshell
description: .Net ORM- Entity Framrwork Alternative
slug: dapper-in-a-nutshell
date: 2016-08-14
image: post/Articles/IMAGES/dapper.png
categories:
  - Dapper
  - ORM
  - Entity Framework
  - C#
  - Performance
  - CSHarp
  - SQL
tags:
  - Dapper
  - Orm
  - Entity
  - framework
  - C#
  - Performance
draft: false
weight: 523
categories_ref:
  - Dapper
  - ORM
  - Entity Framework
  - C#
  - Performance
  - CSHarp
  - SQL
slug_calculated: https://brianbraatz.github.io/p/dapper-in-a-nutshell
lastmod: 2025-03-14T16:40:13.812Z
---
# Dapper in a Nutshell

## What the Heck is Dapper, and Why Should You Care?

So, you’ve been using Entity Framework (EF) and loving its sweet, sweet LINQ queries. But one day, your database starts crying because EF is making a bazillion queries and your app is moving slower than a snail in molasses.

Enter **Dapper**—the lightweight, super-fast micro ORM (Object-Relational Mapper) that lets you talk to your database with the speed of a caffeinated squirrel.

## A Brief History of Dapper (With Minimal Boredom)

Back in 2011, the team at **Stack Overflow** (yes, the very people who save our butts daily) needed something faster than Entity Framework but still easy to use. They wanted **raw SQL speed** without the **boilerplate nonsense** of ADO.NET.

So, they built Dapper.

Dapper basically wraps ADO.NET but makes it ridiculously easy to execute queries and map results to C# objects. Think of it as ADO.NET with a personal assistant who takes care of all the tedious work.

## Why Use Dapper? (Or: Why You Should Break Up With EF—Sometimes)

* **Speed Demon**: Dapper is almost as fast as raw ADO.NET because it avoids all the fancy ORM magic.
* **Lightweight**: No baggage, no unnecessary abstraction—just SQL with some mapping awesomeness.
* **SQL-Friendly**: You write SQL directly, which means no weird query translation problems.
* **Control**: You have full control over what gets executed, how it performs, and how data is shaped.
* **Flexible**: Works with any database (SQL Server, PostgreSQL, MySQL, SQLite—you name it!).

## Dapper vs Entity Framework: The Ultimate Showdown

| Feature          | Dapper                    | Entity Framework                        |
| ---------------- | ------------------------- | --------------------------------------- |
| Performance      | Insanely fast 🚀          | Can be slow on large queries 🐢         |
| Ease of Use      | Simple and direct SQL     | LINQ queries, auto-mapping              |
| Control          | Full control over SQL     | Abstraction layer hides SQL             |
| Learning Curve   | Very easy if you know SQL | Slightly steeper with LINQ              |
| Database Support | Works with everything     | Mostly SQL Server, others need tweaking |
| Best For         | Read-heavy apps, APIs     | Complex domain models                   |

## Dapper in Action

### 1. Install Dapper

```csharp
// Install via NuGet
Install-Package Dapper
```

### 2. Setting Up a Database Connection

```csharp
using System.Data;
using System.Data.SqlClient;

var connectionString = "YourConnectionStringHere";
using (var db = new SqlConnection(connectionString))
{
    db.Open();
    Console.WriteLine("Database connected!");
}
```

### 3. Fetching a Single Record

```csharp
var user = db.QueryFirstOrDefault<User>("SELECT * FROM Users WHERE Id = @Id", new { Id = 1 });
```

### 4. Fetching Multiple Records

```csharp
var users = db.Query<User>("SELECT * FROM Users").ToList();
```

### 5. Insert Data

```csharp
var rowsAffected = db.Execute("INSERT INTO Users (Name, Age) VALUES (@Name, @Age)",
    new { Name = "Alice", Age = 25 });
```

### 6. Update Data

```csharp
var rowsAffected = db.Execute("UPDATE Users SET Age = @Age WHERE Name = @Name",
    new { Name = "Alice", Age = 26 });
```

### 7. Delete Data

```csharp
var rowsAffected = db.Execute("DELETE FROM Users WHERE Name = @Name",
    new { Name = "Alice" });
```

### 8. Handling Transactions

```csharp
using (var transaction = db.BeginTransaction())
{
    db.Execute("INSERT INTO Orders (UserId, Amount) VALUES (@UserId, @Amount)",
        new { UserId = 1, Amount = 99.99 }, transaction);

    transaction.Commit();
}
```

### 9. Stored Procedure Calls

```csharp
var orders = db.Query<Order>("sp_GetUserOrders", new { UserId = 1 }, commandType: CommandType.StoredProcedure);
```

### 10. Mapping Complex Relationships

```csharp
var sql = "SELECT * FROM Users u INNER JOIN Orders o ON u.Id = o.UserId";
var userOrders = db.Query<User, Order, User>(sql, (user, order) =>
{
    user.Orders.Add(order);
    return user;
}, splitOn: "Id").ToList();
```

## Final Thoughts

Dapper is like that reliable, no-nonsense friend who gets things done—fast and efficiently. If you need high performance and love writing SQL, **Dapper is your best bet**.

Of course, EF still has its place, especially when dealing with **complex domain models**. But for pure speed and control, **Dapper wipes the floor with EF**.

Give it a shot, and enjoy blazing-fast database queries without the overhead!

## Key Ideas

| Concept           | Summary                                 |
| ----------------- | --------------------------------------- |
| **Dapper**        | Lightweight, fast micro ORM for C#      |
| **History**       | Created by Stack Overflow for speed     |
| **Performance**   | Almost as fast as raw ADO.NET           |
| **SQL-Friendly**  | Works directly with SQL queries         |
| **Comparison**    | Faster than EF but less automated       |
| **Best Use Case** | APIs, read-heavy apps, high performance |

## References

* [Dapper GitHub](https://github.com/DapperLib/Dapper)
* [Stack Overflow Engineering Blog](https://stackoverflow.blog/)
* [Dapper Documentation](https://dapper-tutorial.net/)
* [NuGet Package](https://www.nuget.org/packages/Dapper)
