---
title: Entity Framework In a Nutshell
description: With Common Code Examples
slug: entity-framework-nutshell
date: 2016-12-03
image: post/Articles/IMAGES/entityframework.png
categories:
  - SQL
  - MySql
  - Microsoft Sql Server
  - Postgres Sql
  - CSharp
  - DotNet
  - Performance Optimization
  - Cloud
  - Entity Framework
  - ORM
tags:
  - Entity
  - Framework
  - LINQ
  - ORM
  - Database
  - SQL
  - PostgreSQL
  - MySQL
  - SQLite
  - Code
  - Examples
  - CSharp
draft: false
weight: 82
lastmod: 2025-03-05T20:51:38.162Z
---
<!-- 
# Entity Framework In Depth: 20 Code Examples, LINQ, Pros, Cons, and Alternate ORMs Explained
-->

Ah, Entity Framework.

The magical ORM (Object-Relational Mapper) that promises to save you from the soul-sucking world of raw SQL queries.

But is it really the knight in shining armor for .NET developers?

Or just another buzzword trying to ruin your day with cryptic exceptions?

## What is Entity Framework?

Entity Framework (EF) is Microsoft's ORM for .NET applications.

It helps you interact with databases using C# objects instead of raw SQL.

In simpler terms, you can think of it as the translator between your code and your database.

### Why EF?

* No more wrestling with SQL strings.
* Query databases with LINQ, which reads like English (if English were written by a developer).
* Automagically updates database schema with Migrations.

***

## Setting Up Entity Framework

Let’s start with the basics:

### 1. Install EF Core

```bash
# For SQL Server
dotnet add package Microsoft.EntityFrameworkCore.SqlServer

# For SQLite
dotnet add package Microsoft.EntityFrameworkCore.Sqlite

# For PostgreSQL
dotnet add package Npgsql.EntityFrameworkCore.PostgreSQL
```

### 2. Configure DbContext

```csharp
public class AppDbContext : DbContext
{
    public DbSet<User> Users { get; set; }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
        optionsBuilder.UseSqlite("Data Source=app.db");
    }
}

public class User
{
    public int Id { get; set; }
    public string Name { get; set; }
    public int Age { get; set; }
}
```

### 3. Apply Migrations

```bash
dotnet ef migrations add InitialCreate
dotnet ef database update
```

Boom! Database created with zero SQL writing.

### 4. Add Data

```csharp
using var context = new AppDbContext();
context.Users.Add(new User { Name = "Alice", Age = 30 });
context.SaveChanges();
```

### 5. Retrieve All Records

```csharp
var users = context.Users.ToList();
users.ForEach(u => Console.WriteLine($"{u.Id}: {u.Name} ({u.Age})"));
```

### 6. Retrieve with LINQ Where Clause

```csharp
var adults = context.Users.Where(u => u.Age >= 18).ToList();
```

### 7. Update Data

```csharp
var user = context.Users.First();
user.Name = "Bob";
context.SaveChanges();
```

### 8. Delete Data

```csharp
var user = context.Users.First(u => u.Name == "Bob");
context.Users.Remove(user);
context.SaveChanges();
```

### 9. Ordering Data

```csharp
var sortedUsers = context.Users.OrderBy(u => u.Name).ToList();
```

### 10. Count Records

```csharp
var userCount = context.Users.Count();
```

### 11. Projecting Data with Select

```csharp
var userNames = context.Users.Select(u => u.Name).ToList();
```

### 12. Aggregating with Sum

```csharp
var totalAge = context.Users.Sum(u => u.Age);
```

### 13. Checking for Existence

```csharp
bool exists = context.Users.Any(u => u.Name == "Alice");
```

### 14. Grouping Data

```csharp
var ageGroups = context.Users.GroupBy(u => u.Age).ToList();
```

### 15. Using Include for Related Data

```csharp
var usersWithOrders = context.Users.Include(u => u.Orders).ToList();
```

### 16. Lazy Loading

```csharp
protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
{
    optionsBuilder.UseSqlite("Data Source=app.db").UseLazyLoadingProxies();
}
```

### 17. Eager Loading

```csharp
var users = context.Users.Include(u => u.Orders).ToList();
```

### 18. Explicit Loading

```csharp
var user = context.Users.First();
context.Entry(user).Collection(u => u.Orders).Load();
```

### 19. Raw SQL Queries

```csharp
var users = context.Users.FromSqlRaw("SELECT * FROM Users").ToList();
```

### 20. Transactions

```csharp
using var transaction = context.Database.BeginTransaction();
try
{
    context.Users.Add(new User { Name = "Charlie", Age = 25 });
    context.SaveChanges();
    transaction.Commit();
}
catch
{
    transaction.Rollback();
}
```

***

## LINQ: The Magic Behind EF Queries

LINQ (Language Integrated Query) lets you write queries using C#. It's like SQL but without the soul-draining syntax.

### LINQ Examples

```csharp
var youngUsers = context.Users.Where(u => u.Age < 30).ToList();
var orderedUsers = context.Users.OrderBy(u => u.Name).ToList();
var groupedUsers = context.Users.GroupBy(u => u.Age).Select(g => new { Age = g.Key, Count = g.Count() }).ToList();
```

### LINQ Gotchas

* Deferred Execution: LINQ queries don't run until you iterate over them.
* Performance: Complex queries can cause performance issues.

***

## Pros and Cons of Entity Framework

### Pros

* Less boilerplate code.
* Database schema management with Migrations.
* Strongly-typed queries.

### Cons

* Performance overhead compared to raw SQL.
* Complex queries may result in inefficient SQL.
* Debugging LINQ-generated SQL can be a nightmare.

***

## Alternate ORMs to Consider

1. **Dapper**: Lightweight, fast, and requires you to write SQL.
2. **NHibernate**: Mature ORM with advanced features.
3. **Linq2Db**: Great for LINQ-based database interactions.
4. **Entity Framework Core**: Yep, the shiny new version of EF.

***

## Key Ideas

* EF abstracts database interactions.
* LINQ simplifies query writing.
* Migrations manage schema changes without SQL.
* Performance tuning is crucial.

***

## References

* [Microsoft Docs](https://docs.microsoft.com/en-us/ef/)
* [Entity Framework Core](https://github.com/dotnet/efcore)
* [LINQ Tutorials](https://docs.microsoft.com/en-us/dotnet/csharp/linq/)
* [Dapper](https://github.com/DapperLib/Dapper)
* [NHibernate](https://nhibernate.info/)
* [Awesome .NET ORM list](https://github.com/quozd/awesome-dotnet#orm)
