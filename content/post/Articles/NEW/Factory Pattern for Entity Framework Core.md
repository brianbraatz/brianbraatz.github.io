---
title: Factory Pattern for Entity Framework Core
description: Switching Database Engines with Ease
slug: factory-pattern-for-entity-framework-core-switching-database-engines-with-ease
date: 2022-12-05
image: post/Articles/IMAGES/factory.png
categories:
  - SQL
  - Algorithms
  - Design Patterns
  - DotNet
  - Entity Framework
  - CSharp
  - Unit Testing
tags:
  - EntityFramework
  - SQL
  - DesignPaterns
  - FactoryPattern
  - Sqlite
  - Mysql
  - Postgres
  - InMemorySQL
  - UnitTest
  - Ioc
  - CSharp
  - DotNet
draft: false
weight: 321
lastmod: 2025-02-09T22:15:26.914Z
---
# Factory Pattern for Entity Framework Core: Switching Database Engines with Ease

Ever feel like your **DatabaseContext** is acting like that clingy friend who refuses to let you try new things?

You know the one – tied down to a single database engine like SQLite, MySQL, or PostgreSQL, making your life a pain when it comes to Inversion of Control (IoC) and unit testing.

In this article we’re breaking those chains with the **Factory Pattern**!

> **Note:** If you’re wondering what the Factory Pattern is, check out the [Wikipedia article on Factory Method Pattern](https://en.wikipedia.org/wiki/Factory_method_pattern) and [Inversion of Control](https://en.wikipedia.org/wiki/Inversion_of_control). Trust me, they’re as cool as they sound.

## The Problem with a Tightly Coupled DatabaseContext

In many C# applications using [Entity Framework Core](https://en.wikipedia.org/wiki/Entity_Framework), the `DbContext` is tightly coupled to the underlying database engine. This means your context is built specifically for, say, SQLite.

When it comes time to use an IoC container to manage dependencies or to swap out the database engine for unit testing, you’re stuck like gum under a desk. Not ideal, right?

The solution? **Factory Pattern!**\
By putting an interface on your DatabaseContext and using a factory to create instances, you can easily switch engines based on your configuration (or even for testing).

It’s like having a magical vending machine that gives you the perfect database context on demand!

## Why the Factory Pattern Rocks

* **Flexibility:** Switch between SQLite, MySQL, PostgreSQL, or even an in-memory provider without rewriting your code.
* **IoC-Friendly:** With an interface in place, you can register your contexts with your favorite IoC container.
* **Unit Testing:** Testing becomes a breeze when you can swap in a fast, in-memory database. (Though, if you like peeking at your data later, SQLite gives you a file to inspect.)
* **Maintainability:** Cleaner code and separation of concerns make for a happy dev team (and fewer coffee spills).

## The Code – Let’s Get Our Hands Dirty

Below is an Entity Framework Core implementation that uses the Factory Pattern to create different `DbContext` instances based on app settings.

### 1. **appsettings.json**

Our configuration file now includes a section for `DBContextFactory`, with settings for SQLite, MySQL, PostgreSQL, and an in-memory provider for unit tests:

```json
{
  "DBContextFactory": {
    "ActiveFactory": "sqlite",
    "factories": [
      {
        "FactoryName": "sqlite",
        "FactoryConfigString": "Data Source=people_addresses.db"
      },
      {
        "FactoryName": "mysql",
        "FactoryConfigString": "Server=localhost;Database=mydb;User=root;Password=mypassword;"
      },
      {
        "FactoryName": "postgres",
        "FactoryConfigString": "Host=localhost;Database=mydb;Username=postgres;Password=mypassword"
      },
      {
        "FactoryName": "inmemory",
        "FactoryConfigString": "InMemoryTestDb"
      }
    ]
  }
}
```

### 2. **FactorySetting Class**

A simple POCO to hold our factory settings:

```csharp
public class FactorySetting
{
    public string FactoryName { get; set; }
    public string FactoryConfigString { get; set; }
}
```

### 3. **IDBContext and IDBContextFactory Interfaces**

We wrap our `DbContext` in an interface (`IDBContext`) to decouple it from specific implementations. The factory interface (`IDBContextFactory`) defines methods for creating contexts:

```csharp
using Microsoft.EntityFrameworkCore;

public interface IDBContext
{
    DbSet<Person> People { get; set; }
    DbSet<Address> Addresses { get; set; }
    void EnsureDatabaseCreated();
}

public interface IDBContextFactory
{
    IDBContext CreateContext(string factoryName);
    IDBContext CreateContext();
}
```

### 4. **Implementing the DBContextFactory**

The factory reads configuration, looks up the desired factory setting, and creates the proper context instance using a switch-case:

```csharp
using Microsoft.Extensions.Configuration;
using System;
using System.Collections.Generic;
using System.Linq;

public class DBContextFactory : IDBContextFactory
{
    private readonly IConfiguration _configuration;
    public string FactoryName { get; private set; }

    public DBContextFactory(IConfiguration configuration)
    {
        _configuration = configuration;
        FactoryName = "";
    }

    public IDBContext CreateContext()
    {
        var activeFactory = _configuration["DBContextFactory:ActiveFactory"];
        return CreateContext(activeFactory);
    }

    public IDBContext CreateContext(string factoryName)
    {
        FactoryName = factoryName;
        var factories = _configuration.GetSection("DBContextFactory:factories").Get<List<FactorySetting>>();
        var factorySetting = factories?.FirstOrDefault(f => f.FactoryName.Equals(factoryName, StringComparison.OrdinalIgnoreCase));

        if (factorySetting == null)
        {
            throw new Exception($"Factory setting for '{factoryName}' not found.");
        }

        switch (factoryName.ToLower())
        {
            case "sqlite":
                return new SqliteDbContext(factorySetting.FactoryConfigString);
            case "mysql":
                return new MySqlDbContext(factorySetting.FactoryConfigString);
            case "postgres":
                return new PostgresDbContext(factorySetting.FactoryConfigString);
            case "inmemory":
                return new InMemoryDbContext(factorySetting.FactoryConfigString);
            default:
                throw new Exception($"Unsupported factory: {factoryName}");
        }
    }
}
```

### 5. **Database Context Implementations**

Each context implements `IDBContext`. Here’s a quick look at the implementations:

#### **SQLite Context**

```csharp
using Microsoft.EntityFrameworkCore;

public class SqliteDbContext : DbContext, IDBContext
{
    private readonly string _connectionString;
    
    public DbSet<Person> People { get; set; }
    public DbSet<Address> Addresses { get; set; }

    public SqliteDbContext(string connectionString)
    {
        _connectionString = connectionString;
    }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
        optionsBuilder.UseSqlite(_connectionString);
    }

    public void EnsureDatabaseCreated()
    {
        Database.EnsureCreated();
    }
}
```

#### **MySQL Context**

```csharp
using Microsoft.EntityFrameworkCore;

public class MySqlDbContext : DbContext, IDBContext
{
    private readonly string _connectionString;
    
    public DbSet<Person> People { get; set; }
    public DbSet<Address> Addresses { get; set; }

    public MySqlDbContext(string connectionString)
    {
        _connectionString = connectionString;
    }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
        optionsBuilder.UseMySql(_connectionString, ServerVersion.AutoDetect(_connectionString));
    }

    public void EnsureDatabaseCreated()
    {
        Database.EnsureCreated();
    }
}
```

#### **PostgreSQL Context**

```csharp
using Microsoft.EntityFrameworkCore;

public class PostgresDbContext : DbContext, IDBContext
{
    private readonly string _connectionString;
    
    public DbSet<Person> People { get; set; }
    public DbSet<Address> Addresses { get; set; }

    public PostgresDbContext(string connectionString)
    {
        _connectionString = connectionString;
    }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
        optionsBuilder.UseNpgsql(_connectionString);
    }

    public void EnsureDatabaseCreated()
    {
        Database.EnsureCreated();
    }
}
```

#### **In-Memory Context (for Unit Testing)**

```csharp
using Microsoft.EntityFrameworkCore;

public class InMemoryDbContext : DbContext, IDBContext
{
    private readonly string _databaseName;
    
    public DbSet<Person> People { get; set; }
    public DbSet<Address> Addresses { get; set; }

    public InMemoryDbContext(string databaseName)
    {
         _databaseName = databaseName;
    }

    // Constructor to allow passing options for unit testing
    public InMemoryDbContext(string databaseName, DbContextOptions<InMemoryDbContext> options) 
        : base(options)
    {
         _databaseName = databaseName;
    }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
         if (!optionsBuilder.IsConfigured)
         {
             optionsBuilder.UseInMemoryDatabase(_databaseName);
         }
    }

    public void EnsureDatabaseCreated()
    {
         Database.EnsureCreated();
    }
}
```

### 6. **Using the Factory in Your Application**

In your `Program.cs`, you can now easily create a context based on your app settings.

No more hardcoding! Check it out:

```csharp
using System;
using System.Linq;
using Microsoft.Extensions.Configuration;

class Program
{
    static void Main()
    {
        // Load configuration
        var configuration = new ConfigurationBuilder()
            .SetBasePath(AppDomain.CurrentDomain.BaseDirectory)
            .AddJsonFile("appsettings.json", optional: false, reloadOnChange: true)
            .Build();

        // Create DB context factory
        IDBContextFactory dbContextFactory = new DBContextFactory(configuration);

        // Create a database context based on the ActiveFactory setting
        using (IDBContext context = dbContextFactory.CreateContext())
        {
            // Ensure database is created
            context.EnsureDatabaseCreated();

            // Add sample data if empty
            if (!context.Addresses.Any())
            {
                var address1 = new Address { Street = "123 Main St", City = "New York" };
                var address2 = new Address { Street = "456 Elm St", City = "Los Angeles" };

                var person1 = new Person { Name = "Alice", Address = address1 };
                var person2 = new Person { Name = "Bob", Address = address1 };
                var person3 = new Person { Name = "Charlie", Address = address2 };

                context.Addresses.AddRange(address1, address2);
                context.People.AddRange(person1, person2, person3);
                context.SaveChanges();
            }

            // Query and display results
            var peopleWithAddresses = context.People.Include(p => p.Address).ToList();

            Console.WriteLine("People and their addresses:");
            foreach (var person in peopleWithAddresses)
            {
                Console.WriteLine($"{person.Name} lives at {person.Address.Street}, {person.Address.City}");
            }
        }
    }
}
```

### 7. **Unit Testing with the In-Memory Provider**

For unit tests, we can use the in-memory database provider. It’s blazing fast (and leaves no mess behind). Here’s a sample unit test using [xUnit](https://xunit.net/):

```csharp
using System.Linq;
using Microsoft.EntityFrameworkCore;
using Xunit;

public class DbContextTests
{
    [Fact]
    public void Test_InMemoryDbContext_InsertAndQuery()
    {
        // Configure options for the in-memory context
        var options = new DbContextOptionsBuilder<InMemoryDbContext>()
            .UseInMemoryDatabase(databaseName: "TestDb")
            .Options;

        // Insert seed data into the database using one instance of the context
        using (var context = new InMemoryDbContext("TestDb", options))
        {
            var address = new Address { Street = "Test St", City = "Test City" };
            var person = new Person { Name = "Test Person", Address = address };
            context.Addresses.Add(address);
            context.People.Add(person);
            context.SaveChanges();
        }

        // Use a separate instance of the context to verify correct data was saved
        using (var context = new InMemoryDbContext("TestDb", options))
        {
            var personCount = context.People.Count();
            Assert.Equal(1, personCount);
        }
    }
}
```

If unit tests were a superhero, in-memory databases would be its cape – light, fast, and always ready to save the day without leaving a paper trail (or a database file).

But, if you love inspecting your data with a GUI, SQLite still gives you that file to peek at.

Point one of the viewers below to your db file.

### 8. **SQLite File Viewer Tools**

If you’re using SQLite and want to inspect the generated `.db` file, here are some awesome tools:

* [DB Browser for SQLite](https://sqlitebrowser.org/)
* [SQLiteStudio](https://sqlitestudio.pl/)
* [SQLite Expert](https://www.sqliteexpert.com/)
* [SQLiteSpy](http://www.yunqa.de/delphi/doku.php/products:sqlitespy:start)

## Key Ideas Table

| **Key Idea**           | **Description**                                                                   |
| ---------------------- | --------------------------------------------------------------------------------- |
| Factory Pattern        | A design pattern for creating objects without exposing the creation logic.        |
| Inversion of Control   | A principle where the control of objects is transferred to a container/framework. |
| Decoupled DbContext    | Avoids tying your context to a specific database engine for better flexibility.   |
| Unit Testing           | Using an in-memory database provider to run fast and isolated tests.              |
| Configurable Factories | Use app settings to choose which database engine to use at runtime.               |

## Reference Links

* [Entity Framework Core - Wikipedia](https://en.wikipedia.org/wiki/Entity_Framework)
* [Factory Method Pattern - Wikipedia](https://en.wikipedia.org/wiki/Factory_method_pattern)
* [Inversion of Control - Wikipedia](https://en.wikipedia.org/wiki/Inversion_of_control)
* [SQLite - Wikipedia](https://en.wikipedia.org/wiki/SQLite)
* [MySQL - Wikipedia](https://en.wikipedia.org/wiki/MySQL)
* [PostgreSQL - Wikipedia](https://en.wikipedia.org/wiki/PostgreSQL)
* [In-Memory Database - Wikipedia](https://en.wikipedia.org/wiki/In-memory_database)
* [xUnit.net](https://xunit.net/)
* [Entity Framework Core Documentation](https://learn.microsoft.com/en-us/ef/core/)
