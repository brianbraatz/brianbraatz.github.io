---
title: How to Use Database Migrations (Flyway, Liquibase, Entity Framework Migrations, and Custom Scripts) to Evolve Schemas Smoothly
description: How to Use Database Migrations (Flyway, Liquibase, Entity Framework Migrations, and Custom Scripts) to Evolve Schemas Smoothly
slug: how-to-use-database-migrations-flyway-liquibase-entity-framework-migrations-and-custom-scripts-to-evolve-schemas-smoothly
date: 2015-11-27
image: post/Articles/IMAGES/birdsmigrating.png
categories:
  - Database
  - Migrations
  - Flyway
  - Liquibase
  - Entity Framework
  - SQL
  - Schema Evolution
  - DevOps
tags:
  - Database
  - Migrations
  - Flyway
  - Liquibase
  - Entity Framework
  - SQL
  - Schema Evolution
  - DevOps
draft: false
weight: 368
lastmod: 2025-02-27T18:24:55.742Z
---
# How to Use Database Migrations (Flyway, Liquibase, Entity Framework Migrations, and Custom Scripts) to Evolve Schemas Smoothly

### Introduction

Ah, database migrations. The art of changing your database schema without breaking everything like a Jenga tower on a wobbly table.

If you've ever been tasked with modifying a database structure in a live application, you know the fear that comes with it. But fear not! Tools like **Flyway**, **Liquibase**, **Entity Framework Migrations**, and even custom scripts can help us evolve database schemas smoothly.

In this article, we'll look at how each of these tools works, with some code examples to make you feel like a migration wizard.

***

## 1. Flyway - The Lightweight Migration Champ

Flyway is a simple and effective migration tool that works with raw SQL scripts. It's great when you want a no-nonsense approach to database migrations.

### Installation

To install Flyway, grab it via SDKMAN or manually download it:

```sh
# Install via SDKMAN
sdk install flyway

# Or download manually from Flyway's website
```

### Creating a Migration

Flyway expects SQL migration files in a `db/migration` folder:

```sql
-- V1__Create_Users_Table.sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL
);
```

Then run:

```sh
flyway migrate
```

And boom! Your schema is updated.

***

## 2. Liquibase - The XML/JSON/YAML SQL Guru

Liquibase is another powerful migration tool that allows you to define migrations in XML, JSON, YAML, or raw SQL.

### Installation

Grab Liquibase with:

```sh
brew install liquibase  # On Mac
choco install liquibase  # On Windows
```

### Writing a Migration

Here's a sample **Liquibase changelog file** using YAML:

```yaml
databaseChangeLog:
  - changeSet:
      id: 1
      author: developer
      changes:
        - createTable:
            tableName: users
            columns:
              - column:
                  name: id
                  type: int
                  autoIncrement: true
                  constraints:
                    primaryKey: true
              - column:
                  name: name
                  type: varchar(100)
              - column:
                  name: email
                  type: varchar(100)
                  constraints:
                    unique: true
```

Apply it with:

```sh
liquibase update
```

Nice and structured!

***

## 3. Entity Framework Migrations - The .NET Dev's Best Friend

If you're working in .NET, **Entity Framework Migrations** is a fantastic way to handle schema changes.

### Adding a Migration

```sh
dotnet ef migrations add CreateUsersTable
dotnet ef database update
```

### Example Migration

EF generates a migration file in C#:

```csharp
public partial class CreateUsersTable : Migration
{
    protected override void Up(MigrationBuilder migrationBuilder)
    {
        migrationBuilder.CreateTable(
            name: "Users",
            columns: table => new
            {
                Id = table.Column<int>(nullable: false)
                    .Annotation("SqlServer:Identity", "1, 1"),
                Name = table.Column<string>(maxLength: 100, nullable: false),
                Email = table.Column<string>(maxLength: 100, nullable: false, unique: true)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_Users", x => x.Id);
            }
        );
    }

    protected override void Down(MigrationBuilder migrationBuilder)
    {
        migrationBuilder.DropTable("Users");
    }
}
```

C# magic at its finest!

***

## 4. Custom Scripts - The DIY Migration Approach

If you like living on the edge (or your project has specific needs), writing custom migration scripts is always an option.

### Example Bash Script

```sh
#!/bin/bash
psql -d mydatabase -U myuser -c "CREATE TABLE users (id SERIAL PRIMARY KEY, name VARCHAR(100), email VARCHAR(100) UNIQUE);"
```

Run it:

```sh
bash migrate.sh
```

You now have full control—just don’t mess it up!

***

## Conclusion

Migrations are essential for evolving database schemas without chaos. Depending on your stack, you can use:

* **Flyway** for simple SQL-based migrations
* **Liquibase** for structured migrations in XML, JSON, YAML, or SQL
* **Entity Framework Migrations** for C#-based schema evolution
* **Custom scripts** if you like to roll up your sleeves

Choose the right tool for your project, and may your schemas evolve smoothly without breaking production! 🚀

***

## Key Ideas

| Concept                     | Description                                                |
| --------------------------- | ---------------------------------------------------------- |
| Flyway                      | Simple SQL-based migration tool                            |
| Liquibase                   | Supports structured migrations in XML, JSON, YAML, and SQL |
| Entity Framework Migrations | .NET-based migration system                                |
| Custom Scripts              | DIY approach for schema evolution                          |
| Migration Tools             | Help apply schema changes without downtime                 |

***

## References

1. [Flyway Documentation](https://flywaydb.org/documentation/)
2. [Liquibase Documentation](https://www.liquibase.org/documentation/)
3. [Entity Framework Migrations](https://docs.microsoft.com/en-us/ef/core/managing-schemas/migrations)
4. [PostgreSQL PSQL Guide](https://www.postgresql.org/docs/current/app-psql.html)
