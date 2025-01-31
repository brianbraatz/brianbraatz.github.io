---
title: 
description: 
slug: 
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
draft: true
lastmod: 2025-01-31T14:41:05.637Z
---
https://chatgpt.com/c/679cb212-d194-8008-97e9-c3ea51544a85

Here is a complete C# program using Entity Framework Core with SQLite. It defines a `Person` and an `Address` entity, where an address can have multiple people. The program will:

1. Set up a SQLite database using EF Core with a **Code-First** approach.
2. Seed the database with some test data (addresses and people).
3. Query the database and print the results to the console.

***

### **Install Required Packages**

Before running this code, ensure you have the following NuGet packages installed:

shell

CopyEdit

`dotnet add package Microsoft.EntityFrameworkCore.Sqlite dotnet add package Microsoft.EntityFrameworkCore.Design`

***

### **C# Code**

csharp

CopyEdit

`using System; using System.Collections.Generic; using System.Linq; using Microsoft.EntityFrameworkCore;  namespace EFCoreSqliteExample {     // Define the Person entity     public class Person     {         public int Id { get; set; }         public string Name { get; set; }         public int AddressId { get; set; }  // Foreign Key         public Address Address { get; set; }  // Navigation Property     }      // Define the Address entity     public class Address     {         public int Id { get; set; }         public string Street { get; set; }         public string City { get; set; }         public List<Person> Residents { get; set; } = new();     }      // Define the database context     public class ApplicationDbContext : DbContext     {         public DbSet<Person> People { get; set; }         public DbSet<Address> Addresses { get; set; }          protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)         {             optionsBuilder.UseSqlite("Data Source=people_addresses.db");         }          protected override void OnModelCreating(ModelBuilder modelBuilder)         {             // Configure One-to-Many relationship             modelBuilder.Entity<Person>()                 .HasOne(p => p.Address)                 .WithMany(a => a.Residents)                 .HasForeignKey(p => p.AddressId);         }     }      class Program     {         static void Main()         {             using (var context = new ApplicationDbContext())             {                 // Ensure database is created                 context.Database.EnsureDeleted(); // Delete existing DB (for testing)                 context.Database.EnsureCreated();                  // Add sample data                 if (!context.Addresses.Any())                 {                     var address1 = new Address { Street = "123 Main St", City = "New York" };                     var address2 = new Address { Street = "456 Elm St", City = "Los Angeles" };                      var person1 = new Person { Name = "Alice", Address = address1 };                     var person2 = new Person { Name = "Bob", Address = address1 };                     var person3 = new Person { Name = "Charlie", Address = address2 };                      context.Addresses.AddRange(address1, address2);                     context.People.AddRange(person1, person2, person3);                     context.SaveChanges();                 }                  // Query and display results                 var peopleWithAddresses = context.People                     .Include(p => p.Address)                     .ToList();                  Console.WriteLine("People and their addresses:");                 foreach (var person in peopleWithAddresses)                 {                     Console.WriteLine($"{person.Name} lives at {person.Address.Street}, {person.Address.City}");                 }             }         }     } }`

***

### **How It Works**

1. **Entities & Relationships:**

   * `Person` has a `Name` and a foreign key `AddressId` linking to `Address`.
   * `Address` can have multiple `Person` records via the `Residents` collection.
2. **Database Context (`ApplicationDbContext`)**

   * Uses **SQLite** (`UseSqlite`).
   * Defines **one-to-many** relationship between `Address` and `Person`.
3. **Data Initialization & Queries**

   * **Creates & seeds** the database with sample addresses and people.
   * Queries all people and their associated addresses using `Include(p => p.Address)`.
4. **Console Output (Example)**

   less

   CopyEdit

   `People and their addresses: Alice lives at 123 Main St, New York Bob lives at 123 Main St, New York Charlie lives at 456 Elm St, Los Angeles`

***

### **Run the Code**

* Compile and run using **.NET 6+**:

  shell

  CopyEdit

  `dotnet run`

* A SQLite database file (`people_addresses.db`) will be created in the project directory.

Would you like additional features, such as filtering or an interactive console menu?

4o

##### You said:

Add to the code app settings support

###### ChatGPT said:

ChatGPT

To add **app settings support**, we will:

1. Use **`appsettings.json`** to store the database connection string.
2. Use **`Microsoft.Extensions.Configuration`** to load settings dynamically.

***

### **Install Required Packages**

Run the following commands to install necessary dependencies:

shell

CopyEdit

`dotnet add package Microsoft.Extensions.Configuration dotnet add package Microsoft.Extensions.Configuration.Json dotnet add package Microsoft.Extensions.Configuration.Binder`

***

### **Step 1: Create `appsettings.json`**

Create a new file named `appsettings.json` in the project root and add the following content:

json

CopyEdit

`{   "ConnectionStrings": {     "DefaultConnection": "Data Source=people_addresses.db"   } }`

***

### **Step 2: Modify `ApplicationDbContext` to Use Configuration**

Modify `ApplicationDbContext` to read the **connection string** from `appsettings.json`:

csharp

CopyEdit

`using Microsoft.EntityFrameworkCore; using Microsoft.Extensions.Configuration; using System.IO;  public class ApplicationDbContext : DbContext {     public DbSet<Person> People { get; set; }     public DbSet<Address> Addresses { get; set; }      protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)     {         var configuration = new ConfigurationBuilder()             .SetBasePath(Directory.GetCurrentDirectory())             .AddJsonFile("appsettings.json", optional: false, reloadOnChange: true)             .Build();          string connectionString = configuration.GetConnectionString("DefaultConnection");         optionsBuilder.UseSqlite(connectionString);     }      protected override void OnModelCreating(ModelBuilder modelBuilder)     {         modelBuilder.Entity<Person>()             .HasOne(p => p.Address)             .WithMany(a => a.Residents)             .HasForeignKey(p => p.AddressId);     } }`

***

### **Step 3: Modify `Program.cs` to Use Configuration**

Modify the `Program.cs` file to initialize configuration and use dependency injection:

csharp

CopyEdit

`using System; using System.Linq; using Microsoft.EntityFrameworkCore; using Microsoft.Extensions.Configuration; using System.IO;  namespace EFCoreSqliteExample {     class Program     {         static void Main()         {             // Load configuration             IConfiguration configuration = new ConfigurationBuilder()                 .SetBasePath(Directory.GetCurrentDirectory())                 .AddJsonFile("appsettings.json", optional: false, reloadOnChange: true)                 .Build();              using (var context = new ApplicationDbContext())             {                 // Ensure database is created                 context.Database.EnsureDeleted(); // Delete existing DB (for testing)                 context.Database.EnsureCreated();                  // Add sample data if the database is empty                 if (!context.Addresses.Any())                 {                     var address1 = new Address { Street = "123 Main St", City = "New York" };                     var address2 = new Address { Street = "456 Elm St", City = "Los Angeles" };                      var person1 = new Person { Name = "Alice", Address = address1 };                     var person2 = new Person { Name = "Bob", Address = address1 };                     var person3 = new Person { Name = "Charlie", Address = address2 };                      context.Addresses.AddRange(address1, address2);                     context.People.AddRange(person1, person2, person3);                     context.SaveChanges();                 }                  // Query and display results                 var peopleWithAddresses = context.People.Include(p => p.Address).ToList();                  Console.WriteLine("People and their addresses:");                 foreach (var person in peopleWithAddresses)                 {                     Console.WriteLine($"{person.Name} lives at {person.Address.Street}, {person.Address.City}");                 }             }         }     } }`

***

### **How This Works**

1. **App Settings Support:**

   * The database connection string is stored in `appsettings.json`.
   * `ConfigurationBuilder` reads the file dynamically.
2. **Dependency Injection Ready:**

   * This structure allows for **future improvements**, such as integrating with **ASP.NET Core DI container**.
3. **Automatic Database Creation & Seeding:**

   * The database is created and seeded **only if empty**.

***

### **Run the Program**

* Execute:

  shell

  CopyEdit

  `dotnet run`

* Output will display:

  less

  CopyEdit

  `People and their addresses: Alice lives at 123 Main St, New York Bob lives at 123 Main St, New York Charlie lives at 456 Elm St, Los Angeles`

Would you like any additional enhancements, such as **logging** or **command-line argument support**?
