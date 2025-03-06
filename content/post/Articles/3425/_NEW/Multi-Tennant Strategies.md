---
title: Multiple Tenants Application Design Strategies
description: Ideas on having a Single Codebase with Multiple Tenants
slug: single-codebase-multiple-tenants
date: 2011-03-14
image: post/Articles/IMAGES/apartments.png
categories:
  - Multi-Tenant
  - Database
  - Software Architecture
tags:
  - Multi-Tenant
  - Database
  - Tenant-Specific Configuration
  - Codebase
  - Data Segregation
draft: false
weight: 4450
lastmod: 2025-03-06T15:11:29.625Z
---
# How to Implement Strategies to Allow a Single Codebase to Serve Multiple Tenants with Separate Databases

So, you’ve got a shiny single codebase, and now you want it to serve not just one tenant, but a whole bunch of tenants. And not just that, but you want each tenant to have their own separate database.

 <!-- Well, buckle up, because we’re diving into the deep end of multi-tenancy, database segregation, and the fine art of keeping everything running smoothly without chaos. -->

## The Dream: One Codebase, Many Tenants, Separate Databases

Alright, let’s lay it out. You want to handle multiple tenants using a single codebase, and each of those tenants needs their own database. Why? Well, it’s mostly about keeping things nice and secure. You don’t want one tenant snooping around in another tenant’s business. And sure, you could just toss everyone into one big database, but that’s just asking for trouble. Separate databases are the way to go. It’s like having different houses on the same street. Everyone has their own space, but they’re still on the same block.

## Step 1: Tenant-Specific Configuration

### Configuration, Baby!

First things first. You need to handle tenant-specific configuration. This means that each tenant has its own settings. These can include things like the database connection strings, themes, custom settings, and anything else that’s specific to that tenant’s environment.

To achieve this, you can store tenant-specific configurations in a central configuration file or a database. But a lot of times, it’s better to keep things in a key-value store like Redis or even a simple config table in your main database.

Let’s say your application needs to know the database credentials for each tenant. You could have a table like this:

| Tenant ID | Database Name | Hostname    | Username | Password |
| --------- | ------------- | ----------- | -------- | -------- |
| tenant\_1 | tenant\_1\_db | db\_host\_1 | user\_1  | pass\_1  |
| tenant\_2 | tenant\_2\_db | db\_host\_2 | user\_2  | pass\_2  |

### Dynamic Database Connections

Next up, you need to dynamically switch between tenant databases. This isn’t as scary as it sounds. You can use a connection string resolver that checks the tenant’s ID and grabs the appropriate connection details from your config.

In a web application, this can be done in the request pipeline. For example, in a C# ASP.NET Core app, you can use middleware to resolve the tenant from the incoming request (usually from a subdomain, URL parameter, or even a header) and then load the appropriate database connection for that tenant.

```csharp
public class TenantMiddleware
{
    private readonly RequestDelegate _next;

    public TenantMiddleware(RequestDelegate next)
    {
        _next = next;
    }

    public async Task InvokeAsync(HttpContext context)
    {
        var tenantId = context.Request.Headers["Tenant-ID"];
        var connectionString = GetConnectionStringForTenant(tenantId);

        // Set the database connection for the current tenant
        context.Items["DbConnectionString"] = connectionString;

        await _next(context);
    }

    private string GetConnectionStringForTenant(string tenantId)
    {
        // Fetch tenant-specific connection string
        return Configuration.GetConnectionString(tenantId);
    }
}
```

This way, every request that comes through gets the correct database connection, and you’re off to the races!

## Step 2: Database Creation and Management

### Automated Database Creation

Now that we’ve got tenant-specific configurations handled, let’s talk about actually creating the databases. Ideally, you want to automate the process of database creation for new tenants. You don’t want to manually create a new database every time a tenant comes onboard. Ain’t nobody got time for that.

You can use migrations or scripts to automate this process. When a new tenant signs up, the system can automatically create a new database and apply any necessary migrations.

In SQL Server, this could look like:

```sql
CREATE DATABASE tenant_3_db;
```

Then, once the database is created, you can apply migrations using Entity Framework or any other ORM you’re using.

```bash
dotnet ef migrations add InitialCreate --context Tenant3DbContext
dotnet ef database update --context Tenant3DbContext
```

### The Good, The Bad, and The Performance

However, there are some performance implications to consider when managing multiple databases. If you’ve got a ton of tenants, having a separate database for each can lead to overhead when it comes to database connections and maintenance.

To balance this, you might want to periodically prune old databases, automate backups, and ensure that your application scales properly. Database connection pooling can also help reduce overhead by reusing database connections across multiple requests.

## Step 3: Secure Data Segregation

Ah, security. The thing that makes or breaks your multi-tenant app. You want to make sure that each tenant’s data is completely segregated from everyone else’s. No peeking over the fence, please!

One way to achieve this is by using row-level security (RLS) or schemas to logically separate data within a single database. But since we’re going for separate databases here, the job gets a little easier.

Just make sure that each tenant’s database is properly isolated from the others. This means:

1. **No shared credentials**: Each tenant has their own credentials for accessing their database.
2. **Database-level permissions**: Only the application has access to the databases, and you restrict access to other tenants’ databases.
3. **No cross-tenant access**: Ensure that each request is scoped to the correct tenant.

You can use a system like the principle of least privilege to enforce these security policies. This means granting only the necessary permissions to the application and database users.

## Step 4: Scaling the Multi-Tenant Architecture

So, let’s say your application grows and you’re now managing hundreds or even thousands of tenants. You need to scale your infrastructure.

The good news is, with separate databases, scaling becomes a bit easier. You can spin up new instances of your database server to handle more tenants, or even move tenants to different servers if one starts getting too hot.

The trick is to keep track of all those databases. This is where a service discovery system or a central configuration manager comes in handy. By keeping track of where each tenant’s database is, you’ll never lose track of where the good stuff is stored.

## Final Thoughts

Multi-tenancy with separate databases can sound complicated, but once you break it down into steps, it becomes much more manageable. Focus on:

1. **Tenant-specific configuration**: Ensure each tenant has its own config.
2. **Database management**: Automate database creation and migrations.
3. **Data segregation**: Keep that data nice and secure.
4. **Scaling**: Plan for growth and infrastructure changes.

With these strategies in place, you’ll have a robust multi-tenant application that can handle multiple clients, secure their data, and scale as needed.

***

## Key Ideas

| Key Idea                        | Description                               |
| ------------------------------- | ----------------------------------------- |
| Tenant-Specific Configuration   | Each tenant has its own settings.         |
| Dynamic Database Connections    | Switch databases dynamically.             |
| Automated Database Creation     | Automate database provisioning.           |
| Secure Data Segregation         | Keep data safe and isolated.              |
| Scaling the Multi-Tenant System | Plan for growth with a scalable solution. |

***

## References

* [Multi-Tenant Database Strategies](https://www.dbta.com)
* [Entity Framework Migrations](https://learn.microsoft.com/en-us/ef/core/managing-schemas/migrations)
* [ASP.NET Core Middleware](https://docs.microsoft.com/en-us/aspnet/core/middleware/)
* [Scaling Multi-Tenant Architectures](https://www.scalablepath.com)

```
```
