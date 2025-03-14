---
title: OpenApi-How to Set Up Swashbuckle UI with ASP.NET Core
description: ""
slug: openapi-setup-swashbuckle-aspnetcore
date: 2016-12-03
image: post/Articles/IMAGES/openapi.png
categories:
  - ASP.NET Core
  - API Documentation
  - Swagger
  - OpenAPI
  - Web Development
tags:
  - Swashbuckle
  - Swagger
  - Core
  - OpenAPI
  - REST
  - API
draft: false
weight: 629
categories_ref:
  - ASP.NET Core
  - API Documentation
  - Swagger
  - OpenAPI
  - Web Development
lastmod: 2025-03-14T15:45:03.580Z
---
<!-- 
# Tutorial: How to Set Up Swashbuckle with ASP.NET Core

If you’re building an **ASP.NET Core** API and want to **generate interactive API documentation**, **Swashbuckle** is your best friend.  

It automatically generates **Swagger/OpenAPI documentation** for your API, so you (and your users) can **see, explore, and even test** your endpoints right from a browser.  

In this tutorial, we'll cover:
✅ **Installing Swashbuckle**  
✅ **Configuring Swagger UI**  
✅ **Customizing OpenAPI Docs**  
✅ **Adding Authentication Support**  
-->

***

![](/post/Articles/26/swashbuckleui.png)

## 🚀 Step 1: Install Swashbuckle

First, open your **ASP.NET Core API project** and install **Swashbuckle.AspNetCore** via NuGet.

### Using .NET CLI:

```sh
dotnet add package Swashbuckle.AspNetCore
```

### Using Package Manager:

```sh
Install-Package Swashbuckle.AspNetCore
```

Once installed, Swashbuckle will automatically **scan your API** and generate an OpenAPI (Swagger) specification for you.

***

## 🔧 Step 2: Configure Swashbuckle in `Program.cs`

Now, modify your **`Program.cs`** file to configure Swagger.

### Update `Program.cs`:

```csharp
using Microsoft.OpenApi.Models;

var builder = WebApplication.CreateBuilder(args);

// Add services
builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(options =>
{
    options.SwaggerDoc("v1", new OpenApiInfo
    {
        Title = "My API",
        Version = "v1",
        Description = "A simple ASP.NET Core API",
        Contact = new OpenApiContact
        {
            Name = "Your Name",
            Email = "your@email.com",
            Url = new Uri("https://yourwebsite.com")
        }
    });
});

var app = builder.Build();

// Enable Swagger
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(c =>
    {
        c.SwaggerEndpoint("/swagger/v1/swagger.json", "My API v1");
        c.RoutePrefix = string.Empty; // Swagger UI available at root URL
    });
}

app.UseAuthorization();
app.MapControllers();
app.Run();
```

***

## 📜 Step 3: Test Your Swagger UI

Run your project:

```sh
dotnet run
```

Then, open **`http://localhost:5000/swagger`** (or **`http://localhost:5001/swagger`** if using HTTPS).

You should see a **beautiful Swagger UI**, listing all your API endpoints and letting you test them.

🎉 **Congrats! Your API is now documented with Swagger!** 🎉

***

## 🎨 Step 4: Customize Your OpenAPI Docs

Swashbuckle allows you to **customize** your Swagger UI **without writing YAML or JSON**.

### Example: Adding More API Info

Modify the `SwaggerGen` options to include:

* **License**
* **Terms of Service**
* **External Docs**

```csharp
builder.Services.AddSwaggerGen(options =>
{
    options.SwaggerDoc("v1", new OpenApiInfo
    {
        Title = "Awesome API",
        Version = "v1",
        Description = "An awesome API with Swagger support!",
        TermsOfService = new Uri("https://yourwebsite.com/terms"),
        Contact = new OpenApiContact
        {
            Name = "API Support",
            Email = "support@yourwebsite.com",
            Url = new Uri("https://yourwebsite.com/contact")
        },
        License = new OpenApiLicense
        {
            Name = "MIT License",
            Url = new Uri("https://opensource.org/licenses/MIT")
        }
    });
});
```

Now, your Swagger UI will show a **license, contact info, and terms of service**.

***

## 🔑 Step 5: Add Authentication Support (JWT Bearer Token)

If your API requires **authentication**, you can add **JWT Bearer token support** in Swagger UI.

### Enable JWT Auth in `Program.cs`:

```csharp
using Microsoft.OpenApi.Models;

builder.Services.AddSwaggerGen(options =>
{
    options.AddSecurityDefinition("Bearer", new OpenApiSecurityScheme
    {
        Name = "Authorization",
        Type = SecuritySchemeType.Http,
        Scheme = "Bearer",
        BearerFormat = "JWT",
        In = ParameterLocation.Header,
        Description = "Enter 'Bearer {token}'"
    });

    options.AddSecurityRequirement(new OpenApiSecurityRequirement
    {
        {
            new OpenApiSecurityScheme
            {
                Reference = new OpenApiReference
                {
                    Type = ReferenceType.SecurityScheme,
                    Id = "Bearer"
                }
            },
            Array.Empty<string>()
        }
    });
});
```

Now, Swagger UI will display an **Authorize** button, allowing users to enter a JWT token before testing API endpoints.

***

## 🏗 Step 6: Organize Endpoints with API Groups

By default, all your endpoints appear under one big **"default"** section.\
You can **group** your API endpoints using `SwaggerDoc`.

### Define API Versions

Modify `SwaggerGen` in `Program.cs`:

```csharp
builder.Services.AddSwaggerGen(options =>
{
    options.SwaggerDoc("v1", new OpenApiInfo { Title = "My API", Version = "v1" });
    options.SwaggerDoc("v2", new OpenApiInfo { Title = "My API", Version = "v2" });
});
```

### Assign Controllers to Versions

In your controllers, **add `ApiExplorerSettings`** to specify which Swagger doc they belong to.

```csharp
[ApiController]
[Route("api/v1/users")]
[ApiExplorerSettings(GroupName = "v1")]
public class UsersV1Controller : ControllerBase
{
    [HttpGet]
    public IActionResult GetUsers()
    {
        return Ok(new { Name = "John Doe", Age = 30 });
    }
}
```

```csharp
[ApiController]
[Route("api/v2/users")]
[ApiExplorerSettings(GroupName = "v2")]
public class UsersV2Controller : ControllerBase
{
    [HttpGet]
    public IActionResult GetUsers()
    {
        return Ok(new { FullName = "John Doe", Age = 30, Email = "john@example.com" });
    }
}
```

Now, **Swagger UI will show separate versions** (`v1` and `v2`).

***

<!-- 
## ✅ Summary: Swashbuckle Features You Just Set Up

| Feature | Status |
|---------|--------|
| **Swagger UI** | ✅ Enabled |
| **API Metadata (Title, Description, Contact Info)** | ✅ Added |
| **JWT Authentication** | ✅ Configured |
| **API Versioning** | ✅ Organized |

---

## 🎯 Conclusion

Adding **Swashbuckle to your ASP.NET Core API** makes it easy to **document, explore, and test your API**.

### 🔥 With Swagger, you can:
✅ **Auto-generate OpenAPI docs**  
✅ **Provide a friendly UI for developers**  
✅ **Support authentication & API versioning**  
✅ **Help users understand your API without digging through code**  

Now go forth and **Swaggerify your API**! 🚀

---

## 🔑 Key Takeaways

| Summary        | Details |
|---------------|---------|
| **What is Swashbuckle?** | A tool that generates OpenAPI documentation for ASP.NET Core. |
| **How to install?** | `dotnet add package Swashbuckle.AspNetCore` |
| **Where to access Swagger UI?** | `http://localhost:5000/swagger` |
| **How to enable JWT authentication?** | Use `AddSecurityDefinition` in SwaggerGen. |
| **Can I support multiple API versions?** | Yes! Use `SwaggerDoc` and `ApiExplorerSettings`. |

```
-->
