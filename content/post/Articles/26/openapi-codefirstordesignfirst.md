---
title: OpenApi- Big Decisions- Code First or Design First Approach?
description: ""
slug: openapi-single-source-truth
date: 2018-04-16
image: post/Articles/IMAGES/openapi.png
categories:
  - API Design
  - OpenAPI
  - Web Development
tags:
  - OpenAPI
  - Swagger
  - API
  - Design
  - Swashbuckle
  - NSwag
  - API
  - Governance
draft: false
weight: 68
lastmod: 2025-03-03T17:16:24.744Z
---
One of the **biggest questions** you need to answer when starting an OpenAPI project is:

> **Where does the OpenAPI definition live?**

Or, as we used to say back in the mid-2000s:

> **"Where is your authoritative source?"**

This question is **critical** because **your entire API ecosystem depends on it**.

When I first started using OpenAPI (then called Swagger), I did what many developers still do today:

1. **Set up Swashbuckle** in an ASP.NET Core project.
2. Let it **automatically generate `swagger.json`** based on controllers.
3. Use **NSwag Studio** to generate client proxy classes for consuming the API.

For **internal APIs with a technical audience**, this is **fast, simple, and lean**.

🔹 **Change an API controller?**\
➡️ Swagger updates automatically.

🔹 **Using CI/CD scripts to generate client proxies**\
➡️ If there's a breaking API change, depending on the change something - somewhere will likely fail to build- so you know you broke something

This approach is **great for sensing change**, but is it the best **long-term strategy**?

***

## 🚨 The API Design Dilemma: Code-First vs. Design-First

When building APIs, you have two approaches:

### **1️⃣ Code-First (Generate OpenAPI from Code)**

✅ Use tools like **Swashbuckle** (C#), **FastAPI** (Python), or **SpringDoc** (Java) to generate OpenAPI from controllers.\
✅ API changes automatically reflect in OpenAPI docs.\
✅ Fast and lightweight for internal teams.\
❌ API design is **tightly coupled** to the implementation.\
❌ No clear API review process before implementation.\
❌ Can result in **inconsistent API design** over time.

### **2️⃣ Design-First (OpenAPI Defines the Code)**

✅ Use **visual tools** like **Stoplight Studio**, **Swagger Editor**, or **Postman** to design APIs.\
✅ The OpenAPI spec becomes the **single source of truth**.\
✅ Code is generated **from OpenAPI** rather than the other way around.\
✅ Encourages **API governance, consistency, and review before implementation**.\
❌ Requires **discipline** to keep code in sync.\
❌ More upfront effort.

🔑 **Key Difference?**

* **Code-First = "Write Code, Docs Follow"**
* **Design-First = "Write API Spec, Code Follows"**

***

## 🛠 Tools for API Design-First Workflows

<!-- 
If you want to **shift left** and make OpenAPI the **source of truth**, you’ll need the right tools.
-->

| Tool                 | Purpose                               | Pros                             |
| -------------------- | ------------------------------------- | -------------------------------- |
| **Stoplight Studio** | Visual API editor                     | No YAML needed, built-in mocking |
| **Swagger Editor**   | OpenAPI editor in the browser         | Simple, fast, free               |
| **Postman**          | API design & testing                  | Great for team collaboration     |
| **Apicurio**         | API governance & lifecycle management | Ideal for large teams            |

With these tools, **API design comes first**.\
Developers then use the OpenAPI definition to generate **code stubs** instead of letting the code generate OpenAPI.

***

## 🔄 Code-First vs. Design-First: Pros & Cons

| Feature                       | Code-First (Swashbuckle, NSwag)                 | Design-First (Stoplight, Swagger Editor) |
| ----------------------------- | ----------------------------------------------- | ---------------------------------------- |
| **Speed**                     | ✅ Fast setup                                    | ⚠️ Slower initial setup                  |
| **Documentation**             | ⚠️ Auto-generated, but not always user-friendly | ✅ Structured, well-documented APIs       |
| **Consistency**               | ❌ Can be inconsistent between teams             | ✅ Encourages standardization             |
| **Collaboration**             | ❌ Harder to collaborate before implementation   | ✅ API design is reviewed before coding   |
| **Breaking Changes**          | ✅ CI/CD detects breaking changes                | ❌ Manual validation required             |
| **Long-Term Maintainability** | ⚠️ Can get messy over time                      | ✅ More scalable                          |

***

## 🏗 Code-First for Internal APIs, Design-First for Public APIs

So which approach should you use?

🔹 **For internal, tech-focused APIs?**\
➡️ Code-first is **fast** and **good enough** (Swashbuckle + NSwag).

🔹 **For public, customer-facing APIs?**\
➡️ Design-first **ensures consistency, better documentation, and API governance**.

**Hybrid Approach?**\
You can **start with Code-First** and transition to Design-First **as your API grows**.

***

## 🔥 How to Implement Design-First API Development

If you’re moving toward **Design-First**, here’s a solid workflow:

### **1️⃣ Design the API First**

Use **Stoplight Studio** or **Swagger Editor** to draft the API before coding.

### **2️⃣ Generate Code Stubs**

Use tools like **NSwag**, **OpenAPI Generator**, or **AutoRest** to generate controllers/models.

Example: Generate C# API controllers:

```sh
npx @openapitools/openapi-generator-cli generate -i openapi.yaml -g aspnetcore -o ./output
```

### **3️⃣ Keep API & Code in Sync**

* Validate API updates using **Spectral Linting**:
  ```sh
  spectral lint openapi.yaml
  ```
* Use **contract tests** (like Pact) to verify API behavior.

### **4️⃣ Automate API Documentation**

Deploy OpenAPI docs with **ReDoc** or **Swagger UI**:

```sh
docker run -p 8080:80 -v $(pwd)/openapi.yaml:/usr/share/nginx/html/openapi.yaml redocly/redoc
```

***

## 🔥 Conclusion: Pick Your Source of Truth Wisely

👉 If **code generates OpenAPI** (Code-First), your API evolves **organically** but may lack consistency.\
👉 If **OpenAPI defines the code** (Design-First), your API is **structured and governed**, but requires discipline.

For **small, internal APIs**, stick with **Swashbuckle + NSwag**.\
For **public APIs**, shift to **Design-First** with **Stoplight** or **Swagger Editor**.

At the end of the day, the **most important thing** is to **choose your authoritative source** early—because **fixing API design later is painful**. 😅

***

## 🔑 Key Takeaways

| Summary                                   | Details                                                            |
| ----------------------------------------- | ------------------------------------------------------------------ |
| **What is the "Single Source of Truth"?** | The authoritative place where your API is defined.                 |
| **Code-First Approach?**                  | API is defined by implementation, OpenAPI is auto-generated.       |
| **Design-First Approach?**                | OpenAPI is written first, then code is generated from it.          |
| **Best for Internal APIs?**               | Code-First (Swashbuckle + NSwag).                                  |
| **Best for Public APIs?**                 | Design-First (Stoplight, Swagger Editor, Postman).                 |
| **How to switch to Design-First?**        | Use OpenAPI design tools, generate stubs, and automate validation. |

```
```
