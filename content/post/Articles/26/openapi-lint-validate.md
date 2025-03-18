---
title: OpenApi How to Lint and Validate Your API
description: ""
slug: lint-validate-api
date: 2022-11-08
image: post/Articles/IMAGES/openapi.png
categories:
  - API Design
  - OpenAPI
  - Web Development
tags:
  - API
  - Validation
  - OpenAPI
  - Spectral
  - Swagger
  - API
  - Governance
draft: false
weight: 731
categories_ref:
  - API Design
  - OpenAPI
  - Web Development
slug_calculated: https://brianbraatz.github.io/p/lint-validate-api
lastmod: 2025-03-14T16:40:10.186Z
---
<!-- 
# How to Lint and Validate Your API

So, you’ve built an OpenAPI definition. **Great!**  
But is it **valid**? Is it **clean**? Is it **consistent**?  

If your OpenAPI file is a mess, it could mean **broken tools, inconsistent APIs, and sad developers**. 😢  

**Good news:** You can **lint and validate** your API definition to catch errors **before** they cause trouble.  

---
-->

## 🚀 Why Lint and Validate Your API?

Validating your API is like **spell-checking** your OpenAPI file.\
Linting is like **code style enforcement** for APIs.

✔ **Catches missing fields, broken references, and syntax errors**\
✔ **Ensures consistency across teams**\
✔ **Prevents bad API design from making it to production**\
✔ **Saves debugging time later**

Now, let’s look at the **tools** that can help.

***

## 🛠 Tools for Linting and Validating OpenAPI

| Tool                            | Purpose                          | Pros                                |
| ------------------------------- | -------------------------------- | ----------------------------------- |
| **Spectral**                    | Linting and style enforcement    | Highly customizable, CI/CD friendly |
| **Swagger Editor**              | Validates OpenAPI specs          | Quick and easy, web-based           |
| **Redocly CLI**                 | Validates OpenAPI for Redoc docs | Great for ReDoc users               |
| **OpenAPI Generator Validator** | Checks schema errors             | CLI-based validation                |
| **Stoplight Studio**            | GUI for API design and linting   | No YAML needed, team collaboration  |

Let’s **set up and use** these tools.

***

## 🔍 1. Validate OpenAPI with Swagger Editor (Quick & Easy)

If you just need a **quick syntax check**, **Swagger Editor** is the easiest way.

1️⃣ Go to **[editor.swagger.io](https://editor.swagger.io/)**\
2️⃣ Paste your OpenAPI YAML\
3️⃣ Errors will **automatically** appear in real-time

🔹 **Pros:** Free, web-based, instant feedback\
🔹 **Cons:** No custom rules, not ideal for large projects

***

## 🔥 2. Lint OpenAPI with Spectral (Best for CI/CD)

[**Spectral**](https://stoplight.io/open-source/spectral) is the **most powerful** OpenAPI linting tool.\
It enforces **best practices** and catches issues **before deployment**.

### ✅ Install Spectral:

```sh
npm install -g @stoplight/spectral
```

### ✅ Run Spectral on Your OpenAPI File:

```sh
spectral lint openapi.yaml
```

Example output:

```
openapi.yaml
  13:10  error  openapi-tags  OpenAPI object should have non-empty 'tags' array
  24:3   warning  operation-description  Operation must have a 'description' field
```

**💡 Fix these issues before shipping your API!**

***

## 🔧 3. Customize Spectral Rules (API Governance)

Spectral lets you **define custom API linting rules** in a `.spectral.yaml` file.

Example `.spectral.yaml`:

```yaml
extends: spectral:oas
rules:
  operation-description:
    description: "Every endpoint must have a description"
    severity: error
  info-contact:
    description: "APIs must include a contact email"
    severity: warning
    given: "$.info.contact"
    then:
      field: email
      function: truthy
```

Now, running `spectral lint openapi.yaml` will **enforce your rules**. 🚀

🔹 **Pros:** Highly customizable, great for teams\
🔹 **Cons:** Requires setup

***

## 🏗 4. Validate OpenAPI with Redocly CLI

If you’re using **ReDoc** for API docs, use **Redocly CLI** to ensure compatibility.

### ✅ Install Redocly:

```sh
npm install -g @redocly/cli
```

### ✅ Validate OpenAPI:

```sh
redocly lint openapi.yaml
```

This checks:\
✔ **Schema structure**\
✔ **Documentation completeness**\
✔ **Redoc-specific optimizations**

🔹 **Pros:** Ideal for ReDoc users\
🔹 **Cons:** Focused on API documentation, not API behavior

***

## 🔄 5. Automate Linting in CI/CD

Want **every API change** to be checked automatically?\
Add **Spectral** to your CI/CD pipeline.

### **GitHub Actions Example:**

Create `.github/workflows/openapi-lint.yml`:

```yaml
name: Lint OpenAPI
on: [push, pull_request]

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install Spectral
        run: npm install -g @stoplight/spectral
      - name: Lint OpenAPI
        run: spectral lint openapi.yaml
```

Now, **every pull request** will be **automatically checked** for OpenAPI errors. 🚀

***

## ❌ Common OpenAPI Mistakes (and How to Fix Them)

| Mistake                     | How to Fix                                       |
| --------------------------- | ------------------------------------------------ |
| **Missing descriptions**    | Add `description` to every endpoint & parameter  |
| **No `contact` info**       | Ensure `info.contact.email` is present           |
| **Duplicate operation IDs** | Each endpoint should have a unique `operationId` |
| **Unused components**       | Remove unused schemas from `components/schemas`  |
| **Invalid JSON references** | Check `$ref` paths are correct                   |

***

<!-- 
## 🔥 Conclusion: Linting = API Quality

If you want **clean, reliable, and future-proof APIs**, **linting and validation are essential**.

### 🏆 Recommended Setup:
✅ **Swagger Editor** – Quick syntax check  
✅ **Spectral** – Best for enforcing rules  
✅ **Redocly CLI** – If using ReDoc  
✅ **CI/CD Linting** – Automate everything  

By **validating APIs early**, you’ll **avoid broken tools, frustrated developers, and bad API experiences**. 🚀

Now go forth and **lint like a pro!** 🏆

---

## 🔑 Key Takeaways

| Summary        | Details |
|---------------|---------|
| **What is API linting?** | Checking OpenAPI specs for errors and best practices. |
| **Best quick validation tool?** | Swagger Editor (Web-based). |
| **Best full linting tool?** | Spectral (Highly customizable). |
| **Best for ReDoc users?** | Redocly CLI. |
| **How to automate linting?** | Add Spectral to CI/CD pipelines. |
| **Common OpenAPI mistakes?** | Missing descriptions, duplicate operation IDs, invalid refs. |

```
-->
