---
title: ReDoc in a Nutshell
description: ReDoc in a Nutshell
slug: redoc-nutshell
date: 2017-06-14
image: post/Articles/IMAGES/openapi.png
categories:
  - API Documentation
  - OpenAPI
  - Web Development
tags:
  - Redoc
  - OpenAPI
  - Swagger
  - API
  - Docs
draft: false
weight: 537
lastmod: 2025-02-26T11:04:34.860Z
---
# ReDoc in a Nutshell

API documentation is often an afterthought, shoved into a dusty corner of a codebase, waiting for some brave soul to decipher cryptic JSON structures.

Enter **ReDoc**,  it makes API docs actually look... nice.

![](/post/Articles/26/redocui.png)

from: (Good Article!)\
https://nordicapis.com/review-of-redoc-instant-api-documentation/

## What is ReDoc?

[ReDoc](https://redocly.com/redoc/) is an open-source tool for generating interactive, human-friendly API documentation from OpenAPI (formerly known as Swagger) specifications. Unlike some other solutions that make your API docs look like a chaotic explosion of JSON, ReDoc presents everything in a clean, three-column layout that’s easy to read and navigate.

### Key Features

* **Beautiful, Minimalist UI** – Your API docs will look professional without extra effort.
* **Three-Column Layout** – Keeps navigation, documentation, and code samples in view.
* **Responsive and Mobile-Friendly** – Looks good on any device.
* **Fully Customizable** – Change styles, branding, and themes to match your project.
* **Supports OpenAPI 2.0 & 3.0** – Works with the latest API specifications.
* **Markdown Support** – Easily add descriptions and guides in plain text.
* **Built-in Code Samples** – Auto-generates request examples for different languages.
* **Lazy Loading** – Improves performance, even for large API specs.

## How Does ReDoc Work?

ReDoc is designed to be lightweight and easy to integrate into any project. You provide an OpenAPI spec, and it does the rest.

### Quick Setup

To get started, you can either use the **ReDoc standalone HTML file** or integrate it into your web app.

#### Option 1: Embed in an HTML File

```html
<!DOCTYPE html>
<html>
  <head>
    <title>My API Docs</title>
    <script src="https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js"></script>
  </head>
  <body>
    <redoc spec-url="openapi.yaml"></redoc>
    <script>
      Redoc.init("openapi.yaml", {}, document.querySelector("redoc"));
    </script>
  </body>
</html>
```

#### Option 2: Run with Docker

For a quick setup without worrying about dependencies:

```sh
docker run -p 8080:80 -v $(pwd)/openapi.yaml:/usr/share/nginx/html/openapi.yaml redocly/redoc
```

Then visit **http://localhost:8080** in your browser.

#### Option 3: Use as an NPM Package

If you're integrating it into a JavaScript-based project:

```sh
npm install redoc
```

Then import it in your app:

```javascript
import Redoc from 'redoc';

Redoc.init('openapi.yaml', {}, document.getElementById('redoc-container'));
```

## Why Use ReDoc?

So why should you choose ReDoc over other API documentation tools?

| Feature             | ReDoc | Swagger UI   | Rapidoc |
| ------------------- | ----- | ------------ | ------- |
| Modern UI           | ✅     | ❌            | ✅       |
| Three-column layout | ✅     | ❌            | ❌       |
| OpenAPI 3.0 Support | ✅     | ✅            | ✅       |
| Easy Theming        | ✅     | ⚠️ (Limited) | ✅       |
| Lightweight         | ✅     | ❌            | ✅       |

ReDoc is great if you want **clean, readable documentation** without unnecessary clutter.

## Customizing ReDoc

ReDoc allows you to tweak its appearance and behavior using a JSON config file or query parameters.

### Example Configuration

```json
{
  "theme": {
    "colors": {
      "primary": { "main": "#4CAF50" }
    },
    "typography": {
      "fontFamily": "Arial, sans-serif",
      "headings": { "fontWeight": "bold" }
    }
  }
}
```

You can pass this configuration into ReDoc via JavaScript:

```javascript
Redoc.init("openapi.yaml", { theme: { colors: { primary: { main: "#4CAF50" } } } }, document.getElementById("redoc-container"));
```

## ReDoc vs. Swagger UI

If you're debating between **Swagger UI** and **ReDoc**, here's a quick comparison:

* **Swagger UI**: Good for trying out API requests directly in the browser.
* **ReDoc**: Better for clear, structured documentation without overwhelming users.

In many cases, **ReDoc is ideal for public-facing API documentation**, while Swagger UI works well for internal debugging.

<!--
## Conclusion

ReDoc is a **fantastic** tool for anyone dealing with API documentation. It's simple, stylish, and makes your OpenAPI specs actually readable. Whether you're a solo developer or part of a large team, integrating ReDoc can improve how you present your API to the world.

If you're tired of messy, unreadable API docs, give ReDoc a shot—you won’t regret it.
-->

***

## 🔑 Key Takeaways

| Summary            | Details                                            |
| ------------------ | -------------------------------------------------- |
| **What is ReDoc?** | A clean, modern tool for OpenAPI documentation.    |
| **Why use it?**    | Sleek UI, three-column layout, fully customizable. |
| **Setup options?** | Standalone HTML, Docker, NPM integration.          |
| **Customization?** | JSON theme support for branding.                   |
| **Best use case?** | Public-facing API docs with a clear structure.     |

```
```
