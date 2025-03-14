---
title: OpenApi-Stoplight in a Nutshell
description: ""
slug: stoplight-openapi-docs
date: 2019-05-07
image: post/Articles/IMAGES/openapi.png
categories:
  - API Documentation
  - OpenAPI
  - Web Development
tags:
  - Stoplight
  - OpenAPI
  - API
  - Docs
  - Swagger
  - API
  - Design
draft: false
weight: 468
categories_ref:
  - API Documentation
  - OpenAPI
  - Web Development
lastmod: 2025-03-14T15:45:03.673Z
---
<!-- 
# Using Stoplight for OpenAPI Documentation

So, you've tried manually updating your OpenAPI docs, and you’ve realized... it's **painful**. YAML files, broken references, and endless PR reviews—oh my! 

Wouldn’t it be great if there were a **better way** to manage OpenAPI documentation?

Good news: **Stoplight** exists. 🎉
-->

![](/post/Articles/26/stoplightui.png)

## What is Stoplight?

[Stoplight](https://stoplight.io/) is an API design and documentation platform.

Instead of fighting with raw JSON or YAML, Stoplight gives you a visual editor.

And it has automated linting, and collaboration tools.

***

## 🚀 Why Use Stoplight?

* **Visual OpenAPI Editor** – No more writing YAML by hand.
* **Live API Mocking** – Test API endpoints **before** coding them.
* **Collaboration-Friendly** – Invite teammates, leave comments, and version-control everything.
* **Automatic API Documentation** – Generates beautiful docs like Redoc or Swagger UI.
* **Linting & Validation** – Catch errors in your OpenAPI spec **before** they break production.

***

## 🔧 Setting Up Stoplight for OpenAPI Docs

### 1. **Install Stoplight Studio (Optional)**

While you can use Stoplight in the browser, some people prefer the **desktop app**.

To install:

* **Mac**: `brew install --cask stoplight-studio`
* **Windows**: Download from [Stoplight.io](https://stoplight.io/studio)
* **Linux**: Use the AppImage from their website.

***

### 2. **Create or Import an OpenAPI Document**

Once inside Stoplight Studio, you can:\
✅ **Create a new OpenAPI file**\
✅ **Import an existing `openapi.yaml`**

Just click **“Import”**, select your existing OpenAPI file, and **boom**—a beautiful editor appears.

***

### 3. **Use the Visual API Editor**

Stoplight **eliminates the need for YAML gymnastics** by letting you **edit OpenAPI visually**.

#### Example: Adding an API Endpoint in Stoplight

1. **Click "Add Endpoint"** 🆕
2. **Define a method** (GET, POST, PUT, DELETE)
3. **Set the path** (`/users/{id}`)
4. **Describe the request & response**
   * Add query parameters (`?name=JohnDoe`)
   * Define request bodies (`application/json`)
   * Set response types (`200 OK`, `404 Not Found`)
5. **Preview the OpenAPI YAML** (Stoplight writes it for you!)

No more missing commas, broken indentation, or YAML-induced headaches. 😎

***

### 4. **Generate API Docs Automatically**

Once your OpenAPI spec is ready, Stoplight **automatically** generates a nice API documentation site.

#### 🛠️ Deployment Options:

* **Hosted by Stoplight** – Free for small projects, paid for enterprise.
* **Self-Hosted** – Export the docs as static files and deploy them anywhere (Netlify, GitHub Pages, etc.).
* **Embed in Your App** – Use an `<iframe>` to include API docs in your existing site.

***

### 5. **Enable Mock Servers (for Testing)**

Need to test your API **before** it's built? Stoplight can **mock API responses**.

**Steps to enable Stoplight Mocking:**

1. **Turn on "Mock Server"** in the settings.
2. **Call the mock API instead of the real one.**

Example:

```sh
curl https://api.stoplight.io/mocks/project-id/users/123

```
