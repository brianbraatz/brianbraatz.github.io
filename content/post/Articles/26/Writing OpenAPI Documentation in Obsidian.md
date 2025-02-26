---
title: OpenAPI-Writing Documentation in Obsidian
description: How to setup Obsidian and Hugo for OpenAPI documentation
slug: obsidian-hugo-openapi-guide
date: 2019-03-22
image: post/Articles/IMAGES/openapi.png
categories:
  - Documentation
  - OpenAPI
  - Web Development
tags:
  - OpenAPI
  - Obsidian
  - Hugo
  - API
  - Docs
  - Static
  - Sites
draft: false
weight: 529
lastmod: 2025-02-26T11:36:56.615Z
---
## Introduction

If you're looking for a **lightweight, version-controlled, and customizable** way to document your APIs, **Obsidian + Hugo** is a killer combo.

My Favorite actually.. :)

You can **write everything in Markdown**, generate a **fast static site**, and **self-host** your API documentation.

**Side Note:** Whats also nice about Obsidian, is since everything is in markdown, you can switch between Obsidian and Visual Studio code (for example), very easily..

<!-- 
In this guide, we'll cover:  

✅ **Setting up Hugo**  
✅ **Installing an OpenAPI-friendly theme**  
✅ **Writing and formatting OpenAPI documentation**  
✅ **Rendering OpenAPI specs with Hugo**  
✅ **Deploying your API docs**  

Let’s dive in! 🚀  
-->

***

## 1️⃣ **Setting Up Hugo**

### **Install Hugo**

First, install Hugo based on your OS:

**Mac (Homebrew):**

```sh
brew install hugo
```

**Windows (Chocolatey):**

```sh
choco install hugo
```

**Linux:**

```sh
sudo apt install hugo
```

Verify the installation:

```sh
hugo version
```

You should see an output like:

```
hugo v0.92.0+extended linux/amd64 BuildDate=...
```

### **Create a New Hugo Site**

```sh
hugo new site openapi-docs
cd openapi-docs
```

***

## 2️⃣ **Choosing an OpenAPI-Compatible Hugo Theme**

Hugo doesn't natively support OpenAPI, but you can use a theme that does!

### **Recommended OpenAPI Hugo Themes**

| Theme       | Link                                            |
| ----------- | ----------------------------------------------- |
| **Docsy**   | <https://github.com/google/docsy>               |
| **Learn**   | <https://github.com/matcornic/hugo-theme-learn> |
| **DocuAPI** | <https://github.com/bep/docuapi>                |

### **Install a Theme**

For this guide, we’ll use **DocuAPI**:

```sh
git submodule add https://github.com/bep/docuapi.git themes/docuapi
echo 'theme = "docuapi"' >> config.toml
```

***

## 3️⃣ **Writing OpenAPI Documentation in Obsidian**

### **Organizing Your Files**

In Obsidian, structure your API docs like this:

```
📂 openapi-docs/
 ├── 📂 content/
 │   ├── 📄 _index.md
 │   ├── 📂 api/
 │   │   ├── 📄 introduction.md
 │   │   ├── 📄 authentication.md
 │   │   ├── 📄 endpoints.md
 │   │   ├── 📂 schemas/
 │   │   │   ├── 📄 user.md
 │   │   │   ├── 📄 order.md
 ├── 📄 config.toml
```

### **Example: Introduction.md**

```markdown
---
title: "Introduction"
description: "Overview of the API"
weight: 1
---

# Welcome to Our API  

This API allows developers to interact with our platform.  

- **Base URL:** `https://api.example.com`
- **Authentication:** API Key in `Authorization` header
```

***

## 4️⃣ **Rendering OpenAPI Specs in Hugo**

To display OpenAPI specs, you need **Redoc or Swagger UI**.

### **Using Redoc (Recommended)**

1️⃣ **Download Redoc JavaScript**

```sh
mkdir -p static/js
curl -o static/js/redoc.standalone.js https://cdn.jsdelivr.net/npm/redoc@latest/bundles/redoc.standalone.js
```

2️⃣ **Create a Page for OpenAPI Docs**\
Create `content/api/openapi.md` and add this:

```markdown
---
title: "API Reference"
description: "OpenAPI Specification"
weight: 3
---

# API Reference  

<script src="/js/redoc.standalone.js"></script>
<redoc spec-url="/openapi.yaml"></redoc>
```

3️⃣ **Add Your OpenAPI Spec**

Place your `openapi.yaml` file in `static/`:

```sh
mv openapi.yaml static/openapi.yaml
```

***

## 5️⃣ **Running Hugo Locally**

Start the Hugo development server:

```sh
hugo server -D
```

Visit: **http://localhost:1313** 🎉

***

## 6️⃣ **Deploying Your OpenAPI Docs**

### **GitHub Pages Deployment**

1️⃣ **Build the static site:**

```sh
hugo --minify
```

2️⃣ **Push to GitHub:**

```sh
git init
git add .
git commit -m "Initial commit"
git remote add origin https://github.com/YOUR_USERNAME/openapi-docs.git
git push -u origin main
```

3️⃣ **Deploy with GitHub Actions**

Create `.github/workflows/hugo.yml`:

```yaml
name: Deploy Hugo Site

on:
  push:
    branches:
      - main

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: peaceiris/actions-hugo@v2
        with:
          hugo-version: 'latest'
          extended: true
      - run: hugo
      - uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./public
```

✅ **Your site is live on** `https://YOUR_USERNAME.github.io/openapi-docs/`

### **Alternative Hosting**

| Service              | Link                           |
| -------------------- | ------------------------------ |
| **Netlify**          | <https://www.netlify.com>      |
| **Vercel**           | <https://vercel.com>           |
| **Cloudflare Pages** | <https://pages.cloudflare.com> |

***

<!-- 
## **Final Thoughts**  

You now have a **fully working OpenAPI documentation site** powered by **Obsidian, Hugo, and Redoc**!  

### **Why This Setup Rocks**  

✅ **Blazing fast** – Hugo generates static HTML for speed.  
✅ **Version controlled** – Everything is tracked in Git.  
✅ **Highly customizable** – Use any Hugo theme and extend it.  
✅ **Free to host** – GitHub Pages, Netlify, and others.  

💡 **Bonus Tip:** Add a `dark mode` switch for better UX!  

Let me know if you have questions! 🚀  

---

## **Key Ideas**  

| Concept | Summary |
|---------|---------|
| **Hugo** | Fast static site generator for docs. |
| **Obsidian** | Write Markdown locally and sync with Git. |
| **Redoc** | Beautiful OpenAPI rendering. |
| **Deployment** | GitHub Pages, Netlify, or Vercel. |

```

---

This guide is **fully structured for Hugo and Obsidian**, with:  

✔ **Step-by-step setup**  
✔ **Code snippets**  
✔ **Deployment guides**  
✔ **OpenAPI integration**  

-->
