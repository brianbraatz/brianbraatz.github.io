---
title: OpenAPI-How to connect Manual Documentation
description: ""
slug: manual-docs-openapi
date: 2019-09-22
image: post/Articles/IMAGES/openapi.png
categories:
  - API
  - Documentation
  - OpenAPI
  - Web Development
tags:
  - Swagger
  - OpenAPI
  - API
  - Documentation
  - REST
  - Manual
  - Documentation
draft: false
weight: 621
lastmod: 2025-02-26T11:04:13.218Z
---
So, you've got yourself an OpenAPI-compliant API, and now you need to manually add some documentation to it.

While OpenAPI is great for generating docs automatically, there are always those little details—business logic, examples, explanations—that need a human touch.

***

## 1. **Use OpenAPI Descriptions and Annotations**

The first step is to make the most of what OpenAPI already provides:

* **`summary` and `description` fields**\
  Each endpoint, request, and response can have a `summary` and `description` field. Use them! They help developers understand the *why*, not just the *what*.

* **Parameter and response descriptions**\
  Every query parameter, header, and response should have a clear explanation.

* **Examples in `example` or `examples` fields**\
  Show real-world usage with examples inside OpenAPI itself.

### Example:

```yaml
paths:
  /users:
    get:
      summary: "Retrieve all users"
      description: "Gets a list of users in the system. Only admin users can see all accounts."
      parameters:
        - name: role
          in: query
          description: "Filter users by role (admin, user, guest)"
          schema:
            type: string
            example: "admin"
      responses:
        "200":
          description: "A list of users"
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: "#/components/schemas/User"
              example:
                - id: 1
                  name: "John Doe"
                  role: "admin"
```

***

## 2. **Write External Documentation with Markdown**

Sometimes, API descriptions in OpenAPI files aren’t enough. You can link external documentation using OpenAPI’s `externalDocs` field.

### Example:

```yaml
externalDocs:
  description: "Detailed API Usage Guide"
  url: "https://docs.example.com/api-guide"
```

For deeper documentation, maintain a separate Markdown file with details like:

* Business rules behind the API
* Advanced usage scenarios
* Code samples in different languages

You can store these docs in a GitHub wiki, a README file, or an API documentation site like **ReadTheDocs**.

***

## 3. **Use API Documentation Platforms (Swagger UI, Redoc, Stoplight)**

Swagger UI, Redoc, and Stoplight let you display OpenAPI specs beautifully. But they also allow manual documentation additions.

### Swagger UI Customization:

* Add descriptions and Markdown formatting.
* Include authentication notes and usage examples.
* Use `x-tags` and `x-summary` for better organization.

### Redoc Features:

* Custom sidebars for extra docs
* Embedded HTML for detailed guides
* Better styling for descriptions

If you want a mix of manual and generated docs, these tools make it easy.

***

## 4. **Provide Real-World Code Examples**

Nothing beats real-world usage examples. Add a `x-codeSamples` extension to OpenAPI or manually include them in Markdown-based documentation.

Example with OpenAPI Extension:

```yaml
x-codeSamples:
  - lang: "cURL"
    source: |
      curl -X GET "https://api.example.com/users" -H "Authorization: Bearer TOKEN"
  - lang: "Python"
    source: |
      import requests
      headers = {"Authorization": "Bearer TOKEN"}
      response = requests.get("https://api.example.com/users", headers=headers)
      print(response.json())
```

If your documentation is in a separate guide, include full API request/response examples in different programming languages.

***

## 5. **Use GitHub (or Any VCS) for Version-Controlled Docs**

Documentation needs to evolve with your API. Store manual documentation alongside your OpenAPI specs in a version-controlled repo.

### Best Practices:

* **Keep docs in `/docs` or `/openapi/docs` folder.**
* **Use GitHub Pages or ReadTheDocs for hosting.**
* **Document API changes in `CHANGELOG.md` or a `docs/updates.md` file.**

This ensures your documentation remains **accurate, up to date, and tied to the API version**.

***

## 6. **Enable User Feedback & Contributions**

No matter how great your documentation is, users will have questions. Enable feedback:

* **GitHub Issues for Documentation**\
  Allow users to report unclear sections.

* **Discussions/Forums**\
  Provide a space where developers can share insights and solutions.

* **Edit Feature in Docs**\
  If hosting on ReadTheDocs, GitHub Pages, or another platform, allow users to contribute.

Example: GitHub’s `Edit on GitHub` button in ReadTheDocs lets users submit pull requests to improve docs.

***

## 7. **Keep Documentation and Code in Sync**

Your API is a moving target. The worst thing is outdated documentation.

* **Automate Documentation Checks**\
  Use tools like **Spectral** to lint OpenAPI specs for missing descriptions.

* **Run Docs as Part of CI/CD**\
  Use GitHub Actions or GitLab CI to ensure documentation updates whenever the API changes.

* **Define a Process for Manual Updates**\
  If your OpenAPI spec is auto-generated from code, maintain a **separate guide for non-OpenAPI details**.

***

<!-- 
## Conclusion

Manually documenting an OpenAPI-compliant API is an art and a science. The goal is to balance **automation** and **human-readable insights**. 

### Key Takeaways:
✅ Use OpenAPI fields (`summary`, `description`, `example`) for inline documentation.  
✅ Link external Markdown docs for deeper explanations.  
✅ Leverage tools like **Swagger UI, Redoc, and Stoplight** for rich API documentation.  
✅ Provide **real-world code samples** in multiple languages.  
✅ Store docs in **version control** and keep them updated.  
✅ Enable **user feedback** to improve documentation.  
✅ Ensure docs **stay in sync with API updates**.

By following these steps, you can ensure your API remains **clear, maintainable, and developer-friendly**!

---

## Key Ideas

| Concept | Summary |
|---------|---------|
| OpenAPI Descriptions | Use `summary`, `description`, and `examples` for inline documentation. |
| External Docs | Link to Markdown or GitHub pages for more detailed guides. |
| Swagger UI & Redoc | Customize API documentation with these tools. |
| Code Samples | Provide usage examples in different languages. |
| Version Control | Store docs in GitHub, GitLab, or Bitbucket. |
| User Feedback | Allow users to report unclear docs and contribute fixes. |
| CI/CD Integration | Keep docs updated as the API evolves. |

---
```

This guide ensures your OpenAPI-compliant API has **clear, concise, and helpful** documentation. Want to go even deeper? Let me know, and I’ll add more details! 🚀

-->
