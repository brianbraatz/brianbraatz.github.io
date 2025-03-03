---
title: JWT in a Nutshell
description: Intro into how JSON Web Tokens JWT works
slug: jwt-in-a-nutshell
date: 2015-12-15
image: post/Articles/IMAGES/jwtlogo.jpg
categories:
  - Cloud
  - Security
  - JWT
tags:
  - JWT
  - Authentication
  - Authorization
  - Security
  - Tokens
  - API
  - oAuth
  - WebDevelopment
  - Javascript
  - Cloud
draft: false
weight: 378
lastmod: 2025-03-02T23:41:49.938Z
---
# JWT in a Nutshell

Ever wondered how websites remember who you are **without making you log in every five minutes**?

Meet **JWT (JSON Web Tokens)**, the little digital passport that proves your identity in a stateless world. Let’s break it down.

## A Brief History of JWT (a.k.a. "Why Do We Need Another Token?")

Before JWT, web authentication relied on **session-based authentication**, where user sessions were stored **server-side**. This led to:

* **Scalability issues** (imagine handling millions of sessions!).
* **Server memory overload**.
* **Session hijacking nightmares**.

JWT was introduced as a **stateless** alternative, allowing authentication **without storing session data** on the server.

JWTs contain **self-contained claims** about a user, meaning the server can **verify** them without storing anything.

## Why JWT? The Core Features

A JWT is essentially a **signed** and **encoded** piece of JSON that contains:

* **Who you are** (e.g., `sub: "123456"`).
* **What you can do** (e.g., `role: "admin"`).
* **When it expires** (`exp: 1716239022`).

### How JWT Works (Step-by-Step)

1. **User logs in** → The server creates a JWT with the user’s details.
2. **Server sends JWT** → The client stores it (usually in localStorage or a cookie).
3. **User makes a request** → The JWT is sent in the request headers.
4. **Server verifies JWT** → If it’s valid, access is granted. No database lookup needed!

## JWT Structure (A Token in Three Parts)

A JWT consists of **three** base64-encoded sections:

```plaintext
header.payload.signature
```

Example JWT:

```plaintext
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvbiBEb2UiLCJpYXQiOjE1MTYyMzkwMjJ9.Dq6P5wO3oKr9FJ_V5I-dE
```

### 1. **Header** (The Algorithm Info)

```json
{
  "alg": "HS256",
  "typ": "JWT"
}
```

This tells us JWT uses **HMAC SHA-256** for signing.

### 2. **Payload** (The Claims)

```json
{
  "sub": "1234567890",
  "name": "John Doe",
  "iat": 1516239022
}
```

Claims store **who the user is** and **what they can do**.

### 3. **Signature** (The Tamper-Proof Seal)

This is created using the **header, payload, and a secret key**. If anyone tampers with the token, the signature won’t match, and the JWT will be **rejected**.

## JWT vs. The Alternatives

| Feature            | JWT      | Session-Based            | OAuth 2.0            |
| ------------------ | -------- | ------------------------ | -------------------- |
| **Stateless**      | Yes      | No                       | Yes                  |
| **Scalable**       | Yes      | No                       | Yes                  |
| **Self-Contained** | Yes      | No                       | No                   |
| **Security Risks** | Moderate | High (session hijacking) | High (token leakage) |

* **Session-Based Auth** requires storing sessions **on the server**, making it hard to scale.
* **OAuth 2.0** uses JWT under the hood but is focused on **authorization** rather than authentication.

## JWT Code Sample (Node.js Example)

Here’s how to create and verify a JWT in **Node.js** using the `jsonwebtoken` package:

```javascript
const jwt = require('jsonwebtoken');

// Secret key (keep this safe!)
const secretKey = "superSecretKey";

// Create a JWT
const token = jwt.sign({ userId: 123, role: "admin" }, secretKey, { expiresIn: "1h" });

console.log("JWT:", token);

// Verify a JWT
try {
    const decoded = jwt.verify(token, secretKey);
    console.log("Decoded JWT:", decoded);
} catch (err) {
    console.log("Invalid token!");
}
```

## Key Ideas

* **JWT** is a stateless, self-contained token used for authentication and authorization.
* JWTs consist of **three parts**: Header, Payload, and Signature.
* They are **signed** with a secret key, making them **tamper-proof**.
* **No server-side session storage** is needed, making JWTs highly **scalable**.
* JWTs are often used in **OAuth 2.0** as access tokens.
* While JWTs solve **session management issues**, they introduce **token leakage risks**.
* JWTs should be **stored securely** (e.g., HttpOnly cookies to prevent XSS attacks).
* Expiration and **token rotation** should be used to improve security.

## References

1. [JWT.io](https://jwt.io/) - Decode and debug JWTs.
2. [RFC 7519](https://datatracker.ietf.org/doc/html/rfc7519) - The official JWT specification.
3. [Node.js jsonwebtoken Library](https://www.npmjs.com/package/jsonwebtoken) - The go-to library for JWTs in Node.js.
4. [OAuth 2.0 and JWT](https://oauth.net/) - How JWTs fit into OAuth authentication.
5. [JWT Security Best Practices](https://auth0.com/blog/a-look-at-the-latest-draft-for-jwt-bcp/) - Best practices for securing JWTs.
