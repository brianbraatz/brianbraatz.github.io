---
title: Code-First OpenAPIs with Node.js
description: For Fun and Profit
slug: building-code-first-openapi-nodejs
date: 2017-08-14
image: post/Articles/IMAGES/nodejs.png
categories:
  - Node.js
  - OpenAPI
  - Swagger
  - API Design
tags:
  - Node.js
  - Openapi
  - Swagger
  - Rest
  - Api Design
  - Express
draft: "False"
weight: "342"
lastmod: 2025-03-02T22:54:28.669Z
---
***

## 🎯 What is Code-First API Development?

In a nutshell, **code-first** means you build your API in **actual code** (JavaScript/TypeScript) and generate an OpenAPI specification **from it automatically**.

This is way cooler than writing a massive YAML file by hand.

With this approach, you get:

* Self-documenting APIs 📖
* Fewer human errors (because who likes debugging indentation in YAML?)
* Automatic validation and type checking
* An easy-to-maintain API spec

## 🛠️ What We’re Using

We’re going to use:

* [Express.js](https://expressjs.com/) – The classic Node.js web framework
* [swagger-jsdoc](https://www.npmjs.com/package/swagger-jsdoc) – Generates OpenAPI specs from JSDoc comments
* [swagger-ui-express](https://www.npmjs.com/package/swagger-ui-express) – Serves the OpenAPI spec in a fancy UI

## 🚀 Setting Up the Project

First things first, let’s create a new Node.js project:

```sh
mkdir openapi-code-first && cd openapi-code-first
npm init -y
```

Now, install the required dependencies:

```sh
npm install express swagger-jsdoc swagger-ui-express
```

***

## 🏗️ Creating an Express Server

Let’s start with a basic Express server (`server.js`):

```js
const express = require('express');
const app = express();
const port = 3000;

app.use(express.json());

app.listen(port, () => {
console.log(`Server running at http://localhost:${port}`);
});
```

Run it with:

```sh
node server.js
```

Boom!

You have a running server.

Not impressed?

Hold my coffee. ☕

***

## 📜 Defining the OpenAPI Spec with swagger-jsdoc

Now, let’s generate an OpenAPI spec using `swagger-jsdoc`.

We add comments **above our routes** and let the magic happen.

First, create a new file `swaggerConfig.js`:

```js
const swaggerJSDoc = require('swagger-jsdoc');

const options = {
definition: {
openapi: '3.0.0',
info: {
title: 'Awesome API',
version: '1.0.0',
},
},
apis: ['./server.js'], // Path to API docs
};

const swaggerSpec = swaggerJSDoc(options);
module.exports = swaggerSpec;
```

Now, update `server.js` to serve the docs:

```js
const swaggerUi = require('swagger-ui-express');
const swaggerSpec = require('./swaggerConfig');

app.use('/api-docs', swaggerUi.serve, swaggerUi.setup(swaggerSpec));
```

Visit [`http://localhost:3000/api-docs`](http://localhost:3000/api-docs) and behold the glory of a **fully documented API!** 🎉

***

## ✨ Adding Routes with OpenAPI Annotations

Let’s add an actual API endpoint with OpenAPI documentation.

Modify `server.js`:

```js
/**
* @swagger
* /hello:
*   get:
*     summary: Returns a greeting
*     responses:
*       200:
*         description: A friendly greeting
*/
app.get('/hello', (req, res) => {
res.json({ message: 'Hello, world!' });
});
```

Now, restart your server and check [`http://localhost:3000/api-docs`](http://localhost:3000/api-docs) again.

You’ll see your `GET /hello` endpoint **fully documented**.

Neat, huh?

***

## 🏗️ Adding Parameters and Responses

Let’s level up and add **route parameters** and **response definitions**.

Modify `server.js`:

```js
/**
* @swagger
* /greet/{name}:
*   get:
*     summary: Greet a user
*     parameters:
*       - name: name
*         in: path
*         required: true
*         schema:
*           type: string
*     responses:
*       200:
*         description: A personalized greeting
*/
app.get('/greet/:name', (req, res) => {
res.json({ message: `Hello, ${req.params.name}!` });
});
```

Now, go to [`http://localhost:3000/api-docs`](http://localhost:3000/api-docs) and test your new **personalized greeting API!** 🎉

***

## 🔥 Wrapping Up

Here’s what we did:

* Set up an Express.js server
* Used `swagger-jsdoc` to generate OpenAPI specs
* Documented routes directly in JSDoc comments
* Served the docs using `swagger-ui-express`

***

## 🗝️ Key Ideas

| Key Idea                   | Summary                                   |
| -------------------------- | ----------------------------------------- |
| Code-First API Development | Generate OpenAPI specs directly from code |
| swagger-jsdoc              | Converts JSDoc comments into OpenAPI docs |
| swagger-ui-express         | Serves the docs in a web UI               |
| Express.js                 | Simple Node.js web framework              |
| OpenAPI                    | Standard for REST API documentation       |

***

## 📚 References

* [Express.js](https://expressjs.com/)
* [swagger-jsdoc](https://www.npmjs.com/package/swagger-jsdoc)
* [swagger-ui-express](https://www.npmjs.com/package/swagger-ui-express)
* [OpenAPI Specification](https://swagger.io/specification/)
* [REST API Best Practices](https://restfulapi.net/)
