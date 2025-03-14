---
title: Node.js  Docker Set Up
description: 
slug: how-to-set-up-nodejs-with
date: 2018-06-23
image: post/Articles/IMAGES/nodejs.png
categories:
  - Node.js
  - Docker
  - Web Development
  - Containers
tags:
  - Node.js
  - Docker
  - Web
  - Development
  - Containers
draft: "False"
weight: "170"
categories_ref:
  - Node.js
  - Docker
  - Web Development
  - Containers
lastmod: 2025-03-14T15:45:04.098Z
---
***

## 🎯 What We’ll Cover

* Setting up a basic Node.js app
* Writing a `Dockerfile`
* Running it in a container
* Making it accessible to the outside world

By the end, you’ll have a containerized Node.js app that works like magic (minus the wizard robes).

***

## 📦 Step 1: Create a Basic Node.js App

First, let’s set up a super simple Node.js application.

Run these commands:

```sh
mkdir node-docker && cd node-docker
npm init -y
```

Now, let’s install Express (because who writes plain HTTP servers anymore?):

```sh
npm install express
```

Then, create a file called `server.js` and paste this inside:

```js
const express = require('express');
const app = express();
const PORT = process.env.PORT || 3000;

app.get('/', (req, res) => {
res.send('Hello from Dockerized Node.js! 🚀');
});

app.listen(PORT, () => {
console.log(`Server is running on port ${PORT}`);
});
```

Boom!

Basic Node.js server ✅

***

## 🛠 Step 2: Write the `Dockerfile`

Now, let’s create a `Dockerfile`.

This file tells Docker how to build our container.

Create a file named `Dockerfile` (yes, no extension) and add this:

```Dockerfile
# Use the official Node.js image as a base
FROM node:18-alpine

# Set the working directory in the container
WORKDIR /app

# Copy package.json and install dependencies
COPY package.json .
RUN npm install

# Copy the rest of the application files
COPY . .

# Expose the port the app runs on
EXPOSE 3000

# Start the application
CMD ["node", "server.js"]
```

This file does a few things:

1. Uses a small, efficient Node.js image (`node:18-alpine`)
2. Sets up a working directory
3. Copies `package.json` and installs dependencies
4. Copies the rest of the files
5. Exposes port `3000`
6. Runs the app with `node server.js`

***

## 🏗 Step 3: Build and Run the Docker Container

Now that we have a `Dockerfile`, let’s build our image and run it.

### 🔨 Build the Image

```sh
docker build -t my-node-app .
```

This tells Docker to build an image named `my-node-app` using the `Dockerfile` in the current directory (`.`).

### �� Run the Container

```sh
docker run -p 3000:3000 my-node-app
```

Here’s what’s happening:

* `-p 3000:3000` maps port 3000 inside the container to port 3000 on your machine.
* `my-node-app` is the name of the image we just built.

Now, visit `http://localhost:3000/`, and you should see:

```
Hello from Dockerized Node.js! 🚀
```

***

## 🐳 Extra: Running in Detached Mode

If you want the container to run in the background (so your terminal isn’t stuck), run:

```sh
docker run -d -p 3000:3000 my-node-app
```

Then, to stop the container later, use:

```sh
docker ps # Get the container ID
docker stop <container_id>
```

***

<!-- 
## 🎉 Conclusion

You did it!

You successfully dockerized a Node.js app.

Now you can:

- Share your app easily with others
- Deploy it anywhere Docker is supported
- Pretend you’re a DevOps guru 😎

Docker makes development cleaner, more consistent, and way less painful than "it works on my machine."

Go forth and containerize everything! 🚀

--- -->

## 📌 Key Ideas

| Concept           | Summary                                       |
| ----------------- | --------------------------------------------- |
| **Dockerfile**    | Defines how to build the Node.js container    |
| **docker build**  | Builds the image from the `Dockerfile`        |
| **docker run**    | Runs the container and exposes the port       |
| **Detached Mode** | Allows the container to run in the background |
| **Port Mapping**  | Maps the container’s port to the host machine |

***

## 🔗 References

* [Docker Official Docs](https://docs.docker.com/)
* [Node.js Official Docs](https://nodejs.org/en/docs/)
* [Express.js Official Docs](https://expressjs.com/)
