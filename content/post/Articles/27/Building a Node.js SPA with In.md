---
title: Building a Node.js SPA with Infinite Scroll
description: 
slug: nodejs-spa-infinite-scrol
date: 2018-06-14
image: post/Articles/IMAGES/nodejs.png
categories:
  - Node.js
  - SPA
  - Web Development
  - Infinite Scroll
  - Async
tags:
  - Node.js
  - Spa
  - Web
  - development
  - Infinite
  - scroll
  - Express
  - Frontend
  - Backend
draft: "False"
weight: "386"
lastmod: 2025-02-28T03:17:49.226Z
---
# Building a Node.js SPA with Infinite Scroll for Fake Emails 📩

## Introduction

Ever wondered how Gmail manages to show a bazillion emails without crashing your browser?

No?

Well, too bad, because that’s exactly what we’re doing today!

We’ll build a **Node.js-powered Single Page Application (SPA)** that **pretends to be Gmail** but is actually filled with **Lorem Ipsum** emails.

The goal?

To create an **infinite scrolling email list**, where new emails are fetched as the user scrolls down.

No, we’re not loading a million emails at once—because we actually like our users.

***

## How It Works 🛠️

Here’s the grand plan:

1. **The frontend** starts with an empty email list and fetches the first batch from the server.
2. As the user scrolls down, the **next batch of emails** is requested.
3. The server generates **fake emails** on demand using a Lorem Ipsum generator.
4. When the user clicks on an email, the server generates a **fake email body** and returns it.

Boom!

We’ve got infinite scrolling and dynamic email loading, just like the pros.

***

## The Tech Stack ⚙️

We’ll use:

* **Node.js + Express** for the backend (because it’s fast and fun 🎉).
* **Vue.js (or React)** for the frontend (SPA vibes, baby!).
* **Lorem Ipsum Generator** for fake emails (because writing 1,000,000 emails manually is a bad idea).

Let’s get coding! 💻

***

## Step 1: The Backend (Node.js + Express) 🏗️

First, create a Node.js project:

```sh
mkdir fake-email-spa && cd fake-email-spa
npm init -y
npm install express cors faker
```

Now, create `server.js`:

```js
const express = require("express");
const cors = require("cors");
const faker = require("faker");

const app = express();
const PORT = 3000;
app.use(cors());

// Generate fake emails
themes) => ({
id: i,
sender: faker.internet.email(),
subject: faker.lorem.sentence(),
preview: faker.lorem.sentences(2),
date: faker.date.past().toDateString(),
}));
};

app.get("/emails", (req, res) => {
const page = parseInt(req.query.page) || 1;
const limit = 20;
const emails = generateEmails(limit, page);
res.json(emails);
});

app.get("/email/:id", (req, res) => {
res.json({
id: req.params.id,
body: faker.lorem.paragraphs(5),
});
});

app.listen(PORT, () => console.log(`Server running on port ${PORT}`));
```

This API:

* Serves **20 emails per request**.
* Uses `faker.js` to generate fake senders, subjects, and previews.
* Provides full email bodies when an email is clicked.

***

## Step 2: The Frontend (Vue.js Example) 🎨

Install Vue.js (or React, whatever floats your boat):

```sh
npm install vue axios
```

Now, create `index.html`:

```html
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Fake Gmail</title>
</head>
<body>
<div id="app"></div>
<script src="https://cdn.jsdelivr.net/npm/vue@2"></script>
<script src="https://cdn.jsdelivr.net/npm/axios"></script>
<script>
new Vue({
el: "#app",
data: {
emails: [],
page: 1,
loading: false
},
mounted() {
this.loadEmails();
window.addEventListener("scroll", this.handleScroll);
},
methods: {
async loadEmails() {
if (this.loading) return;
this.loading = true;
const res = await axios.get(`http://localhost:3000/emails?page=${this.page}`);
this.emails.push(...res.data);
this.page++;
this.loading = false;
},
handleScroll() {
if (window.innerHeight + window.scrollY >= document.body.offsetHeight - 500) {
this.loadEmails();
}
}
},
template: `
<div>
<div v-for="email in emails" :key="email.id">
<h3>{{ email.sender }}</h3>
<p>{{ email.subject }}</p>
<p>{{ email.preview }}</p>
</div>
</div>`
});
</script>
</body>
</html>
```

Now:

* The app loads the **first page of emails**.
* As you scroll, more emails **load automatically**.
* Clicking on an email would make another API request to fetch the full content.

***

## Server-Side Rendering vs SPA 🌍

How does this compare to **Server-Side Rendering (SSR)?**

* **SPA (our approach)** loads a small chunk of emails, keeping the experience smooth.
* **SSR** would load the entire email list upfront, which is **a bad idea** when dealing with millions of emails.
* Our approach keeps the server happy and **reduces initial load time**.

***

## Conclusion 🎉

We just built a **Gmail-like email viewer** with **infinite scroll** using **Node.js, Vue.js, and a fake email generator**.

It’s fast, fun, and won’t set your server on fire. 🔥

Now go forth and spam yourself with Lorem Ipsum emails!

***

## Key Ideas

| Concept           | Explanation                                  |
| ----------------- | -------------------------------------------- |
| Infinite Scroll   | Loads more emails as you scroll              |
| Fake Data         | Uses `faker.js` for realistic test data      |
| SPA vs SSR        | SPA loads small chunks, SSR loads everything |
| Node.js + Express | Backend that serves emails dynamically       |
| Vue.js Frontend   | Fetches and displays emails dynamically      |

***

## References

* [Vue.js Documentation](https://vuejs.org/)
* [Express.js Documentation](https://expressjs.com/)
* [Faker.js](https://fakerjs.dev/)
