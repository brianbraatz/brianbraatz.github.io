---
title: Understanding Node.js Electron Connection
description: Understanding Node.js Electron Connection
slug: understanding-nodejs-elec
date: 2018-04-15
image: post/Articles/IMAGES/electronlogo.png
categories:
  - Node.js
  - Electron
  - Obsidian
  - Visual Studio Code
tags:
  - Node.js
  - Electron
  - Obsidian
  - Visual Studio Code
  - Server
  - JavaScript
draft: "False"
weight: "472"
categories_ref:
  - Node.js
  - Electron
  - Obsidian
  - Visual Studio Code
slug_calculated: https://brianbraatz.github.io/p/understanding-nodejs-elec
lastmod: 2025-03-14T16:40:12.634Z
---
<!-- ## Understanding Node.js Electron Connection

Alright, buckle up, because we’re about to dive deep into the magical world where Node.js and Electron hold hands and make desktop apps happen.

You’ve probably heard of Node.js running servers, but did you know it’s also the secret sauce behind apps like Obsidian and Visual Studio Code?

Let’s break it down, in the most casual way possible.

--- -->

### Node.js: The Unstoppable JavaScript Engine

So, what even is Node.js?

If you’ve been living under a rock, here’s the deal: Node.js is JavaScript on steroids, running outside the browser.

Think of it as JavaScript escaping the cage of the browser and deciding it wants to be a full-fledged backend powerhouse.

And it’s not just for web servers—oh no, it does **way** more than that!

#### Node.js as a Server

Yeah, Node.js is famous for running servers.

It’s like the friendly neighborhood pizza guy delivering JSON data instead of pepperoni pizzas.

Thanks to its non-blocking, event-driven architecture, it can handle a **ton** of requests at once without breaking a sweat.

That’s why it’s the backbone of so many modern web applications.

But what if I told you that Node.js isn’t just some backend-only tech?

Enter **Electron**.

***

### What is Electron?

A Desktop Revolution

Electron is like that one person in a group project who does everything.

It lets you build full-fledged desktop applications using web technologies like HTML, CSS, and JavaScript.

And guess what powers it?

**Node.js!**

#### How Electron Uses Node.js

Electron combines the Chromium browser with Node.js, making it possible to write desktop apps with the same JavaScript code you’d use for a website.

It’s like running a mini web app, but instead of being trapped in Chrome, it gets its own window like a *real* application.

Your desktop app in Electron actually consists of two key parts:

1.

**Main Process** – Runs in Node.js and controls things like menus, system dialogs, and file access.

2.

**Renderer Process** – Runs in Chromium and takes care of displaying the UI, just like a web browser.

Thanks to this duo, Electron apps can do things normal web apps can’t, like accessing the file system, running background processes, and communicating with the OS.

*Fancy!*

***

### Node.js and Electron in Obsidian and VS Code

Now, let’s talk about **two** of the biggest Electron-based apps: **Obsidian** and **Visual Studio Code**.

#### Obsidian: A Markdown Lover’s Paradise

If you’re here, you probably love **Markdown** (because this article is written in it—*meta!*).

Obsidian is a Markdown-based knowledge management tool, and guess what?

It runs on **Electron**, meaning it’s a full-fledged Node.js app wrapped in a desktop UI.

Obsidian takes advantage of Node.js to:

* Read and write files directly to your local storage (no need for an internet connection!).

* Use plugins, many of which tap into Node.js features.

* Keep your vault running smoothly without relying on a remote server.

So, every time you open Obsidian and write notes, you’re basically interacting with a JavaScript-powered desktop app.

Cool, right?

#### Visual Studio Code: The Editor That Runs Everything

VS Code is **THE** code editor of choice for a lot of developers, and—surprise, surprise—it’s also built using **Electron**.

That means every time you open VS Code, you’re basically running a **mini web app** on your desktop that’s powered by Node.js.

VS Code makes heavy use of Node.js for:

* Running the integrated terminal (which is just a shell interface powered by Node.js).

* Managing extensions, many of which hook into Node.js functionalities.

* Background processing and file system operations.

So yeah, even your beloved code editor is secretly just a giant JavaScript app pretending to be a desktop application.

🤯

***

### Wrapping Up: Node.js is Everywhere

To sum it all up:

* **Node.js isn’t just for servers**; it’s the backbone of many desktop applications thanks to Electron.

* **Electron lets you build desktop apps** using web technologies, with Node.js running behind the scenes.

* **Obsidian and VS Code** are prime examples of Node.js-powered desktop apps you probably use daily.

Basically, if you love JavaScript, Node.js will keep showing up in your life whether you’re building web servers, desktop apps, or something in between.

<!-- 
Now go forth and appreciate Node.js in all its glory! -->

🚀

***

## 🔑 Key Ideas

| Key Idea            | Summary                                                                            |
| ------------------- | ---------------------------------------------------------------------------------- |
| Node.js as a server | Node.js is commonly used to run backend servers, handling requests asynchronously. |

|\
\| Electron overview | Electron combines Node.js and Chromium to create desktop applications using web technologies.

|\
\| Electron structure | Electron apps have a main process (Node.js) and a renderer process (Chromium).

|\
\| Obsidian | A markdown-based knowledge management app built using Electron and Node.js.

|\
\| VS Code | A powerful code editor that runs on Electron and heavily uses Node.js.

|\
\| Node.js beyond the server | Node.js is a key player in desktop app development, not just web servers.

|

***

## 🔗 References

1.

[Node.js Official Site](https://nodejs.org/)\
2\.

[Electron Documentation](https://www.electronjs.org/)\
3\.

[Obsidian](https://obsidian.md/)\
4\.

[Visual Studio Code](https://code.visualstudio.com/)
