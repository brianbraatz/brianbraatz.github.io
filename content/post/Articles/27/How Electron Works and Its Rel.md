---
title: How Electron Works
description: How Electron Works
slug: how-electron-works
date: 2017-08-15
image: post/Articles/IMAGES/34.jpg
categories:
  - Electron
  - Node.js
  - JavaScript
  - Web Development
tags:
  - Electron
  - Node.js
  - JavaScript
  - Web
  - Development
draft: "False"
weight: "432"
lastmod: 2025-02-27T14:35:11.735Z
---
## What the Heck is Electron?

Electron is an open-source framework developed by GitHub (yes, the place where you push your code and hope for the best).

It allows developers to build cross-platform desktop applications using web technologies.

Think of it as putting a Chrome browser inside a standalone app that runs locally on your computer.

Sounds weird?

That’s because it is.

Electron powers some big-name apps you probably use every day, like:

* **VS Code** – Your favorite code editor (unless you’re an Emacs or Vim person, in which case, enjoy your superior keyboard wizardry).
* **Slack** – For work messages that could’ve been an email.
* **Discord** – For gaming messages that could’ve been a Slack message.
* **Spotify Desktop** – For blasting music while pretending to work.

## How Does Electron Work?

Electron is basically two things mashed together:

1. **Chromium (the open-source part of Google Chrome)** – This lets you render HTML, CSS, and JavaScript just like a regular web browser.
2. **Node.js (the back-end JavaScript runtime)** – This allows your Electron app to interact with the file system, operating system, and other native features.

### The Two Processes of Electron

Electron apps have two main processes:

1. **Main Process:** Runs in Node.js and controls the entire application. This is where you create windows, interact with the OS, and handle core functionality.
2. **Renderer Process:** Runs in a Chromium browser instance and handles the UI. Each window in an Electron app has its own renderer process.

Imagine a restaurant.

The **Main Process** is like the head chef managing the kitchen, while the **Renderer Process** is the waiter who takes orders and serves the customers.

The waiter (renderer) communicates with the kitchen (main process) through messages (IPC – Inter-Process Communication).

If you don’t have good communication between them, chaos ensues (or worse, you get a terrible user experience).

## Relationship Between Electron and Node.js

At this point, you might be wondering, *"Why do I need Node.js for this?"*

Well, web browsers are great for showing content, but they can’t do everything a real desktop app needs to do—like accessing the file system, handling native menus, and interacting with the operating system.

This is where Node.js comes in.

Since Electron bundles Node.js, you can do things like:

* Read and write files.
* Run background processes.
* Communicate with system APIs.

Basically, Node.js acts as the glue that holds everything together.

Without it, Electron would just be another Chrome tab hogging all your RAM.

## Why Use Electron? (And Why Not?)

### Pros:

✅ **Cross-Platform** – Write once, run on Windows, macOS, and Linux.\
✅ **Uses Web Tech** – If you know JavaScript, HTML, and CSS, you're already halfway there.\
✅ **Active Community** – Tons of libraries and support.\
✅ **Access to Native APIs** – Unlike a browser, you can interact with the OS directly.

### Cons:

❌ **Memory Hungry** – Running a full Chromium instance for each window?

Oof.\
❌ **Large Bundle Sizes** – Electron apps are chonky.

Even a simple "Hello World" can be 50MB+.\
❌ **Not Always the Best Choice** – If you just need a native app, frameworks like Qt or SwiftUI might be better.

## Conclusion

Electron is a fantastic tool if you want to create a cross-platform desktop app without learning an entirely new programming language.

It’s backed by Chromium and Node.js, which means you get the best (and worst) of both worlds.

Sure, it eats RAM like a buffet, but hey—if Slack and VS Code can get away with it, why can’t your app?

Now go forth, install Electron, and build something awesome (or at least something that doesn't crash immediately). 🚀

***

## Key Ideas Table

| Key Idea                 | Summary                                                 |
| ------------------------ | ------------------------------------------------------- |
| What is Electron?        | A framework for building desktop apps with web tech.    |
| How it Works             | Uses Chromium for UI and Node.js for system access.     |
| Electron’s Two Processes | Main process (backend) and Renderer process (frontend). |
| Why Use Electron?        | Easy, cross-platform, and built on web tech.            |
| Downsides of Electron    | High memory usage and large bundle sizes.               |
| Relationship to Node.js  | Node.js provides access to OS features and files.       |

***

## References

1. [Electron Official Docs](https://www.electronjs.org/docs)
2. [Node.js Official Site](https://nodejs.org/)
3. [Chromium Project](https://www.chromium.org/)
4. [VS Code Built with Electron](https://code.visualstudio.com/)
5. [Slack’s Electron Journey](https://slack.engineering/slack-desktop-2-0-released/)
