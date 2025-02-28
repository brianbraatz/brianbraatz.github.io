---
title: NPM and Node Modules in a Nutshell
description: NPM and Node Modules in a Nutshell
slug: npm-and-node-modules-nuts
date: 2018-06-22
image: post/Articles/IMAGES/npm.png
categories:
  - Node.js
  - Npm
  - JavaScript
  - Modules
  - Package Management
tags:
  - Node.js
  - Npm
  - JavaScript
  - Modules
  - Package Management
draft: "False"
weight: "476"
lastmod: 2025-02-27T17:27:20.626Z
---
# NPM and Node Modules in a Nutshell

## What the Heck is NPM?

Alright, so you've decided to dive into the world of Node.js, and suddenly, everyone is throwing around the term "NPM" like it's common sense.

But what is it really?

NPM stands for **Node Package Manager**, but honestly, it should be called **Never-ending Pile of Modules** because that's what it feels like when you start using it.

It’s basically a massive warehouse of JavaScript packages that you can install with just a simple command.

Think of NPM as the app store, but for JavaScript libraries.

Need a library to generate fake data?

There’s a package for that.

Need one to handle dates?

There are *too many* packages for that.

Want a package that does absolutely nothing but exists for meme purposes?

Yep, there’s one of those too.

## Installing Node and NPM

First things first, if you want to use NPM, you gotta have **Node.js** installed.

### 1.

Download Node.js\
Go to [Node.js official website](https://nodejs.org/) and download the **LTS version** (because unless you like living on the edge, you want the stable version).

### 2. Verify the Installation

Once installed, check if everything works by running:

```sh
node -v
npm -v
```

If those commands spit out version numbers instead of error messages, congratulations!

You’re officially in the game.

## Using NPM Like a Pro (or at Least Faking It)

### 1. Initializing a New Project

Creating a new Node.js project?

Use:

```sh
npm init
```

This will walk you through setting up a `package.json` file.

If you’re lazy (or just don’t care about prompts), use:

```sh
npm init -y
```

This skips all the questions and sets up a default `package.json` file faster than you can say "I should have read the docs."

### 2. Installing Packages

Want to install a package?

Run:

```sh
npm install some-package
```

Or, if you're a fan of shorthand:

```sh
npm i some-package
```

By default, this installs the package in your project under the `node_modules/` folder and adds it to `package.json`.

### 3. Installing Global Packages

Some tools need to be installed globally (meaning they work everywhere on your system).

For that, add the `-g` flag:

```sh
npm install -g some-tool
```

Common global packages:

* **nodemon** (auto-restarts Node app)
* **http-server** (quick local server)
* **typescript** (because JavaScript wasn’t confusing enough)

### 4. Installing Dependencies from `package.json`

If you clone a project that already has a `package.json`, just run:

```sh
npm install
```

This will pull down all the necessary dependencies, saving you the pain of manually installing each one.

## What’s Up with `node_modules/`?

Ah yes, the **node\_modules** folder, the Bermuda Triangle of web development.

It contains all the packages you install, and it grows faster than you expect.

Fun fact: If you ever feel like your hard drive is too empty, just install some JavaScript packages, and in no time, your storage will be *mysteriously* full.

## NPM Scripts: Automate the Boring Stuff

In your `package.json`, you can define scripts to automate tasks.

For example:

```json
"scripts": {
  "start": "node index.js",
  "dev": "nodemon index.js",
  "test": "jest"
}
```

Then, run them with:

```sh
npm run dev
```

Or if the script name is `start`, just type:

```sh
npm start
```

No "run" needed.

Magic.

## Removing Packages

Installed something and instantly regretted it?

Remove it with:

```sh
npm uninstall some-package
```

Or, if you want to keep your `package.json` tidy:

```sh
npm uninstall some-package --save
```

For global packages:

```sh
npm uninstall -g some-tool
```

## Keeping Dependencies Fresh

Outdated packages can be a security risk.

To check for updates:

```sh
npm outdated
```

To update everything:

```sh
npm update
```

To upgrade a specific package:

```sh
npm install some-package@latest
```

For a full refresh (and a potential breakage of your app):

```sh
rm -rf node_modules package-lock.json
npm install
```

## Bonus: `npx` - The Secret Weapon

Ever need to run a package **just once** without installing it?

Use `npx`:

```sh
npx create-react-app my-app
```

This downloads and runs the package without permanently installing it.

It’s like borrowing a tool from a friend instead of cluttering your garage.

<!-- 
## Wrapping It Up

- **NPM is life** when you're working with Node.js.
- The **node_modules** folder will always be bigger than expected.
- `npx` is your friend when you just need to run something once.
- Global packages are useful but don’t overuse them.
- Run `npm update` occasionally so your app doesn’t turn into a security nightmare.

And remember: **With great package power comes great dependency hell.** -->

***

## 🔑 Key Ideas

| Key Concept                  | Summary                                                         |
| ---------------------------- | --------------------------------------------------------------- |
| **NPM**                      | Node Package Manager for installing JavaScript packages.        |
| **Installing Packages**      | Use `npm install some-package` or `npm i some-package`.         |
| **Global Packages**          | Use `npm install -g some-tool` for system-wide tools.           |
| **Removing Packages**        | `npm uninstall some-package`.                                   |
| **Keeping Packages Updated** | Run `npm outdated` and `npm update`.                            |
| **Scripts**                  | Define tasks in `package.json` and run with `npm run <script>`. |
| **npx**                      | Run a package without installing it permanently.                |
| **node\_modules/**           | A black hole of dependencies.                                   |

***

## 📚 References

* [Official NPM Docs](https://docs.npmjs.com/)
* [Node.js Official Site](https://nodejs.org/)
* [NPM Trends](https://www.npmtrends.com/) (See how popular a package is)
* [NPX Guide](https://blog.npmjs.org/post/162869356040/introducing-npx-an-npm-package-runner)
