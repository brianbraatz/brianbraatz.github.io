---
title: Debugging Obsidian Javascript Plugins
description: Tips and tricks for plugin debugging in Obsidian
slug: debug-obsidian-plugins
date: 2023-11-29
image: post/Articles/IMAGES/obsidian.webp
categories:
  - Javascript
  - Obsidian
  - PlugIn Development
tags:
  - Obsidian
  - Plugins
  - Debugging
  - Developer
  - Console
  - JavaScript
  - Typescript
draft: false
weight: 70
categories_ref:
  - Javascript
  - Obsidian
  - PlugIn Development
slug_calculated: https://brianbraatz.github.io/p/debug-obsidian-plugins
lastmod: 2025-03-14T16:40:20.616Z
---
# Debugging Obsidian Plugins Like a Pro (Or At Least Like Someone Who Knows Where the Console Is)

So, I finally decided to create my own Obsidian plugin.

Early on i got hit with this cryptic error message  `Uncaught (in promise) TypeError: Cannot read properties of null (reading 'path')`.

So I had to figure out how to debug this, in Obsidian, and decied to record the steps here for posterity .

***

## Step 1: Open the Developer Console (The Magic Window)

Before we start debugging, let's get access to our **best friend**—the Developer Console! Here's how you open it:

* On **Windows/Linux**: `Ctrl + Shift + I`
* On **Mac**: `Cmd + Option + I`

You'll now have a fancy panel open with tabs like **Elements**, **Console**, **Sources**, and more. If you suddenly feel like a hacker in a Hollywood movie—you're doing it right. 😎

Detailed explanation of the Developer Console, check out [Wikipedia page](https://en.wikipedia.org/wiki/Web_developer_tools).

If you have done alot of deb dev-it will look familar.

***

## Step 2: Load Your Plugin Without Breaking Everything

If you're developing a plugin, you need to manually install and load it.

Here’s how:

1. Navigate to your plugin’s development folder.
2. Copy your plugin into `.obsidian/plugins/` inside your vault.
3. Head to **Settings** > **Community Plugins** and enable your plugin.

💡 **Pro tip:** Use hot reloading if supported—it'll save you from a million restarts!

[Obsidian Plugin Development Docs](https://publish.obsidian.md/dev/).

***

## Step 3: Console.log()—Your Best Friend (or Worst Enemy)

When in doubt, `console.log()` it out!

Add logs in your plugin's JavaScript code to debug what's going on:

```javascript
console.log("Hello, Debugging World!");
console.log(tp.file ? tp.file.path : "No active file found.");
```

Then, check the **Console tab** to see your logs. If nothing shows up, you probably forgot to save or restart the plugin—classic dev mistake. 🙃

More on `console.log()`: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/console/log).

***

## Step 4: Set Breakpoints Like a Boss

Want to pause execution and inspect variables **mid-run**? Use breakpoints:

1. Open the **Sources** tab.
2. Find your plugin’s JavaScript file (usually under `file://` paths).
3. Click the line number where you want to pause execution.
4. Reload the plugin and trigger the function.

BOOM! Your code will stop, letting you inspect variables. No more blind debugging. 🔥

***

## Step 5: Network Tab—Because API Calls Are Sneaky

If your plugin interacts with APIs, head over to the **Network tab**. Here, you can:

* Monitor API requests.
* Check response payloads.
* Debug slow calls and errors.

For more details: [Google's Web Dev Tools Guide](https://developer.chrome.com/docs/devtools/network/).

***

## Step 6: Debugging Obsidian-Specific Issues

Obsidian has its own set of quirks. Use these built-in objects to inspect its state:

```javascript
console.log(app.workspace.getActiveFile());
console.log(app.metadataCache);
console.log(app.vault);
```

These can help you understand what’s happening under the hood. More info: [Obsidian API Docs](https://github.com/obsidianmd/obsidian-api).

***

## Step 7: Fixing the "Cannot Read Properties of Null" Error

This error usually means Templater is trying to access a file that **doesn't exist or isn't active**. Fix it with a simple check:

```javascript
if (tp.file && tp.file.path) {
    console.log(`File path: ${tp.file.path}`);
} else {
    console.log("No active file found.");
}
```

## References and More Info

* [Obsidian Plugin Development Docs](https://publish.obsidian.md/dev/)
* [Chrome DevTools Documentation](https://developer.chrome.com/docs/devtools/)
* [MDN Console.log()](https://developer.mozilla.org/en-US/docs/Web/API/console/log)
* [Wikipedia: Web Developer Tools](https://en.wikipedia.org/wiki/Web_developer_tools)
* [Obsidian API Documentation](https://github.com/obsidianmd/obsidian-api)

Happy debugging, and may your plugins be bug-free! 🚀
