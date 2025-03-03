---
title: PrimeNG with Angular in a Nutshell
description: 
slug: primeng-with-angular
date: 2017-08-14
image: post/Articles/IMAGES/primeng.png
categories:
  - Angular
  - PrimeNG
  - UI Components
  - Frontend
tags:
  - Angular
  - Primeng
  - Ui components
  - Frontend
draft: "False"
weight: "452"
lastmod: 2025-03-02T23:05:12.006Z
---
<!-- 
# PrimeNG with Angular in a Nutshell 🚀

Alright, folks, today we're diving into **PrimeNG**—Angular's flashy UI component library that makes your frontend look less like a 90s HTML website and more like a modern web app. 😎

PrimeNG gives you **buttons, tables, dropdowns, dialogs, charts, and more**—all styled, interactive, and ready to use without you pulling your hair out over CSS.

So, buckle up as we break down **PrimeNG** with some **fun, code examples, and just a dash of sarcasm**. 🤓 -->

***

## 🎯 Why Use PrimeNG?

1. **Ready-made UI components** – Why reinvent the wheel when PrimeNG gives you one that spins smoothly?
2. **Theming & Styling** – Comes with pre-built themes so your app doesn't look like a default Bootstrap disaster.
3. **Highly Customizable** – Want a pink button with rounded edges and a shadow? No problem.
4. **Active Community & Support** – Unlike some abandoned libraries, this one actually gets updates.

***

## 🚀 Setting Up PrimeNG in Angular

### Step 1: Install PrimeNG and PrimeIcons

Run this command in your Angular project:

```sh
npm install primeng primeicons
```

Also, install **PrimeFlex** for responsive layouts (optional but recommended):

```sh
npm install primeflex
```

### Step 2: Import PrimeNG Styles

In your `angular.json`, add the styles:

```json
"styles": [
"node_modules/primeng/resources/themes/lara-light-blue/theme.css",
"node_modules/primeng/resources/primeng.min.css",
"node_modules/primeicons/primeicons.css"
]
```

Or, if you're using **SCSS**, you can import them directly in `styles.scss`:

```scss
@import "node_modules/primeng/resources/themes/lara-light-blue/theme.css";
@import "node_modules/primeng/resources/primeng.min.css";
@import "node_modules/primeicons/primeicons.css";
```

***

## 🛠️ Using PrimeNG Components

Now for the fun part!

Let's slap some PrimeNG components into an Angular component.

### 🏆 Button Component

```html
<button pButton type="button" label="Click Me" class="p-button-success"></button>
```

That single line of HTML gives you a **styled button** with a success theme!

No need to battle CSS.

### 📑 Table Component

Want to display data like a pro?

Use the `p-table` component.

```html
<p-table [value]="users" [paginator]="true" [rows]="5">
<ng-template pTemplate="header">
<tr>
<th>Name</th>
<th>Email</th>
<th>Role</th>
</tr>
</ng-template>
<ng-template pTemplate="body" let-user>
<tr>
<td>{{ user.name }}</td>
<td>{{ user.email }}</td>
<td>{{ user.role }}</td>
</tr>
</ng-template>
</p-table>
```

And in your `app.component.ts`:

```ts
users = [
{ name: "Alice", email: "alice@example.com", role: "Admin" },
{ name: "Bob", email: "bob@example.com", role: "User" },
{ name: "Charlie", email: "charlie@example.com", role: "Moderator" }
];
```

### 🎭 Dialog Component (Modals)

Need a **popup dialog** to pester users with confirmation messages?

```html
<p-dialog header="Confirm" [(visible)]="display" [modal]="true">
<p>Are you sure you want to proceed?</p>
<p-footer>
<button pButton type="button" label="Yes" class="p-button-success" (click)="confirm()"></button>
<button pButton type="button" label="No" class="p-button-danger" (click)="display=false"></button>
</p-footer>
</p-dialog>
<button pButton type="button" label="Show Dialog" (click)="display=true"></button>
```

And in your component:

```ts
display = false;
confirm() {
console.log("User clicked Yes!");
this.display = false;
}
```

Boom!

You’ve got a **dialog box** without wrestling with JavaScript modals.

***

## 🎨 Theming & Customization

PrimeNG has **themes** like Lara, Bootstrap, and Fluent.

You can switch themes by changing the CSS import in `angular.json`.

Want a **dark mode**?

Just change the theme:

```json
"styles": [
"node_modules/primeng/resources/themes/lara-dark-blue/theme.css"
]
```

You can also customize themes using CSS variables.

Example:

```scss
:root {
--surface-card: #222;
--text-color: #fff;
}
```

Now your app looks sleek and modern. 😍

***

## 🔥 Wrapping Up

PrimeNG **saves you tons of time** when building Angular apps.

It gives you a **ton of components**, a **beautiful UI**, and **easy customization**.

🚀 Want to make your Angular app look amazing? **Use PrimeNG!**

***

## 📌 Key Ideas

| Topic        | Summary                                                   |
| ------------ | --------------------------------------------------------- |
| Why PrimeNG? | Pre-built UI components, theming, and customization       |
| Installation | `npm install primeng primeicons`                          |
| Basic Usage  | Use components like `p-button`, `p-table`, and `p-dialog` |
| Theming      | Change themes and use CSS variables for custom styles     |

***

## 🔗 References

* [PrimeNG Official Docs](https://www.primefaces.org/primeng/)
* [Angular Official Docs](https://angular.io/docs)
* [PrimeFlex Docs](https://www.primefaces.org/primeflex/)

<!-- 
---

There you go!

A **quick and fun** rundown on **PrimeNG with Angular**.

Hope you enjoyed it! 🎉🔥 -->
