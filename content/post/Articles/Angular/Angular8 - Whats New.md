---
title: Angular 8 - Whats New?
description: Its like christmas for web UI devs...
slug: whats-new-in-angular-8
date: 2022-10-10
image: post/Articles/IMAGES/angular2.png
categories:
  - HTML
  - Angular
  - Typescript
  - Javascript
  - Web Development
tags:
  - Angular
  - Typescript
  - WebDevelopment
  - Javascript
draft: false
weight: 100
lastmod: 2025-02-19T10:50:47.186Z
---
# Whats New in Angular 8?

Ah, Angular 8!

## The version where things get a bit fancier, a little faster, and developers collectively sighed in relief at some much-needed improvements.

## 🚀 New Features in Angular 8

### 1. Differential Loading of JavaScript

Angular 8 introduced differential loading, which means your modern browsers get modern JS, while older browsers get the legacy stuff. This improves performance significantly.

```html
<script type="module" src="modern-bundle.js"></script>
<script nomodule src="legacy-bundle.js"></script>
```

### 2. Lazy Loading with Dynamic Imports

Previously, lazy loading used a string-based syntax. Now, it's more TypeScript-y and supports dynamic imports!

```typescript
const routes: Routes = [
  { path: 'lazy', loadChildren: () => import('./lazy/lazy.module').then(m => m.LazyModule) }
];
```

### 3. Web Workers for Performance

Need to offload heavy computations to a separate thread? Web Workers are officially supported now!

```bash
ng generate web-worker my-worker
```

### 4. Bazel Support (Experimental)

Bazel allows faster incremental builds and better dependency management. It’s still experimental but promising!

```json
"builders": {
  "build": "@angular/bazel"
}
```

### 5. Ivy Rendering Engine (Still in Preview)

Ivy was getting polished in Angular 8 but wasn’t the default yet. It improves bundle size and debugging!

```typescript
{ "enableIvy": true }
```

### 6. Support for TypeScript 3.4+

Angular 8 brings support for the latest TypeScript goodies, like const assertions:

```typescript
const numbers = [10, 20, 30] as const;
```

### 7. Location Strategies Improved

Base href handling got better for lazy-loaded modules!

```typescript
providers: [{ provide: APP_BASE_HREF, useValue: '/' }]
```

***

## 📜 Angular Versions and Features

| Version       | Release Date   | Key Features                                                |
| ------------- | -------------- | ----------------------------------------------------------- |
| Angular 2     | 2016-09-14     | Component-based architecture, TypeScript adoption           |
| Angular 4     | 2017-03-23     | Smaller bundles, better animations, improved AOT            |
| Angular 5     | 2017-11-01     | Build optimizer, HttpClient, Service workers                |
| Angular 6     | 2018-05-04     | Angular Elements, Tree-shakable providers, CLI improvements |
| Angular 7     | 2018-10-18     | CLI prompts, better performance, Virtual Scrolling          |
| **Angular 8** | **2019-05-28** | **Differential loading, Ivy preview, Web Workers**          |

<!-- 
| Angular 9  | 2020-02-06  | Ivy by default, smaller bundles, improved testing |
| Angular 10 | 2020-06-24  | Stricter settings, TypeScript 3.9+, better warnings |
| Angular 11 | 2020-11-11  | Faster builds, HMR support, stricter types |
| Angular 12 | 2021-05-12  | View Engine removed, Ivy improvements, Webpack 5 |
| Angular 13 | 2021-11-03  | No more IE11, Angular Test improvements, faster builds |
| Angular 14 | 2022-06-02  | Typed forms, Standalone components, more CLI power |
| Angular 15 | 2022-11-16  | Directive Composition API, better performance |
| Angular 16 | 2023-05-03  | Reactivity model updates, Signal API |
-->

***

## 🔗 Reference Links

* [Angular 8 Official Docs](https://angular.io/)
* [Angular Wikipedia](https://en.wikipedia.org/wiki/Angular_\(web_framework\))
* [Angular GitHub](https://github.com/angular/angular)

***

## 📝 Key Ideas Summary

| Feature              | Description                                                |
| -------------------- | ---------------------------------------------------------- |
| Differential Loading | Modern JS for modern browsers, legacy JS for old ones      |
| Lazy Loading         | Uses dynamic imports, making it more modular               |
| Web Workers          | Allows background processing for performance               |
| Bazel Support        | Faster builds, better dependency management (experimental) |
| Ivy Rendering Engine | New, lightweight rendering engine (preview in Angular 8)   |
| TypeScript 3.4+      | Supports new TypeScript features                           |
| Location Strategy    | Improved handling of base hrefs                            |
