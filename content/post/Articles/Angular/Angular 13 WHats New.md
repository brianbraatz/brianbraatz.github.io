---
title: "Angular 13: What's New?"
description: WHats New in Angular 13?
slug: whats-new-in-angular-13
date: 2022-11-11
image: post/Articles/IMAGES/angular2.png
categories:
  - HTML
  - Angular
  - Typescript
  - Javascript
  - Web Development
tags:
  - Cheatsheet
  - Angular
  - Typescript
  - WebDevelopment
  - React
  - Javascript
draft: false
weight: 300
categories_ref:
  - HTML
  - Angular
  - Typescript
  - Javascript
  - Web Development
slug_calculated: https://brianbraatz.github.io/p/whats-new-in-angular-13
lastmod: 2025-03-14T16:40:18.387Z
---
Ah, Angular 13—the version where the Angular team decided to go full throttle on performance, developer experience, and saying goodbye to things that were holding us back.

***

## 🚀 New Features in Angular 13

### 1. No More View Engine (Goodbye, Old Friend)

Angular 13 completely removes the old View Engine and fully embraces Ivy. If you’ve been procrastinating on upgrading, now’s the time!

```json
{ "enableIvy": true }
```

### 2. Goodbye IE11 (No More Ancient Tech!)

Angular 13 drops support for Internet Explorer 11. This means no more worrying about outdated JS features for an almost extinct browser.

### 3. Faster Builds with Persistent Build Cache

Angular CLI 13 enables persistent build caching by default, making your builds significantly faster.

```bash
ng build --configuration production
```

### 4. Component API Enhancements

Creating dynamic components just got a whole lot easier with simplified API updates.

```typescript
const componentRef = viewContainerRef.createComponent(MyComponent);
```

### 5. Improved Angular Tests

The TestBed now does smarter cleanup between tests, making test runs faster and less memory-hungry.

```typescript
beforeEach(() => {
  TestBed.configureTestingModule({
    declarations: [MyComponent]
  }).compileComponents();
});
```

### 6. TypeScript 4.4+ Support

Angular 13 embraces TypeScript 4.4+, unlocking better type inference and stricter typing.

```typescript
const obj: Readonly<{ name: string }> = { name: "Angular" };
```

### 7. Inline Fonts Optimization

By default, Angular now inlines fonts in your production builds for faster loading times.

```json
{
  "optimization": true,
  "inlineFonts": true
}
```

### 8. Forms API Updates

Angular 13 removes support for "AbstractControl#parent" and introduces improvements to typed forms.

```typescript
const myForm = new FormGroup({
  name: new FormControl<string>(''),
});
```

***

## 📜 Angular Versions and Features

| Version        | Release Date   | Key Features                                                        |
| -------------- | -------------- | ------------------------------------------------------------------- |
| Angular 8      | 2019-05-28     | Differential loading, Ivy preview, Web Workers                      |
| Angular 9      | 2020-02-06     | Ivy by default, smaller bundles, improved testing                   |
| Angular 10     | 2020-06-24     | Stricter settings, TypeScript 3.9+, better warnings                 |
| Angular 11     | 2020-11-11     | Faster builds, HMR support, stricter types                          |
| Angular 12     | 2021-05-12     | View Engine removed, Ivy improvements, Webpack 5                    |
| **Angular 13** | **2021-11-03** | **No IE11, Faster Builds, Persistent Cache, Component API Updates** |

***

## 🔗 Reference Links

* [Angular 13 Official Docs](https://angular.io/)
* [Angular Wikipedia](https://en.wikipedia.org/wiki/Angular_\(web_framework\))
* [Angular GitHub](https://github.com/angular/angular)

***

## 📝 Key Ideas Summary

| Feature             | Description                              |
| ------------------- | ---------------------------------------- |
| View Engine Removed | Ivy is the only rendering engine now     |
| No More IE11        | Legacy browser support dropped           |
| Faster Builds       | Persistent caching improves build speeds |
| Component API       | Creating dynamic components simplified   |
| TypeScript 4.4+     | Stricter types, better inference         |
| Inline Fonts        | Faster loading for production builds     |
| Forms API Update    | More robust, typed form controls         |
