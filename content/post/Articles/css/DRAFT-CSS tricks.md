---
title: DRAFT
description: 
slug: DRAFT-cheatsheet
date: 2014-05-06
image: post/Articles/IMAGES/basicarticleplaceholder.png
categories: 
tags:
  - Cheatsheet
  - css
  - WebDevelopment
weight: 30
draft: true
lastmod: 2025-01-31T17:35:50.057Z
---
5 Crazy, Weird, Unusual, and Strange Things You Can Do in Pure CSS

# 25 Crazy, Weird, Unusual, and Strange Things You Can Do in Pure CSS

CSS, traditionally used for styling web pages, has evolved to enable developers to create intricate designs and animations without any graphics or scripting.

Here are 25 unconventional, strange, and unusual things you can achieve using pure CSS, each accompanied by a code example and a reference to similar work.

## 1. Pure CSS Portraits

Artists have recreated famous portraits using only CSS, showcasing the language's capability to render complex images.

**Inspired Work:** [You Won't Believe That These Portraits Were Created With CSS](https://mymodernmet.com/css-art-diana-smith/)

**HTML & CSS Example:**

```html
<div class="portrait"></div>
```

```css
.portrait {
  width: 200px;
  height: 300px;
  background: linear-gradient(to bottom, #f0c27b, #4b1248);
  border-radius: 50% 50% 0 0;
  position: relative;
}
.portrait::before {
  content: '';
  position: absolute;
  top: 50px;
  left: 50px;
  width: 100px;
  height: 100px;
  background: #fff;
  border-radius: 50%;
}
```

![CSS Portrait](https://mymodernmet.com/wp/wp-content/uploads/2019/01/css-art-diana-smith-1.jpg)

***

## 2. CSS-Only 3D Cube

Create a rotating 3D cube using CSS transformations.

**Inspired Work:** [CSS 3D Cube](https://1stwebdesigner.com/css-3d-cube/)

**HTML & CSS Example:**

```html
<div class="cube">
  <div class="face front"></div>
  <div class="face back"></div>
  <div class="face left"></div>
  <div class="face right"></div>
  <div class="face top"></div>
  <div class="face bottom"></div>
</div>
```

```css
.cube {
  position: relative;
  width: 200px;
  transform-style: preserve-3d;
  animation: rotate 5s infinite linear;
}
.face {
  position: absolute;
  width: 200px;
  height: 200px;
  background: rgba(255, 255, 255, 0.9);
  border: 1px solid #ccc;
}
.front  { transform: translateZ(100px); }
.back   { transform: rotateY(180deg) translateZ(100px); }
.left   { transform: rotateY(-90deg) translateZ(100px); }
.right  { transform: rotateY(90deg) translateZ(100px); }
.top    { transform: rotateX(90deg) translateZ(100px); }
.bottom { transform: rotateX(-90deg) translateZ(100px); }
@keyframes rotate {
  from { transform: rotateY(0deg); }
  to { transform: rotateY(360deg); }
}
```

![CSS 3D Cube](https://1stwebdesigner.com/wp-content/uploads/2020/08/css-3d-cube.jpg)

***

## 3. Animated CSS Loader

Design a dynamic loading animation purely with CSS.

**Inspired Work:** [CSS Loader](https://css-tricks.com/css-only-loaders/)

**HTML & CSS Example:**

```html
<div class="loader"></div>
```

```css
.loader {
  width: 50px;
  height: 50px;
  border: 5px solid #f3f3f3;
  border-top: 5px solid #3498db;
  border-radius: 50%;
  animation: spin 1s linear infinite;
}
@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
```

![CSS Loader](https://css-tricks.com/wp-content/uploads/2013/07/loader.gif)

***

## 4. CSS-Only Accordion Menu

Implement an interactive accordion menu without JavaScript.

**Inspired Work:** [Pure CSS Accordion](https://www.w3schools.com/howto/howto_js_accordion.asp)

**HTML & CSS Example:**

```html
<div class="accordion">
  <input type="checkbox" id="section1">
  <label for="section1">Section 1</label>
  <div class="content">Content for section 1.</div>
  <input type="checkbox" id="section2">
  <label for="section2">Section 2</label>
  <div class="content">Content for section 2.</div>
</div>
```

```css
.accordion input {
  display: none;
}
.accordion label {
  display: block;
  background: #3498db;
  color: #fff;
  padding: 10px;
  cursor: pointer;
}
.accordion .content {
  display: none;
  padding: 10px;
  background: #f3f3f3;
}
.accordion input:checked + label + .content {
  display: block;
}
```

![CSS Accordion](https://1stwebdesigner.com/wp-content/uploads/2020/08/css-accordion.jpg)

***

## 5. CSS-Only Tooltip

Display tooltips using only CSS.

**Inspired Work:** [Pure CSS Tooltips](https://css-tricks.com/css-tooltips/)

**HTML & CSS Example:**

```html
<button class="tooltip">Hover me
  <span class="tooltiptext">Tooltip text</span>
</button>
```

```css
.tooltip {
  position: relative;
  display: inline-block;
  cursor: pointer;
}
.tooltip .tooltiptext {
  visibility: hidden;
  width: 120px;
  background-color: black;
  color: #fff;
  text-align: center;
  padding: 5px;
  border-radius: 5px;
  position: absolute;
  z-index: 1;
  bottom: 100%;
  left: 50%;
  margin-left: -60px;
}
.tooltip:hover .tooltiptext {
  visibility: visible;
}
```

![CSS Tooltip](https://css-tricks.com/wp-content/uploads/2013/07/css-tooltips.gif)

***

# 25 Crazy, Weird, Unusual, and Strange Things You Can Do in Pure CSS

CSS, traditionally used for styling web pages, has evolved to enable developers to create intricate designs and animations without any graphics or scripting.

Here are 25 unconventional, strange, and unusual things you can achieve using pure CSS, each accompanied by a code example and a reference to similar work.

## 1. Pure CSS Portraits

Artists have recreated famous portraits using only CSS, showcasing the language's capability to render complex images.

**Inspired Work:** [You Won't Believe That These Portraits Were Created With CSS](https://mymodernmet.com/css-art-diana-smith/)

### Example:

```html
<div style="width: 200px; height: 300px; background: linear-gradient(to bottom, #f0c27b, #4b1248); border-radius: 50% 50% 0 0; position: relative;">
  <div style="position: absolute; top: 50px; left: 50px; width: 100px; height: 100px; background: #fff; border-radius: 50%;"></div>
</div>
```

![CSS Portrait](https://mymodernmet.com/wp/wp-content/uploads/2019/01/css-art-diana-smith-1.jpg)

***

## 2. CSS-Only 3D Cube

Create a rotating 3D cube using CSS transformations.

**Inspired Work:** [CSS 3D Cube](https://1stwebdesigner.com/css-3d-cube/)

### Example:

```html
<div style="width: 200px; height: 200px; position: relative; transform-style: preserve-3d; animation: rotate 5s infinite linear;">
  <div style="position: absolute; width: 200px; height: 200px; background: rgba(255, 255, 255, 0.9); border: 1px solid #ccc; transform: translateZ(100px);"></div>
</div>
<style>
@keyframes rotate {
  from { transform: rotateY(0deg); }
  to { transform: rotateY(360deg); }
}
</style>
```

![CSS 3D Cube](https://1stwebdesigner.com/wp-content/uploads/2020/08/css-3d-cube.jpg)

***

## 3. Animated CSS Loader

Design a dynamic loading animation purely with CSS.

**Inspired Work:** [CSS Loader](https://css-tricks.com/css-only-loaders/)

### Example:

```html
<div style="width: 50px; height: 50px; border: 5px solid #f3f3f3; border-top: 5px solid #3498db; border-radius: 50%; animation: spin 1s linear infinite;"></div>
<style>
@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
</style>
```

![CSS Loader](https://css-tricks.com/wp-content/uploads/2013/07/loader.gif)

***

## 4. CSS-Only Accordion Menu

Implement an interactive accordion menu without JavaScript.

**Inspired Work:** [Pure CSS Accordion](https://www.w3schools.com/howto/howto_js_accordion.asp)

### Example:

```html
<div>
  <input type="checkbox" id="section1" style="display: none;">
  <label for="section1" style="display: block; background: #3498db; color: #fff; padding: 10px; cursor: pointer;">Section 1</label>
  <div style="display: none; padding: 10px; background: #f3f3f3;" id="content1">Content for section 1.</div>

  <input type="checkbox" id="section2" style="display: none;">
  <label for="section2" style="display: block; background: #3498db; color: #fff; padding: 10px; cursor: pointer;">Section 2</label>
  <div style="display: none; padding: 10px; background: #f3f3f3;" id="content2">Content for section 2.</div>
</div>
<style>
input:checked + label + div { display: block; }
</style>
```

![CSS Accordion](https://1stwebdesigner.com/wp-content/uploads/2020/08/css-accordion.jpg)

***

## 5. CSS-Only Tooltip

Display tooltips using only CSS.

**Inspired Work:** [Pure CSS Tooltips](https://css-tricks.com/css-tooltips/)

### Example:

```html
<button style="position: relative; display: inline-block; cursor: pointer;">Hover me
  <span style="visibility: hidden; width: 120px; background-color: black; color: #fff; text-align: center; padding: 5px; border-radius: 5px; position: absolute; z-index: 1; bottom: 100%; left: 50%; margin-left: -60px;">Tooltip text</span>
</button>
<style>
button:hover span { visibility: visible; }
</style>
```

![CSS Tooltip](https://css-tricks.com/wp-content/uploads/2013/07/css-tooltips.gif)

***

*Stay tuned for the next 20 crazy things you can do with pure CSS!*

Here are some interesting sites that showcase unusual and creative things you can do with CSS:

1. **[CSS-Tricks: 50+ Interesting CSS Properties & Values](https://css-tricks.com/lets-look-50-interesting-css-properties-values/)** - This article explores a variety of unique CSS properties and values, including some experimental ones.
2. **[Hongkiat: 20 Cool Things You Won’t Believe Were Built Using CSS](https://www.hongkiat.com/blog/built-with-css/)** - This list features creative projects built purely with CSS, such as animated characters and interactive elements.
3. **[Web Designer Depot: 11 Experimental CSS Projects That’ll Blow Your Mind](https://www.webdesignerdepot.com/11-experimental-css-projects-thatll-blow-your-mind/)** - This collection highlights experimental CSS projects that showcase the true power of CSS.

These resources should give you plenty of inspiration for your article on weird and unusual things you can do with CSS. If you need more specific examples or code snippets, feel free to ask!
