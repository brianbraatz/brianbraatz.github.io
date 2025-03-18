---
title: How to Make Web UIs That Are Responsive, Accessible, and Cross-Browser Compatible
description: WAI-ARIA Compatible
slug: web-uis-responsive-accessible
date: 2018-07-14
image: post/Articles/IMAGES/29.jpg
categories:
  - Web Development
  - UI/UX
  - Accessibility
  - WAI-ARIA
tags:
  - Web Development
  - Ui
  - Ux
  - Accessibility
  - Responsive Design
  - Cross-Browser Compatibility
  - Css
  - Html
  - Javascript
draft: false
weight: 563
categories_ref:
  - Web Development
  - UI/UX
  - Accessibility
  - WAI-ARIA
slug_calculated: https://brianbraatz.github.io/p/how-to-make-web-uis-that-are-responsive-accessible-and-cross-browser-compatible
lastmod: 2025-03-18T22:57:27.393Z
---
# How to Make Web UIs That Are Responsive, Accessible, and Cross-Browser Compatible

Building a web UI that works for everyone is like throwing a party where all your friends, their grandmas, and that one cousin who still uses Internet Explorer all feel welcome. It’s an art and a science (with a little bit of wizardry thrown in).

Let’s break it down into three main ingredients: responsiveness, accessibility, and cross-browser compatibility.

<!-- If you get these right, your UI will be smoother than a well-aged whiskey. -->

***

## 1. Responsive Design: Make Your UI Fit All Screens (Even That Fridge Display)

### **Use a Mobile-First Approach**

Design for the smallest screen first, then scale up. If your UI looks good on a tiny phone, it’ll be a breeze to expand it for bigger screens.

```css
body {
    font-size: 16px;
}

@media (min-width: 768px) {
    body {
        font-size: 18px;
    }
}

@media (min-width: 1024px) {
    body {
        font-size: 20px;
    }
}
```

### **Use Flexible Grid Layouts (CSS Grid and Flexbox FTW)**

Tables are for data. Flexbox and CSS Grid are for layouts. No more `<div>` soup!

```css
.container {
    display: grid;
    grid-template-columns: 1fr 1fr 1fr;
    gap: 10px;
}
```

### **Images and Videos Shouldn’t Break Your Layout**

Make sure they scale properly and don’t overflow.

```css
img, video {
    max-width: 100%;
    height: auto;
}
```

### **Avoid Fixed Heights (Seriously, Just Don’t)**

Let content breathe. Fixed heights will betray you when content grows taller than expected.

***

## 2. Accessibility: Because Everyone Deserves a Great UX (Not Just Tech Bros)

### **Use Semantic HTML (Please, No More `<div>` Hell)**

Proper HTML structure makes life easier for screen readers and SEO.

```html
<header>
    <h1>My Awesome Website</h1>
</header>
<nav>
    <ul>
        <li><a href="#home">Home</a></li>
        <li><a href="#about">About</a></li>
    </ul>
</nav>
```

### **Add ARIA Attributes (But Don’t Overdo It)**

ARIA (`Accessible Rich Internet Applications`) helps make UIs better for assistive technologies.

```html
<button aria-label="Close menu">&times;</button>
```

### **Make Sure Everything is Keyboard Accessible**

If someone can’t navigate your UI without a mouse, you’ve got a problem. Test with `Tab` and `Enter` keys.

### **Color Contrast Matters (No Light Gray on White, Please!)**

Use tools like [WebAIM Contrast Checker](https://webaim.org/resources/contrastchecker/) to ensure text is readable.

### **Don’t Forget `alt` Text for Images**

```html
<img src="dog.jpg" alt="A golden retriever playing in the park">
```

***

## 3. Cross-Browser Compatibility: Making It Work Everywhere (Even on Internet Explorer, If You Must)

### **Use Feature Detection, Not Browser Detection**

Instead of checking for browsers, check if they support what you need.

```javascript
if ('grid' in document.body.style) {
    console.log("CSS Grid is supported!");
} else {
    console.log("Fallback needed!");
}
```

### **Normalize Styles with a CSS Reset**

Every browser has its own default styles. Normalize them!

```css
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}
```

### **Use Progressive Enhancement and Graceful Degradation**

Start with a simple, functional version, then add fancy features for modern browsers.

### **Test on Multiple Browsers (Yes, Even Edge)**

Use tools like BrowserStack or Sauce Labs to test on different devices and browsers.

### **Don’t Rely on Vendor Prefixes Forever**

Yes, there was a time when `-webkit-`, `-moz-`, and `-ms-` were needed, but check if they're still relevant before using them.

```css
.button {
    background: linear-gradient(to right, blue, red);
}
```

***

<!-- 
## Wrapping Up: The Holy Grail of UI Development

To make a UI that actually works for people (and not just your dev friends who always have the latest Chrome update), keep these three things in mind:

- **Responsive Design** ensures it works on any screen size.
- **Accessibility** makes sure everyone can use it.
- **Cross-Browser Compatibility** prevents it from breaking on random browsers.

So, next time you're designing a UI, think about that one user on a Windows XP machine with Internet Explorer 8. If they can still use your site, you’re a legend.

Happy coding! -->

***

## Key Ideas

| Key Idea                    | Summary                                                                 |
| --------------------------- | ----------------------------------------------------------------------- |
| Responsive Design           | Use flexible layouts, media queries, and fluid images.                  |
| Accessibility               | Follow semantic HTML, keyboard navigation, and color contrast rules.    |
| Cross-Browser Compatibility | Test in multiple browsers, use feature detection, and normalize styles. |

***

## References

* [MDN Web Docs: Responsive Design](https://developer.mozilla.org/en-US/docs/Learn/CSS/CSS_layout/Responsive_Design)
* [WebAIM Contrast Checker](https://webaim.org/resources/contrastchecker/)
* [Can I Use?](https://caniuse.com/)
* [W3C Accessibility Guidelines](https://www.w3.org/WAI/WCAG21/quickref/)
