---
title: JavaScript Throttling and Debouncing with Lodash ...
description: 
slug: javascript-throttling-debouncing-lodash
date: 2023-03-10
image: post/Articles/IMAGES/bouncingballs.png
categories:
  - JavaScript
  - Performance Optimization
  - Frontend Development
  - Lodash
tags:
  - JavaScript
  - Throttling
  - Debouncing
  - Lodash
  - Performance
  - Angular
  - React
  - Blazor
  - RxJS
draft: false
weight: 45
categories_ref:
  - JavaScript
  - Performance Optimization
  - Frontend Development
  - Lodash
lastmod: 2025-03-14T15:45:29.301Z
---
# JavaScript Throttling and Debouncing with Lodash and Other Methods

Modern web applications often suffer from **performance bottlenecks** caused by **frequent event triggers**.

Scrolling, resizing, keypresses, and API calls can **overwhelm the browser and server** if not optimized properly

. Enter **throttling and debouncing**—two essential techniques for **controlling the execution of functions**.

<!-- 
By the end of this guide, you’ll understand:
✅ **What throttling and debouncing are and their key differences**  
✅ **How to implement them using JavaScript, Angular, React, and Blazor**  
✅ **How Lodash simplifies throttling and debouncing**  
✅ **Other tools and libraries for handling performance optimizations**  

Let’s boost our web app’s performance! 🚀
-->

***

## **1. What Are Throttling and Debouncing?**

### **1.1 Throttling vs Debouncing: Key Differences**

| Technique      | Purpose                                                       | Example Use Cases                            |
| -------------- | ------------------------------------------------------------- | -------------------------------------------- |
| **Throttling** | Ensures a function is executed **at most once** in a set time | Scroll events, API polling, resize events    |
| **Debouncing** | Ensures a function executes **only after a delay**            | Search inputs, form validation, autocomplete |

🔹 **Throttling** is useful when you **want regular updates but not too frequently**.\
🔹 **Debouncing** is ideal when you **only need the final result after a user stops an action**.

***

## **2. Implementing Throttling in JavaScript**

### **2.1 Vanilla JavaScript Throttling**

```javascript
function throttle(func, delay) {
    let lastCall = 0;
    return function (...args) {
        const now = Date.now();
        if (now - lastCall >= delay) {
            lastCall = now;
            func.apply(this, args);
        }
    };
}

// Example: Throttling a scroll event
window.addEventListener("scroll", throttle(() => {
    console.log("Throttled Scroll Event");
}, 1000)); // Executes at most once per second
```

### **2.2 Throttling with Lodash**

[Lodash](https://lodash.com/) provides a built-in `_.throttle` function:

```javascript
const throttledFunction = _.throttle(() => {
    console.log("Lodash Throttled Function");
}, 1000);

window.addEventListener("scroll", throttledFunction);
```

🔹 **Why use Lodash?**

* Reduces **complexity**
* Supports **leading/trailing execution**
* Works with **UI interactions and API throttling**

#### **Customizing Lodash Throttle**

```javascript
const throttledFunction = _.throttle(() => {
    console.log("Throttled with leading and trailing execution");
}, 1000, { leading: true, trailing: false });
```

***

## **3. Implementing Debouncing in JavaScript**

### **3.1 Vanilla JavaScript Debouncing**

```javascript
function debounce(func, delay) {
    let timer;
    return function (...args) {
        clearTimeout(timer);
        timer = setTimeout(() => func.apply(this, args), delay);
    };
}

// Example: Debounce an input event
document.getElementById("search").addEventListener("input", debounce(() => {
    console.log("Debounced Input Event");
}, 500)); // Executes only after 500ms of inactivity
```

### **3.2 Debouncing with Lodash**

```javascript
const debouncedFunction = _.debounce(() => {
    console.log("Lodash Debounced Function");
}, 500);

document.getElementById("search").addEventListener("input", debouncedFunction);
```

🔹 **Lodash debouncing is optimized for performance** and supports leading/trailing execution.

***

## **4. Using Throttling and Debouncing in Frameworks**

### **4.1 Angular Example (Using RxJS)**

```typescript
import { Component } from '@angular/core';
import { debounceTime } from 'rxjs/operators';
import { Subject } from 'rxjs';

@Component({
  selector: 'app-root',
  template: `<input (input)="onSearch($event.target.value)" placeholder="Search">`
})
export class AppComponent {
  searchSubject = new Subject<string>();

  constructor() {
    this.searchSubject.pipe(debounceTime(500)).subscribe(value => {
      console.log("Debounced Search: ", value);
    });
  }

  onSearch(value: string) {
    this.searchSubject.next(value);
  }
}
```

🔹 **Uses RxJS `debounceTime()` to delay API requests while typing.**

***

### **4.2 React Example (Using Lodash Debounce)**

```jsx
import React, { useState, useCallback } from 'react';
import { debounce } from 'lodash';

const SearchComponent = () => {
    const [query, setQuery] = useState("");

    const handleSearch = useCallback(debounce((value) => {
        console.log("Debounced Search:", value);
    }, 500), []);

    return (
        <input 
            type="text" 
            onChange={(e) => { 
                setQuery(e.target.value); 
                handleSearch(e.target.value); 
            }} 
            placeholder="Search..." 
        />
    );
};

export default SearchComponent;
```

🔹 **Uses Lodash `debounce` inside a React `useCallback` to optimize API calls.**

***

### **4.3 Blazor Example (C# Implementation)**

```csharp
@code {
    private string SearchQuery;
    private Timer debounceTimer;

    private void OnSearchChanged(ChangeEventArgs e) {
        if (debounceTimer != null) debounceTimer.Dispose();
        debounceTimer = new Timer(ExecuteSearch, null, 500, Timeout.Infinite);
    }

    private void ExecuteSearch(object state) {
        Console.WriteLine($"Searching for: {SearchQuery}");
    }
}

<input @bind="SearchQuery" @oninput="OnSearchChanged" placeholder="Search..." />
```

🔹 **Implements a manual debounce using `Timer` in Blazor.**

***

## **5. Alternatives to Lodash**

| Library                     | Features                                         |
| --------------------------- | ------------------------------------------------ |
| **RxJS**                    | Debounce, Throttle, Observable Streams           |
| **Underscore.js**           | Similar to Lodash but lighter                    |
| **Throttle-debounce (npm)** | Lightweight standalone debounce/throttle library |

### **Example: RxJS Throttling in JavaScript**

```javascript
import { fromEvent } from 'rxjs';
import { throttleTime } from 'rxjs/operators';

const button = document.getElementById('button');

fromEvent(button, 'click')
  .pipe(throttleTime(1000))
  .subscribe(() => console.log('Throttled Click Event'));
```

***

## **6. Final Thoughts: Best Practices**

✅ **Use Throttling for continuous events (scroll, resize, API polling)**\
✅ **Use Debouncing for delayed interactions (search, form validation)**\
✅ **Lodash simplifies both techniques with `_.throttle` and `_.debounce`**\
✅ **Use framework-specific solutions in Angular, React, and Blazor**

<!-- 
By mastering **throttling and debouncing**, we **enhance performance, prevent unnecessary processing, and create smoother user experiences**. 🚀
-->

***

## **Reference Links**

* [Lodash Throttle Documentation](https://lodash.com/docs/4.17.21#throttle)
* [Lodash Debounce Documentation](https://lodash.com/docs/4.17.21#debounce)
* [RxJS Throttling & Debouncing](https://rxjs.dev/guide/operators)
