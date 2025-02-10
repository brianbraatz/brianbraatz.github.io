---
title: 10 Reasons JavaScript is WEIRD
description: Like really weird..
slug: 10-reasons-javascript-is-insane
date: 2009-09-15
image: post/Articles/IMAGES/watermelonguy-trimmed.jpg
categories:
  - Javascript
  - Web Development
tags:
  - JavaScript
  - Hoisting
  - Closures
  - Programming
  - WebDevelopment
draft: false
weight: 387
lastmod: 2025-02-09T22:27:22.038Z
---
# 10 Reasons JavaScript is Weird

JavaScript is **weird**. No, really. If you've ever written JavaScript, you know exactly what I'm talking about.

It’s the only language where **writing clean, predictable code is optional**.

It can turn seasoned developers into **detectives**, scouring Stack Overflow to figure out **why the heck `this` is suddenly `undefined`**.

Nice....

Nice and weird...

## 1. Dynamic Typing: Because Who Needs Consistency?

In JavaScript, a variable can be **any type at any time**. Declare a variable as a number, and **boom!** You can suddenly assign a string to it.

```javascript
let x = 10;  
x = "hello"; // JavaScript: "No problem!"
```

In C# or Java? Nah, they’ll throw errors at you like a **strict parent** catching you sneaking out at night. JavaScript, on the other hand, is like that **cool uncle** who lets you do whatever you want.

## 2. `var` is Function-Scoped, Not Block-Scoped (What?)

Most languages scope variables to the **nearest set of curly braces `{}`**. But in JavaScript, `var` only cares about **functions**.

```javascript
function test() {  
    if (true) {  
        var x = 10;  
    }  
    console.log(x); // 10 (still exists outside the block!)  
}  
```

In C#/Java, that variable **dies** at the end of the block. In JavaScript? Nope. It’s still hanging around like a bad memory.

## 3. `let` and `const` to the Rescue! (Sort of...)

JavaScript realized it messed up with `var`, so it introduced `let` and `const`—which **actually** follow block scoping.

```javascript
if (true) {  
    let x = 10;  
}  
console.log(x); // ReferenceError: x is not defined  
```

Finally, some sanity... but it’s **too late**. The `var` chaos is already in **millions** of legacy projects.

## 4. Hoisting: JavaScript Moves Your Code Around Like a Madman

JavaScript **hoists** your variables and functions to the **top of their scope** before executing.

```javascript
console.log(x); // undefined (not an error??)
var x = 5;
```

In most languages, this would **explode**. In JavaScript, it just silently **initializes `x` as `undefined`** and moves on. Why? 🤷

## 5. Implicit Global Variables: The "Oops, I Forgot `let`" Problem

JavaScript **doesn’t care** if you forget `var`, `let`, or `const`. It just **creates a global variable instead**.

```javascript
function foo() {  
    y = 20; // No declaration? No problem!  
}  
foo();  
console.log(y); // 20 (now a global variable!)  
```

C#/Java would **yell at you** for this. JavaScript? "Go ahead, make a mess!"

## 6. `this` Is the Most Unpredictable Thing Ever

In JavaScript, `this` changes depending on **how** a function is called.

```javascript
function test() {  
    console.log(this);  
}  
test(); // Logs `window` in browsers, `global` in Node.js  
```

In C#/Java, `this` **always** refers to the instance of the class. But JavaScript? Nope! Good luck debugging.

## 7. Closures: Functions That Remember the Past

JavaScript functions can "remember" the variables from the scope where they were created.

```javascript
function outer() {  
    let count = 0;  
    return function inner() {  
        count++;  
        console.log(count);  
    };  
}  
const counter = outer();  
counter(); // 1  
counter(); // 2  
```

C#/Java require **explicit classes** to do this.

## 8. JavaScript Lets You Mix Types Like a Mad Scientist

```javascript
let a = 10;  
a = "hello"; // JavaScript: "Sure, go ahead!"  
```

C#/Java: "Are you **INSANE**?"

## 9. Prototype-Based Inheritance: No Classes Needed!

Instead of **class-based inheritance** (like C#/Java), JavaScript has **prototypes**.

```javascript
function Person(name) {  
    this.name = name;  
}  
Person.prototype.sayHello = function () {  
    console.log(`Hi, I'm ${this.name}`);  
};  
let bob = new Person("Bob");  
bob.sayHello(); // Hi, I'm Bob  
```

JavaScript had no **real classes** until ES6. Before that? **Prototype wizardry.**

## 10. Strict Mode: The Fix That Came Too Late

JavaScript **fixed** some of its weirdness with `"use strict"` mode.

```javascript
"use strict";  
x = 10; // ERROR! Variable must be declared  
```

C#/Java? They’ve been **strict since day one**.

***

## Key Ideas that Make Javascript WEIRD

| Feature           | What Makes JavaScript Weird?                  |
| ----------------- | --------------------------------------------- |
| Dynamic Typing    | Variables can change types on the fly         |
| `var` Scope       | Function-scoped, not block-scoped             |
| `let` and `const` | Finally, proper block scoping                 |
| Hoisting          | Moves variable declarations to the top        |
| Implicit Globals  | Forget `let`? JavaScript makes it global      |
| `this`            | Changes depending on how a function is called |
| Closures          | Functions can "remember" variables            |
| No Type Safety    | No complaints when types change               |
| Prototypes        | No traditional class-based inheritance        |
| Strict Mode       | Fixes JavaScript’s weirdness (a little)       |

***

## References

* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow\_functions
* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict\_mode
* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/var
