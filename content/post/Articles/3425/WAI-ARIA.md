---
title: Understanding WAI-ARIA Authoring
description: "WAI-ARIA (Web Accessibility Initiative - Accessible Rich Internet Applications) "
slug: wai-aria-guidelines
date: 2018-06-22
image: post/Articles/IMAGES/w3c.png
categories:
  - Accessibility
  - Web Development
  - WAI-ARIA
tags:
  - Accessibility
  - WAI-ARIA
  - Web Development
  - ARIA Roles
  - Assistive Technology
draft: false
weight: 680
categories_ref:
  - Accessibility
  - Web Development
  - WAI-ARIA
slug_calculated: https://brianbraatz.github.io/p/wai-aria-guidelines
lastmod: 2025-03-14T16:40:15.097Z
---
## Why Should You Care About WAI-ARIA?

Alright, let’s be real. Web accessibility is important, but it’s also one of those things developers often get wrong.

WAI-ARIA (Web Accessibility Initiative - Accessible Rich Internet Applications) is a fancy name for a set of rules that make sure people using assistive technologies—like screen readers—can actually use your web app.

Sounds great, right?

Well, it is—**if you use it correctly**. The problem? **Bad ARIA is worse than no ARIA.** So let’s go over how to use it without making things worse.

## The Golden Rules of ARIA

Before we get into the nitty-gritty, keep these simple rules in mind:

1. **Use HTML First**

   * If you can use a native HTML element (`<button>`, `<input>`, `<label>`), do that instead of ARIA.
   * HTML is already accessible—ARIA is just a backup plan.

2. **Don't Mess With Native Accessibility**

   * Some elements come with built-in magic.
   * If you set `role="presentation"` or `role="none"` on something important, you might break it.

3. **Keyboard Navigation is a Must**

   * If people can’t use your app with just a keyboard, **you're doing it wrong**.
   * Make sure every interactive element can be reached and activated without a mouse.

4. **Control Focus Like a Pro**

   * Users should never get lost.
   * Use `tabindex` wisely and make sure focus moves logically through your interface.

5. **Keep ARIA Up to Date**

   * If something changes (like a dropdown opening), **update your ARIA attributes** so assistive tech knows what’s happening.

## How to Use ARIA Without Breaking Everything

### ARIA Roles: What Are They?

Roles tell screen readers **what an element is supposed to be**. Here are some you’ll actually use:

* `role="button"` → For when you’re making a button out of something that isn’t a `<button>` (but… why not just use a `<button>`?)
* `role="alert"` → When you need something read out loud ASAP
* `role="dialog"` → For modals and popups
* `role="tabpanel"` → For making accessible tabs

#### Example:

```html
<div role="button" tabindex="0" onclick="doSomething()">Click Me</div>
```

### ARIA States and Properties: Keeping Users in the Loop

ARIA states and properties tell assistive tech **what’s going on** with an element.

#### Example: Making a Toggle Button That Actually Works

```html
<button aria-pressed="false" onclick="toggleButton(this)">Toggle</button>
<script>
  function toggleButton(button) {
    const pressed = button.getAttribute("aria-pressed") === "true";
    button.setAttribute("aria-pressed", !pressed);
  }
</script>
```

## Common ARIA Mistakes That Make the Internet Worse

🚨 **Please don’t do these things:**

1. **Slapping ARIA on Everything**

   * If you can use a native HTML element, **just use it**.
   * `<div role="button">` is **not** better than `<button>`.

2. **Forgetting to Update ARIA States**

   * If a dropdown opens but `aria-expanded` still says `false`, assistive tech users will be **very confused**.

3. **Making ARIA Widgets Impossible to Use with a Keyboard**

   * If your custom dropdown doesn’t support the arrow keys, **you’ve failed**.

4. **Misusing `aria-hidden`**

   * If something is visible and important, **do NOT set `aria-hidden="true"` on it**.
   * That makes it invisible to screen readers.

## How to Do ARIA Right

* **Use ARIA only when absolutely necessary**.
* **Keep ARIA attributes updated**—especially for interactive elements.
* **Test your site with a screen reader**. NVDA, JAWS, and VoiceOver are your best friends.
* **Follow the [WAI-ARIA Authoring Practices](https://www.w3.org/TR/wai-aria-practices/)** for proven patterns that actually work.

<!-- 
## Wrapping It Up

ARIA is a **powerful tool**—but with great power comes great responsibility. 

Misusing it can create **huge barriers** for users who rely on assistive tech. So **use HTML first, keep ARIA updated, and always test with a screen reader**.

Now go forth and build accessible, frustration-free websites! 🚀

---

## Quick ARIA Cheat Sheet

| Key Tip                          | What It Means                                  |
|----------------------------------|-----------------------------------------------|
| Use native HTML elements first  | They already have built-in accessibility.    |
| Don’t add ARIA unless needed    | ARIA should fix gaps, not replace HTML.      |
| Make sure it works with a keyboard | No mouse? No problem!                      |
| Keep ARIA states updated        | `aria-expanded`, `aria-pressed`, etc. matter. |
| Test with screen readers        | NVDA, JAWS, and VoiceOver will tell you the truth. |

--- -->
