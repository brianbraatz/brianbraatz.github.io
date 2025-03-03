---
title: HTML 5 in a Nutshell
description: Looking at the new HTML 5 Additions
slug: html-5-in-a-nutshell
date: 2021-05-26
image: post/Articles/IMAGES/html5logo-wide.png
categories:
  - HTML
  - HTML 5
  - Web Development
tags:
  - Html5
  - Web
  - Development
  - Frontend
  - Html
  - Elements
  - Html
  - Syntax
draft: false
weight: 16
lastmod: 2025-03-03T13:55:46.165Z
---
## A Brief History of HTML5

Once upon a time, in the early days of the internet (cue dial-up noise), HTML was just a humble markup language, helping nerds put text on a webpage. Fast forward to the 2000s, the web evolved, but HTML wasn’t keeping up—so **HTML5** was born!

fficially introduced in 2014, HTML5 brought a boatload of new features: built-in audio and video support, new semantic elements, and APIs that made the web feel more like a real application platform rather than a fancy document viewer.

It basically said, *"Hey Flash, you're fired!"*

## HTML5 Keyword Reference Table

| Element        | Description                                         |
| -------------- | --------------------------------------------------- |
| `<article>`    | Defines an independent self-contained content block |
| `<section>`    | Represents a standalone section of content          |
| `<header>`     | Represents introductory content or navigation links |
| `<footer>`     | Represents footer information for a section or page |
| `<nav>`        | Defines navigation links                            |
| `<figure>`     | Groups media content with captions                  |
| `<figcaption>` | Defines a caption for a `<figure>`                  |
| `<audio>`      | Embeds sound content                                |
| `<video>`      | Embeds video content                                |
| `<canvas>`     | Creates graphics via JavaScript                     |
| `<progress>`   | Displays a progress bar                             |
| `<meter>`      | Represents a scalar measurement                     |

## Common Uses of HTML5 (With Code Samples!)

### 1. Embedding a Video (Goodbye, Flash!)

Back in the dark ages, embedding a video meant dealing with **Flash**, which was about as fun as debugging spaghetti code. Now, HTML5 makes it easy:

```html
<video width="640" height="360" controls>
  <source src="video.mp4" type="video/mp4">
  Your browser does not support the video tag.
</video>
```

Reference: <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video>

### 2. The `<canvas>` Element (For Those Who Love Drawing with Code)

Want to draw graphics using JavaScript? HTML5 has your back:

```html
<canvas id="myCanvas" width="500" height="300"></canvas>
<script>
  var canvas = document.getElementById("myCanvas");
  var ctx = canvas.getContext("2d");
  ctx.fillStyle = "blue";
  ctx.fillRect(20, 20, 150, 100);
</script>
```

Reference: <https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API>

### 3. The `<audio>` Tag (For Your Podcast or Sick Beats)

Want to add sound without using janky plugins? HTML5 has a native solution:

```html
<audio controls>
  <source src="sound.mp3" type="audio/mp3">
  Your browser does not support the audio element.
</audio>
```

Reference: <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio>

### 4. The `<progress>` Element (Because Loading Screens Are a Vibe)

You can create a simple progress bar with HTML5:

```html
<progress value="70" max="100"></progress>
```

Reference: <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress>

## Key Ideas Summary

| Concept                    | Summary                                                   |
| -------------------------- | --------------------------------------------------------- |
| **HTML5 Introduction**     | A modern update to HTML with new elements and API support |
| **New Elements**           | Includes `<video>`, `<audio>`, `<canvas>`, and more       |
| **Goodbye Flash**          | HTML5 eliminates the need for plugins                     |
| **Built-in Multimedia**    | Native support for audio and video                        |
| **Canvas API**             | Enables drawing graphics using JavaScript                 |
| **Progress Bars & Meters** | Provides built-in UI elements for displaying progress     |

## Reference Links

* [HTML5 Introduction](https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/HTML5)
* [HTML5 Elements](https://developer.mozilla.org/en-US/docs/Web/HTML/Element)
* [HTML5 Video](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video)
* [HTML5 Canvas API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API)
* [HTML5 Progress Element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress)
* [HTML5 Audio Element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio)
