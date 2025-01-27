---
title: Image gallery
description: Create beautiful interactive image gallery using Markdown
date: 2023-08-26 00:00:00+0000
image: 2.jpg
lastmod: 2025-01-27T11:55:27.571Z
---
Hugo theme Stack supports the creation of interactive image galleries using Markdown. It's powered by [PhotoSwipe](https://photoswipe.com/) and its syntax was inspired by [Typlog](https://typlog.com/).

To use this feature, the image must be in the same directory as the Markdown file, as it uses Hugo's page bundle feature to read the dimensions of the image. **External images are not supported.**

## Syntax

```markdown
![Image 1](1.jpg) ![Image 2](2.jpg)
```

## Result

![Image 1](/static2/post/image-gallery/1.jpg) ![Image 2](/static2/post/image-gallery/2.jpg)

> Photo by [mymind](https://unsplash.com/@mymind) and [Luke Chesser](https://unsplash.com/@lukechesser) on [Unsplash](https://unsplash.com/)
