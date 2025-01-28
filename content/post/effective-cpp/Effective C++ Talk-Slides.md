---
title: Effective C++ Talk-Slides
description: Slides for talk on Effective C++
slug: 
date: 2006-07-02
image: cover.jpg
categories: 
tags:
  - CPP
weight: 3
lastmod: 2025-01-28T00:57:25.439Z
---
Details

# 1

#### *Brian Braatz*

<!--- 
image embed
![](__/Templates/cover.jpg)

> select * from BasketballPlayer, SoccerPlayer

LINK format
[markdown-syntax](__/OLDContent/markdown-syntax/index.md) 

[DISPLAY](LINK) 



-->

# one

```
```

## CODE PREVIEW

<div id="adobe-dc-view"></div>
<div id="adobe-dc-view"></div>
<script src="https://acrobatservices.adobe.com/view-sdk/viewer.js"></script>
<script type="text/javascript">
	document.addEventListener("adobe_dc_view_sdk.ready", function(){ 
		var adobeDCView = new AdobeDC.View({clientId: "cefffa8444c94515b33607ebff74a924", divId: "adobe-dc-view"});
		adobeDCView.previewFile({
			content:{location: {url: "https://acrobatservices.adobe.com/view-sdk-demo/PDFs/Bodea Brochure.pdf"}},
			metaData:{fileName: "Bodea Brochure.pdf"}
		}, {});
	});
</script>

Embedding a PDF file in a Hugo website can be accomplished through several methods. Here are the most common options:

***

### 1. **Direct Link to the PDF**

You can link directly to the PDF file, allowing users to open or download it:

markdown

CopyEdit

\[View PDF]\(https://acrobatservices.adobe.com/view-sdk-demo/PDFs/Bodea Brochure.pdf)\`

This approach doesn't embed the PDF but provides an easy way for users to access it.

***

### 2. **HTML `<embed>` Tag**

The `<embed>` tag is a simple way to display a PDF file directly on a webpage:

html

CopyEdit

<embed src="https://acrobatservices.adobe.com/view-sdk-demo/PDFs/Bodea Brochure.pdf" type="application/pdf" width="100%" height="600px">\`

This will show the PDF file embedded within the page.

***

### 3. **HTML `<iframe>` Tag**

Using an `<iframe>` allows you to embed a PDF in a specific section of the webpage:

html

CopyEdit

<iframe src="https://acrobatservices.adobe.com/view-sdk-demo/PDFs/Bodea Brochure.pdf" width="100%" height="600px" style="border:none;"></iframe>`

This approach is widely supported and easy to implement.

***

### 4. **Using a JavaScript PDF Viewer**

For enhanced control and better UX, you can use a JavaScript-based PDF viewer like PDF.js. Add the PDF.js library to your Hugo project and embed the viewer with the PDF file:

html

CopyEdit

<div id="pdf-viewer" style="width: 100%; height: 600px;"></div> <script src="js/pdf.js"></script> <script>   const pdfViewer = document.getElementById('pdf-viewer');   const pdfUrl = 'https://acrobatservices.adobe.com/view-sdk-demo/PDFs/Bodea Brochure.pdf';   pdfjsLib.getDocument(pdfUrl).promise.then((pdfDoc) => {     pdfDoc.getPage(1).then((page) => {       const viewport = page.getViewport({ scale: 1 });       const canvas = document.createElement('canvas');       const context = canvas.getContext('2d');       canvas.height = viewport.height;       canvas.width = viewport.width;       pdfViewer.appendChild(canvas);       page.render({ canvasContext: context, viewport });     });   }); </script>`

***

### 5. **Markdown Shortcodes**

Hugo supports custom shortcodes. You can create a shortcode for embedding PDFs. For example, create a file `pdf.html` in the `layouts/shortcodes/` directory:

html

CopyEdit

`<iframe src="{{ .Get 0 }}" width="100%" height="600px" style="border:none;"></iframe>`

Use the shortcode in your Markdown file:

markdown

CopyEdit

***

### 6. **Third-Party Embedding Services**

Use services like Google Drive or Scribd to host your PDF and embed it in your Hugo site using an embed code:

html

CopyEdit

`<iframe src="https://drive.google.com/file/d/FILE_ID/preview" width="100%" height="600px" style="border:none;"></iframe>`

***

### Recommendations:

* **For simplicity**: Use `<embed>` or `<iframe>`.
* **For enhanced UX**: Use PDF.js or a similar JavaScript library.
* **For custom styling and reusability**: Create a shortcode.

If you need help setting up any of these, let me know!
