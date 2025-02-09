---
title: Generating Postscript-PDF from Old Broken HTML with Python
description: Python-Parsing Old and New HTML and Converting Into PDF Files
slug: parsing-old-and-new-html-and-generating-postscript-to-pdf
date: 2024-12-15
image: post/Articles/IMAGES/postscriptlogo.png
categories: 
tags:
  - Html
  - Postscript
  - Pdf
  - Parsing
  - Legacy
  - Systems
  - Automation
  - Document
  - Processing
  - Python
draft: false
weight: 276
lastmod: 2025-02-09T17:54:10.779Z
---
## The Nightmare: A Horde of Ancient HTML Files

So there I was, at my job, staring into the abyss of a massive pile of HTML files. And not just any HTML—some of these bad boys were from the **HTML 1.0 era**.

You know, back when `<font>` tags were cool, tables ruled the web, and `<blink>` was somehow acceptable.

The goal? Take all of this ancient, inconsistent HTML and normalize it into consistently formatted PDFs.

Simple, right? **Nope.**

Some documents were pristine; others were crime scenes of mismatched `<div>`s, inline styles, and `<table>` layouts that made modern CSS cry.

Parsing it all into something usable was going to take a serious plan.

## Step 1: Fixing and Cleaning the Old HTML

The first step was extracting **usable content** while cleaning up **horrible, outdated practices**. This meant:

1. **Removing junk tags** (`<blink>`, `<marquee>`, `<script>`, `<style>`).
2. **Fixing unmatched tags** (because some files had *random* `<div>` openings with no closures).
3. **Handling encoding nightmares** (ISO-8859-1 mixed with UTF-8).
4. **Detecting HTML bleeding into output** (sometimes, raw HTML was mistakenly treated as content).
5. **Finding content even if `<div>`s were broken or misused**.
6. **Logging warnings for human review** (because you never know what horrors lurk in old HTML).

### 📝 Python Code for Cleaning Old HTML

We used `BeautifulSoup` for fixing the HTML, along with some **custom logic** to detect malformed structures.

```python
from bs4 import BeautifulSoup
import re
import logging

# Set up a log file for warnings
logging.basicConfig(filename="html_fix_warnings.log", level=logging.WARNING, format="%(asctime)s - %(message)s")

def clean_html(html_content):
    """ Cleans and normalizes old HTML content """

    # Parse HTML with BeautifulSoup
    soup = BeautifulSoup(html_content, "html.parser")

    # Remove <blink>, <marquee>, <script>, <style>
    for tag in soup(["blink", "marquee", "script", "style"]):
        tag.decompose()  # Removes the tag entirely

    # Fix unmatched tags by wrapping the content properly
    fixed_html = str(soup.prettify())

    # Detect raw HTML bleeding into output
    if "<" in soup.get_text():
        logging.warning("Possible HTML content bleeding detected.")

    # Return cleaned content
    return fixed_html

# Example HTML (badly formatted)
old_html = """
<html>
<head><title>Old Page</title></head>
<body>
    <blink>Important notice!</blink>
    <div><p>Some content <b>bold</p></div>
</body>
</html>
"""

fixed_html = clean_html(old_html)
print(fixed_html)
```

### 🔥 What This Does:

* **Removes `<blink>` and `<marquee>`** (because no one needs those anymore).
* **Fixes broken tags** (using `BeautifulSoup.prettify()` ensures they are closed properly).
* **Detects if raw HTML is leaking into content**.
* **Logs warnings** so we can manually review problematic files.

## Step 2: Extracting Content Even with Broken `<div>`s

Some HTML files had *zero structure*. Like this mess:

```html
<div><div>Content A</div>
<div>Content B
<p>Content C
</div>
```

To **extract content even if divs are broken**, we used:

```python
def extract_text_fallback(html_content):
    """ Extracts text from broken HTML files, handling missing div closures """
    soup = BeautifulSoup(html_content, "html.parser")

    # Find all text while ignoring missing div closures
    text_content = " ".join(soup.stripped_strings)
    
    return text_content

broken_html = "<div><div>Content A</div><div>Content B<p>Content C</div>"
fixed_text = extract_text_fallback(broken_html)
print(fixed_text)
```

## Step 3: Generating PostScript from the Fixed HTML

Now that we have **cleaned HTML content**, it’s time to **generate PostScript**.

### 📝 Simple Example: Basic Text Output

```python
def generate_postscript_simple(text):
    """ Generates basic PostScript output from cleaned text """
    ps_content = f"""
    %!PS-Adobe-3.0
    /Courier findfont 12 scalefont setfont
    72 720 moveto
    ({text}) show
    showpage
    """
    return ps_content

text = "Hello, this is a PostScript document."
ps_output = generate_postscript_simple(text)
print(ps_output)
```

## Step 4: Complex PostScript with Layout

For the **real project**, the PostScript output had **columns, images, and formatted text**. Here’s a **more advanced version**:

```python
def generate_postscript_complex(text, title):
    """ Generates a formatted PostScript document with title and structured text """
    ps_content = f"""
    %!PS-Adobe-3.0
    /Helvetica-Bold findfont 16 scalefont setfont
    72 750 moveto
    ({title}) show

    /Times-Roman findfont 12 scalefont setfont
    72 720 moveto
    ({text}) show

    newpath
    50 700 moveto
    500 700 lineto
    stroke

    showpage
    """
    return ps_content

ps_output = generate_postscript_complex("This is the main content.", "Document Title")
print(ps_output)
```

### 🔥 What This Does:

* **Uses Helvetica-Bold for the title**
* **Uses Times-Roman for content**
* **Draws a horizontal line for formatting**
* **Ensures proper text placement**

## Step 5: Converting to PDF

Once we had **PostScript files**, we converted them to **PDFs** using:

```sh
ps2pdf output.ps final.pdf
```

This gave us **clean, consistently formatted PDFs**, with **zero surprises**—exactly what we needed.

***

## 🔑 Key Ideas

* **Old HTML is a mess**—parsing it requires patience (and sometimes therapy).
* **HTML rendering is inconsistent**, so PostScript was chosen for **absolute control** over formatting.
* **Python can fix broken HTML**, detect issues, and **extract content even from disasters**.
* **PostScript allows precise placement of text, images, and layout**.
* **Converting PostScript to PDF is straightforward**, making it a great pipeline for document conversion.

***

## 📚 References

1. *PostScript Language Reference Manual* – Adobe
2. *ps2pdf Documentation* – Ghostscript
3. *BeautifulSoup Documentation* – Python
4. *Parsing HTML in Python* – `lxml` & `html.parser`
