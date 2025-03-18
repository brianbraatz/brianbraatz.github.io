---
title: Quick and Dirty Low Ceremony Website Testing with Python and Playwright
description: 
slug: unittesting-web-low-ceremony
date: 2019-06-14
image: post/Articles/IMAGES/pythonlogo.png
categories:
  - Testing
  - Python
  - Playwright
  - Automation
  - Web
tags:
  - Testing
  - Python
  - Playwright
  - Automation
  - Web
draft: false
weight: 178
categories_ref:
  - Testing
  - Python
  - Playwright
  - Automation
  - Web
slug_calculated: https://brianbraatz.github.io/p/unittesting-web-low-ceremony
lastmod: 2025-03-14T16:40:23.981Z
---
<!--
# Quick and Dirty Low Ceremony Website Testing with Python and Playwright
-->

So, you’ve got a website.

It’s not pretty- or perhaps not complex, but it does the job.

I use Hugo for making this blog, and it has this weird behavior- if it tries to parse my markdown and the YAML has something it doesnt like, it prints a warning and keeps on going.

Many times- i have pushed the site to the server, but I have missing pages.. Maybe I edited the yaml for an article and missed a quote or something.. and now my article is not on the site..

Maybe you have a blog or an admin panel, a dashboard, or just a place where your backend magic happens.

And you, like me, would like to just quickly see if the basic text on the page exists like it did last time..

In this situation, we don’t need a full-blown test suite with Selenium, headless browsers, and a million dependencies...

We just want to know:

* Do the pages load?
* Is the content still what you expect?
* Can I test this quickly without spending three days setting up a test framework?

Yes. Yes, you can.

Enter **Playwright**, a slick automation tool that lets you scrape, test, and interact with web pages with minimal fuss. We’ll write a dead-simple Python script to:

1. Visit a webpage.
2. Save its text content.
3. Store the content in a directory.
4. Compare it with a baseline using a simple `diff` command.

If something changes, you’ll know immediately. If everything’s the same, you can go back to sipping your coffee.

## The Magic Script: `SavePageText.py`

Here's the whole deal:

```python
import sys
import os
import re
import datetime
from playwright.sync_api import sync_playwright

def sanitize_filename(name):
    """Convert page name into a safe filename."""
    name = re.sub(r'[\\/:*?"<>|]', '_', name)  # Remove forbidden characters
    name = re.sub(r'[\s\-:]+', '_', name)  # Convert spaces, dashes, colons to underscores
    name = re.sub(r'_+', '_', name).strip('_')  # Remove consecutive underscores
    return name

def save_page_text(url, subdir=""):
    """Save webpage text content to a file."""
    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page()
        page.goto(url)
        text_content = page.inner_text("body")  # Extract text from body tag
        browser.close()

    timestamp = datetime.datetime.now().strftime("%Y-%m-%d-%s")
    save_dir = os.path.join("testing", subdir if subdir else timestamp)
    os.makedirs(save_dir, exist_ok=True)

    filename = sanitize_filename(url.split('//')[-1]) + ".txt"
    filepath = os.path.join(save_dir, filename)

    with open(filepath, "w", encoding="utf-8") as f:
        f.write(text_content)

    print(f"Saved: {filepath}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python SavePageText.py <URL> [subdir]")
        sys.exit(1)

    url = sys.argv[1]
    subdir = sys.argv[2] if len(sys.argv) > 2 else ""
    save_page_text(url, subdir)
```

### How It Works

* Run it with a URL and (optionally) a subdirectory name.
* It grabs the text from the page and saves it in `./testing/{subdir}/`.
* If you don’t specify a subdirectory, it makes one using the current date and time.

## Running the Script

### On Windows:

```powershell
python SavePageText.py "https://example.com" base
```

### On Linux:

```bash
python3 SavePageText.py "https://example.com" base
```

It saves the page’s text in `./testing/base/example_com.txt`. Run it for all your important pages, and you now have a snapshot of what they should look like.

## Checking for Changes

Later, run the script again with a different subdirectory (or let it default to a timestamped one). Then, compare the results:

### Windows (using `fc`):

```powershell
fc /L testing\base\example_com.txt testing\2026-01-10-1234567890\example_com.txt
```

### Linux (using `diff`):

```bash
diff testing/base/example_com.txt testing/2026-01-10-1234567890/example_com.txt
```

If there’s a difference, you’ll see it. If everything’s the same, no output means all is well.

## Why This is Awesome

* No need for complex test frameworks.
* No waiting for UI elements to load.
* No need for an entire Selenium setup.
* Just a quick way to make sure your site isn’t broken.

It’s not fancy, but it gets the job done.

Happy hacking! 🚀

## Key Ideas

| Concept                | Explanation                                            |
| ---------------------- | ------------------------------------------------------ |
| Playwright for Testing | Using Playwright to grab webpage text                  |
| Simple Web Scraping    | Extracting page content without a full test framework  |
| Quick Snapshot         | Saving web page content for later comparison           |
| Command Line Diffing   | Comparing files using `fc` (Windows) or `diff` (Linux) |
| Low Ceremony Testing   | Testing without setting up a huge test framework       |

## References

* [Playwright Documentation](https://playwright.dev/python/)
* [Python Official Website](https://www.python.org/)
* [Diff Command in Linux](https://man7.org/linux/man-pages/man1/diff.1.html)
* [fc Command in Windows](https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/fc)
