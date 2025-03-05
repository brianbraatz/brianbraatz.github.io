---
title: Monitor Websites for Changes with Python and Playwright
description: Webscraping with intelligent change dectection
slug: monitor-websites-with-python
date: 2024-02-03
image: post/Articles/IMAGES/windsheildscraping.jpg
categories:
  - Python
  - Python-Playwright
  - Web Development
  - Testing
  - Unit Testing
tags:
  - WebScraping
  - Automation
  - Python
  - Selenium
  - Playwright
  - BeautifulSoup
  - Website
  - Monitoring
  - WebDevelopment
  - Chrome
draft: false
weight: 33
lastmod: 2025-03-05T21:11:13.092Z
---
# How to Monitor Websites for Changes Like a Cyber Detective

So, you’ve got a bunch of websites you want to **keep an eye on**.

Maybe it’s for **price tracking**, **competitor analysis**, or you’re just super nosy (hey, no judgment).

The problem? Websites **change**—sometimes **silently**, without you even knowing.

And how to sense WHAT changed? You wonder.. well so do I.. so keep reading...

SIDENOTE: This technique is also great for simple automated smoke testing of website projects.. :)

<!--
What if you could **automate** this check?? 

What if you had a **virtual detective** that alerts you the moment something changes? That’s exactly what we’re going to do today.

Buckle up—this is a **deep dive into website monitoring**, complete with **scrapers, automation, and even using your Chrome session** to sneak in undetected. 
-->

***

## 🚀 The Game Plan

To monitor websites, we need to:

1. **Fetch the webpage**
2. **Detect changes**
3. **Notify a human** (because machines still need us… for now).

There are a bunch of ways to do this. Let’s **break them down**, from the **quick and dirty** to the **fully automated magic**.

***

## 🕵️‍♂️ 1. Web Scraping + Change Detection

If the website doesn’t require a login, **this is the easiest way**.

### 🔧 **How It Works**

4. **Grab the webpage’s HTML** using Python’s `requests` library.
5. **Parse it** with `BeautifulSoup` to extract the juicy parts.
6. **Compare old vs. new results** to detect changes.

### 📝 **Code Example**

```python
import requests
from bs4 import BeautifulSoup

# The URL you want to monitor
url = "https://example.com/search-results"

# Fetch the page
response = requests.get(url)
soup = BeautifulSoup(response.text, "html.parser")

# Extract the search results (adjust selector based on the site)
results = soup.find_all("div", class_="result")

# Save or compare with previous results...
print(results)
```

💡 **Pros**:\
✅ **Fast**\
✅ **Simple**\
✅ **No browser needed**

⚠️ **Cons**:\
❌ Won’t work for **JavaScript-heavy sites**\
❌ Won’t work for sites that need a **login**

***

## 🔐 2. What If the Site Requires Login?

If the website needs a login, **we have to be sneaky**. Here are your options:

### 🏎️ **Option 1: Use `requests.Session` for Simple Logins**

Some sites let you log in with **plain old form data**. If so, we can **log in programmatically** and scrape pages like a ninja.

```python
import requests

session = requests.Session()

# Login URL
login_url = "https://example.com/login"

# Your login data
payload = {
    "username": "your_username",
    "password": "your_password"
}

# Log in
session.post(login_url, data=payload)

# Now scrape the search results page
response = session.get("https://example.com/search-results")
print(response.text)
```

✅ **Works for simple login forms**\
❌ **Fails if the site uses JavaScript for authentication**

***

## 🖥️ 3. Use Your Existing Chrome Login (The Easiest Hack!)

Let’s say you’re already **logged into the site in Chrome**.

Can we just… **use that session**?

**Yes, we can.** 🎩

### **Option 1: Selenium with Your Chrome Profile**

```python
from selenium import webdriver
from selenium.webdriver.chrome.options import Options

chrome_profile_path = "C:/Users/YourUser/AppData/Local/Google/Chrome/User Data"

chrome_options = Options()
chrome_options.add_argument(f"user-data-dir={chrome_profile_path}")  
chrome_options.add_argument("profile-directory=Default")  

driver = webdriver.Chrome(options=chrome_options)
driver.get("https://example.com/search-results")

# Scrape the page...
print(driver.page_source)

driver.quit()
```

💡 **Why is this cool?**

* ✅ Uses your **already logged-in** session.
* ✅ Works with **JavaScript-heavy** sites.
* ✅ No need to log in every time.

⚠️ **Downsides?**

* ❌ Requires **ChromeDriver** installed.
* ❌ Slower than `requests`.

***

## ⚡ 4. The *Playwright* Alternative (Faster Than Selenium)

Selenium is **great**, but **Playwright** is **faster and more stable**.

```python
from playwright.sync_api import sync_playwright
from bs4 import BeautifulSoup

chrome_profile_path = "C:/Users/YourUser/AppData/Local/Google/Chrome/User Data"

with sync_playwright() as p:
    browser = p.chromium.launch_persistent_context(
        user_data_dir=chrome_profile_path,
        headless=False
    )
    page = browser.new_page()
    
    page.goto("https://example.com/search-results")

    soup = BeautifulSoup(page.content(), "html.parser")
    print(soup.prettify())

    browser.close()
```

✅ **Much faster than Selenium**\
✅ **Uses your existing Chrome session**\
✅ **Handles JavaScript**

***

## 🔔 How to Detect Changes & Alert a Human

Once you scrape the site, how do you **detect changes**?

### **Option 1: Compare Old vs. New Results**

```python
old_results = open("previous_results.txt").read()
new_results = str(soup)

if old_results != new_results:
    print("⚠️ ALERT! Something changed!")
    open("previous_results.txt", "w").write(new_results)
```

### **Option 2: Git-Based Tracking**

* Store search results in **text files**.
* Use **Git** to track changes.
* When Git detects a change, it triggers an **email or Slack alert**.

***

### **How Does BeautifulSoup work?**

```python
soup = BeautifulSoup(page.content(), "html.parser")
```

### **Breaking It Down:**

1. **`page.content()`**
   * Retrieves the **HTML content** of the webpage from `Playwright`.
   * If using `requests`, you'd use `response.text` instead.

2. **`BeautifulSoup(..., "html.parser")`**
   * `BeautifulSoup` is a **library for parsing HTML and XML**.
   * `"html.parser"` tells BeautifulSoup to use **Python’s built-in HTML parser**, which is fast and doesn’t require extra dependencies.

***

### **What It Does**

* It **converts the raw HTML** from the webpage into a **structured, searchable format**.
* Now, you can **easily extract data** from the page using `soup.find()`, `soup.select()`, etc.

***

### **Example Usage**

#### **Before Parsing (Raw HTML)**

```html
<html>
  <body>
    <h1>Hello, World!</h1>
    <p class="message">This is a test.</p>
  </body>
</html>
```

#### **After Parsing (Using BeautifulSoup)**

```python
from bs4 import BeautifulSoup

html = "<html><body><h1>Hello, World!</h1><p class='message'>This is a test.</p></body></html>"
soup = BeautifulSoup(html, "html.parser")

# Extract text
print(soup.h1.text)  # Output: Hello, World!
print(soup.p.text)   # Output: This is a test.
```

***

## 🚀 Key Ideas

### 🏆 **If the site is simple** → `requests + BeautifulSoup`

### 🏎️ **If the site has a login** → `requests.Session` or **use Chrome cookies**

### 🎭 **If the site is JavaScript-heavy** → **Selenium or Playwright with Chrome Profile**

### 📣 **If you need alerts** → **Store results & compare old vs. new**
