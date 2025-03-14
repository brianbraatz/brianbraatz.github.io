---
title: AWK in a Nutshell
description: AWK in a Nutshell
slug: awk-in-a-nutshell
date: 2022-11-07
image: post/Articles/IMAGES/myrnaloy.png
categories:
  - AWK
  - Scripting
  - Unix
  - Linux
  - Mac OS
tags:
  - Awk
  - Linux
  - Unix
  - Text
  - Processing
  - Command
  - Line
  - Automation
  - Sed
  - Regex
  - Scripting
draft: false
weight: 414
categories_ref:
  - AWK
  - Scripting
  - Unix
  - Linux
  - Mac OS
lastmod: 2025-03-14T15:45:20.319Z
---
[Myrna Loy - Wikipedia](https://en.wikipedia.org/wiki/Myrna_Loy)

# AWK in a Nutshell

## Introduction

If `sed` is the Swiss Army knife of text processing, then `awk` is the entire hardware store. This little tool, originally created for pattern scanning and processing, has evolved into one of the most powerful text-processing languages in UNIX.

AWK is named after its creators: **A**ho, **W**einberger, and **K**ernighan. Yes, that **Kernighan**, as in the co-author of *The C Programming Language*—so you know AWK means business.

## This article will walk you through AWK’s history, how it relates to UNIX/Linux, and why it’s a must-know tool for anyone working with text files, logs, or data processing.

## A Brief History of `awk`

Back in **1977**, **Alfred Aho**, **Peter Weinberger**, and **Brian Kernighan** developed AWK as a powerful yet simple language for processing text. It was included in **Version 7 UNIX**, and from there, it became a standard part of UNIX and Linux systems.

For history nerds, here’s the Wikipedia page on AWK:\
<https://en.wikipedia.org/wiki/AWK>

***

## Why Use AWK?

Why should you learn `awk` when you already have `sed`? Because:

* **It's powerful**: AWK can handle text files like a database.
* **It processes structured data easily**: CSVs, logs, reports—AWK thrives on them.
* **It supports variables, loops, and functions**: Making it more flexible than `sed`.
* **It can replace simple shell scripts**: Why write 20 lines of Bash when 1 line of AWK will do?

***

## AWK Syntax Basics

An AWK command generally looks like this:

```sh
awk 'pattern { action }' filename
```

* **`pattern`** → Specifies the lines to process (optional).
* **`action`** → What to do with those lines (print, replace, calculate, etc.).

Example:

```sh
awk '{ print $1 }' file.txt
```

This prints the **first column** of `file.txt`.

### AWK Fields (`$1`, `$2`, etc.)

* `$1` → First column
* `$2` → Second column
* `$NF` → Last column

Example:

```sh
echo "John Doe 30" | awk '{ print $2 }'
```

Output:

```
Doe
```

***

## Common `awk` Use Cases

### Print Specific Columns

```sh
awk '{ print $2, $3 }' file.txt
```

### Filter Lines Based on a Condition

```sh
awk '$3 > 100' numbers.txt
```

### Find and Replace

```sh
awk '{ gsub("apple", "orange"); print }' fruits.txt
```

### Summing a Column

```sh
awk '{ sum += $2 } END { print sum }' sales.txt
```

### Print Only Matching Lines

```sh
awk '/error/' logs.txt
```

***

## Using `awk` with `sed`

### Find and Replace with `sed`, then Process with `awk`

```sh
sed 's/foo/bar/g' data.txt | awk '{ print $1, $NF }'
```

### Extract Lines with `sed`, Process with `awk`

```sh
sed -n '5,10p' data.txt | awk '{ print $2 }'
```

***

## Wrapping Up

If you've made it this far, congratulations! You now have a solid grasp of `awk`. With `sed` and `awk` in your toolkit, you're practically a text-processing ninja.

***

## Key Ideas

* `awk` is a **pattern scanning and processing language** for UNIX/Linux.
* Developed in **1977** at **Bell Labs**.
* Uses **patterns and actions** to manipulate text.
* Works with **columns and fields** (`$1`, `$2`, etc.).
* Can **filter, format, replace, sum, and transform** text.
* Can be combined with **`sed`** for even more power.

***

## References

1. **Wikipedia on `awk`**: <https://en.wikipedia.org/wiki/AWK>
2. **GNU AWK Manual**: <https://www.gnu.org/software/gawk/manual/gawk.html>
3. **AWK One-Liners**: <https://awk.info/?awk1line>
