---
title: AWK and SED Together
description: AWK and SED, Compared and how best to Combine
slug: awk-vs-sed
date: 2021-12-30
image: post/Articles/IMAGES/ManhattanMelodrama.jpg
categories:
  - Scripting
  - AWK
  - SED
  - Unix
  - Linux
  - Mac OS
tags:
  - Awk
  - Sed
  - Linux
  - Unix
  - Text
  - Processing
  - Command
  - Line
  - Automation
  - Comparison
  - Scripting
draft: false
weight: 269
categories_ref:
  - Scripting
  - AWK
  - SED
  - Unix
  - Linux
  - Mac OS
slug_calculated: https://brianbraatz.github.io/p/awk-vs-sed
lastmod: 2025-03-14T16:40:27.310Z
---
[Manhattan Melodrama - Wikipedia](https://en.wikipedia.org/wiki/Manhattan_Melodrama)

## Introduction

So you've heard about `sed` and `awk`, two legendary tools from the golden age of UNIX. But now you're stuck in a classic dilemma: **Which one should you use?** Or, **should you use both?**

It's like choosing between a sword (`sed`) and a magic staff (`awk`). One is great for quick, powerful cuts, while the other is an entire spellbook for data manipulation. This guide will help you figure out when to reach for `sed`, when to unleash `awk`, and when to **combine** them for ultimate text-processing domination.

***

## A Brief History of AWK and SED

Both `awk` and `sed` were born at **Bell Labs**, the legendary birthplace of UNIX.

* **`sed`** (**Stream Editor**) was created in **1973** by **Lee E. McMahon**. It was designed as a **non-interactive text editor**, perfect for **batch editing and automation**.
* **`awk`** was created in **1977** by **Alfred Aho, Peter Weinberger, and Brian Kernighan**. Unlike `sed`, it wasn’t just for substitution—it had **built-in programming constructs**, making it a **lightweight scripting language** for text processing.

For the history buffs, here are the Wikipedia pages:

* **SED**: <https://en.wikipedia.org/wiki/Sed>
* **AWK**: <https://en.wikipedia.org/wiki/AWK>

Both tools became **core components of UNIX and Linux**, and today, they are still widely used.

***

## When to Use `sed` vs `awk`

Here's the **quick and dirty** comparison:

| Feature                    | SED                                                               | AWK                                                                    |
| -------------------------- | ----------------------------------------------------------------- | ---------------------------------------------------------------------- |
| Primary Purpose            | Text substitution and filtering                                   | Advanced text processing and formatting                                |
| Best For                   | Simple **find & replace**, deleting lines, modifying text streams | Extracting, formatting, and performing calculations on structured data |
| Supports Programming Logic | ❌ No                                                              | ✅ Yes (Variables, Loops, Conditionals)                                 |
| Works with Columns/Fields  | ❌ No (Processes entire lines)                                     | ✅ Yes (Handles structured data like CSVs)                              |
| Handles Arithmetic         | ❌ No                                                              | ✅ Yes (Can sum, average, and manipulate numbers)                       |
| Complexity                 | Easy                                                              | More Advanced                                                          |
| Speed                      | **Faster** for simple substitutions                               | Slightly slower but **more powerful** for complex tasks                |

**Rule of thumb:**

* If you **just need to replace, delete, or filter text**, **use `sed`**.
* If you need **complex text processing, calculations, or column-based operations**, **use `awk`**.
* If you want to **automate everything** like a wizard, **use them together!**

***

## SED vs AWK: Side-by-Side Examples

### **1. Replace Text**

**Using `sed` (Better for simple substitutions)**:

```sh
sed 's/error/fix/g' logs.txt
```

**Using `awk` (Overkill for this task)**:

```sh
awk '{ gsub("error", "fix"); print }' logs.txt
```

### **2. Extract a Column**

**Using `awk` (Perfect for this task)**:

```sh
awk '{ print $2 }' file.txt
```

(*Prints the second column from a file*)

**Using `sed` (Hacky, not recommended)**:

```sh
sed -E 's/^[^ ]+ ([^ ]+).*//' file.txt
```

### **3. Delete Blank Lines**

**Using `sed` (Best for this)**:

```sh
sed '/^$/d' file.txt
```

**Using `awk` (Also works, but overkill)**:

```sh
awk 'NF > 0' file.txt
```

### **4. Sum a Column of Numbers**

**Using `awk` (Best choice, as `sed` can't do math)**:

```sh
awk '{ sum += $2 } END { print sum }' numbers.txt
```

**Using `sed` (Not possible—`sed` doesn’t do math! 😢)**

***

## Using `sed` and `awk` Together

Why choose when you can have both? Here's how to **combine `sed` and `awk`** for maximum efficiency.

### **1. Pre-process with `sed`, then Format with `awk`**

```sh
sed 's/,/ /g' data.csv | awk '{ print $1, $3 }'
```

(*Replaces commas with spaces, then prints columns 1 and 3*)

### **2. Extract Lines with `sed`, Process with `awk`**

```sh
sed -n '5,10p' file.txt | awk '{ print $2 }'
```

(*Extracts lines 5-10, then prints column 2*)

### **3. Remove Duplicates with `sed`, then Summarize with `awk`**

```sh
sed 's/foo/bar/g' data.txt | awk '{ sum += $2 } END { print sum }'
```

(*Replaces "foo" with "bar", then sums column 2*)

***

## Syntax Tables

### **SED Syntax Reference**

| Command                        | Description                                 |
| ------------------------------ | ------------------------------------------- |
| `sed 's/old/new/g' file.txt`   | Replace all occurrences of "old" with "new" |
| `sed '/pattern/d' file.txt`    | Delete lines matching "pattern"             |
| `sed -n '5,10p' file.txt`      | Print lines 5 to 10                         |
| `sed 's/^/Prefix: /' file.txt` | Add a prefix to each line                   |
| `sed 's/$/ Suffix/' file.txt`  | Add a suffix to each line                   |

### **AWK Syntax Reference**

| Command                                             | Description                                    |
| --------------------------------------------------- | ---------------------------------------------- |
| `awk '{ print $1 }' file.txt`                       | Print first column                             |
| `awk '$3 > 100' file.txt`                           | Print lines where column 3 is greater than 100 |
| `awk '{ gsub("apple", "orange"); print }' file.txt` | Replace "apple" with "orange"                  |
| `awk '{ sum += $2 } END { print sum }' file.txt`    | Sum column 2                                   |

***

## Conclusion

* Use `sed` for **quick substitutions, deletions, and simple text processing**.
* Use `awk` for **working with structured data, arithmetic, and complex text transformations**.
* Use **both together** for the ultimate power combo.

Now, go forth and automate!

***

## References

1. **SED Wikipedia**: <https://en.wikipedia.org/wiki/Sed>
2. **AWK Wikipedia**: <https://en.wikipedia.org/wiki/AWK>
3. **GNU Sed Manual**: <https://www.gnu.org/software/sed/manual/sed.html>
4. **GNU AWK Manual**: <https://www.gnu.org/software/gawk/manual/gawk.html>
