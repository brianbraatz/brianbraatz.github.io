---
title: Regex Cheatsheet
slug: regex-cheatsheet
date: 2013-12-16
image: post/Articles/IMAGES/regex.png
categories:
  - RegEx
  - Perl
tags:
  - Perl
  - Regex
  - Scripting
draft: false
weight: 106
description: Regex Cheat Sheet
categories_ref:
  - RegEx
  - Perl
lastmod: 2025-03-14T15:45:13.993Z
---
![Perl Logo](https://cdn.perl.org/perlweb/images/icons/header_camel.png)

[Perl on Wikipedia](https://en.wikipedia.org/wiki/Perl)

[Regular Expressions on Wikipedia](https://en.wikipedia.org/wiki/Regular_expression)

## Regex Cheat Sheet

| Pattern          | Description                                            |
| ---------------- | ------------------------------------------------------ |
| `.`              | Matches any character except a newline                 |
| `^`              | Matches the start of a string                          |
| `$`              | Matches the end of a string                            |
| `\d`             | Matches any digit (0-9)                                |
| `\D`             | Matches any non-digit                                  |
| `\w`             | Matches any word character (alphanumeric + underscore) |
| `\W`             | Matches any non-word character                         |
| `\s`             | Matches any whitespace character                       |
| `\S`             | Matches any non-whitespace character                   |
| `*`              | Matches 0 or more of the preceding element             |
| `+`              | Matches 1 or more of the preceding element             |
| `?`              | Matches 0 or 1 of the preceding element                |
| `{n}`            | Matches exactly n occurrences                          |
| `{n,}`           | Matches n or more occurrences                          |
| `{n,m}`          | Matches between n and m occurrences                    |
| `[]`             | Matches any one of the enclosed characters             |
| `[^]`OR operator | Matches any character *not* in the brackets            |
| `()`             | Capturing group                                        |
| `(?=...)`        | Lookahead assertion                                    |
| `(?<=...)`       | Lookbehind assertion                                   |
| `(?!...)`        | Negative lookahead                                     |
| `(?<!...)`       | Negative lookbehind                                    |

see https://brianbraatz.github.io/tags/perl/
