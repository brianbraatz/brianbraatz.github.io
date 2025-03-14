---
title: SED Cheatsheet
description: A quick reference table for SED commands
slug: sed-reference-table
date: 2023-06-15
image: post/Articles/IMAGES/Sed.jpg
categories:
  - SED
  - AWK
  - Unix
  - Linux
  - Mac OS
  - Scripting
tags:
  - Sed
  - Linux
  - Unix
  - Command
  - Line
  - Text
  - Processing
  - Reference
draft: false
weight: 198
categories_ref:
  - SED
  - AWK
  - Unix
  - Linux
  - Mac OS
  - Scripting
lastmod: 2025-03-14T15:45:20.643Z
---
# SED Reference Table

This table provides a quick reference for commonly used `sed` commands.

| Command                                            | Description                                                       |                                               |
| -------------------------------------------------- | ----------------------------------------------------------------- | --------------------------------------------- |
| `sed 's/old/new/g' file.txt`                       | Replace all occurrences of "old" with "new" in file.txt   "error" |                                               |
| `sed -n '5p' file.txt`                             | Print only the 5th line                                           |                                               |
| `sed -n '10,20p' file.txt`                         | Print lines 10 to 20                                              |                                               |
| `sed 's/^/PREFIX: /' file.txt`                     | Add a prefix to each line                                         |                                               |
| `sed 's/$/ SUFFIX/' file.txt`                      | Add a suffix to each line                                         |                                               |
| `sed '3s/apple/orange/' file.txt`                  | Replace "apple" with "orange" only on the 3rd line                |                                               |
| `sed '/^$/d' file.txt`                             | Remove all blank lines                                            |                                               |
| `sed 's/[0-9]+/###/g' file.txt`                    | Replace all numbers with "###"                                    |                                               |
| `sed 's/[a-z]/\U&/g' file.txt`                     | Convert lowercase to uppercase                                    |                                               |
| \`grep "error" file.txt                            | sed 's/error/ERROR/g'\`                                           | Find "error" and replace it with "ERROR"      |
| \`awk '{print \$2}' file.txt                       | sed 's/foo/bar/g'\`                                               | Replace "foo" with "bar" in the second column |
| `sed -e 's/dog/cat/g' -e 's/bird/fish/g' file.txt` | Replace multiple words in one command                             |                                               |
| `sed 's/foo/bar/g' input.txt > output.txt`         | Save the changes to a new file                                    |                                               |
| `sed 's/^[ 	]*//' file.txt`                        | Remove leading spaces and tabs                                    |                                               |
| `sed 's/[ 	]*$//' file.txt`                        | Remove trailing spaces and tabs                                   |                                               |

This reference should help you quickly find the `sed` command you need. Happy scripting! 🎩

***

## References

1. **Wikipedia on `sed`**: <https://en.wikipedia.org/wiki/Sed>
2. **GNU Sed Manual**: <https://www.gnu.org/software/sed/manual/sed.html>
3. **Sed One-Liners**: <https://sed.sourceforge.io/sed1line.txt>
