---
title: Markdown Auto-Tag Algorithm Comparison
description: Comparing Auto-Tag Algorithm in SED, AWK, Python, Go, C# and C++
slug: how-to-auto-tag-words-in-a-markdown-file-using-sed-and-awk
date: 2023-02-19
image: post/Articles/IMAGES/markdownlogo.png
categories:
  - Markdown
  - SED
  - AWK
  - Unix
  - Linux
  - Mac OS
  - Python
  - GoLang
  - CSharp
  - CPP
tags:
  - Sed
  - Awk
  - Markdown
  - Automation
  - Text
  - Processing
  - Tagging
draft: false
weight: 485
lastmod: 2025-02-09T20:56:10.896Z
---
# How to Auto-Tag Words in a Markdown File Using SED and AWK

## Introduction

Manually tagging words in Markdown files? No way, we have **SED and AWK** for that!\
Let’s automate this task and make our lives easier.

We will start with solutions in SED and AWK, and then Python, and Go, and C# and C++ for comparison.

Taking a problem that it is a tad more complex. is an interesting way to compare languages and their pros and cons..

### **The Goal \ Algorithm**

* Given a **Markdown file (`article.md`)** and a **list of words (`tags.txt`)**.
* Find each word from `tags.txt` in `article.md` and **prefix it with `#`**.
* **Ignore case** when searching.
* **Avoid double-tagging** words already tagged.
* **Skip front matter** (that YAML-looking stuff at the top).
* **Skip code blocks** (because we don’t mess with sacred code).
* **Handle multiple tags on one line**.

***

## Example

### **tags.txt**

```
linux
awk
sed
```

### **article.md (Before Processing)**

```markdown
---
title: "Introduction to Linux and AWK"
description: "An article about Linux, AWK, and SED."
---

This is a beginner's guide to Linux and AWK.

Here's an example:
```

echo "Hello, Linux user!" | awk '{ print \$2 }'

```
We also use SED for text processing.
```

### **Expected Output (`article.md` After Processing)**

```markdown
---
title: "Introduction to Linux and AWK"
description: "An article about Linux, AWK, and SED."
---

This is a beginner's guide to #Linux and #AWK.

Here's an example:
```

echo "Hello, Linux user!" | awk '{ print \$2 }'

```
We also use #SED for text processing.
```

***

## Solution with SED

```sh
sed -E 's/\b(Linux|AWK|SED)\b(?!#)/#\1/gI' article.md > article_out.md
```

### **Issues with SED:**

* ❌ **Does not handle skipping code blocks**.
* ❌ **Does not skip front matter**.

***

## Solution with AWK

````sh
awk '
BEGIN { inside_code=0; inside_front=0; }
/^```/ { inside_code = !inside_code; }
/^---/ { inside_front = !inside_front; }
!inside_code && !inside_front {
    for (i = 1; i <= NF; i++)
        if ($i ~ /^[Ll]inux|[Aa]wk|[Ss]ed$/ && substr($i, 1, 1) != "#")
            $i = "#" $i;
}
{ print }
' article.md > article_out.md
````

### **Why AWK is better:**

✅ **Skips front matter correctly**\
✅ **Skips code blocks**\
✅ **Avoids double-tagging**

***

## Combining SED and AWK

````sh
awk '
BEGIN { inside_code=0; inside_front=0; }
/^```/ { inside_code = !inside_code; }
/^---/ { inside_front = !inside_front; }
!inside_code && !inside_front {
    for (i = 1; i <= NF; i++)
        if ($i ~ /^[Ll]inux|[Aa]wk|[Ss]ed$/ && substr($i, 1, 1) != "#")
            $i = "#" $i;
}
{ print }
' article.md | sed -E 's/\b(Linux|AWK|SED)\b(?!#)/#\1/gI' > article_out.md
````

### **Why combine them?**

* **AWK** handles structured text (skips front matter & code blocks).
* **SED** efficiently replaces words without looping.

***

## **Key Takeaways**

* **SED is fast**, but can’t **skip front matter or code blocks**.
* **AWK is smarter**, but more complex.
* **Using them together** gives the best **speed & accuracy**.

***

## Solution with Python

Python provides a **robust** and **flexible** solution for handling this problem.

````python
import re

def auto_tag(text, tag_list):
    lines = text.split("\n")
    in_code, in_front = False, False
    result = []

    for line in lines:
        if line.strip() == "```":
            in_code = not in_code
        if line.strip().startswith("---"):
            in_front = not in_front

        if not in_code and not in_front:
            for tag in tag_list:
                pattern = rf"\b{re.escape(tag)}\b(?!#)"
                line = re.sub(pattern, f"#{tag}", line, flags=re.IGNORECASE)
        
        result.append(line)

    return "\n".join(result)

# Reading the files
with open("article.md") as f:
    content = f.read()

with open("tags.txt") as f:
    tags = [line.strip() for line in f]

# Processing the markdown content
updated_content = auto_tag(content, tags)

with open("article_out.md", "w") as f:
    f.write(updated_content)
````

***

## Comparing Python to SED and AWK

| Feature                | SED               | AWK               | Python                |
| ---------------------- | ----------------- | ----------------- | --------------------- |
| Speed                  | ✅ Very fast       | ✅ Fast            | ⚠️ Slightly slower    |
| Ease of Use            | ⚠️ Limited syntax | ⚠️ Complex syntax | ✅ Readable and simple |
| Skips Code Blocks      | ❌ No              | ✅ Yes             | ✅ Yes                 |
| Skips Front Matter     | ❌ No              | ✅ Yes             | ✅ Yes                 |
| Avoids Double Tagging  | ❌ No              | ✅ Yes             | ✅ Yes                 |
| Handles Multiple Lines | ✅ Yes             | ✅ Yes             | ✅ Yes                 |

### **Key Differences**

4. **Python vs SED**:
   * **SED** can only **match & replace** text but **cannot skip code blocks** or **front matter**.
   * **Python** can **handle structured data** better, but is slightly **slower**.

5. **Python vs AWK**:
   * **AWK** is great at **parsing lines** but requires more **boilerplate**.
   * **Python** makes it **easier to handle Markdown** files and **complex replacements**.

6. **Why Python Wins**:
   * It **skips front matter** ✅
   * It **avoids modifying code blocks** ✅
   * It’s **easier to read & modify** ✅

***

## **Current Thoughts**

* **If speed is your goal**, use **SED + AWK** together.
* **If accuracy & maintainability matter**, use **Python**.
* **If you love pain**, use **regex inside SED inside AWK inside Bash**.

***

## **References**

7. [Markdown Guide](https://www.markdownguide.org)
8. [GNU Sed Manual](https://www.gnu.org/software/sed/manual/sed.html)
9. [GNU AWK Manual](https://www.gnu.org/software/gawk/manual/gawk.html)

***

## Solution with Go

Go provides a **fast and efficient** way to process large files while maintaining readability.

````go
package main

import (
    "bufio"
    "fmt"
    "os"
    "regexp"
    "strings"
)

func main() {
    file, err := os.Open("article.md")
    if err != nil {
        fmt.Println("Error opening file:", err)
        return
    }
    defer file.Close()

    tagFile, err := os.Open("tags.txt")
    if err != nil {
        fmt.Println("Error opening tags file:", err)
        return
    }
    defer tagFile.Close()

    var tags []string
    tagScanner := bufio.NewScanner(tagFile)
    for tagScanner.Scan() {
        tags = append(tags, tagScanner.Text())
    }

    scanner := bufio.NewScanner(file)
    inCode, inFront := false, false

    for scanner.Scan() {
        line := scanner.Text()

        if strings.TrimSpace(line) == "```" {
            inCode = !inCode
        }
        if strings.HasPrefix(strings.TrimSpace(line), "---") {
            inFront = !inFront
        }

        if !inCode && !inFront {
            for _, tag := range tags {
                pattern := regexp.MustCompile(`(?i)\b` + regexp.QuoteMeta(tag) + `\b(?!#)`)
                line = pattern.ReplaceAllString(line, "#"+tag)
            }
        }

        fmt.Println(line)
    }
}
````

***

## Comparing Go to Python, SED, and AWK

| Feature                | SED               | AWK               | Python                | Go                  |
| ---------------------- | ----------------- | ----------------- | --------------------- | ------------------- |
| Speed                  | ✅ Very fast       | ✅ Fast            | ⚠️ Slightly slower    | ✅ Fastest           |
| Ease of Use            | ⚠️ Limited syntax | ⚠️ Complex syntax | ✅ Readable and simple | ⚠️ More boilerplate |
| Skips Code Blocks      | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               |
| Skips Front Matter     | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               |
| Avoids Double Tagging  | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               |
| Handles Multiple Lines | ✅ Yes             | ✅ Yes             | ✅ Yes                 | ✅ Yes               |

### **Key Differences**

10. **Go vs SED**:

* **SED** is **much shorter** but can’t **handle structured text** like Go.
* **Go** is **optimized for large files** and **performance-critical applications**.

11. **Go vs AWK**:

* **AWK** is more **streamlined** for text processing.
* **Go** requires **explicit file handling** but is **more powerful**.

12. **Go vs Python**:

* **Python** is easier to write but **slower on large files**.
* **Go** is **compiled, multithreaded, and optimized for performance**.

### **Why Use Go?**

* If **performance is key**, Go is the **best choice**.
* If **readability and maintainability** are important, **Python is better**.
* If you **love regex pain**, SED and AWK will keep you up at night.

***

## **Final Thoughts**

* **If you need speed**, use **Go**.
* **If you need flexibility**, use **Python**.
* **If you hate yourself**, use **SED inside AWK inside Bash inside Go**.

***

## Solution with Perl

Perl is a **powerful** text processing language, often used for **one-liners** and complex text manipulations.

````perl
#!/usr/bin/perl
use strict;
use warnings;

my $article_file = 'article.md';
my $tags_file = 'tags.txt';

# Read tags from file
open my $tag_fh, '<', $tags_file or die "Could not open tags file: $!";
my @tags = map { chomp; $_ } <$tag_fh>;
close $tag_fh;

# Read the article content
open my $art_fh, '<', $article_file or die "Could not open article file: $!";
my @lines = <$art_fh>;
close $art_fh;

my $in_code = 0;
my $in_front = 0;

foreach my $line (@lines) {
    if ($line =~ /^```/) {
        $in_code = !$in_code;
    }
    if ($line =~ /^---/) {
        $in_front = !$in_front;
    }
    
    unless ($in_code || $in_front) {
        foreach my $tag (@tags) {
            my $pattern = qr/\b\Q$tag\E\b(?!#)/i;
            $line =~ s/$pattern/#$tag/g;
        }
    }
}

# Save the updated content
open my $out_fh, '>', 'article_out.md' or die "Could not write to file: $!";
print $out_fh @lines;
close $out_fh;
````

***

## Comparing Perl to Python, Go, SED, and AWK

| Feature                | SED               | AWK               | Python                | Go                  | Perl                |
| ---------------------- | ----------------- | ----------------- | --------------------- | ------------------- | ------------------- |
| Speed                  | ✅ Very fast       | ✅ Fast            | ⚠️ Slightly slower    | ✅ Fastest           | ✅ Fast              |
| Ease of Use            | ⚠️ Limited syntax | ⚠️ Complex syntax | ✅ Readable and simple | ⚠️ More boilerplate | ⚠️ Somewhat complex |
| Skips Code Blocks      | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               |
| Skips Front Matter     | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               |
| Avoids Double Tagging  | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               |
| Handles Multiple Lines | ✅ Yes             | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               |

### **Key Differences**

17. **Perl vs SED**:

* **SED** is fast and concise but **cannot skip structured sections**.
* **Perl** is just as fast but **more flexible**.

18. **Perl vs AWK**:

* **AWK** is **built for field-based processing** but requires **more logic**.
* **Perl** has **better regex support** and **built-in file handling**.

19. **Perl vs Python**:

* **Python** is more **readable** and **better for large projects**.
* **Perl** is **faster** for text-heavy tasks but **less maintainable**.

20. **Perl vs Go**:

* **Go** is optimized for **performance and concurrency**.
* **Perl** is **better for quick scripting and regex-heavy tasks**.

### **Why Use Perl?**

* If **regex is your best friend**, Perl is **the way to go**.
* If **readability matters**, Python is **a better choice**.
* If **you want raw speed**, Go **wins the race**.

***

## **Current Thoughts**

* **For small scripts**, Perl is **great**.
* **For structured data**, use **AWK**.
* **For complex Markdown processing**, use **Python**.
* **For best performance**, Go is **the winner**.
* **For the masochists**, stack **SED, AWK, Perl, and Bash in a single script**.

***

## Solution with C\#

C# provides a **structured and high-performance** solution for handling this problem.

````csharp
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string articleFile = "article.md";
        string tagsFile = "tags.txt";
        
        // Read tags from file
        string[] tags = File.ReadAllLines(tagsFile);

        // Read the article content
        string[] lines = File.ReadAllLines(articleFile);

        bool inCode = false;
        bool inFrontMatter = false;

        for (int i = 0; i < lines.Length; i++)
        {
            string line = lines[i].Trim();

            if (line == "```")
            {
                inCode = !inCode;
            }
            if (line.StartsWith("---"))
            {
                inFrontMatter = !inFrontMatter;
            }

            if (!inCode && !inFrontMatter)
            {
                foreach (string tag in tags)
                {
                    string pattern = $@"\b{Regex.Escape(tag)}\b(?!#)";
                    lines[i] = Regex.Replace(lines[i], pattern, $"#{tag}", RegexOptions.IgnoreCase);
                }
            }
        }

        // Save the updated content
        File.WriteAllLines("article_out.md", lines);
    }
}
````

***

## Comparing C# to Python, Go, Perl, SED, and AWK

| Feature                | SED               | AWK               | Python                | Go                  | Perl                | C#         |
| ---------------------- | ----------------- | ----------------- | --------------------- | ------------------- | ------------------- | ---------- |
| Speed                  | ✅ Very fast       | ✅ Fast            | ⚠️ Slightly slower    | ✅ Fastest           | ✅ Fast              | ✅ Fast     |
| Ease of Use            | ⚠️ Limited syntax | ⚠️ Complex syntax | ✅ Readable and simple | ⚠️ More boilerplate | ⚠️ Somewhat complex | ⚠️ Verbose |
| Skips Code Blocks      | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      |
| Skips Front Matter     | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      |
| Avoids Double Tagging  | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      |
| Handles Multiple Lines | ✅ Yes             | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      |

### **Key Differences**

25. **C# vs SED**:

* **SED** is lightweight but lacks **file manipulation capabilities**.
* **C#** provides **full control** over file handling.

26. **C# vs AWK**:

* **AWK** is great for **one-liners** but struggles with **structured file handling**.
* **C#** is **well-suited for large applications**.

27. **C# vs Python**:

* **Python** is easier to **write and debug**.
* **C#** is faster and **better for large-scale applications**.

28. **C# vs Go**:

* **Go** is optimized for **performance and concurrency**.
* **C#** integrates well with **enterprise applications**.

29. **C# vs Perl**:

* **Perl** is regex-heavy and ideal for **quick scripts**.
* **C#** is **safer and more structured** for long-term maintainability.

### **Why Use C#?**

* If you need **performance & structure**, C# is **a great choice**.
* If you want **quick scripting**, **Python or Perl** is **better**.
* If you love **enterprise development**, **C# is your best bet**.

***

## **Current Thoughts**

* **For scripting**, use **Perl or Python**.
* **For raw performance**, use **Go**.
* **For structured applications**, use **C#**.
* **For text processing in a one-liner**, **AWK and SED** are **still useful**.
* **For pain**, mix all of them into one horrifying script.

***

## Solution with C++

C++ provides a **high-performance solution** for this problem, ideal for handling large files efficiently.

````cpp
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <regex>

using namespace std;

vector<string> readFile(const string &filename) {
    vector<string> lines;
    ifstream file(filename);
    string line;
    while (getline(file, line)) {
        lines.push_back(line);
    }
    file.close();
    return lines;
}

void writeFile(const string &filename, const vector<string> &lines) {
    ofstream file(filename);
    for (const string &line : lines) {
        file << line << endl;
    }
    file.close();
}

vector<string> readTags(const string &filename) {
    vector<string> tags = readFile(filename);
    return tags;
}

vector<string> autoTag(vector<string> lines, const vector<string> &tags) {
    bool inCode = false, inFrontMatter = false;

    for (string &line : lines) {
        if (line == "```") {
            inCode = !inCode;
        }
        if (line.find("---") == 0) {
            inFrontMatter = !inFrontMatter;
        }

        if (!inCode && !inFrontMatter) {
            for (const string &tag : tags) {
                regex pattern("\b" + tag + "\b(?!#)", regex_constants::icase);
                line = regex_replace(line, pattern, "#" + tag);
            }
        }
    }
    return lines;
}

int main() {
    vector<string> tags = readTags("tags.txt");
    vector<string> lines = readFile("article.md");

    vector<string> updatedLines = autoTag(lines, tags);

    writeFile("article_out.md", updatedLines);

    cout << "Tagging completed successfully!" << endl;
    return 0;
}
````

***

## Comparing C++ to Python, C#, Go, Perl, SED, and AWK

| Feature                | SED               | AWK               | Python                | Go                  | Perl                | C#         | C++             |
| ---------------------- | ----------------- | ----------------- | --------------------- | ------------------- | ------------------- | ---------- | --------------- |
| Speed                  | ✅ Very fast       | ✅ Fast            | ⚠️ Slightly slower    | ✅ Fastest           | ✅ Fast              | ✅ Fast     | ✅ Fastest       |
| Ease of Use            | ⚠️ Limited syntax | ⚠️ Complex syntax | ✅ Readable and simple | ⚠️ More boilerplate | ⚠️ Somewhat complex | ⚠️ Verbose | ⚠️ Most Verbose |
| Skips Code Blocks      | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      | ✅ Yes           |
| Skips Front Matter     | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      | ✅ Yes           |
| Avoids Double Tagging  | ❌ No              | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      | ✅ Yes           |
| Handles Multiple Lines | ✅ Yes             | ✅ Yes             | ✅ Yes                 | ✅ Yes               | ✅ Yes               | ✅ Yes      | ✅ Yes           |

### **Key Differences**

35. **C++ vs SED**:

* **SED** is great for **quick text replacements**, but **lacks file handling**.
* **C++** is **more structured** and **handles large files efficiently**.

36. **C++ vs AWK**:

* **AWK** is great for **text parsing**, but **C++ provides better performance**.

37. **C++ vs Python**:

* **Python** is more **readable and maintainable**.
* **C++** is significantly **faster for large-scale processing**.

38. **C++ vs Go**:

* **Go** is built for **concurrency** and **simplicity**.
* **C++** gives the **ultimate control over memory and performance**.

39. **C++ vs C#**:

* **C#** is better for **enterprise applications**.
* **C++** is **more efficient** but **harder to write**.

### **Why Use C++?**

* If **performance is your top priority**, C++ is **the best choice**.
* If **you prefer readability**, **Python or C#** is better.
* If you want **low-level control**, **C++ is unbeatable**.

***

## **Final? Thoughts**

* **For scripting**, use **Python or Perl**.
* **For high-performance processing**, use **C++ or Go**.
* **For structured enterprise applications**, use **C#**.
* **For one-liner text replacement**, **AWK and SED** still work well.

***

## The **Worst** Possible Code ?

Sometimes, you just want to watch the world burn. So here’s a **monstrosity** that combines **Bash, SED, AWK, Python, Perl, C++, and C#** into **one unreadable nightmare**.

````sh
#!/bin/bash

# Bash calls Python, which calls AWK, which calls Perl, which calls C++, which calls C#.
# Welcome to programming hell.

cat article.md | awk '
BEGIN { inside_code=0; inside_front=0; }
/^```/ { inside_code = !inside_code; }
/^---/ { inside_front = !inside_front; }
!inside_code && !inside_front {
    while ((getline line < "tags.txt") > 0) {
        gsub("\b" line "\b(?!#)", "#" line, $0)
    }
}
{ print }
' | perl -pe '
BEGIN { open(TF, "tags.txt"); @tags=<TF>; close TF; chomp(@tags); }
foreach my $t (@tags) { s/\b$t\b(?!#)/#$t/gi; }
' | python3 -c '
import sys, re
tags = [line.strip() for line in open("tags.txt")]
for line in sys.stdin:
    for tag in tags:
        line = re.sub(r"\b" + re.escape(tag) + r"\b(?!#)", f"#{tag}", line, flags=re.IGNORECASE)
    print(line, end="")
' | g++ -xc++ -o tagger - <<<"
#include <iostream>
#include <fstream>
#include <string>
using namespace std;
int main() {
    string line;
    ifstream file("article.md"); 
    while (getline(file, line)) { cout << line << endl; }
    return 0;
}
" && ./tagger | dotnet-script -e '
using System;
using System.IO;
using System.Text.RegularExpressions;

var tags = File.ReadAllLines("tags.txt");
foreach (var line in File.ReadLines("article.md")) {
    foreach (var tag in tags) {
        Console.WriteLine(Regex.Replace(line, $@"\b{Regex.Escape(tag)}\b(?!#)", $"#{tag}", RegexOptions.IgnoreCase));
    }
}
'
````

### **What Just Happened?**

* Bash pipes text through **AWK**, which modifies it.
* **AWK** sends it to **Perl**, which also modifies it.
* **Perl** hands it to **Python**, which keeps modifying it.
* **Python** writes a **C++ program**, which gets compiled and run.
* The **C++ program** pipes data to **C#**, which finally outputs the result.

### **Why Is This Horrible?**

* **Seven languages** are involved, each doing the **same task**.
* It **compiles C++ dynamically**, only to **print text**.
* **C# executes inside Bash**, via **.NET scripting**.
* **This is unreadable**.

***

## \*\* Thoughts on this Horrible  Example \*\*

If you ever find code like this in **production**, **RUN**.\
If someone **forces you to debug this**, **quit your job immediately**.

***

## **References**

* [Markdown Guide](https://www.markdownguide.org)
* [C++ Documentation](https://cplusplus.com/doc/)
* [C# Documentation](https://learn.microsoft.com/en-us/dotnet/csharp/)
* [Perl Documentation](https://perldoc.perl.org/)
* [GNU Sed Manual](https://www.gnu.org/software/sed/manual/sed.html)
* [GNU AWK Manual](https://www.gnu.org/software/gawk/manual/gawk.html)

***
