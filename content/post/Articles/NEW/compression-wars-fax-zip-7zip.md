---
title: "Compression Evolution: Fax Machines, ZIP, and 7Zip"
description: Overview of how we went from Fax to 7zip
slug: the-compression-wars-fax-machines-zip-7zip
date: 2020-06-20
image: post/Articles/IMAGES/Hookes-law-springs.png
categories:
  - History
  - Algorithms
  - Compression
tags:
  - Compression
  - Fax
  - Machines
  - ZIP
  - 7Zip
  - Algorithms
  - Data
  - Compression
  - Run-Length
  - Encoding
draft: false
weight: 345
lastmod: 2025-02-09T22:05:15.202Z
---
![](/post/Articles/IMAGES/Hookes-law-springs.png)

Image fron :\
<https://en.wikipedia.org/wiki/Hooke%27s_law>\
:)

# The Compression Evolution: Fax Machines, ZIP, and 7Zip

## Introduction

Once upon a time, before cloud storage, SSDs, and the miracle of "unlimited" Google Drive storage (RIP 😢), we had to **squeeze** every last byte of our data **to make it fit**.

Enter **fax machines, ZIP files, and 7Zip**—three very different but related technologies that **use compression to make our digital lives easier**.

Today, we’re diving into:

1. **The ancient relic that is the fax machine** (and why it made those weird screeching sounds).
2. **How ZIP and 7Zip compress data** (spoiler: it’s smarter than just “squishing” stuff).
3. **How these three technologies are similar and different.**

<!-- 
Buckle up, because this is a **compression deep dive with a side of humor!** 🎢
-->

***

## **The History of Fax Machines, ZIP, and 7Zip**

### **Fax Machines: The OG Data Compression Tool**

Back in the **1980s and 1990s**, if you wanted to send a document, you had two choices:

1. **Mail it and wait a week.** 📬
2. **Fax it and listen to robotic screams over the phone.** 🤖📞

Fax machines **used phone lines to send images of documents**. The problem? **Phone lines were slow**. So engineers got smart and **compressed the images** before sending them.

This is why, **when receiving a fax, the speed would change**—some lines of text would go **fast**, others **slow**. That’s because fax machines **used an algorithm called Run-Length Encoding (RLE)**.

### **ZIP Files: The 1990s Hero of Compression**

The **ZIP file format** was introduced in **1989** by Phil Katz (RIP, compression legend 🙏). ZIP files use **multiple compression techniques**, but the most famous one is **Deflate**, which combines:

* **Huffman Coding** (think of it as “shorter codes for common stuff”).
* **Lempel-Ziv (LZ77) Compression** (removes repeated patterns).

ZIP files became the **standard** for compressing multiple files into one neat package. 📦

### **7Zip: The Overachiever of Compression**

In **1999**, along came **Igor Pavlov**, a Russian programmer who thought:

> "ZIP is great, but what if we could do better?"

And thus, **7Zip was born**. The **7z format** uses a more powerful **LZMA (Lempel-Ziv-Markov chain) algorithm**, which achieves **higher compression ratios** than ZIP.

The tradeoff? **It’s slower to compress, but much better at squeezing files down**.

***

## **How These Algorithms Work**

| Algorithm                          | Used In      | How It Works                                                                      |
| ---------------------------------- | ------------ | --------------------------------------------------------------------------------- |
| **Run-Length Encoding (RLE)**      | Fax Machines | Replaces sequences of repeated data with a shorter code (e.g., "000000" → "6x0"). |
| **Huffman Coding**                 | ZIP, 7Zip    | Uses variable-length codes (shorter codes for frequent characters).               |
| **LZ77 (Lempel-Ziv)**              | ZIP          | Finds repeated patterns and replaces them with pointers to previous occurrences.  |
| **LZMA (Lempel-Ziv-Markov chain)** | 7Zip         | Like LZ77 but smarter: uses **better prediction models** to find repeating data.  |

***

## **How Are They Similar?**

All three methods (**fax, ZIP, and 7Zip**) are about **reducing redundancy** in data.

* **Fax Machines use Run-Length Encoding** to shrink black-and-white images.
* **ZIP and 7Zip use dictionary-based compression** (LZ77, LZMA) to remove repeating patterns in text and files.

The goal? **Shrink data as much as possible while still being readable on the other end.**

***

## **How Are They Different?**

| Feature              | Fax (RLE)                      | ZIP (LZ77 + Huffman)         | 7Zip (LZMA)                         |
| -------------------- | ------------------------------ | ---------------------------- | ----------------------------------- |
| **Compression Type** | Simple repetition removal      | Dictionary-based compression | Advanced dictionary + Markov models |
| **Efficiency**       | Fast but basic                 | Medium                       | Best compression ratio              |
| **Speed**            | Fast                           | Fast                         | Slower                              |
| **Best For**         | Images with large blank spaces | General file compression     | Maximum compression                 |
| **File Support**     | Only images (faxes)            | Everything                   | Everything (better than ZIP)        |

***

## **Which One is the Best?**

* If you’re **stuck in 1995**, **fax machines** are still kinda cool. 📠
* If you want **fast and easy compression**, **ZIP files are the way to go**. 🗜️
* If you’re okay with **slower compression for better results**, **7Zip wins**. 🏆

***

## **Final Thoughts**

* **Fax machines were the original compression hackers**, using **RLE** to make images smaller.
* **ZIP files made compression standard** with LZ77 and Huffman Coding.
* **7Zip took it to the next level**, squeezing files even smaller with **LZMA**.

Now, next time you hear the **chaotic beeping of a fax machine**, you’ll know it’s just **Run-Length Encoding doing its job!** 😂

***

## **Key Ideas Table**

| Concept                   | Explanation                                                               |
| ------------------------- | ------------------------------------------------------------------------- |
| Run-Length Encoding (RLE) | Used in fax machines, replaces repeated pixels with a shorter description |
| ZIP (LZ77 + Huffman)      | Removes repeated patterns in files and replaces them with pointers        |
| 7Zip (LZMA)               | A smarter version of LZ77 that finds **even better compression patterns** |
| Huffman Coding            | Shorter codes for frequently used data                                    |
| Dictionary Compression    | Finding patterns and storing them efficiently                             |
| Fax Machine Speed Changes | Caused by RLE compressing **some lines better than others**               |

***

## **References**

* https://en.wikipedia.org/wiki/Run-length\_encoding
* https://en.wikipedia.org/wiki/ZIP\_(file\_format)
* https://en.wikipedia.org/wiki/7z
* https://www.howtogeek.com/451717/what-is-7z-and-how-do-you-open-it/
