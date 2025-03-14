---
title: Understanding HP's Printer Control Language-PCL
description: Dive into HP's PCL Language
slug: deep-dive-into-hps-pcl-language
date: 2002-12-15
image: post/Articles/IMAGES/hplaserjet2.jpg
categories:
  - Postscript
  - PDF
  - Printer Control Language-PCL
  - Document Imaging
tags:
  - PCL
  - HP
  - Laser
  - Printers
  - Printing
  - Postscript
  - Adobe
  - Programming
  - Postscript
  - PDF
draft: false
weight: 31
categories_ref:
  - Postscript
  - PDF
  - Printer Control Language-PCL
  - Document Imaging
lastmod: 2025-03-14T15:45:20.856Z
---
# Deep Dive into HP's PCL Language

You ever send a document to the printer and wonder what black magic happens inside? Welcome to the world of **Printer Command Language (PCL)**—HP’s way of making sure your documents don’t turn into a scrambled mess.

## A Brief History of PCL

Before we get into the nitty-gritty, let’s take a quick trip down memory lane. PCL was developed by **Hewlett-Packard (HP) in 1984** to control laser printers, starting with the HP LaserJet. Unlike PostScript (which we’ll compare later), PCL was designed to be a **simpler, more efficient** way to communicate with printers.

Here’s how it all connects:

* **HP created PCL** as a lightweight alternative to complex printer languages.
* **It became the de facto standard** for office and consumer printers.
* **Over time, it evolved**, with different versions adding more features like fonts, color printing, and better graphics.

If you want to geek out more, check out:

* [Wikipedia: PCL](https://en.wikipedia.org/wiki/Printer_Command_Language)
* [HP's official PCL documentation](https://support.hp.com/us-en/document/c05051541)

***

## How PCL Works

PCL is **a page description language** that tells your printer how to lay out text and graphics on a page. Unlike PostScript, which is **Turing-complete** (meaning it’s practically a full programming language), PCL is **more straightforward**.

### Example 1: Basic PCL Commands

```pcl
E            # Reset Printer
&l1O        # Set orientation to portrait
(s10H       # Set font size to 10pt
Hello, PCL!
```

* `E` - Resets the printer.
* `&l1O` - Sets the page orientation to portrait.
* `(s10H` - Sets font size to 10pt.
* `"Hello, PCL!"` - Prints text.

Unlike PostScript, which processes everything before rendering, **PCL processes commands line by line**, making it faster for simple printing tasks.

***

## Debugging PCL

PCL debugging is a bit tricky since most modern tools are geared toward PostScript. However, you can **test PCL without printing** using:

1. **HP Print Preview Tools** (Some HP utilities allow you to preview PCL output.)
2. **Using a PCL Viewer** such as:
   * [PCL Parser](https://www.pclparaphernalia.eu/)
   * [GhostPCL](https://www.ghostscript.com/)
3. **Converting PCL to PDF** for preview:
   ```sh
   pcl6 -sDEVICE=pdfwrite -sOutputFile=output.pdf input.pcl
   ```

***

## Testing PCL Without a Printer

### Using GhostPCL

GhostPCL is part of Ghostscript and allows you to interpret and render PCL files.

```sh
gpcl6 input.pcl
```

This lets you check output **before sending it to the printer**, saving time (and paper).

***

## Essential PCL Commands with Examples

| Command       | Description                 | Example             |
| ------------- | --------------------------- | ------------------- |
| `E`          | Reset printer               | `E`                |
| `&l1O`       | Set orientation to portrait | `&l1O`             |
| `(s10H`      | Set font size to 10pt       | `(s10H`            |
| `&l3A`       | Selects 3 copies            | `&l3A`             |
| `*p100x100Y` | Moves cursor to (100,100)   | `*p100x100Y`       |
| `(s1B`       | Switch to bold text         | `(s1BHello!`       |
| `(s1P`       | Change font to Courier      | `(s1PCourier Font` |

### Example: Printing a Bold Header

```pcl
E            # Reset Printer
(s1B       # Bold text on
Header Line
(s0B       # Bold text off
Normal Text
```

### Example: Printing in Color (PCL 5+)

```pcl
E            # Reset Printer
*rB         # Begin raster graphics
*v255a0b0C  # Set color to red (RGB)
Hello in Red!
```

PCL 5 introduced **basic color printing**, though it’s nothing compared to what PostScript can do.

***

### 1. Setting Margins

```pcl
&l2E       # Set top margin at 2 lines
```

### 2. Changing Line Spacing

```pcl
&l6D       # Set line spacing to 6 lines per inch
```

### 3. Drawing a Box (PCL 6+)

```pcl
*c100a100B # Draw a 100x100 box
```

### 4. Printing a Barcode (PCL 5+)

```pcl
(s1P(s4099T12345678
```

### 5. Rotating Text

```pcl
&a10R      # Rotate text 10 degrees
```

### 6. Changing Paper Size

```pcl
&l26A      # Set paper size to A4
```

### 7. Printing Underlined Text

```pcl
&d0DHello &d@ World
```

### 8. Changing Text Justification

```pcl
&a1J       # Set text justification to centered
```

### 9. Creating a Header and Footer

```pcl
&l0LHeader
&l8LMain Text
&l66LFooter
```

### 10. Using Macros for Repeating Content

```pcl
&f1X       # Define Macro
```

***

## PCL vs. PostScript: The Printer Wars

So how does **PCL compare to PostScript**? Let’s break it down:

| Feature                 | PCL             | PostScript              |
| ----------------------- | --------------- | ----------------------- |
| **Developer**           | HP              | Adobe                   |
| **Device Independence** | No              | Yes                     |
| **Complexity**          | Simple          | Complex                 |
| **Speed**               | Faster for text | Slower but more precise |
| **Graphics Quality**    | Limited         | High-quality            |
| **Best for**            | Office printing | Professional publishing |

### Why PCL is Faster but Simpler

* **PCL interprets commands line by line**, making it **faster for standard office documents**.
* **PostScript is a full programming language**, meaning it **processes the entire page before rendering**, leading to higher quality but slower performance.

### When to Use What?

* **PCL** is great for **office printing, invoices, and documents**.
* **PostScript** is ideal for **graphic design, PDFs, and publishing**.

### Learn More

* [Wikipedia: PCL](https://en.wikipedia.org/wiki/Printer_Command_Language)
* [Wikipedia: PostScript](https://en.wikipedia.org/wiki/PostScript)
* [HP PCL Documentation](https://support.hp.com/us-en/document/c05051541)
