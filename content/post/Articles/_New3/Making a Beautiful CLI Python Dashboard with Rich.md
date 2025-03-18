---
title: Making a Beautiful CLI Python Dashboard with Rich
description: Exploring Rich package with Python
slug: beautiful-cli-dashboard-rich
date: 2021-12-05
image: post/Articles/IMAGES/pythonlogo.png
categories:
  - Python
  - Scripting
  - Python-Rich
  - GUI
  - Python-Gui
tags:
  - Python
  - CLI
  - Rich
  - Dashboard
  - Terminal
  - Automation
draft: false
weight: 412
categories_ref:
  - Python
  - Scripting
  - Python-Rich
  - GUI
  - Python-Gui
slug_calculated: https://brianbraatz.github.io/p/beautiful-cli-dashboard-rich
lastmod: 2025-03-14T16:40:27.054Z
---
# Making a Beautiful CLI Dashboard with Rich

Let’s be honest: **command-line interfaces are usually boring**. Plain text, no colors, no pizzazz. It’s like staring at a blank wall—functional, but zero excitement.

But what if I told you that **your CLI could look beautiful**? Like, **"people will actually enjoy using it"** kind of beautiful?

That’s where **Rich** comes in. Rich is a Python library that makes your terminal **look like it went to design school**. You can add colors, tables, progress bars, markdown rendering, and even animations—all in the terminal!

***

## 🚀 What is Rich?

**Rich** is a Python library for **pretty-printing in the terminal**. It can handle:

✔ **Colorful text formatting**\
✔ **Tables and data grids**\
✔ **Progress bars**\
✔ **Live updating dashboards**\
✔ **Markdown and emoji rendering**

And best of all? **It works on Windows, Mac, and Linux** without any extra setup.

***

## 🛠 Installing Rich

First, let’s install it:

```sh
pip install rich
```

Now, let’s start making our CLI look **glorious**.

***

## 🎨 Adding Some Color to Your Terminal

Let’s start small. Here’s how you **print colorful text** using Rich:

```python
from rich import print

print("[bold red]Hello, [green]World![/green][/bold red] 🌍")
```

Run it, and suddenly, your terminal looks **alive**.

You can use **bold, italic, underline**, and even **rainbow colors**. No more boring white text!

***

## 📊 Making a CLI Dashboard

Okay, let’s get serious. Time to build a **real CLI dashboard**.

We’re going to make a **live dashboard** that shows:

* A **table** of data
* A **progress bar**
* A **live clock**
* A **status panel**

This will feel like a **real-time monitoring system**—but for your CLI.

### **Step 1: Creating a Simple Dashboard Layout**

```python
from rich.console import Console
from rich.layout import Layout
from rich.panel import Panel

console = Console()

def make_layout():
    """Defines the layout of the dashboard."""
    layout = Layout()

    layout.split(
        Layout(name="header", size=3),
        Layout(name="body"),
        Layout(name="footer", size=3),
    )

    return layout

layout = make_layout()
layout["header"].update(Panel("🔥 [bold magenta]CLI Dashboard[/bold magenta] 🔥", expand=False))
layout["footer"].update(Panel("[bold cyan]Press Ctrl+C to exit[/bold cyan]", expand=False))

console.print(layout)
```

**Run it**, and you’ll see a **clean structured layout** with a **header and footer**. This is the skeleton of our **CLI dashboard**.

***

## 📅 Adding Real-Time Data (Live Dashboard)

A static dashboard is cool, but a **live updating** dashboard? **Now we’re talking.**

Let’s add:

* **A live clock**
* **A progress bar**
* **A table of random stats**

```python
import time
import random
from rich.console import Console
from rich.layout import Layout
from rich.panel import Panel
from rich.progress import Progress
from rich.table import Table
from datetime import datetime

console = Console()

# Dashboard Layout
layout = Layout()
layout.split(
    Layout(name="header", size=3),
    Layout(name="main"),
    Layout(name="footer", size=3),
)

# Add header and footer
layout["header"].update(Panel("[bold magenta]📊 Live CLI Dashboard[/bold magenta]"))
layout["footer"].update(Panel("[bold cyan]Press Ctrl+C to exit[/bold cyan]"))

def generate_table():
    """Creates a table with random data"""
    table = Table(title="📈 System Stats")
    table.add_column("Metric", style="cyan", justify="right")
    table.add_column("Value", style="magenta", justify="right")

    table.add_row("CPU Usage", f"{random.randint(10, 90)}%")
    table.add_row("Memory Usage", f"{random.randint(30, 80)}%")
    table.add_row("Disk Space", f"{random.randint(50, 95)}%")

    return table

with Progress() as progress:
    task = progress.add_task("[green]Processing...[/green]", total=100)
    
    while not progress.finished:
        layout["main"].update(
            Panel(
                generate_table(), 
                title=f"🕒 {datetime.now().strftime('%H:%M:%S')}"
            )
        )
        console.print(layout)
        progress.update(task, advance=random.randint(1, 10))
        time.sleep(1)
```

**What’s Happening Here?**

* We **update** the dashboard **every second**.
* We use **Rich Tables** to show **system stats**.
* We use **Progress Bars** to simulate loading.
* The **clock updates in real-time**.

Run this, and you’ll feel like **a DevOps pro**.

***

## 🏆 Final Touch: Making It Look Even Cooler

Rich has **tons of built-in magic**. You can add:

* **Animations**
* **Spinners**
* **Markdown rendering**
* **Even emoji support!**

Here’s a **spinner** to make things fun:

```python
from rich.console import Console
from rich.progress import track
import time

console = Console()

for _ in track(range(10), description="Processing..."):
    time.sleep(0.5)

console.print("[bold green]Done! ✅[/bold green]")
```

This makes your CLI look **polished and professional**—with **zero effort**.

***

## 🎉 Conclusion

Rich makes **CLI dashboards beautiful**. With just a few lines of code, you can:

✅ **Format text with colors and styles**\
✅ **Create real-time dashboards**\
✅ **Add tables, progress bars, and animations**\
✅ **Make your CLI look like a high-tech mission control center**

Your terminal doesn’t have to be **boring**. Give it **some life** with **Rich**!

<!-- 
---

## 💡 Ideas for Future Articles

- **"Building a Real-Time System Monitor with Rich"**
- **"How to Create Animated CLI Apps in Python"**
- **"Using Rich to Build Interactive Reports in the Terminal"**
-->

***

## 📚 References

1. [Rich Official Documentation](https://rich.readthedocs.io/en/stable/)
2. [Rich GitHub Repository](https://github.com/Textualize/rich)
3. [Rich on PyPI](https://pypi.org/project/rich/)
