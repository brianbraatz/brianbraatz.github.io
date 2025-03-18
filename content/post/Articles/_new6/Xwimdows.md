---
title: X Windows in a Nutshell
description: Intro to X Windows - with command line Cheat sheet
slug: how-the-x-windows-protocol-works
date: 2021-10-28
image: post/Articles/IMAGES/xwindowslogo.png
categories:
  - XWIndows
  - RDP
  - SSH
  - Linux
  - Unix
  - Protocols
  - Cloud
tags:
  - X11
  - X
  - Windows
  - Xorg
  - Wayland
  - Linux
  - Gui
  - Remote
  - Desktop
  - Display
  - Protocols
  - Networking
draft: false
weight: 422
categories_ref:
  - XWIndows
  - RDP
  - SSH
  - Linux
  - Unix
  - Protocols
  - Cloud
slug_calculated: https://brianbraatz.github.io/p/how-the-x-windows-protocol-works:-history-relationship-to-alternatives-and-10-code-examples
lastmod: 2025-03-18T18:29:25.813Z
---
<!--
# How the X Windows Protocol Works: History, Relationship to Alternatives, and 10 Code Examples
-->

## Introduction

Ever wondered how **graphical applications** run on **Linux and Unix systems**? Meet the **X Window System (X11)**—the technology that has powered graphical user interfaces on Unix-based systems since the **1980s**.

<!--
This article will **demystify X11** and explain:  

- The **history and motivation** behind X11.  
- How the **X Windows protocol works**.  
- **X11 vs. modern alternatives** like **Wayland and RDP**.  
- **10 practical code examples** to interact with X11.  
-->

***

## The History of X Windows (X11)

The **X Window System** was created at **MIT in 1984** as a way to provide **network-transparent GUIs** for Unix workstations. Unlike Windows or macOS, X11 separates the **display server** (which draws the graphics) from the **client applications** (which request graphics to be drawn).

### **Why Was X11 Created?**

* Early Unix systems lacked a **standardized graphical system**.
* Needed to support **remote applications** over a network.
* Required a **device-independent** graphics framework.

### **Key Innovations of X11**

✅ **Network Transparency** → Run GUI apps on **one machine**, display them **on another**.\
✅ **Modular Architecture** → Separates **display server, window manager, and clients**.\
✅ **Device Independence** → Works across different **hardware and operating systems**.\
✅ **Extendable** → Supports **custom window managers and compositors**.

> **Further Reading:**
>
> * [X Window System Wikipedia](https://en.wikipedia.org/wiki/X_Window_System)
> * [X.Org Foundation](https://www.x.org/wiki/)

***

## How X11 Works

X11 follows a **client-server architecture**:

1. **X Server** → Manages **display, input devices (mouse, keyboard), and screen drawing**.
2. **X Clients** → Applications that send **requests to the X Server** (e.g., open a window, draw a button).
3. **Window Manager** → Handles **window placement, decorations, and user interactions**.

### **Step-by-Step Communication in X11**

4. **X Client (app) starts** and connects to the **X Server**.
5. The client **requests a window** to be created.
6. The **X Server processes the request** and sends back a window ID.
7. The client **draws buttons, text, and graphics** inside the window.
8. User **interacts with the window**, and the X Server **relays input events**.

This model allows **remote applications** to display windows on a **different computer** (e.g., SSH forwarding).

***

## X11 vs. Modern Display Alternatives

| Feature                  | X11 (X Windows)        | Wayland       | RDP                    |
| ------------------------ | ---------------------- | ------------- | ---------------------- |
| **Network Transparency** | ✅ Yes                  | ❌ No          | ✅ Yes                  |
| **Performance**          | ❌ Higher latency       | ✅ Faster      | ✅ Optimized for remote |
| **Security**             | ❌ Weak (no sandboxing) | ✅ Better      | ✅ Secure               |
| **Customization**        | ✅ Highly configurable  | ❌ Limited     | ❌ Fixed                |
| **Used By**              | Linux, BSD, Unix       | Linux (newer) | Windows, Linux         |

💡 **Verdict:** X11 is great for **networked applications**, but **Wayland** is replacing it for **local performance**.

***

## 10 X11 Code Examples

### **1. Check Running X Server Version**

```bash
xdpyinfo | grep "version"
```

### **2. List Available Screens and Displays**

```bash
xrandr --listmonitors
```

### **3. Open a Remote X11 Application Over SSH**

```bash
ssh -X user@remote-machine gedit
```

### **4. Move a Window to a Different Screen Position**

```bash
xdotool search --onlyvisible --name "Firefox" windowmove 100 100
```

### **5. Take a Screenshot Using X11 Tools**

```bash
import -window root screenshot.png  # Requires ImageMagick
```

### **6. Create a Simple X11 Window in Python (Xlib)**

```python
from Xlib import X, display

d = display.Display()
s = d.screen()
w = s.root.create_window(10, 10, 300, 200, 1, X.CopyFromParent)
w.map()
d.flush()
input("Press Enter to close...")
```

### **7. Change Screen Resolution Using XRandR**

```bash
xrandr --output HDMI-1 --mode 1920x1080
```

### **8. Capture Keyboard Events Using xev**

```bash
xev | grep KeyPress
```

### **9. Close a Window Using XKill**

```bash
xkill
```

### **10. Set the X Display Variable for Remote Applications**

```bash
export DISPLAY=:0.0
```

***

## Key Takeaways

* **X11 enables GUI applications on Linux, Unix, and BSD systems.**
* **It follows a client-server model, allowing remote applications.**
* **Modern alternatives like Wayland improve performance but lack network transparency.**
* **X11 is still widely used but slowly being replaced.**

***

## References

9. [X Window System Wikipedia](https://en.wikipedia.org/wiki/X_Window_System)
10. [X.Org Foundation](https://www.x.org/wiki/)
11. [Wayland vs. X11](https://wayland.freedesktop.org/)
12. [Using X11 Forwarding Over SSH](https://wiki.archlinux.org/title/SSH_keys#X11_forwarding)
