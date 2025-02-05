---
title: PyQt and PySide Compared
description: PyQt and PySide Compared
slug: pyqt-and-pyside-compared
date: 2023-05-18
image: post/Articles/IMAGES/27.jpg
categories: []
tags:
  - Pyqt
  - Pyside
  - Python
  - Gui
  - Qt
  - Comparison
  - Programming
draft: true
weight: 30
lastmod: 2025-02-05T19:05:55.359Z
---
# PyQt and PySide Compared

If you’ve ever wanted to build a GUI in Python, you’ve probably heard of **PyQt** and **PySide**. But what’s the difference? Which one should you use? And why do they both exist? Let’s break it all down.

## A Brief History of PyQt and PySide

### PyQt

PyQt is a set of Python bindings for the Qt application framework, developed by **Riverbank Computing**. It has been around since the early 2000s and has been one of the most popular choices for building Python-based GUI applications.

* Official site: <https://www.riverbankcomputing.com/software/pyqt/>
* Documentation: <https://www.riverbankcomputing.com/static/Docs/PyQt6/>

### PySide

PySide was created by **Nokia** in 2009 as a response to licensing restrictions in PyQt. The primary goal was to provide a **more permissive licensing model** (LGPL instead of GPL). The project eventually passed into the hands of **The Qt Company**, and they maintain it today.

* Official site: <https://doc.qt.io/qtforpython/>
* Documentation: <https://doc.qt.io/qtforpython/tutorials/index.html>

## How Are They Similar?

Both PyQt and PySide:

* Provide Python bindings for the Qt framework.
* Offer nearly identical APIs.
* Support QtWidgets, QtCore, QtGui, and more.
* Can be used to create cross-platform GUI applications.
* Work with Qt Designer for UI design.

## Key Differences

| Feature             | PyQt                | PySide                |
| ------------------- | ------------------- | --------------------- |
| License             | GPL (or commercial) | LGPL                  |
| Maintainer          | Riverbank Computing | The Qt Company        |
| Installation        | `pip install pyqt6` | `pip install pyside6` |
| API Naming          | Some methods differ | Closer to C++ Qt API  |
| Qt Designer Support | Yes                 | Yes                   |
| Stability           | More established    | Actively developed    |

Here’s a table summarizing the conclusions from the video about the different Python GUI libraries:

| **Library**     | **Pros**                                                                | **Cons**                                                | **Best For**                                |
| --------------- | ----------------------------------------------------------------------- | ------------------------------------------------------- | ------------------------------------------- |
| **Tkinter**     | - Built-in with Python- Simple to use- Lightweight                      | - Basic UI elements- Outdated look- Limited styling     | Small, simple applications & beginners      |
| **PyQt**        | - Feature-rich- Professional look- Cross-platform- Strong documentation | - Learning curve- GPL/commercial license issues         | Complex applications, professional software |
| **PySide**      | - Similar to PyQt but LGPL license- Feature-rich                        | - Less community support than PyQt                      | Alternative to PyQt with LGPL licensing     |
| **PySimpleGUI** | - Simplifies GUI development- Wraps Tkinter, Qt, WxPython               | - Limited flexibility compared to full frameworks       | Quick prototypes, simple UIs                |
| **Kivy**        | - Supports mobile and touch-based UIs- Modern look                      | - Not native-looking on desktop- Can be tricky to style | Mobile apps, multi-touch interfaces         |
| **wxPython**    | - Native look & feel- Cross-platform- Good documentation                | - API complexity- Not as widely used as PyQt/PySide     | Desktop applications needing native UI      |

Each library has its strengths and weaknesses, making them suitable for different types of projects. If you're making a quick prototype, PySimpleGUI is great. If you need a full-fledged, modern UI, PyQt or PySide are excellent choices. For mobile or touch-based apps, Kivy is the way to go.

## Code Comparison

Let’s look at a simple example: a basic Qt window in both PyQt and PySide.

### PyQt6 Example

```python
from PyQt6.QtWidgets import QApplication, QWidget
import sys

app = QApplication(sys.argv)
window = QWidget()
window.setWindowTitle("Hello, PyQt!")
window.show()
sys.exit(app.exec())
```

### PySide6 Example

```python
from PySide6.QtWidgets import QApplication, QWidget
import sys

app = QApplication(sys.argv)
window = QWidget()
window.setWindowTitle("Hello, PySide!")
window.show()
sys.exit(app.exec())
```

As you can see, the differences are **minimal**, mostly in the import statement.

## Which One Should You Choose?

* **Use PyQt** if you’re okay with the GPL license or are purchasing a commercial license.
* **Use PySide** if you need LGPL licensing, want official Qt Company support, or prefer an API closer to Qt’s C++.

## Learning Resources

### PyQt Tutorials

* [Real Python: Build a GUI with PyQt](https://realpython.com/pyqt-python-gui-framework/)
* [Python GUIs: PyQt6 tutorial](https://www.pythonguis.com/tutorials/)

### PySide Tutorials

* [Qt for Python Official Tutorials](https://doc.qt.io/qtforpython/tutorials/index.html)
* [Python GUIs: PySide6 tutorial](https://www.pythonguis.com/tutorials/pyside6-creating-multi-window-applications/)

***

So, whether you go with PyQt or PySide, you’re getting a **powerful** and **flexible** GUI framework for Python. Just make sure to pick the one that fits your needs best!

Now go forth and build awesome Python GUIs! 🚀
