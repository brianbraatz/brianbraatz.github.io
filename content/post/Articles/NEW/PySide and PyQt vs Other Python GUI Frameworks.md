---
title: PySide and PyQt vs Other Python GUI Frameworks
description: Read if you are trying to decide PySide, PyQt between kinter, Kivy, and wxPython
slug: pyside-pyqt-vs-other-python-gui-frameworks
date: 2022-12-07
image: post/Articles/IMAGES/pysidevspyqt.jpg
categories:
  - Python
  - Python-PyQt
  - Python-PySide
  - GUI
  - Qt GUI Framework
  - Python-Gui
tags:
  - Pyside
  - Pyqt
  - Tkinter
  - Kivy
  - Wxpython
  - Gui
  - Python
  - Frameworks
  - Comparison
  - Qt
  - PythonGuI
draft: false
weight: 30
categories_ref:
  - Python
  - Python-PyQt
  - Python-PySide
  - GUI
  - Qt GUI Framework
  - Python-Gui
slug_calculated: https://brianbraatz.github.io/p/pyside-pyqt-vs-other-python-gui-frameworks
lastmod: 2025-03-14T16:40:22.328Z
---
# PySide and PyQt vs Other Python GUI Frameworks

<!--
When building a **GUI application in Python**, the choice of framework can make a big difference. PySide and PyQt are two of the most powerful options, but they aren’t the only ones. How do they compare to other frameworks like **Tkinter, Kivy, and wxPython**? Let's find out!
-->

**I found a great video on youtube - that goes more in depth.. with visual examples!**

kinter, Kivy, and wxPython\
{{< youtube Q72b6tDQMKQ >}}

below is more of a cheat sheet  of collected info to help you (and me :) ) decide

## Overview of Python GUI Frameworks

| Framework       | Licensing                                                 | Best For                                          | Learning Curve               | Cross-Platform |
| --------------- | --------------------------------------------------------- | ------------------------------------------------- | ---------------------------- | -------------- |
| **PyQt**        | GPL/Commercial                                            | Feature-rich, professional apps                   | Moderate                     | Yes            |
| **PySide**      | LGPL                                                      | Feature-rich, Qt-based apps                       | Moderate                     | Yes            |
| **Tkinter**     | Built-in (Tcl/Tk)                                         | Simple desktop apps                               | Easy                         | Yes            |
| **Kivy**        | MIT                                                       | Mobile & touch-friendly apps                      | Steep                        | Yes            |
| **wxPython**    | LGPL                                                      | Native look-and-feel apps                         | Moderate                     | Yes            |
| **PySimpleGUI** | - Simplifies GUI development- Wraps Tkinter, Qt, WxPython | - Limited flexibility compared to full frameworks | Quick prototypes, simple UIs |                |
| **PySimpleGUI** | - Simplifies GUI development- Wraps Tkinter, Qt, WxPython | - Limited flexibility compared to full frameworks | Quick prototypes, simple UIs |                |

## PySide and PyQt: The Power Duo

PySide and PyQt are both **bindings for the Qt framework**, providing a rich set of widgets, styling options, and cross-platform capabilities.

### Why Choose PySide or PyQt?

* **You need a professional-looking UI with modern widgets.**
* **You want a stable framework that works across Windows, Mac, and Linux.**
* **You want to use Qt Designer to visually design your UI.**
* **You need advanced features like signals, slots, and animations.**

### Key Differences Between PyQt and PySide

| Feature           | PyQt                | PySide               |
| ----------------- | ------------------- | -------------------- |
| License           | GPL (or commercial) | LGPL                 |
| Maintainer        | Riverbank Computing | The Qt Company       |
| API Naming        | Some differences    | Closer to C++ Qt API |
| Community Support | Larger              | Growing              |

Good link on the licensing issues with PyQt and PySide\
https://www.pythonguis.com/faq/pyqt-vs-pyside/

## How PySide and PyQt Compare to Other Frameworks

### **Tkinter: The Built-in Choice**

Tkinter comes **pre-installed** with Python and is great for **simple desktop applications**. It has a basic widget set but lacks the **modern look and feel** of Qt-based frameworks.

**Use Tkinter if:**

* You want a quick and easy way to build simple GUIs.
* You don’t need advanced UI elements.
* You prefer a lightweight solution.

### **Kivy: The Mobile-Friendly Framework**

Kivy is focused on touch interfaces and is great for **mobile and multi-touch applications**. However, it has a **steeper learning curve** and is not ideal for traditional desktop applications.

**Use Kivy if:**

* You need to build a cross-platform mobile app.
* You want to use multi-touch gestures.
* You don’t mind a non-native look.

### **wxPython: The Native Look-and-Feel Option**

wxPython is another solid option that provides **native-looking** applications across platforms. It’s a bit less popular than PyQt/PySide but can be a great choice for **lightweight applications**.

**Use wxPython if:**

* You need an app that looks and feels native on each OS.
* You prefer a simpler licensing model than PyQt.

## Side By Side

| **Library**  | **Pros**                                                                                  | **Cons**                                                        | **Best For**                                |
| ------------ | ----------------------------------------------------------------------------------------- | --------------------------------------------------------------- | ------------------------------------------- |
| **Tkinter**  | - Built-in with Python  <br>- Simple to use  <br>- Lightweight                            | - Basic UI elements  <br>- Outdated look  <br>- Limited styling | Small, simple applications & beginners      |
| **PyQt**     | - Feature-rich  <br>- Professional look  <br>- Cross-platform  <br>- Strong documentation | - Learning curve  <br>- GPL/commercial license issues           | Complex applications, professional software |
| **PySide**   | - Similar to PyQt but LGPL license  <br>- Feature-rich                                    | - Less community support than PyQt                              | Alternative to PyQt with LGPL licensing     |
| **Kivy**     | - Supports mobile and touch-based UIs  <br>- Modern look                                  | - Not native-looking on desktop  <br>- Can be tricky to style   | Mobile apps, multi-touch interfaces         |
| **wxPython** | - Native look & feel  <br>- Cross-platform  <br>- Good documentation                      | - API complexity  <br>- Not as widely used as PyQt/PySide       | Desktop applications needing native UI      |

<!-- 
Each library has its strengths and weaknesses, making them suitable for different types of projects. If you're making a quick prototype, PySimpleGUI is great. If you need a full-fledged, modern UI, PyQt or PySide are excellent choices. For mobile or touch-based apps, Kivy is the way to go.

## Conclusion
PyQt and PySide are **powerful choices** for GUI development, offering the **richest** feature set among Python GUI frameworks. However, if you need something simple, **Tkinter** is a great starting point. If you're targeting **mobile**, **Kivy** is the way to go, and if you want a **native desktop look**, **wxPython** is worth considering.

No matter what you choose, Python has **great GUI options** for every type of project. Now go build something awesome! 🚀

-->

## PySide and PyQt Compared

**HAHAH!! can you spot the differences?**\*\*

### PyQt Code Sample

```python
from PyQt6.QtWidgets import QApplication, QLabel, QWidget
import sys

app = QApplication(sys.argv)
window = QWidget()
window.setWindowTitle("PyQt Example")
label = QLabel("Hello, PyQt!", parent=window)
window.show()
sys.exit(app.exec())
```

### PySide Code Sample

```python
from PySide6.QtWidgets import QApplication, QLabel, QWidget
import sys

app = QApplication(sys.argv)
window = QWidget()
window.setWindowTitle("PySide Example")
label = QLabel("Hello, PySide!", parent=window)
window.show()
sys.exit(app.exec())
```

## How PySide and PyQt Compare to Other Frameworks

### **Tkinter: The Built-in Choice**

Tkinter comes **pre-installed** with Python and is gear to **simple desktop applications**.

It has a basic widget set but lacks the modern look and feel of Qt-based frameworks.

**Use Tkinter if:**

* You want a quick and easy way to build simple GUIs.
* You don’t need advanced UI elements.
* You prefer a lightweight solution.

#### Tkinter Code Sample

```python
import tkinter as tk

root = tk.Tk()
root.title("Tkinter Example")
label = tk.Label(root, text="Hello, Tkinter!")
label.pack()
root.mainloop()
```

### **Kivy: The Mobile-Friendly Framework**

Kivy is focused on touch interfaces.  ala iPhone and Android.\
However, humans on the interwebs seem to complain it has a steeper learning curve .

**Use Kivy if:**

* You need to build a cross-platform mobile app.
* You want to use multi-touch gestures.
* You don’t mind a non-native look.

#### Kivy Code Sample

```python
from kivy.app import App
from kivy.uix.label import Label

class MyApp(App):
    def build(self):
        return Label(text="Hello, Kivy!")

MyApp().run()
```

### **wxPython: The Native Look-and-Feel Option**

wxPython is another solid option that provides **native-looking** applications across platforms. It’s a bit less popular than PyQt/PySide but can be a great choice for **lightweight applications**.

**Use wxPython if:**

* You need an app that looks and feels native on each OS.
* You prefer a simpler licensing model than PyQt.

#### wxPython Code Sample

```python
import wx

app = wx.App()
frame = wx.Frame(None, title="wxPython Example")
panel = wx.Panel(frame)
text = wx.StaticText(panel, label="Hello, wxPython!", pos=(10,10))
frame.Show()
app.MainLoop()
```

Yes, you **can** use **PyQt** for an internal application within your company **without selling it**, but there's a licensing detail you need to be aware of:

## The confusing Licensing situation with PyQt and Pyside

### **PyQt Licensing**

* **PyQt is dual-licensed**:

- \***GPL (General Public License)** – **Free**, but you must **open-source** your code if you distribute the app outside your company.
- . **Commercial License** – Required if you want to keep your code **proprietary** and distribute it outside your company.

### **PySide Licensing**

* **PySide is licensed under LGPL (Lesser General Public License)**.
* This means you can use it **for free**, even in **commercial and closed-source applications**, as long as you **dynamically link** against the Qt libraries.
* Unlike PyQt (which has a GPL license that requires you to open-source your code unless you buy a license), PySide allows proprietary software **without needing a commercial license**.

### **When Might You Need a Commercial License for Qt?**

1. If you **statically link** Qt instead of dynamically linking it (LGPL requires dynamic linking).
2. If you use certain **Qt commercial-only modules** (e.g., some enterprise features).
3. If you want **official support** from The Qt Company

## Reference Links

* **PyQt (Qt Designer)**: <https://www.riverbankcomputing.com/software/pyqt/>
* **PySide (Qt Designer)**: <https://doc.qt.io/qtforpython/>
* **Tkinter (PAGE - Python GUI Editor)**: <http://page.sourceforge.net/>
* **Kivy (Kivy Designer - Unofficial)**: <https://github.com/kivy/kivy-designer>
* **wxPython (wxFormBuilder)**: <https://github.com/wxFormBuilder/wxFormBuilder>

<!--

--------

---
title: "PySide and PyQt vs Other Python GUI Frameworks"
description: "PySide and PyQt compared to other popular Python GUI frameworks."
slug: "pyside-pyqt-vs-other-python-gui-frameworks"
date: "2022-12-07"
image: "post/Articles/IMAGES/14.jpg"
categories: []
tags: ["Pyside", "Pyqt", "Tkinter", "Kivy", "Wxpython", "Gui", "Python", "Frameworks", "Comparison"]
draft: false
weight: 30
---

# PySide and PyQt vs Other Python GUI Frameworks

When building a **GUI application in Python**, the choice of framework can make a big difference. PySide and PyQt are two of the most powerful options, but they aren’t the only ones. How do they compare to other frameworks like **Tkinter, Kivy, and wxPython**? Let's find out!

## A Quick Overview of Python GUI Frameworks

| Framework  | Licensing | Best For | Learning Curve | Cross-Platform |
|------------|-----------|-----------|---------------|---------------|
| **PyQt**   | GPL/Commercial | Feature-rich, professional apps | Moderate | Yes |
| **PySide** | LGPL | Feature-rich, Qt-based apps | Moderate | Yes |
| **Tkinter** | Built-in (Tcl/Tk) | Simple desktop apps | Easy | Yes |
| **Kivy**   | MIT | Mobile & touch-friendly apps | Steep | Yes |
| **wxPython** | LGPL | Native look-and-feel apps | Moderate | Yes |

## PySide and PyQt: The Power Duo

PySide and PyQt are both **bindings for the Qt framework**, providing a rich set of widgets, styling options, and cross-platform capabilities. 

### Why Choose PySide or PyQt?
- **You need a professional-looking UI with modern widgets.**
- **You want a stable framework that works across Windows, Mac, and Linux.**
- **You want to use Qt Designer to visually design your UI.**
- **You need advanced features like signals, slots, and animations.**

### Key Differences Between PyQt and PySide
| Feature        | PyQt | PySide |
|---------------|------|--------|
| License       | GPL (or commercial) | LGPL |
| Maintainer    | Riverbank Computing | The Qt Company |
| API Naming    | Some differences | Closer to C++ Qt API |
| Community Support | Larger | Growing |

### PyQt Code Sample
~~~python
from PyQt6.QtWidgets import QApplication, QLabel, QWidget
import sys

app = QApplication(sys.argv)
window = QWidget()
window.setWindowTitle("PyQt Example")
label = QLabel("Hello, PyQt!", parent=window)
window.show()
sys.exit(app.exec())
~~~

### PySide Code Sample
~~~python
from PySide6.QtWidgets import QApplication, QLabel, QWidget
import sys

app = QApplication(sys.argv)
window = QWidget()
window.setWindowTitle("PySide Example")
label = QLabel("Hello, PySide!", parent=window)
window.show()
sys.exit(app.exec())
~~~

## How PySide and PyQt Compare to Other Frameworks

### **Tkinter: The Built-in Choice**
Tkinter comes **pre-installed** with Python and is great for **simple desktop applications**. It has a basic widget set but lacks the **modern look and feel** of Qt-based frameworks.

**Use Tkinter if:**
- You want a quick and easy way to build simple GUIs.
- You don’t need advanced UI elements.
- You prefer a lightweight solution.

#### Tkinter Code Sample
~~~python
import tkinter as tk

root = tk.Tk()
root.title("Tkinter Example")
label = tk.Label(root, text="Hello, Tkinter!")
label.pack()
root.mainloop()
~~~

### **Kivy: The Mobile-Friendly Framework**
Kivy is focused on touch interfaces and is great for **mobile and multi-touch applications**. However, it has a **steeper learning curve** and is not ideal for traditional desktop applications.

**Use Kivy if:**
- You need to build a cross-platform mobile app.
- You want to use multi-touch gestures.
- You don’t mind a non-native look.

#### Kivy Code Sample
~~~python
from kivy.app import App
from kivy.uix.label import Label

class MyApp(App):
    def build(self):
        return Label(text="Hello, Kivy!")

MyApp().run()
~~~

### **wxPython: The Native Look-and-Feel Option**
wxPython is another solid option that provides **native-looking** applications across platforms. It’s a bit less popular than PyQt/PySide but can be a great choice for **lightweight applications**.

**Use wxPython if:**
- You need an app that looks and feels native on each OS.
- You prefer a simpler licensing model than PyQt.

#### wxPython Code Sample
~~~python
import wx

app = wx.App()
frame = wx.Frame(None, title="wxPython Example")
panel = wx.Panel(frame)
text = wx.StaticText(panel, label="Hello, wxPython!", pos=(10,10))
frame.Show()
app.MainLoop()
~~~

## Choosing the Right Framework

| If you need... | Best Choice |
|---------------|------------|
| A full-featured professional UI | **PyQt or PySide** |
| A simple built-in option | **Tkinter** |
| A mobile-friendly framework | **Kivy** |
| A native look on each OS | **wxPython** |

## Conclusion
PyQt and PySide are **powerful choices** for GUI development, offering the **richest** feature set among Python GUI frameworks. However, if you need something simple, **Tkinter** is a great starting point. If you're targeting **mobile**, **Kivy** is the way to go, and if you want a **native desktop look**, **wxPython** is worth considering.

No matter what you choose, Python has **great GUI options** for every type of project. Now go build something awesome! 🚀

--------------------

-----------------

---
title: "GUI Design Tools for Python Frameworks Compared"
description: "Comparison of GUI design tools for PySide, PyQt, Tkinter, Kivy, and wxPython."
slug: "gui-design-tools-python-frameworks"
date: "2023-03-14"
image: "post/Articles/IMAGES/22.jpg"
categories: []
tags: ["GUI", "Design Tools", "Pyqt", "Pyside", "Tkinter", "Kivy", "Wxpython", "Python", "Comparison"]
draft: false
weight: 30
---

# GUI Design Tools for Python Frameworks Compared

Building a GUI application is much easier when you have a **visual design tool** to help lay out elements. But not all Python GUI frameworks have the same level of tool support. Let’s compare the most popular **GUI design tools** for **PyQt, PySide, Tkinter, Kivy, and wxPython**.

## A Quick Overview of GUI Design Tools

| Framework  | GUI Design Tool | Drag-and-Drop? | Code Export? | Ease of Use |
|------------|----------------|----------------|--------------|-------------|
| **PyQt**   | Qt Designer | ✅ Yes | ✅ Yes (XML/UI or Python) | Moderate |
| **PySide** | Qt Designer | ✅ Yes | ✅ Yes (XML/UI or Python) | Moderate |
| **Tkinter** | PAGE (Python GUI Editor) | ✅ Yes | ✅ Yes (Python) | Easy |
| **Kivy**   | Kivy Designer (Unofficial) | ✅ Yes | ✅ Yes (KV Language) | Moderate |
| **wxPython** | wxFormBuilder | ✅ Yes | ✅ Yes (Python) | Moderate |

## PyQt and PySide: Qt Designer
Both **PyQt** and **PySide** support **Qt Designer**, a powerful WYSIWYG editor for designing GUIs visually.

### Features of Qt Designer:
- **Drag-and-drop UI building**
- **Exports .ui files**, which can be converted into Python code
- **Supports complex layouts, widgets, and styles**

#### Qt Designer Workflow Example:
~~~bash
# Convert .ui file to Python
pyuic6 -x mydesign.ui -o mydesign.py
~~~

**Use Qt Designer if:**
- You need a full-featured, professional GUI designer
- You want an easy way to create complex layouts

## Tkinter: PAGE (Python GUI Editor)
**PAGE** (Python Automatic GUI Generator) is one of the few tools available for designing **Tkinter** interfaces visually.

### Features of PAGE:
- **Drag-and-drop widget placement**
- **Generates Python code for Tkinter**
- **Basic widget support**

#### PAGE Workflow Example:
4. Design your interface visually in PAGE.
5. Generate Python code and integrate it into your application.

**Use PAGE if:**
- You’re working with Tkinter and want a visual editor
- You need a quick and simple layout tool

## Kivy: Kivy Designer (Unofficial)
While Kivy does not have an official GUI design tool, **Kivy Designer** (a community project) provides a way to visually arrange widgets.

### Features of Kivy Designer:
- **Drag-and-drop layout creation**
- **Exports UI in KV language**
- **Supports Kivy’s unique properties and layouts**

#### Kivy Designer Workflow Example:
~~~kv
BoxLayout:
    Label:
        text: "Hello, Kivy!"
    Button:
        text: "Click Me"
~~~

**Use Kivy Designer if:**
- You’re working with Kivy and want a visual layout tool
- You don’t mind using an unofficial tool

## wxPython: wxFormBuilder
For **wxPython**, **wxFormBuilder** is a popular choice for creating interfaces visually.

### Features of wxFormBuilder:
- **Drag-and-drop UI creation**
- **Generates Python code**
- **Supports sizers for flexible layouts**

#### wxFormBuilder Workflow Example:
6. Design your interface in wxFormBuilder.
7. Export Python code and integrate it into your application.

**Use wxFormBuilder if:**
- You need a native-looking GUI with wxPython
- You want a tool similar to Qt Designer

## Choosing the Right GUI Design Tool

| If you need... | Best Choice |
|---------------|------------|
| A professional GUI editor | **Qt Designer (PyQt/PySide)** |
| A simple drag-and-drop tool for Tkinter | **PAGE** |
| A designer for mobile-friendly interfaces | **Kivy Designer** |
| A native-looking UI with wxPython | **wxFormBuilder** |

## Conclusion
The best GUI design tool depends on the **framework** you are using. **Qt Designer** is the most feature-rich option, but if you're working with **Tkinter, Kivy, or wxPython**, you still have some great tools available. 

Now go forth and **drag-and-drop your way to GUI greatness!** 🚀



========

---
title: "GUI Design Tools for Python Frameworks Compared"
description: "Comparison of GUI design tools for PySide, PyQt, Tkinter, Kivy, and wxPython."
slug: "gui-design-tools-python-frameworks"
date: "2023-03-14"
image: "post/Articles/IMAGES/22.jpg"
categories: []
tags: ["GUI", "Design Tools", "Pyqt", "Pyside", "Tkinter", "Kivy", "Wxpython", "Python", "Comparison"]
draft: false
weight: 30
---

# GUI Design Tools for Python Frameworks Compared

Building a GUI application is much easier when you have a **visual design tool** to help lay out elements. But not all Python GUI frameworks have the same level of tool support. Let’s compare the most popular **GUI design tools** for **PyQt, PySide, Tkinter, Kivy, and wxPython**.

## A Quick Overview of GUI Design Tools

| Framework  | GUI Design Tool | Drag-and-Drop? | Code Export? | Ease of Use |
|------------|----------------|----------------|--------------|-------------|
| **PyQt**   | Qt Designer | ✅ Yes | ✅ Yes (XML/UI or Python) | Moderate |
| **PySide** | Qt Designer | ✅ Yes | ✅ Yes (XML/UI or Python) | Moderate |
| **Tkinter** | PAGE (Python GUI Editor) | ✅ Yes | ✅ Yes (Python) | Easy |
| **Kivy**   | Kivy Designer (Unofficial) | ✅ Yes | ✅ Yes (KV Language) | Moderate |
| **wxPython** | wxFormBuilder | ✅ Yes | ✅ Yes (Python) | Moderate |

## PyQt and PySide: Qt Designer
Both **PyQt** and **PySide** support **Qt Designer**, a powerful WYSIWYG editor for designing GUIs visually.

### Features of Qt Designer:
- **Drag-and-drop UI building**
- **Exports .ui files**, which can be converted into Python code
- **Supports complex layouts, widgets, and styles**

#### Qt Designer Workflow Example:
~~~bash
# Convert .ui file to Python
pyuic6 -x mydesign.ui -o mydesign.py
~~~

**Use Qt Designer if:**
- You need a full-featured, professional GUI designer
- You want an easy way to create complex layouts

## Tkinter: PAGE (Python GUI Editor)
**PAGE** (Python Automatic GUI Generator) is one of the few tools available for designing **Tkinter** interfaces visually.

### Features of PAGE:
- **Drag-and-drop widget placement**
- **Generates Python code for Tkinter**
- **Basic widget support**

#### PAGE Workflow Example:
8. Design your interface visually in PAGE.
9. Generate Python code and integrate it into your application.

**Use PAGE if:**
- You’re working with Tkinter and want a visual editor
- You need a quick and simple layout tool

## Kivy: Kivy Designer (Unofficial)
While Kivy does not have an official GUI design tool, **Kivy Designer** (a community project) provides a way to visually arrange widgets.

### Features of Kivy Designer:
- **Drag-and-drop layout creation**
- **Exports UI in KV language**
- **Supports Kivy’s unique properties and layouts**

#### Kivy Designer Workflow Example:
~~~kv
BoxLayout:
    Label:
        text: "Hello, Kivy!"
    Button:
        text: "Click Me"
~~~

**Use Kivy Designer if:**
- You’re working with Kivy and want a visual layout tool
- You don’t mind using an unofficial tool

## wxPython: wxFormBuilder
For **wxPython**, **wxFormBuilder** is a popular choice for creating interfaces visually.

### Features of wxFormBuilder:
- **Drag-and-drop UI creation**
- **Generates Python code**
- **Supports sizers for flexible layouts**

#### wxFormBuilder Workflow Example:
10. Design your interface in wxFormBuilder.
11. Export Python code and integrate it into your application.

**Use wxFormBuilder if:**
- You need a native-looking GUI with wxPython
- You want a tool similar to Qt Designer

## Choosing the Right GUI Design Tool

| If you need... | Best Choice |
|---------------|------------|
| A professional GUI editor | **Qt Designer (PyQt/PySide)** |
| A simple drag-and-drop tool for Tkinter | **PAGE** |
| A designer for mobile-friendly interfaces | **Kivy Designer** |
| A native-looking UI with wxPython | **wxFormBuilder** |

## Conclusion
The best GUI design tool depends on the **framework** you are using. **Qt Designer** is the most feature-rich option, but if you're working with **Tkinter, Kivy, or wxPython**, you still have some great tools available. 

Now go forth and **drag-and-drop your way to GUI greatness!** 🚀

## Reference Links
- **PyQt (Qt Designer)**: [https://www.riverbankcomputing.com/software/pyqt/](https://www.riverbankcomputing.com/software/pyqt/)
- **PySide (Qt Designer)**: [https://doc.qt.io/qtforpython/](https://doc.qt.io/qtforpython/)
- **Tkinter (PAGE - Python GUI Editor)**: [http://page.sourceforge.net/](http://page.sourceforge.net/)
- **Kivy (Kivy Designer - Unofficial)**: [https://github.com/kivy/kivy-designer](https://github.com/kivy/kivy-designer)
- **wxPython (wxFormBuilder)**: [https://github.com/wxFormBuilder/wxFormBuilder](https://github.com/wxFormBuilder/wxFormBuilder)



-->
