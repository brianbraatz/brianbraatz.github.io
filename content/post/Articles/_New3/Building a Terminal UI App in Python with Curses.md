---
title: Building a Terminal UI App in Python with Curses
description: Party Like its the late 1980s...
slug: building-terminal-ui-python-curses
date: 2022-08-30
image: post/Articles/IMAGES/Curses.jpg
categories:
  - Python
  - Python-Curses
  - GUI
tags:
  - Python
  - CLI
  - Curses
  - Terminal
  - UI
  - Development
draft: false
weight: 333
categories_ref:
  - Python
  - Python-Curses
  - GUI
lastmod: 2025-03-14T15:45:19.787Z
---
Image from this great book:

https://www.amazon.com/Programming-curses-Manipulation-Nutshell-Handbooks/dp/0937175021/?tag=wkss20-20

# Building a Terminal UI App in Python with Curses

## 🏛️ A Brief History of Curses

Long before **fancy GUIs** and **React-powered dashboards**, developers lived in the **dark ages of computing**—a time when all they had was **a black screen and a blinking cursor**.

Enter **curses**, a library that made text-based user interfaces (TUIs) feel *less* like staring into the abyss. Originally developed for **UNIX** in the late 1980s, curses allowed programmers to **draw windows, handle input, and create interactive terminal apps** without going insane.

Fast forward to today, and **Python still has curses**! If you’ve ever wanted to **build a terminal dashboard, a text-based game, or an old-school menu system**, curses is **the way to go**.

<!-- 
So, grab your keyboard, channel your inner 1980s hacker, and let’s build a **TUI app with Python and curses!**
-->

***

## 🚀 What is Curses?

Curses is a library that allows you to:

* **Create windows and panels** in a terminal.
* **Move the cursor** and **handle user input** dynamically.
* **Change text colors and styles** (because plain text is boring).
* **Build complex terminal-based UIs** without losing your mind.

It’s **perfect for building dashboards, interactive menus, and text-based applications**.

***

## 🛠 Setting Up Curses in Python

### **Installing Curses**

The good news? **Curses is built into Python!** 🎉

For **Linux/macOS**, you’re good to go. Just import it.

For **Windows**, you’ll need to install a version of curses:

```sh
pip install windows-curses
```

Now, let’s write some **basic curses code**.

***

## 🔥 Your First Curses App

Let’s start simple: **print text on the screen using curses.**

Create a file called `curses_app.py`:

```python
import curses

def main(stdscr):
    # Clear the screen
    stdscr.clear()

    # Print some text
    stdscr.addstr(2, 5, "Hello, Terminal UI!")
    
    # Refresh the screen to show changes
    stdscr.refresh()

    # Wait for user input
    stdscr.getch()

# Run the curses application
curses.wrapper(main)
```

### **Run it:**

```sh
python curses_app.py
```

You'll see **"Hello, Terminal UI!"** at row 2, column 5.

When you press a key, **the app exits**. **Congratulations! You just wrote your first curses program.**

***

## 🎨 Adding Colors and Styling

Plain white text? **Nah. Let’s add some color!**

```python
import curses

def main(stdscr):
    # Enable color support
    curses.start_color()
    curses.init_pair(1, curses.COLOR_RED, curses.COLOR_BLACK)

    stdscr.clear()
    stdscr.addstr(2, 5, "This is red text!", curses.color_pair(1))
    stdscr.refresh()
    stdscr.getch()

curses.wrapper(main)
```

### **What’s Happening?**

* `curses.start_color()` → Enables color support.
* `curses.init_pair(1, curses.COLOR_RED, curses.COLOR_BLACK)` → Defines a **color pair** (red text, black background).
* `stdscr.addstr(..., curses.color_pair(1))` → Prints text **in red**.

Run it, and behold: **colored text in the terminal!** 🔥

***

## 📦 Creating a Terminal UI Layout

Now, let’s create a **simple terminal dashboard** with **multiple windows**.

```python
import curses

def main(stdscr):
    curses.curs_set(0)  # Hide cursor
    stdscr.clear()

    # Create two windows
    height, width = 10, 40
    win1 = curses.newwin(height, width, 2, 2)
    win2 = curses.newwin(height, width, 2, 45)

    # Add borders and text
    win1.box()
    win2.box()
    win1.addstr(1, 1, "Window 1: Logs")
    win2.addstr(1, 1, "Window 2: Status")

    # Refresh windows
    win1.refresh()
    win2.refresh()

    stdscr.getch()

curses.wrapper(main)
```

### **What’s Happening?**

* We create **two terminal windows**.
* Each window has a **box (border)**.
* We add some **text inside each window**.

Run it, and you’ll see a **split-terminal UI** with two labeled sections. **Pretty cool, right?**

***

## ⌨️ Handling User Input

Now, let’s make **an interactive menu**.

```python
import curses

menu = ["Start", "Settings", "Exit"]

def main(stdscr):
    curses.curs_set(0)  # Hide cursor
    current_row = 0

    while True:
        stdscr.clear()
        for i, item in enumerate(menu):
            if i == current_row:
                stdscr.addstr(i + 2, 5, item, curses.A_REVERSE)  # Highlight selected
            else:
                stdscr.addstr(i + 2, 5, item)

        key = stdscr.getch()

        if key == curses.KEY_UP and current_row > 0:
            current_row -= 1
        elif key == curses.KEY_DOWN and current_row < len(menu) - 1:
            current_row += 1
        elif key == ord("
"):  # Enter key
            if menu[current_row] == "Exit":
                break

curses.wrapper(main)
```

### **How It Works:**

* Use **arrow keys** to move through the menu.
* Press **Enter** to select an option.
* Selecting **"Exit"** closes the program.

Now, you have a **fully interactive terminal menu!** 🎉

***

## 🖥️ OS Compatibility: Does Curses Work Everywhere?

| Feature              | Linux | macOS |           Windows          |
| -------------------- | :---: | :---: | :------------------------: |
| Basic Curses Support |   ✅   |   ✅   | ❌ (Needs `windows-curses`) |
| Colors               |   ✅   |   ✅   |              ✅             |
| Multi-Windows        |   ✅   |   ✅   |              ✅             |
| Keyboard Input       |   ✅   |   ✅   |              ✅             |
| Mouse Support        |   ✅   |   ✅   |              ❌             |

### **Explanation**

* **Linux/macOS**: Curses works **out-of-the-box** with full functionality.
* **Windows**: Needs **`windows-curses`** (`pip install windows-curses`) for basic support, but **mouse handling is tricky**.
* **Colors, multi-windows, and keyboard input** work on all platforms, but **mouse input is limited on Windows**.

## If you’re targeting **cross-platform development**, make sure to test on all systems!

## 🚀 Conclusion

With curses, you can build **complex terminal-based applications** without needing a GUI. Today, we covered:\
✅ **Creating a basic curses app**\
✅ **Adding colors and styles**\
✅ **Creating multiple windows**\
✅ **Handling user input**

You now have **the foundation for building a full-fledged terminal UI app**!

<!-- 
---

## 💡 Ideas for Future Articles

- **"Building a Real-Time System Monitor with Curses"**  
- **"Creating a Terminal-Based To-Do List App"**  
- **"How to Use Panels and Subwindows in Curses"**  
-->

***

## 📚 References

1. [Python Curses Documentation](https://docs.python.org/3/library/curses.html)
2. [Curses Programming with Python](https://docs.python.org/3/howto/curses.html)
3. [Unix Curses Guide](https://invisible-island.net/ncurses/)
