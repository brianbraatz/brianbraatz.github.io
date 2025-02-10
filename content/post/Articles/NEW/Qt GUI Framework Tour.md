---
title: Qt GUI Framework Tour
description: Layouts, Menus, Dialogs and Resources
slug: qt-ui-tour
date: 2021-06-18
image: post/Articles/IMAGES/Qt-framework-logo-wide.png
categories:
  - Qt GUI Framework
  - Python
  - Python-PyQt
  - CPP
tags:
  - Qt
  - Gui
  - Cross-platform
  - ApplicationDevelopment
  - SignalsAndSlots
  - Layouts
  - UserInterface
  - QtCreator
  - ObserverPattern
  - QTWidgets
  - Cross-Platform
  - CPP
draft: false
weight: 18
lastmod: 2025-02-09T23:06:09.066Z
---
## Qt

Qt is a free and open-source widget toolkit for creating graphical user interfaces as well as cross-platform applications that run on various software and hardware platforms.

Developed by the Qt Company, Qt was first released in 1995.

Qt supports GUI development as well as  database access, and XML parsing.

***

## **1. Setting Up Your Qt Project**

Before we write the code, make sure you have **Qt Creator** installed. If you haven’t done that yet, head over to [Qt's official website](https://www.qt.io/download) and grab the latest version.

Once installed:

1. Open **Qt Creator**.
2. Select **File > New Project**.
3. Choose **Qt Widgets Application**.
4. Give it a name like `HelloQt`.

***

## **2Hellow World in QT**

Here’s the simplest possible **Hello World GUI** using **Qt Widgets**.

### **hello\_qt.cpp**

```cpp
#include <QApplication>
#include <QLabel>

int main(int argc, char *argv[]) {
    QApplication app(argc, argv); // Initializes the Qt application

    QLabel label("Hello, Qt!"); // Creates a label with text
    label.show(); // Displays the label window

    return app.exec(); // Runs the application event loop
}
```

***

## **3. Explanation of the Code**

* **`#include <QApplication>`**\
  This is the core class that manages the Qt application. It handles everything, including **event loops and GUI rendering**.

* **`#include <QLabel>`**\
  This is a simple widget that displays text. Think of it as **a text box in the GUI**.

* **`QApplication app(argc, argv);`**\
  Every Qt application needs an instance of `QApplication`. It **manages application-wide resources**.

* **`QLabel label("Hello, Qt!");`**\
  We create a `QLabel` object and pass in `"Hello, Qt!"` as the text.

* **`label.show();`**\
  This makes the label **visible on the screen**.

* **`return app.exec();`**\
  This **starts the event loop**, which keeps the GUI running and responsive.

***

## Signals and Slots: Qt’s Secret Sauce 🍔

So, what **are** signals and slots?

Think of **signals** as little messages that objects send out when something interesting happens. And **slots**? They're functions that listen for those messages and spring into action. It’s like your microwave beeping (signal) when your food is ready, and you opening the door (slot) to grab your steaming-hot pizza.

This is how Qt **avoids the mess** of callbacks and makes event-driven programming as smooth as butter.

### 1. **Define the Signal and Slot** 🚦

First, you need a class that can send and receive signals. It should inherit from `QObject`, and don’t forget the magical `Q_OBJECT` macro (Qt will cry if you forget this).

```cpp
#include <QObject>

class MyObject : public QObject
{
    Q_OBJECT

public:
    MyObject(QObject *parent = nullptr);

signals:
    void mySignal(int value); // This is our signal

public slots:
    void mySlot(int value);  // This is our slot
};
```

### 2. **Implement the Slot** 🎯

Let’s say our slot just prints out the received value (because we like knowing what’s going on).

```cpp
#include "myobject.h"
#include <QDebug>

MyObject::MyObject(QObject *parent) : QObject(parent) {}

void MyObject::mySlot(int value)
{
    qDebug() << "Slot called with value:" << value;
}
```

### 3. **Connect the Signal to the Slot** 📡

This is where the magic happens! We **connect** the signal from one object to the slot of another (or the same) object.

```cpp
#include <QCoreApplication>
#include "myobject.h"

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    MyObject sender;
    MyObject receiver;

    QObject::connect(&sender, &MyObject::mySignal, &receiver, &MyObject::mySlot);

    emit sender.mySignal(42); // Boom! Magic happens here.

    return a.exec();
}
```

When `sender` emits `mySignal(42)`, the `receiver` gets the message, and `mySlot` prints:

```
Slot called with value: 42
```

That’s it! No function pointers, no global variables, **just clean, beautiful, event-driven programming**.

***

## Without Signals and Slots: The Nightmare Version 👻

Let’s imagine for a second that Qt **didn’t** have signals and slots. How would we handle communication between objects?

### Option 1: Callbacks 😨

You’d have to manually pass function pointers, manage lifetimes, and probably end up crying into your coffee.

```cpp
typedef void (*Callback)(int);

class MyObject {
public:
    void setCallback(Callback cb) { callback = cb; }
    void doSomething() { if (callback) callback(42); }

private:
    Callback callback = nullptr;
};
```

And then call it like this:

```cpp
void myFunction(int value) {
    std::cout << "Callback received: " << value << std::endl;
}

int main() {
    MyObject obj;
    obj.setCallback(myFunction);
    obj.doSomething();
}
```

It works, **but it’s ugly, prone to errors, and lacks flexibility**.

### Option 2: Observer Pattern 📜

This is a bit cleaner but requires **manual bookkeeping of subscribers**, leading to more code complexity.

```cpp
class Observer {
public:
    virtual void notify(int value) = 0;
};

class Subject {
    std::vector<Observer*> observers;
public:
    void addObserver(Observer* obs) { observers.push_back(obs); }
    void notifyObservers(int value) {
        for (auto* obs : observers) {
            obs->notify(value);
        }
    }
};
```

<!-- 
Good luck maintaining that in a large project! 😵
-->

***

## Pros and Cons of Signals and Slots

### ✅ Pros:

* **No Manual Observer Management:** Qt handles it for you.
* **Type-Safe:** No function mismatches or weird pointer issues.
* **Loose Coupling:** Objects don’t need to know who they’re talking to.
* **Multiple Listeners:** One signal can trigger multiple slots.

### ❌ Cons:

* **Slight Performance Overhead:** Signals and slots are slightly slower than direct function calls (but seriously, it’s negligible).
* **Requires Qt’s Meta-Object System (MOC):** Meaning you can’t use signals/slots in plain C++ without Qt’s preprocessing magic.

***

## Multithreading with Signals and Slots 🧵

Qt  supports cross-thread communication using signals and slots, and it’s safer than doing it manually.

Here’s the deal:

* If the **signal and slot are in the same thread**, the slot runs **immediately**.
* If they’re in **different threads**, Qt **queues the call** safely.
* You don’t have to worry about **race conditions or mutexes**!

```cpp
QObject::connect(sender, &MyObject::mySignal, receiver, &MyObject::mySlot, Qt::QueuedConnection);
```

This tells Qt to **queue** the signal in the receiver’s thread, ensuring safe execution.

***

## **1. Why You Need Layout Managers**

Without layout managers, every time you resize a window, your UI elements would **stay where they are** or get **weirdly stretched**. Instead, Qt’s layout managers:

* Adjust widget positions **dynamically**.
* Ensure UI components **don’t overlap** or disappear.
* Make your application **responsive** across different screen sizes.

In short, **you should always use a layout manager** unless you enjoy chaos.

***

## **2. The Qt Layout Managers**

### **2.1 QHBoxLayout: Lining Things Up Horizontally**

Widgets are arranged **in a row**, from left to right.

#### **Example**

```cpp
#include <QApplication>
#include <QWidget>
#include <QPushButton>
#include <QHBoxLayout>

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QWidget window;

    QHBoxLayout *layout = new QHBoxLayout;
    layout->addWidget(new QPushButton("Button 1"));
    layout->addWidget(new QPushButton("Button 2"));
    layout->addWidget(new QPushButton("Button 3"));

    window.setLayout(layout);
    window.show();
    return app.exec();
}
```

✅ **Best for**: Toolbars, small button groups\
❌ **Bad for**: Forms, grids, complex layouts

***

### **2.2 QVBoxLayout: Stacking Widgets Vertically**

Widgets are arranged **top to bottom**.

#### **Example**

```cpp
#include <QVBoxLayout>

QVBoxLayout *layout = new QVBoxLayout;
layout->addWidget(new QPushButton("Top Button"));
layout->addWidget(new QPushButton("Middle Button"));
layout->addWidget(new QPushButton("Bottom Button"));
window.setLayout(layout);
```

✅ **Best for**: Stacking buttons, settings panels\
❌ **Bad for**: Horizontal elements

***

### **2.3 QGridLayout: Making a Grid**

Widgets are arranged **in a table-like structure**, using rows and columns.

#### **Example**

```cpp
#include <QGridLayout>

QGridLayout *layout = new QGridLayout;
layout->addWidget(new QPushButton("Top Left"), 0, 0);
layout->addWidget(new QPushButton("Top Right"), 0, 1);
layout->addWidget(new QPushButton("Bottom Left"), 1, 0);
layout->addWidget(new QPushButton("Bottom Right"), 1, 1);
window.setLayout(layout);
```

✅ **Best for**: Forms, dashboards, structured layouts\
❌ **Bad for**: Single-column or single-row layouts

***

### **2.4 QFormLayout: The Official “Input Form” Layout**

Designed specifically for **label-input pairs**, making it perfect for forms.

#### **Example**

```cpp
#include <QFormLayout>
#include <QLineEdit>

QFormLayout *layout = new QFormLayout;
layout->addRow("Username:", new QLineEdit());
layout->addRow("Password:", new QLineEdit());
window.setLayout(layout);
```

✅ **Best for**: Forms, login screens, settings panels\
❌ **Bad for**: Random widget placement

***

### **2.5 QStackedLayout: Switching Between Layouts**

Allows you to stack **multiple layouts** and switch between them, making it great for **multi-page interfaces**.

#### **Example**

```cpp
#include <QStackedLayout>

QStackedLayout *layout = new QStackedLayout;
QWidget *page1 = new QWidget;
QWidget *page2 = new QWidget;

layout->addWidget(page1);
layout->addWidget(page2);
layout->setCurrentIndex(0); // Show first page

window.setLayout(layout);
```

✅ **Best for**: Tabbed views, wizards, multi-step forms\
❌ **Bad for**: Static layouts

***

## **3. Comparing Qt Layout Managers**

Here’s a handy comparison to help you pick the right layout:

| **Layout**         | **Best For**                           | **Pros**                                | **Cons**                                         |
| ------------------ | -------------------------------------- | --------------------------------------- | ------------------------------------------------ |
| **QHBoxLayout**    | Toolbars, button rows                  | Simple, easy to use                     | Not great for stacking widgets vertically        |
| **QVBoxLayout**    | Sidebars, vertical stacking            | Great for arranging elements top-down   | Can waste space horizontally                     |
| **QGridLayout**    | Forms, dashboards, structured UIs      | Flexible, supports row/column spanning  | Can be tricky to manage with complex layouts     |
| **QFormLayout**    | Login forms, settings panels           | Automatically aligns labels with inputs | Limited to forms, not flexible for other layouts |
| **QStackedLayout** | Multi-step UIs, wizards, tabbed panels | Allows switching between layouts        | Requires manual switching between pages          |

***

## **4. Nesting Layouts for Ultimate Control**

Sometimes, you need **more than one layout**.

You can **nest** them!

### **Example: Combining QVBoxLayout and QHBoxLayout**

```cpp
QVBoxLayout *mainLayout = new QVBoxLayout;
QHBoxLayout *topLayout = new QHBoxLayout;
QHBoxLayout *bottomLayout = new QHBoxLayout;

topLayout->addWidget(new QPushButton("Left"));
topLayout->addWidget(new QPushButton("Right"));

bottomLayout->addWidget(new QPushButton("Bottom Left"));
bottomLayout->addWidget(new QPushButton("Bottom Right"));

mainLayout->addLayout(topLayout);
mainLayout->addLayout(bottomLayout);
window.setLayout(mainLayout);
```

✅ You can create **more complex layouts** by combining different styles.

***

## **Layout Managers Key Ideas**

Qt’s layout managers **do the heavy lifting** so you don’t have to manually position everything. Here’s what to remember:

* **Use QHBoxLayout or QVBoxLayout for simple structures.**
* **Use QGridLayout for forms and grids.**
* **Use QFormLayout for input fields.**
* **Use QStackedLayout when switching between pages.**
* **Nesting layouts gives you the best of all worlds.**

# **Dialogs and Popups**

***

## **1. Message Boxes: The Simple Pop-Up**

A `QMessageBox` is the easiest way to show an alert. You can use it for:

* **Information messages** (e.g., "File saved successfully!")
* **Warnings** (e.g., "Are you sure you want to delete this?")
* **Errors** (e.g., "Oops! Something went wrong.")

### **Example: Simple Message Box**

```cpp
#include <QMessageBox>

void showMessage() {
    QMessageBox::information(nullptr, "Hello", "This is a message box!");
}
```

✅ **This creates a pop-up with an "OK" button.**

### **Different Message Types**

Qt provides different types of message boxes:

```cpp
QMessageBox::information(nullptr, "Info", "This is an info message");
QMessageBox::warning(nullptr, "Warning", "This is a warning message");
QMessageBox::critical(nullptr, "Error", "Something went wrong!");
QMessageBox::question(nullptr, "Question", "Are you sure?");
```

✅ **Use the right type to match the situation.**

***

## **2. Asking Users a Yes/No Question**

What if you want the user to **confirm an action**? Use `QMessageBox::question`.

### **Example: Yes/No Confirmation**

```cpp
int result = QMessageBox::question(nullptr, "Exit", "Are you sure you want to quit?",
                                   QMessageBox::Yes | QMessageBox::No);

if (result == QMessageBox::Yes) {
    qApp->quit();
}
```

✅ **This will pop up a Yes/No dialog. If the user clicks "Yes", the app closes.**

***

## **3. File Dialogs: Letting Users Open or Save Files**

Nobody likes **hardcoding file paths**. Instead, use `QFileDialog` to let users select a file.

### **Example: Open a File**

```cpp
#include <QFileDialog>
#include <QDebug>

void openFile() {
    QString fileName = QFileDialog::getOpenFileName(nullptr, "Open File", "", "Text Files (*.txt);;All Files (*.*)");
    if (!fileName.isEmpty()) {
        qDebug() << "Selected file:" << fileName;
    }
}
```

✅ **This will show a file picker. The user can select a file, and the path will be printed.**

### **Example: Save a File**

```cpp
QString saveFile = QFileDialog::getSaveFileName(nullptr, "Save File", "", "Text Files (*.txt)");
if (!saveFile.isEmpty()) {
    qDebug() << "Saving file to:" << saveFile;
}
```

✅ **Now users can save files instead of losing all their work.**

***

## **4. Input Dialogs: Getting User Input**

Need the user to **enter a number or text**? Use `QInputDialog`.

### **Example: Asking for a Text Input**

```cpp
#include <QInputDialog>

void getUserName() {
    bool ok;
    QString name = QInputDialog::getText(nullptr, "Enter Name", "What’s your name?", QLineEdit::Normal, "", &ok);
    if (ok && !name.isEmpty()) {
        qDebug() << "User entered:" << name;
    }
}
```

✅ **This asks for the user's name and prints it.**

### **Example: Asking for a Number**

```cpp
int age = QInputDialog::getInt(nullptr, "Enter Age", "How old are you?", 25, 0, 120);
qDebug() << "User age:" << age;
```

✅ **This asks for a number between 0 and 120.**

***

## **5. Custom Dialogs: Because Sometimes Built-ins Aren’t Enough**

What if you need something more complex? **Make your own dialog!** Inherit from `QDialog` and add widgets manually.

### **Example: Custom Dialog with OK/Cancel**

```cpp
#include <QDialog>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>

class MyDialog : public QDialog {
public:
    MyDialog(QWidget *parent = nullptr) : QDialog(parent) {
        setWindowTitle("Custom Dialog");

        QVBoxLayout *layout = new QVBoxLayout(this);
        QLabel *label = new QLabel("This is a custom dialog!");
        QPushButton *okButton = new QPushButton("OK");
        QPushButton *cancelButton = new QPushButton("Cancel");

        layout->addWidget(label);
        layout->addWidget(okButton);
        layout->addWidget(cancelButton);

        connect(okButton, &QPushButton::clicked, this, &QDialog::accept);
        connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
    }
};

// Usage
void showCustomDialog() {
    MyDialog dialog;
    if (dialog.exec() == QDialog::Accepted) {
        qDebug() << "User clicked OK!";
    } else {
        qDebug() << "User clicked Cancel!";
    }
}
```

✅ **This creates a custom dialog with OK/Cancel buttons.**

***

## **6. Comparing Qt Dialogs**

| **Dialog Type**    | **Purpose**                    | **Best For**                        |
| ------------------ | ------------------------------ | ----------------------------------- |
| **QMessageBox**    | Shows an alert or message      | Errors, warnings, confirmation      |
| **QFileDialog**    | Lets users open/save files     | File selection                      |
| **QInputDialog**   | Gets user input (text, number) | Asking for a name, age, etc.        |
| **Custom QDialog** | Fully customizable pop-up      | Anything that needs multiple inputs |

## **Menus and Toolbars**

## **1. Creating a Basic Menu Bar**

Qt makes adding a menu bar **super easy** with `QMenuBar`. Here’s how to add a **File menu** with an **Exit** option:

### **Example: Simple Menu Bar**

```cpp
#include <QApplication>
#include <QMainWindow>
#include <QMenuBar>
#include <QAction>

class MainWindow : public QMainWindow {
public:
    MainWindow() {
        // Create menu bar
        QMenuBar *menuBar = new QMenuBar(this);
        
        // Create "File" menu
        QMenu *fileMenu = menuBar->addMenu("File");

        // Add "Exit" action
        QAction *exitAction = new QAction("Exit", this);
        fileMenu->addAction(exitAction);
        
        // Connect "Exit" action to close() function
        connect(exitAction, &QAction::triggered, this, &QMainWindow::close);

        // Set menu bar
        setMenuBar(menuBar);
    }
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    MainWindow window;
    window.show();
    return app.exec();
}
```

### **How It Works**

* We create a **menu bar** using `QMenuBar`.
* We add a **File menu** using `addMenu("File")`.
* We add an **Exit action** inside the File menu.
* When clicked, the **Exit action closes the app**.

✅ **Now your app has a real menu bar!**

***

## **2. Adding More Menus and Submenus**

Let’s take things further and add:

* **Edit menu** (with Undo and Redo)
* **Help menu** (with an About dialog)

### **Example: Multiple Menus and Submenus**

```cpp
#include <QMenu>
#include <QMessageBox>

class MainWindow : public QMainWindow {
public:
    MainWindow() {
        QMenuBar *menuBar = new QMenuBar(this);

        // File Menu
        QMenu *fileMenu = menuBar->addMenu("File");
        QAction *exitAction = new QAction("Exit", this);
        fileMenu->addAction(exitAction);
        connect(exitAction, &QAction::triggered, this, &QMainWindow::close);

        // Edit Menu with Undo/Redo
        QMenu *editMenu = menuBar->addMenu("Edit");
        QAction *undoAction = new QAction("Undo", this);
        QAction *redoAction = new QAction("Redo", this);
        editMenu->addAction(undoAction);
        editMenu->addAction(redoAction);

        // Help Menu with About Dialog
        QMenu *helpMenu = menuBar->addMenu("Help");
        QAction *aboutAction = new QAction("About", this);
        helpMenu->addAction(aboutAction);
        connect(aboutAction, &QAction::triggered, this, &MainWindow::showAboutDialog);

        setMenuBar(menuBar);
    }

private:
    void showAboutDialog() {
        QMessageBox::about(this, "About", "This is a Qt app with menus!");
    }
};
```

***

## **8. Handling Resources**

## **1. What Are Qt Resources?**

Qt uses a **resource system** to package assets like:\
✅ **Images (PNG, JPG, SVG, etc.)**\
✅ **Icons**\
✅ **Stylesheets (CSS for Qt!)**\
✅ **Translation files**\
✅ **Custom data files**

These files are stored inside a **Qt Resource Collection File (.qrc)** and are embedded in your executable. This means:

* No need to worry about file paths.
* No missing images when distributing your app.
* Faster loading times.

***

## **2. Creating a Resource File (.qrc)**

You need a `.qrc` file to manage your resources. Here’s how to create one:

### **Step 1: Add a New Resource File**

1. In Qt Creator, go to **File > New File or Project**.
2. Select **Qt > Qt Resource File** and click **Next**.
3. Name it something like `resources.qrc`.
4. Click **Finish**.

### **Step 2: Add Resources to the .qrc File**

Once you have the `.qrc` file, you need to **add resources**. Edit `resources.qrc` to look like this:

```xml
<RCC>
    <qresource prefix="/images">
        <file>icons/logo.png</file>
        <file>icons/exit.png</file>
    </qresource>
</RCC>
```

✅ **This means you can now access these files inside your app using the prefix `/images/`.**

***

## **3. Loading Images from Resources**

Now that your resources are inside the `.qrc` file, you can load them like this:

### **Example: Setting an Image on a QLabel**

```cpp
#include <QLabel>
#include <QPixmap>

QLabel *label = new QLabel;
label->setPixmap(QPixmap(":/images/icons/logo.png"));
```

✅ **No need for absolute paths! The image is embedded in your app.**

***

## **4. Using Icons in Buttons and Windows**

Want to add icons to your buttons or the app window? Easy.

### **Example: Setting an Icon for a QPushButton**

```cpp
#include <QPushButton>
#include <QIcon>

QPushButton *button = new QPushButton("Exit");
button->setIcon(QIcon(":/images/icons/exit.png"));
```

### **Example: Setting a Window Icon**

```cpp
#include <QApplication>
#include <QMainWindow>

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QMainWindow window;

    // Set window icon
    window.setWindowIcon(QIcon(":/images/icons/logo.png"));

    window.show();
    return app.exec();
}
```

✅ **Your app now has a proper icon instead of the boring default!**

***

## **5. Using Stylesheets for Custom UI**

Qt supports **stylesheets (QSS)**, which work like **CSS for Qt widgets**.

### **Example: Adding a Stylesheet to Your App**

1. Add a `style.qss` file to your `.qrc`:

```xml
<RCC>
    <qresource prefix="/styles">
        <file>style.qss</file>
    </qresource>
</RCC>
```

2. Load the stylesheet in your app:

```cpp
#include <QFile>
#include <QApplication>

void loadStyleSheet(QApplication &app) {
    QFile file(":/styles/style.qss");
    if (file.open(QFile::ReadOnly)) {
        QString styleSheet = file.readAll();
        app.setStyleSheet(styleSheet);
    }
}
```

✅ **Your app now has custom styles! No need to hardcode ugly colors.**

***

## **6. Using Other Resource Types**

You can also store **custom files** like JSON, XML, or audio files inside `.qrc`.

### **Example: Loading a JSON File from Resources**

```cpp
#include <QFile>
#include <QJsonDocument>
#include <QJsonObject>

void loadJson() {
    QFile file(":/data/config.json");
    if (!file.open(QIODevice::ReadOnly)) {
        qDebug() << "Failed to open JSON file!";
        return;
    }
    
    QJsonDocument doc = QJsonDocument::fromJson(file.readAll());
    QJsonObject obj = doc.object();
    qDebug() << "App Name:" << obj["appName"].toString();
}
```

✅ **Now you can load config files without worrying about paths!**

***

## **7. Comparing Different Resource Management Methods**

| **Method**                    | **Pros**                          | **Cons**                           |
| ----------------------------- | --------------------------------- | ---------------------------------- |
| **Hardcoded Paths**           | Simple, no setup needed           | Breaks if file is missing or moved |
| **Qt Resource System (.qrc)** | Embedded in app, always available | Can increase binary size           |
| **Relative File Paths**       | Works for dev environment         | Breaks when packaging app          |
| **Dynamic File Loading**      | Great for user-generated content  | Requires manual file management    |

✅ **For UI elements, icons, and built-in assets, always use `.qrc`!**

***

## **8. Packaging Resources for Deployment**

When you **deploy your Qt app**, all `.qrc` resources are **bundled into the executable**—no extra files needed.\
For **external files (like user documents or logs)**, **store them in a writable directory** instead.

### **Example: Getting a Writable Path**

```cpp
#include <QStandardPaths>
QString writablePath = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
qDebug() << "Save files here:" << writablePath;
```

✅ **Use this for saving user-generated content instead of `.qrc`.**
