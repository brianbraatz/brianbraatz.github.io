---
title: How to Write a Windows App in C
slug: windows-app-c
date: 2017-12-15
image: post/Articles/IMAGES/winlogo-old.png
categories:
  - Windows
  - C
  - CPP
  - MFC
  - OWL
  - GUI
  - Qt GUI Framework
  - Windows API
  - WinApi
tags:
  - C
  - CPP
  - MFC
  - OWL
  - Windows
  - API
  - GUI
  - MessageLoop
  - Qt
  - WinAPI
draft: false
weight: 220
description: Old Skool Guis Explained with comparison to MFC, OWL and Qt
lastmod: 2025-02-28T20:42:28.226Z
---
<!-- 
How to Write a Windows App in C

Hey there! Ready to take a trip down memory lane?

Let's dive into how we used to whip up Windows applications using good ol' C. 


-->

![](/post/Articles/IMAGES/Windows3_ProgMan.png)

## The Old-School Way: WinAPI

Back in the day, before fancy frameworks, we rolled up our sleeves and used the Windows API (WinAPI) directly. It was like cooking from scratch—no pre-made ingredients.

Here's a basic example:

```c
#include <windows.h>

#define ID_BUTTON 1
#define ID_TEXTBOX 2

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    static HWND hTextBox;

    switch (uMsg) {
        case WM_CREATE:
            CreateWindow("STATIC", "Your name:", WS_VISIBLE | WS_CHILD,
                         10, 10, 100, 20, hwnd, NULL, NULL, NULL);
            hTextBox = CreateWindow("EDIT", "", WS_VISIBLE | WS_CHILD | WS_BORDER,
                                    120, 10, 150, 20, hwnd, (HMENU)ID_TEXTBOX, NULL, NULL);
            CreateWindow("BUTTON", "Greet", WS_VISIBLE | WS_CHILD,
                         10, 40, 100, 30, hwnd, (HMENU)ID_BUTTON, NULL, NULL);
            break;
        case WM_COMMAND:
            if (LOWORD(wParam) == ID_BUTTON) {
                char name[100];
                GetWindowText(hTextBox, name, sizeof(name));
                char message[120];
                sprintf(message, "Hello %s!", name);
                MessageBox(hwnd, message, "Greeting", MB_OK);
            }
            break;
        case WM_DESTROY:
            PostQuitMessage(0);
            break;
        default:
            return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    const char CLASS_NAME[] = "SampleWindowClass";

    WNDCLASS wc = { };
    wc.lpfnWndProc = WindowProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = CLASS_NAME;

    RegisterClass(&wc);

    HWND hwnd = CreateWindowEx(
        0,
        CLASS_NAME,
        "Old-School C Windows App",
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT, 300, 150,
        NULL,
        NULL,
        hInstance,
        NULL
    );

    if (hwnd == NULL) {
        return 0;
    }

    ShowWindow(hwnd, nCmdShow);

    MSG msg = { };
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return 0;
}
```

Its a simple app with a label, a text box, and a button.

When you type your name and hit the button, a message box will pop up saying, "Hello " with whatever you typed in the text edit field..

### Breaking Down the Code

1. **Includes and Defines**: We include the `windows.h` header and define IDs for our button and text box.

2. **Window Procedure**: This function, `WindowProc`, handles messages sent to our window. It's like the brain of our application.

   * `WM_CREATE`: This message is sent when the window is created. We create a static label, an edit control (text box), and a button here.
   * `WM_COMMAND`: This is sent when an action occurs, like a button click. We check if our button was clicked, retrieve the text from the text box, and display it in a message box.
   * `WM_DESTROY`: Sent when the window is about to be destroyed. We post a quit message to end the application.

3. **WinMain Function**: The entry point of a Windows application.

   * We define and register a window class.
   * Create the window with `CreateWindowEx`.
   * Show the window using `ShowWindow`.
   * Enter the message loop, where we retrieve and dispatch messages.

### The Message Loop

Ah, the message loop—a core concept in Windows applications. It's like the heartbeat, keeping the app responsive.

```c
MSG msg;
while (GetMessage(&msg, NULL, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
}
```

For a deeper dive into message loops, check out this [MSDN article](https://learn.microsoft.com/en-us/windows/win32/winmsg/using-messages-and-message-queues).

**Multi-Threading !!! ???**

Everything is  actually one thread, and when the messages are processed, your code is owning the whole GUI thread.

This is why Windows C apps can have a UI freeze if they spend too much time processing the message.

Also in early windows (1.0-3.1) the WHOLE OS  shared one thread!

So if you got into an infinte loop while responding to a message, you can freeze the whole OS!.

This was a motivating factor in the development of Windows 95 and Windows Nt...

You still have a message pump and it still works like this, but if your app goes into an infinite loop, it freezes only your app and NOT the whole operating system...

## The Challenge of Managing State

Now, while this approach works, managing data and state can get tricky. Imagine you have multiple instances of the same window. Each window uses the same window procedure, so how do you keep track of data for each instance?

A common trick was to use a dictionary-like data structure, mapping each window handle (`HWND`) to its associated data. This way, you could look up the data based on the window that's interacting with the user.

## Enter C++ Frameworks: OWL, MFC, and later Qt

To make life easier, C++ frameworks like Object Windows Library (OWL) and Microsoft Foundation Class (MFC) were introduced. They allowed developers to associate instance data with C++ objects tied to windows, simplifying state management.

> \[!NOTE] Where is the message pump in the C++ Sample Code Below?\
> The message loop is handled internally by the framework, making it simpler for developers. There are ways to override and change it, but for most C++ Windows Gui apps this is not needed.

### MFC Example

Here's how you'd create a similar application using MFC:

```cpp
#include <afxwin.h>

class CMyApp : public CWinApp {
public:
    virtual BOOL InitInstance();
};

class CMyFrame : public CFrameWnd {
public:
    CMyFrame();
    afx_msg void OnButtonClicked();
    DECLARE_MESSAGE_MAP()
private:
    CEdit m_Edit;
    CButton m_Button;
};

BEGIN_MESSAGE_MAP(CMyFrame, CFrameWnd)
    ON_BN_CLICKED(1, OnButtonClicked)
END_MESSAGE_MAP()

BOOL CMyApp::InitInstance() {
    CMyFrame* Frame = new CMyFrame();
    m_pMainWnd = Frame;
    Frame->ShowWindow(SW_NORMAL);
    return TRUE;
}

CMyFrame::CMyFrame() {
    Create(NULL, "MFC Windows App");
    m_Edit.Create(WS_CHILD | WS_VISIBLE | WS_BORDER, CRect(120, 10, 270, 30), this, 2);
    m_Button.Create("Greet", WS_CHILD | WS_VISIBLE, CRect(10, 40, 100, 30), this, 1);
}
```

### OWL Example

```c++
#include <owl/pch.h>
#include <owl/framewin.h>
#include <owl/edit.h>
#include <owl/button.h>
#include <owl/static.h>
#include <owl/dialog.h>

class TMyWindow : public TFrameWindow {
public:
    TMyWindow(TApplication& app);
    void CmGreet();

protected:
    TEdit* EditBox;
    TButton* Button;
    
    DECLARE_RESPONSE_TABLE(TMyWindow);
};

DEFINE_RESPONSE_TABLE1(TMyWindow, TFrameWindow)
    EV_COMMAND(1001, CmGreet),
END_RESPONSE_TABLE;

TMyWindow::TMyWindow(TApplication& app)
    : TFrameWindow(0, "OWL Windows App", new TWindow(0, 0, 0)) {
    new TStatic(this, 1000, "Your name:", 10, 10, 100, 20);
    EditBox = new TEdit(this, 1002, "", 120, 10, 150, 20);
    Button = new TButton(this, 1001, "Greet", 10, 40, 100, 30);
}

void TMyWindow::CmGreet() {
    char name[100];
    EditBox->GetText(name, sizeof(name));
    char message[120];
    sprintf(message, "Hello %s!", name);
    MessageBox(message, "Greeting", MB_OK);
}

class TMyApp : public TApplication {
public:
    void InitMainWindow() {
        SetMainWindow(new TMyWindow(*this));
    }
};

int OwlMain(int, char* []) {
    return TMyApp().Run();
}
```

### Qt Example

```C++
#include <QApplication>
#include <QWidget>
#include <QLabel>
#include <QPushButton>
#include <QLineEdit>
#include <QVBoxLayout>
#include <QMessageBox>

class MyWindow : public QWidget {
    Q_OBJECT
public:
    MyWindow(QWidget *parent = nullptr);
private slots:
    void greet();
private:
    QLineEdit *nameInput;
};

MyWindow::MyWindow(QWidget *parent) : QWidget(parent) {
    QLabel *label = new QLabel("Your name:", this);
    nameInput = new QLineEdit(this);
    QPushButton *button = new QPushButton("Greet", this);
    QVBoxLayout *layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(nameInput);
    layout->addWidget(button);
    connect(button, &QPushButton::clicked, this, &MyWindow::greet);
}

void MyWindow::greet() {
    QMessageBox::information(this, "Greeting", "Hello " + nameInput->text() + "!");
}

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    MyWindow window;
    window.show();
    return app.exec();
}
```

## The Evolution of C++ GUI Frameworks

| Framework | Initial Release | Windows OS Released In Same Period |
| --------- | --------------- | ---------------------------------- |
| MFC       | 1992            | Windows 3.1 (1992)                 |
| OWL       | 1991            | Windows 3.0 (1990)                 |
| Qt        | 1995            | Windows 95 (1995)                  |

# Key Ideas

| Concept        | Description                                               |
| -------------- | --------------------------------------------------------- |
| Windows API    | Old-school method for creating Windows apps in C.         |
| Message Loop   | Handles Windows events/messages.                          |
| Managing State | Tricky in C; often used dictionaries to map HWND to data. |
| MFC, OWL, Qt   | GUI frameworks making state management easier.            |

# Related Links

* [Windows Message Loop](https://learn.microsoft.com/en-us/windows/win32/winmsg/using-messages-and-message-queues)
* [MFC Overview](https://learn.microsoft.com/en-us/cpp/mfc/mfc-desktop-applications)
* [OWL Framework](https://en.wikipedia.org/wiki/Object_Windows_Library)
* [Qt](https://en.wikipedia.org/wiki/Qt_\(software\))

***
