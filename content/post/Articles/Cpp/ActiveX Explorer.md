---
title: What is ActiveX and Why It Was a Security Nightmare?
description: Exploring ActiveX history with some MFC
slug: activex-security-nightmare
date: 2018-07-23
image: post/Articles/IMAGES/activex.png
categories:
  - ActiveX
  - Security
  - MFC
  - Programming
  - WinAPI
  - Visual Basic
  - Visual Basic 6
tags:
  - ActiveX
  - Security
  - Visual
  - Basic
  - Web
  - Browsers
  - MFC
  - Hello
  - World
  - Windows
draft: false
weight: 456
categories_ref:
  - ActiveX
  - Security
  - MFC
  - Programming
  - WinAPI
  - Visual Basic
  - Visual Basic 6
lastmod: 2025-03-14T15:45:08.464Z
---
# What is ActiveX and Why It Was a Security Nightmare?

## What is ActiveX?

ActiveX is like that one friend who overstays their welcome and eventually burns your house down.

It was Microsoft's way of making web pages and applications more interactive by embedding small software components (called ActiveX controls) inside web browsers, primarily Internet Explorer.

Think of it like Java applets or Flash, but with more security holes than Swiss cheese.

Microsoft introduced ActiveX in the 90s, and it worked closely with Visual Basic (VB) and C++ (via MFC) to create interactive web content and desktop applications.

If you wanted to add fancy buttons, play videos, or execute scripts on a web page back in the day, ActiveX was *the* tool to use—until people realized it was basically an open invitation to hackers.

## Why Was ActiveX a Security Dumpster Fire?

The problem with ActiveX was that it had way too much power.

Once you installed an ActiveX control, it could pretty much do whatever it wanted on your system—like downloading other programs, modifying files, or sending your embarrassing search history straight to hackers.

* **No Sandboxing** – Unlike Java or Flash, ActiveX ran with full system permissions. That's like giving a toddler a flamethrower and hoping for the best.
* **Social Engineering Galore** – Websites tricked users into installing malicious ActiveX controls by making pop-ups that said, "Click here to win an iPhone!" (Spoiler: You never got an iPhone.)
* **Internet Explorer Was the Only Guardian** – ActiveX was tied to Internet Explorer, which was *not* known for its strong security record.
* **Once Installed, Always Installed** – If you installed a bad ActiveX control, it stayed on your system forever unless you manually removed it.

### ActiveX turned web browsers into a hacker's playground.

## MFC ActiveX Example: Hello World

Now, let's write an MFC-based ActiveX control that takes your name and greets you when you click a button. This is like the Windows 98 equivalent of a chatbot.

### Step 1: Create an MFC ActiveX Control Project

1. Open **Visual Studio**.
2. Go to **File > New > Project**.
3. Select **MFC ActiveX Control** and name it `HelloActiveX`.
4. Click **Next** and keep all defaults.

### Step 2: Add a Text Field and a Button

We need a place to enter our name and a button to trigger the greeting. In the `HelloActiveXCtrl.h` header file, add:

```cpp
#pragma once
#include "afxwin.h"

class CHelloActiveXCtrl : public COleControl
{
    DECLARE_DYNCREATE(CHelloActiveXCtrl)

public:
    CHelloActiveXCtrl();

protected:
    DECLARE_MESSAGE_MAP()
    afx_msg void OnSayHello();
    CString m_Name;
    CEdit m_NameBox;
    CButton m_HelloButton;
};
```

### Step 3: Implement the Logic

Now, in `HelloActiveXCtrl.cpp`, implement the button click event:

```cpp
#include "pch.h"
#include "HelloActiveX.h"
#include "HelloActiveXCtrl.h"
#include "afxdialogex.h"

IMPLEMENT_DYNCREATE(CHelloActiveXCtrl, COleControl)

BEGIN_MESSAGE_MAP(CHelloActiveXCtrl, COleControl)
    ON_BN_CLICKED(1, &CHelloActiveXCtrl::OnSayHello)
END_MESSAGE_MAP()

CHelloActiveXCtrl::CHelloActiveXCtrl()
{
    m_NameBox.Create(WS_CHILD | WS_VISIBLE | WS_BORDER, CRect(10, 10, 150, 30), this, 2);
    m_HelloButton.Create(_T("Say Hello"), WS_CHILD | WS_VISIBLE, CRect(10, 40, 150, 70), this, 1);
}

void CHelloActiveXCtrl::OnSayHello()
{
    m_NameBox.GetWindowText(m_Name);
    CString message;
    message.Format(_T("Hello, %s!"), m_Name);
    AfxMessageBox(message);
}
```

### Step 4: Register the ActiveX Control

After compiling, run this command in the Developer Command Prompt to register the control:

```sh
regsvr32 HelloActiveX.ocx
```

### Step 5: Use It in a Web Page (If You Dare)

```html
<object classid="clsid:YOUR-CLSID-HERE" width="300" height="200">
    <param name="Name" value="World">
</object>
```

## Explaining the Code

* We **inherit from `COleControl`**, which makes our class an ActiveX control.
* We **create a text box and button** dynamically in the constructor.
* The **`OnSayHello` function gets the text** from the text box and shows a message box with "Hello, \[Your Name]!"
* We **map the button click** to `OnSayHello()` using `ON_BN_CLICKED(1, &CHelloActiveXCtrl::OnSayHello)`.
* `regsvr32` registers our `.ocx` file so it can be used in web pages and applications.

## Key Ideas so far....

| Key Idea            | Explanation                                                                     |
| ------------------- | ------------------------------------------------------------------------------- |
| **ActiveX**         | Microsoft's old tech for embedding interactive components in web pages and apps |
| **Security Issues** | No sandboxing, full system permissions, exploited by malware                    |
| **MFC Example**     | Simple "Hello, World" ActiveX control using MFC                                 |
| **How It Works**    | Creates an input box and button, then displays a greeting when clicked          |
| **Why It Died**     | Too many security risks, replaced by modern web technologies                    |

And that's how ActiveX worked—until everyone realized it was a security nightmare and ditched it for JavaScript, HTML5, and other modern tech.

\=========

***

title: "Using an MFC ActiveX Control in MFC and Visual Basic 6 Applications"\
description: "Using an MFC ActiveX Control in MFC and Visual Basic 6 Applications"\
slug: "using-mfc-activex-in-mfc-and-vb6"\
date: 2017-03-15\
image: "post/Articles/IMAGES/27.jpg"\
categories: \["ActiveX", "MFC", "Visual Basic 6", "Programming"]\
tags: \["ActiveX", "MFC", "Visual Basic 6", "COM", "Windows", "Legacy Applications"]\
draft: false\
weight: 392
-----------

# Using an MFC ActiveX Control in MFC and Visual Basic 6 Applications

So, you’ve built an MFC ActiveX control. Congratulations! 🎉 Now, what do you do with it? Well, you embed it in an MFC application (duh!) and for extra spice, let’s also drop it into a good ol’ Visual Basic 6 application. Because nothing says “nostalgia” like VB6.

## Step 1: Register Your ActiveX Control

Before we do anything, we need to make sure Windows knows about our control.

### Registering the Control

After compiling your MFC ActiveX control, run this in an **Administrator** command prompt:

```sh
regsvr32 HelloActiveX.ocx
```

If you get an error, check that you’re running the command from the correct directory where the `.ocx` file exists.

***

## Part 1: Using the ActiveX Control in an MFC Application

### Step 1: Create an MFC Dialog-Based Application

1. Open **Visual Studio**.
2. Select **New Project** > **MFC Application**.
3. Choose **Dialog-Based Application**.
4. Click **Next** through the wizard, keeping default settings.

### Step 2: Insert the ActiveX Control into the Dialog

1. Open **Resource View**.
2. Open your **Dialog Resource (IDD\_DIALOG1)**.
3. Right-click the dialog and select **Insert ActiveX Control**.
4. Choose **HelloActiveX Control** from the list.
5. Resize and position it as needed.

### Step 3: Add a Member Variable for the Control

1. In **Class View**, open `YourDialog.h`.
2. Add the following line in your `CYourDialog` class:

```cpp
#include "afxwin.h"
#include "HelloActiveX.h"  // Include the ActiveX header

class CYourDialog : public CDialogEx
{
    CWnd m_ActiveXControl;
};
```

### Step 4: Initialize the Control in Code

Modify `OnInitDialog` in `YourDialog.cpp` to interact with the control:

```cpp
BOOL CYourDialog::OnInitDialog()
{
    CDialogEx::OnInitDialog();

    CWnd* pWnd = GetDlgItem(IDC_YOUR_ACTIVEX_CONTROL_ID);
    if (pWnd)
    {
        m_ActiveXControl.Attach(pWnd->m_hWnd);
    }
    return TRUE;
}
```

That’s it! When you run your MFC app, the ActiveX control should be loaded and ready to use via the m\_ActiveXControl pointer. 🚀

***

## Part 2: Using the ActiveX Control in a Visual Basic 6 Application

Yes, VB6 still exists! Some companies still use it, and if you ever need to integrate an ActiveX control into a VB6 app, here’s how you do it.

### Step 1: Open a New VB6 Project

1. Open **Visual Basic 6**.
2. Select **Standard EXE** and click **Open**.

### Step 2: Add the ActiveX Control to the Toolbox

1. Go to **Project > Components (Ctrl+T)**.
2. Find **HelloActiveX Control** in the list.
3. Check the box and click **OK**.
4. The control should now appear in the toolbox.

### Step 3: Drop It into Your Form

1. Drag and drop the **HelloActiveX Control** from the toolbox onto **Form1**.
2. Resize and position it as needed.

### Step 4: Write Code to Interact with It

Modify `Form1`'s code to respond to a button click:

```vb
Private Sub Command1_Click()
    HelloActiveX1.Name = Text1.Text
    MsgBox "Hello, " & HelloActiveX1.Name & "!"
End Sub
```

This simple VB6 code takes text from a `TextBox` and updates the ActiveX control’s `Name` property, then displays a greeting.

### Step 5: Run It!

Click **Run** and enjoy the beauty of 90s technology still working in the 21st century. 🎉

***

## Summary Table

| Step                 | Description                                                         |
| -------------------- | ------------------------------------------------------------------- |
| **Register Control** | `regsvr32 HelloActiveX.ocx` to register the control                 |
| **MFC Application**  | Insert ActiveX via the dialog editor, initialize it in code         |
| **VB6 Application**  | Add the control via Components, drop it in a form, interact with it |

***

## References

* [Microsoft Docs on ActiveX](https://docs.microsoft.com/en-us/windows/win32/com/activex-controls-overview)
* [MFC ActiveX Development](https://docs.microsoft.com/en-us/cpp/mfc/creating-an-activex-control-in-mfc?view=msvc-170)
* [Visual Basic 6 and ActiveX](https://en.wikipedia.org/wiki/Visual_Basic_6.0)

Now, go forth and integrate your ActiveX control like a pro! And remember: **just because you *can* use ActiveX doesn’t mean you *should*.** 😆
