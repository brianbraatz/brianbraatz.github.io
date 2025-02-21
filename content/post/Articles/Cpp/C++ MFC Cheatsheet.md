---
title: Microsoft Foundation Classes Cheatsheet
description: 
slug: cpp-mfc-cheatsheet
date: 2014-05-06
image: post/Articles/IMAGES/mfxlogo.png
categories:
  - Windows
  - MFC
  - C
  - CPP
  - Cheatsheet
tags:
  - Cheatsheet
  - CPP
  - MFC
  - CPP-MFC
  - Win16
  - Win32
  - Win64
  - WinAPI
weight: 180
draft: false
lastmod: 2025-02-20T22:01:06.943Z
---
## MFC Cheatsheet

| **Concept**                | **Syntax/Example**                                      | **Description**               |
| -------------------------- | ------------------------------------------------------- | ----------------------------- |
| Application Class          | `class CMyApp : public CWinApp { ... };`                | Defining an application class |
| Message Map                | `BEGIN_MESSAGE_MAP(CMyWnd, CWnd) ... END_MESSAGE_MAP()` | Defining message map          |
| Document/View Architecture | `class CMyDoc : public CDocument { ... };`              | Defining a document class     |
| Dialog-Based Application   | `class CMyDlg : public CDialog { ... };`                | Defining a dialog class       |
| Controls                   | `CButton myButton;`                                     | Declaring a button control    |
| GDI Objects                | `CPen myPen;`                                           | Declaring a GDI object        |
| Event Handling             | `afx_msg void OnMyEvent();`                             | Handling an event             |
| Serialization              | `void Serialize(CArchive& ar);`                         | Implementing serialization    |
| Windows Management         | `void ShowWindow(int nCmdShow);`                        | Managing windows              |
| Resource Management        | `AfxGetResourceHandle();`                               | Accessing resources           |

### MFC Class Hierarchy Overview

plaintext

```lua
CObject
 ├── CCmdTarget
 │    ├── CWinThread
 │    │    ├── CWinApp
 │    │    └── CDocument
 │    └── CWnd
 │         ├── CFrameWnd
 │         │    └── CMDIFrameWnd
 │         ├── CMDIChildWnd
 │         ├── CDialog
 │         └── CView
 └── CArchive
```

* **CObject:** The base class for all MFC classes.

* **CCmdTarget:** Provides the ability to handle commands.

* **CWinThread:** Represents a thread of execution in an MFC application.

  * **CWinApp:** Represents an application instance.

  * **CDocument:** Represents the data used by an application (part of the Document/View architecture).

* **CWnd:** Represents a window.

  * **CFrameWnd:** Represents a frame window.

    * **CMDIFrameWnd:** Represents the main frame window in an MDI application.

  * **CMDIChildWnd:** Represents a child window in an MDI application.

  * **CDialog:** Represents a dialog box.

  * **CView:** Represents a view (part of the Document/View architecture).

* **CArchive:** Provides support for serializing data to persistent storage.
