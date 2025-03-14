---
title: We Invented Async and Await in the 1980s!!!
description: How we did Async-Await type algorithms in the old days compared to today. Examples in C, C#, QT, OWL and MFC
slug: we-invented-async-1980s
date: 2018-12-15
image: post/Articles/IMAGES/oldmanyellsatcloudwide.png
categories:
  - Algorithms
  - Windows
  - Windows API
  - C
  - CPP
  - Qt GUI Framework
  - MFC
  - OWL
  - DotNet
  - History
  - Concurrency
tags:
  - Windows
  - MFC
  - Qt
  - Threads
  - GUI
  - BubbleSort
  - StateMachine
  - FSM
  - Win16
  - WindowsAPI
  - Win32
  - Algorithms
  - CPP
  - DotNetAsyncAwait
  - Threading
  - WPF
  - Maui
draft: false
weight: 18
categories_ref:
  - Algorithms
  - Windows
  - Windows API
  - C
  - CPP
  - Qt GUI Framework
  - MFC
  - OWL
  - DotNet
  - History
  - Concurrency
lastmod: 2025-03-14T15:45:21.008Z
---
## AND WE WOULD HAVE GOTTEN AWAY WITH TOO!! IF IT HADNT BEEN FOR YOU KIDS!!!!!

![](/post/Articles/IMAGES/ScoobyDooUnmasking.jpg)

<https://scoobydoo.fandom.com/wiki/List_of_%22And_I_Would_Have_Gotten_Away_With_It_Too,_If_It_Weren%27t_For_You_Meddling_Kids%22_Quotes>

<!-- 
# We Invented Async and Await in the Early 1990s

Hey there, fellow code enthusiasts! Gather 'round, because today we're diving into a tale from the early '90s—a time when grunge was in, the internet was just a baby, and Windows was... well, let's just say it had some growing up to do. We're talking about how we, the unsung heroes of software development, came up with a technique eerily similar to today's async and await. Buckle up!
-->

![](/post/Articles/IMAGES/Windows3_ProgMan.png)

## Windows and Threads: A Rocky Start

First, let's set the stage.

Back in the day, Windows wasn't the multitasking powerhouse it is now. Here's a quick rundown of Windows releases up to Windows NT 3.1 and Windows 95, along with their release dates and thread support:

| Windows Version | Release Date  | Thread Support |
| --------------- | ------------- | -------------- |
| Windows 1.0     | November 1985 | No             |
| Windows 2.0     | December 1987 | No             |
| Windows 3.0     | May 1990      | No             |
| Windows 3.1     | April 1992    | No             |
| Windows NT 3.1  | July 1993     | Yes            |
| Windows 95      | August 1995   | Yes            |

As you can see, before Windows NT 3.1 and Windows 95, thread support was non-existent.

This meant that if your application decided to take a nap (like during a long bubble sort), the whole system could grind to a halt. Not cool.

I started writing windows apps around 1990! We would use DOS editors to write code, edit our own Make files etc.. Then "run" windows (win.exe!) and test our app.. Usually we would then crash windows and reboot and repeat...

AHH! the good ole days...

This article is a modern attempt to explain to a modern audience (i.e young whipper snapper kids :) ) some of the techniques we had to use to get around the single threaded operating system we were working in ...

## The Challenge: Keeping the GUI Responsive

Imagine this: you're building a Windows application on Windows 3.1 using the Windows API (i.e. Win16 as we now call it).

Lets say your app has a button labeled "Sort it now!" that, when clicked, loads a massive text file (one word per line), sorts it using bubble sort, and then proudly proclaims, "Sorting done!" in a message box.

(would anyone buy this app??? maybe not.. there was no internet back then to sell things.. so that makes the profitablity of this example much less than we can imagine... )

Here's the kicker: on Windows NT and Windows 95 (and later), if your bubble sort takes, say, 10 minutes, your application's GUI will freeze during that time.

The rest of the system?

It'll chug along just fine because of process isolation.

But on 16-bit Windows without threads?

Your app's 10-minute siesta means the *entire* operating system is on hold. Yikes!

## Our Solution: The Proto-Async Await

Without threads, we had to get creative to keep the GUI responsive.

Enter the state machine—a design pattern that allowed us to break down the bubble sort into manageable chunks, processing a little bit at a time and returning control to the GUI in between.

Sound familiar?

It's pretty close to the async-await pattern in modern C#.

### The `SortingJob` Class

#### C++ Implementation

```cpp
#include <afxwin.h>
#include <vector>
#include <string>
#include <fstream>

enum SortState {
    Constructed,
    DataLoaded,
    Processing,
    Done
};

class SortingJob {
public:
    SortingJob() : state(Constructed), i(0), j(0) {}

    void DoSomeProcessing() {
        switch (state) {
            case Constructed:
                LoadData();
                state = DataLoaded;
                break;
            case DataLoaded:
                i = 0;
                j = 0;
                state = Processing;
                break;
            case Processing:
                ProcessSorting();
                break;
            case Done:
                break;
        }
    }

    bool IsDone() const {
        return state == Done;
    }

    std::vector<std::string> data;
    SortState state;

private:
    void LoadData() {
        std::ifstream file("c:\\myhugedatafile.txt");
        std::string line;
        while (std::getline(file, line)) {
            data.push_back(line);
        }
    }

    void ProcessSorting() {
        int iterations = 0;
        bool swapped;
        do {
            swapped = false;
            for (; i < data.size() - 1; ++i) {
                for (; j < data.size() - i - 1; ++j) {
                    if (data[j] > data[j + 1]) {
                        std::swap(data[j], data[j + 1]);
                        swapped = true;
                    }
                    if (++iterations >= 10) {
                        return;
                    }
                }
                j = 0;
            }
            i = 0;
        } while (swapped);
        state = Done;
    }

    size_t i, j;
};
```

<!-- 
> [!NOTE] IMPORTANT NOTE ABOUT MEMORY MANAGEMENT!
> In this example I have used "Modern C++" (ish- modern to early 1990s). In the old days we used things like malloc and free and the code above would be much more difficult to read if I coded it that way. So I used some STL 
-->

**IMPORTANT NOTE ABOUT MEMORY MANAGEMENT!**\
In this example I have used "Modern C++" (ish- modern to early 1990s).

In the old days we used things like malloc and free and the code above would be much more difficult to read if I coded it that way.

So I used some STL for these examples.

**Another important thing: 16 Bit Windows and Memory Leaks were HELL**\
In windows 1.0 - 3.11 , all the applications shared the same memory space.

!!!! UGH! SO that means if my app did not properly free memory...

it would eat all the memory for the whole OS and bring the whole system down..

Many of us of that era spent many many hours tracking down crazy issues with memory...

I look on those memories fondly ...but I **DON'T** miss them....  :)

### Fast Forward: Async and Await in C\#

#### Modern C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using System.Windows.Forms;

public class SortingJob
{
    public List<string> Data { get; private set; } = new List<string>();

    public async Task LoadDataAsync()
    {
        using (var reader = new StreamReader("c:\\myhugedatafile.txt"))
        {
            string line;
            while ((line = await reader.ReadLineAsync()) != null)
            {
                Data.Add(line);
            }
        }
    }

    public async Task SortDataAsync()
    {
        bool swapped;
        do
        {
            swapped = false;
            for (int i = 0; i < Data.Count - 1; i++)
            {
                if (string.Compare(Data[i], Data[i + 1]) > 0)
                {
                    var temp = Data[i];
                    Data[i] = Data[i + 1];
                    Data[i + 1] = temp;
                    swapped = true;
                }
            }
            await Task.Yield(); // Yield control to keep the UI responsive
        } while (swapped);
    }
}
```

***

### How The C++ code and Async Await  Example is Similar

* Both approaches split the sorting operation into chunks so that the UI remains responsive.
* Both rely on a way to yield control and resume execution later.
* The C++ version manually tracks state transitions, while the C# version uses `await` and `Task.Yield()`.

***

#### How to use the C++ SortingJob class in a Windows API Gui

```cpp
#include <windows.h>
#include <vector>
#include <string>
#include <fstream>

enum SortState {
    Constructed,
    DataLoaded,
    Processing,
    Done
};

class SortingJob {
public:
    SortingJob() : state(Constructed), i(0), j(0) {}
    ~SortingJob() {
        data.clear();
    }

    void DoSomeProcessing() {
        switch (state) {
            case Constructed:
                LoadData();
                state = DataLoaded;
                break;
            case DataLoaded:
                i = 0;
                j = 0;
                state = Processing;
                break;
            case Processing:
                ProcessSorting();
                break;
            case Done:
                break;
        }
    }

    bool IsDone() const {
        return state == Done;
    }

    std::vector<std::string> data;
    SortState state;

private:
    void LoadData() {
        std::ifstream file("c:\\myhugedatafile.txt");
        std::string line;
        while (std::getline(file, line)) {
            data.push_back(line);
        }
    }

    void ProcessSorting() {
        int iterations = 0;
        bool swapped;
        do {
            swapped = false;
            for (; i < data.size() - 1; ++i) {
                for (; j < data.size() - i - 1; ++j) {
                    if (data[j] > data[j + 1]) {
                        std::swap(data[j], data[j + 1]);
                        swapped = true;
                    }
                    if (++iterations >= 10) {
                        return;
                    }
                }
                j = 0;
            }
            i = 0;
        } while (swapped);
        state = Done;
    }

    size_t i, j;
};

SortingJob* sortingJob = nullptr;

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    switch (uMsg) {
        case WM_COMMAND:
            if (LOWORD(wParam) == ID_SORT_BUTTON) {
                EnableWindow(GetDlgItem(hwnd, ID_SORT_BUTTON), FALSE);
                if (sortingJob) delete sortingJob;
                sortingJob = new SortingJob();
                SetTimer(hwnd, 1, 1000, NULL);
            }
            break;
        case WM_TIMER:
            if (sortingJob) {
                sortingJob->DoSomeProcessing();
                if (sortingJob->IsDone()) {
                    KillTimer(hwnd, 1);
                    MessageBox(hwnd, "Bubble sort is done!", "Info", MB_OK);
                    EnableWindow(GetDlgItem(hwnd, ID_SORT_BUTTON), TRUE);
                    delete sortingJob;
                    sortingJob = nullptr;
                }
            }
            break;
        case WM_DESTROY:
            if (sortingJob) delete sortingJob;
            PostQuitMessage(0);
            break;
        default:
            return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}
```

***

### C# WPF Example using Async/Await

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using System.Windows;

public partial class MainWindow : Window
{
    private SortingJob sortingJob;

    public MainWindow()
    {
        InitializeComponent();
    }

    private async void SortButton_Click(object sender, RoutedEventArgs e)
    {
        SortButton.IsEnabled = false;
        sortingJob = new SortingJob();
        await sortingJob.LoadDataAsync();
        await sortingJob.SortDataAsync();
        MessageBox.Show("Bubble sort is done!");
        SortButton.IsEnabled = true;
    }
}

public class SortingJob
{
    public List<string> Data { get; private set; } = new List<string>();

    public async Task LoadDataAsync()
    {
        using (var reader = new StreamReader("c:\\myhugedatafile.txt"))
        {
            string line;
            while ((line = await reader.ReadLineAsync()) != null)
            {
                Data.Add(line);
            }
        }
    }

    public async Task SortDataAsync()
    {
        bool swapped;
        do
        {
            swapped = false;
            for (int i = 0; i < Data.Count - 1; i++)
            {
                if (string.Compare(Data[i], Data[i + 1]) > 0)
                {
                    var temp = Data[i];
                    Data[i] = Data[i + 1];
                    Data[i + 1] = temp;
                    swapped = true;
                }
            }
            await Task.Yield(); // Yield control to keep the UI responsive
        } while (swapped);
    }
}
```

This WPF version mirrors the C++ example by disabling the button, running the sort asynchronously, and then re-enabling the button after displaying a completion message.

***

### How The C++ code and Async Await  Example is Different

* The C++ version requires explicit state tracking, while C# abstracts that away with async/await.
* The C# version runs on a separate thread managed by the runtime, whereas the C++ version still runs in the main loop.
* The C++ version needs to be manually called in an event loop, while async/await automatically resumes execution.

***

## For Fun: MFC and Qt versions of the C Example

## C++ Implementation with MFC (GUI Code)

Here’s a simple **MFC-based GUI application** that demonstrates async-like behavior using **Windows messages** instead of modern `async/await`.

### Step 1: Add SortingJob.h

```cpp
#pragma once
#include <vector>
#include <string>
#include <fstream>

enum SortState {
    Constructed,
    DataLoaded,
    Processing,
    Done
};

class SortingJob {
public:
    SortingJob() : state(Constructed), i(0), j(0) {}
    ~SortingJob() { data.clear(); }

    void DoSomeProcessing() {
        switch (state) {
            case Constructed:
                LoadData();
                state = DataLoaded;
                break;
            case DataLoaded:
                i = 0;
                j = 0;
                state = Processing;
                break;
            case Processing:
                ProcessSorting();
                break;
            case Done:
                break;
        }
    }

    bool IsDone() const { return state == Done; }

private:
    void LoadData() {
        std::ifstream file("c:\\myhugedatafile.txt");
        std::string line;
        while (std::getline(file, line)) {
            data.push_back(line);
        }
    }

    void ProcessSorting() {
        int iterations = 0;
        bool swapped;
        do {
            swapped = false;
            for (; i < data.size() - 1; ++i) {
                for (; j < data.size() - i - 1; ++j) {
                    if (data[j] > data[j + 1]) {
                        std::swap(data[j], data[j + 1]);
                        swapped = true;
                    }
                    if (++iterations >= 10) { // Process in small chunks
                        return;
                    }
                }
                j = 0;
            }
            i = 0;
        } while (swapped);
        state = Done;
    }

    size_t i, j;
    std::vector<std::string> data;
    SortState state;
};
```

***

### Step 2: Modify MFC Dialog Code to Use SortingJob

#### **`AsyncDemoDlg.h`**

```cpp
#pragma once
#include "afxwin.h"
#include "SortingJob.h"

class CAsyncDemoDlg : public CDialogEx
{
public:
    CAsyncDemoDlg(CWnd* pParent = nullptr);

#ifdef AFX_DESIGN_TIME
    enum { IDD = IDD_ASYNCDEMO_DIALOG };
#endif

protected:
    virtual void DoDataExchange(CDataExchange* pDX);
    virtual BOOL OnInitDialog();
    afx_msg void OnBnClickedStartButton();
    afx_msg void OnTimer(UINT_PTR nIDEvent);
    DECLARE_MESSAGE_MAP()

private:
    CButton m_startButton;
    CStatic m_statusText;
    SortingJob* sortingJob;
};
```

***

#### **`AsyncDemoDlg.cpp`**

```cpp
#include "pch.h"
#include "AsyncDemo.h"
#include "AsyncDemoDlg.h"
#include "afxdialogex.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

CAsyncDemoDlg::CAsyncDemoDlg(CWnd* pParent)
    : CDialogEx(IDD_ASYNCDEMO_DIALOG, pParent), sortingJob(nullptr)
{
}

void CAsyncDemoDlg::DoDataExchange(CDataExchange* pDX)
{
    CDialogEx::DoDataExchange(pDX);
    DDX_Control(pDX, IDC_BUTTON_START, m_startButton);
    DDX_Control(pDX, IDC_STATIC_TEXT, m_statusText);
}

BEGIN_MESSAGE_MAP(CAsyncDemoDlg, CDialogEx)
    ON_BN_CLICKED(IDC_BUTTON_START, &CAsyncDemoDlg::OnBnClickedStartButton)
    ON_WM_TIMER()
END_MESSAGE_MAP()

BOOL CAsyncDemoDlg::OnInitDialog()
{
    CDialogEx::OnInitDialog();
    SetDlgItemText(IDC_STATIC_TEXT, _T("Press Start to sort."));
    return TRUE;
}

void CAsyncDemoDlg::OnBnClickedStartButton()
{
    m_startButton.EnableWindow(FALSE); // Disable button

    if (sortingJob) delete sortingJob;
    sortingJob = new SortingJob();

    SetDlgItemText(IDC_STATIC_TEXT, _T("Sorting started..."));
    
    // Start a timer to process sorting in chunks
    SetTimer(1, 100, NULL);
}

void CAsyncDemoDlg::OnTimer(UINT_PTR nIDEvent)
{
    if (nIDEvent == 1 && sortingJob)
    {
        sortingJob->DoSomeProcessing();

        if (sortingJob->IsDone())
        {
            KillTimer(1);
            MessageBox(_T("Sorting completed!"), _T("Info"), MB_OK);
            SetDlgItemText(IDC_STATIC_TEXT, _T("Sorting done!"));
            m_startButton.EnableWindow(TRUE); // Re-enable button
            delete sortingJob;
            sortingJob = nullptr;
        }
    }

    CDialogEx::OnTimer(nIDEvent);
}
```

***

### **How It Works**

1. **Button Click (`OnBnClickedStartButton`)**
   * Creates a new **`SortingJob`** instance.
   * Starts a **100ms timer** to process the sorting **in chunks**.

2. **Timer Event (`OnTimer`)**
   * Calls **`DoSomeProcessing()`** on **SortingJob** every 100ms.
   * Once sorting is complete, stops the timer and displays a **message box**.

***

### Qt Version of the C++ Example

```cpp
#include <QApplication>
#include <QPushButton>
#include <QTimer>
#include <QMessageBox>
#include <QStringList>
#include <QFile>
#include <QTextStream>

class SortingJob {
public:
    QStringList data;
    bool isDone = false;

    void loadData() {
        QFile file("c:/myhugedatafile.txt");
        if (file.open(QIODevice::ReadOnly)) {
            QTextStream in(&file);
            while (!in.atEnd()) {
                data.append(in.readLine());
            }
            file.close();
        }
    }

    void processSorting() {
        std::sort(data.begin(), data.end());
        isDone = true;
    }
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QWidget window;
    QPushButton button("Sort it now!", &window);
    SortingJob *sortingJob = new SortingJob();
    QTimer *timer = new QTimer(&window);

    QObject::connect(&button, &QPushButton::clicked, [&]() {
        button.setEnabled(false);
        sortingJob->loadData();
        timer->start(1000);
    });

    QObject::connect(timer, &QTimer::timeout, [&]() {
        sortingJob->processSorting();
        if (sortingJob->isDone) {
            timer->stop();
            QMessageBox::information(&window, "Done", "Bubble sort is done!");
            button.setEnabled(true);
            delete sortingJob;
        }
    });

    window.show();
    return app.exec();
}
```

This Qt version mirrors the behavior of the MFC example using Qt's `QTimer` to ensure the UI remains responsive.
