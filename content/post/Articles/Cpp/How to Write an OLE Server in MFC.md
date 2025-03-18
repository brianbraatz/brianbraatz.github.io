---
title: Write an OLE Server in MFC
description: How we used to roll, in MFC , back in the old days
slug: out-of-process-ole-servers-in-mfc
date: 2022-01-19
image: post/Articles/IMAGES/mfcclassic.png
categories:
  - Windows
  - WinAPI
  - MFC
  - OLE
tags:
  - Ole
  - Mfc
  - Com
  - Out of process
  - In process
  - Multithreading
  - Apartment threading
  - Windows
  - Legacy code
draft: false
weight: 665
categories_ref:
  - Windows
  - WinAPI
  - MFC
  - OLE
slug_calculated: https://brianbraatz.github.io/p/out-of-process-ole-servers-in-mfc
lastmod: 2025-03-14T16:40:19.270Z
---
# How We Used to Write an Out-of-Process OLE Server in MFC, Back in the Old Days

Ah, the good old days of Windows programming!

Back when OLE (Object Linking and Embedding) was cutting-edge and writing an out-of-process OLE server in MFC was a rite of passage.

If you ever wanted to create embeddable documents, automation objects, or just confuse future developers, OLE was your friend

. Nowadays, there are more modern ways to accomplish the same tasks, but let's take a fun trip down memory lane and see how we did it "back in the day."

## What is OLE? (For the Young Folks)

OLE (Object Linking and Embedding) was a technology introduced by Microsoft to allow applications to interact with one another.

It let you embed an Excel spreadsheet inside a Word document, control applications via automation, and generally make Windows development way more complicated than necessary.

At the core of OLE was COM (Component Object Model), a technology that brought us joys like IUnknown, reference counting, and debugging nightmares.

## In-Process vs. Out-of-Process OLE Servers

There were two main types of OLE servers:

* **In-Process Servers**: These ran as DLLs inside the client process. They were fast but could take down the entire client application if they crashed.
* **Out-of-Process Servers**: These were separate EXEs that communicated with the client via COM. They were more robust because a crash wouldn’t affect the client, but they were slower due to inter-process communication.

### Pros and Cons

| Type                 | Pros                                | Cons                                      |
| -------------------- | ----------------------------------- | ----------------------------------------- |
| In-Process (DLL)     | Fast, runs in the same memory space | Can crash the client app                  |
| Out-of-Process (EXE) | Isolated from client crashes        | Slower due to inter-process communication |

Since we’re talking about an **out-of-process** OLE server today, let's roll up our sleeves and write one using **MFC**!

## Writing an Out-of-Process OLE Server in MFC

### Step 1: Create an MFC Application

Fire up **Visual Studio** (preferably an old-school version like VC++ 6.0 for full nostalgia) and create a new **MFC App**.

### Step 2: Enable OLE Support

Make sure OLE support is enabled by calling `AfxOleInit()`. Without this, your app will fail spectacularly.

```cpp
BOOL CMyApp::InitInstance()
{
    if (!AfxOleInit())
    {
        AfxMessageBox("OLE Initialization Failed!");
        return FALSE;
    }
    return TRUE;
}
```

### Step 3: Derive from COleObjectFactory

We need an **OLE factory** to create our objects. In MFC, this is done using `COleObjectFactory`.

```cpp
class CMyOleFactory : public COleObjectFactory
{
public:
    CMyOleFactory() : COleObjectFactory(CLSID_MyOleObject, RUNTIME_CLASS(CMyOleObject), TRUE, _T("MyOleApp")) {}
};
```

### Step 4: Register the OLE Server

An out-of-process OLE server needs to **register itself with the system**, so Windows knows it exists.

```cpp
BOOL CMyApp::ExitInstance()
{
    AfxOleTerm(FALSE); // Uninitialize OLE
    return CWinApp::ExitInstance();
}
```

Then, in `InitInstance()`, we make sure the application is properly registered:

```cpp
if (!COleObjectFactory::RegisterAll())
{
    AfxMessageBox("OLE Registration Failed!");
    return FALSE;
}
```

### Step 5: Implement the OLE Object

Your OLE object must derive from `CCmdTarget` and use the `DECLARE_OLECREATE` macro.

```cpp
class CMyOleObject : public CCmdTarget
{
    DECLARE_DYNCREATE(CMyOleObject)
    DECLARE_OLECREATE(CMyOleObject)
};
```

MFC makes COM implementation easier, but it’s still COM, meaning you get to enjoy **IUnknown, reference counting, and apartment threading!**

## Threading in OLE: Apartment Model

COM introduced a threading model called **apartment threading**. There are two main types:

1. **Single-Threaded Apartment (STA)** – Each COM object lives in its own thread and requires marshaling for cross-thread access.
2. **Multi-Threaded Apartment (MTA)** – COM objects can be called from multiple threads.

MFC mostly sticks to **STA**, which means each OLE object instance runs in a dedicated thread. This makes debugging a little easier, but also means you have to be careful about concurrency issues.

## Can You Still Do This Today?

Yes! But…why would you? Nowadays, you can accomplish the same things using:

* **.NET COM Interop** (for those still using COM)
* **REST APIs** (for inter-process communication)
* **gRPC** (for high-performance IPC)
* **Local WebSockets** (for real-time communication)

## Generally....

Writing an out-of-process OLE server in MFC was an adventure.

It was powerful, but also a headache.

Today, there are easier ways to get applications to talk to each other.

And there is still ALOT of old OLE code out there. Ironically i stil run into alot..

That old code just keeps runnnig and runnning...

# Using Our MFC OLE Server in an MFC Dialog Application

So, you've built an out-of-process OLE server in MFC.

But now, the big question: **How do we actually use this thing?**

## Step 1: Creating an MFC Dialog-Based Client Application

First, fire up **Visual Studio** and create a new **MFC Dialog Application** project.

Make sure you choose a dialog-based app so we can focus on embedding our OLE server instead of dealing with complex UI layouts.

## Step 2: Initialize OLE in the Client Application

Before we start calling into our OLE server, we need to initialize OLE in our client application. This is done in `InitInstance()`:

```cpp
BOOL CMyClientApp::InitInstance()
{
    if (!AfxOleInit())
    {
        AfxMessageBox("OLE Initialization Failed!");
        return FALSE;
    }
    return TRUE;
}
```

If `AfxOleInit()` fails, your application won’t be able to talk to the OLE server. Trust me, you don’t want to skip this step unless you enjoy debugging mysterious failures.

## Step 3: Import the OLE Server's Type Library

To communicate with our OLE server, we need to import its type library. This generates wrapper classes that make working with the server easier.

Add this to your **client application’s header file**:

```cpp
#import "MyOleServer.tlb" no_namespace
```

This tells the compiler to import the type library without wrapping everything in a namespace. If you prefer, you can use `named_guids` to generate CLSIDs and IIDs automatically.

## Step 4: Create an Instance of the OLE Server

Now we need to create an instance of the OLE object from our server. This is done using `CoCreateInstance()`:

```cpp
CComPtr<IMyOleObject> spOleObject;
HRESULT hr = spOleObject.CoCreateInstance(CLSID_MyOleObject);
if (FAILED(hr))
{
    AfxMessageBox("Failed to create OLE object!");
    return;
}
```

If everything is working correctly, you now have an instance of your OLE object running inside your MFC dialog application!

## Step 5: Call a Method on the OLE Server

Let’s say our OLE server has a method called `DoSomething()`. We can call it like this:

```cpp
spOleObject->DoSomething();
```

That’s it! You’ve successfully invoked a method on your OLE server from your MFC dialog application.

## Step 6: Embedding the OLE Object in the Dialog

If you want to embed the OLE object inside your dialog (instead of just using it programmatically), you can use the `COleClientItem` class to display the object inside a control.

First, add a `COleClientItem` member to your dialog class:

```cpp
class CMyClientDlg : public CDialogEx
{
    COleClientItem m_oleItem;
};
```

Then, initialize it with your OLE object:

```cpp
m_oleItem.AttachDispatch(spOleObject, TRUE);
m_oleItem.DoVerb(OLEIVERB_SHOW, NULL);
```

This will display your OLE object inside the dialog window. Now you can interact with it just like you would with an embedded Excel spreadsheet or Word document.

## Conclusion

Integrating an **MFC OLE server** into an **MFC dialog application** isn’t as hard as it looks—especially when you’ve got a good grasp on `CoCreateInstance()`, `COleClientItem`, and OLE initialization.

Sure, it may not be the hottest tech in 2019, but it’s a fun (and still useful) look into Windows programming history.

## Key Ideas

| Concept               | Description                                                             |
| --------------------- | ----------------------------------------------------------------------- |
| OLE                   | Object Linking and Embedding for inter-application communication        |
| In-Process Server     | Runs inside the client’s process as a DLL                               |
| Out-of-Process Server | Runs as a separate EXE and communicates via COM                         |
| Apartment Threading   | COM's threading model for concurrency                                   |
| MFC and OLE           | MFC simplifies COM implementation but still follows apartment threading |
| CoCreateInstance      | Used to create an instance of an OLE server object                      |
| Type Library          | Generated file containing COM interface definitions                     |
| COleClientItem        | MFC class used for embedding OLE objects in dialogs                     |

## References

* [Microsoft OLE Documentation](https://learn.microsoft.com/en-us/windows/win32/com/ole)
* [Understanding COM Apartments](https://learn.microsoft.com/en-us/windows/win32/com/com-threading-models)
* [MFC and OLE](https://learn.microsoft.com/en-us/cpp/mfc/ole-programming-in-mfc)
* [The Death of OLE?](https://devblogs.microsoft.com/oldnewthing/20190702-00/?p=102625)
