---
title: How To Write an OLE Server in Raw C++
description: Cause thats how we roll..no MFC Here...
slug: write-an-ole-server-in-raw-c++
date: 2015-09-09
image: post/Articles/IMAGES/ole.png
categories:
  - C++
  - OLE
  - COM
  - MFC
  - WinAPi
tags:
  - Ole
  - Com
  - In
  - Process
  - Out
  - Of
  - Process
  - Windows
  - Api
  - Dll
  - Exe
  - Automation
  - CPP
draft: false
weight: 39
lastmod: 2025-03-03T14:41:27.907Z
---
<!-- 
# Write an OLE Server in Raw C++
-->

## Introduction: OLE, The Ancient Windows Magic

So, what is OLE? It’s basically a way to allow applications to talk to each other, like embedding an Excel spreadsheet into a Word document (you've seen that, right?).

It runs on top of COM (Component Object Model), which is another relic of Windows past that, believe it or not, still powers a lot of modern applications.

Now, there are two ways to write an OLE server:

1. **In-Process Server** – This is a fancy way of saying "DLL." The OLE server runs inside the same process as the client, making it faster but harder to isolate if something goes wrong.
2. **Out-of-Process Server** – This is a standalone EXE that runs separately, meaning better isolation but slightly slower communication.

And this can be done in MFC, or just RAW C++.. the later is the one we will explore in this article..

***

## Writing an In-Process OLE Server (DLL)

An in-process OLE server is a DLL that exposes some COM objects. We will create a simple **calculator OLE server** that lets a client add and subtract numbers.

### Step 1: Define the COM Interface

We define a simple interface with two methods, `Add` and `Subtract`.

```cpp
// Calculator.idl
import "oaidl.idl";
import "ocidl.idl";

[ object, uuid(12345678-1234-1234-1234-123456789012), oleautomation ]
interface ICalculator : IUnknown
{
    HRESULT Add([in] int a, [in] int b, [out, retval] int* result);
    HRESULT Subtract([in] int a, [in] int b, [out, retval] int* result);
};
```

### Step 2: Implement the OLE Server

```cpp
// Calculator.cpp
#include <windows.h>
#include "Calculator_h.h"

class Calculator : public ICalculator
{
    LONG refCount;
public:
    Calculator() : refCount(1) {}
    
    HRESULT __stdcall QueryInterface(REFIID riid, void** ppv) override {
        if (riid == IID_IUnknown || riid == __uuidof(ICalculator)) {
            *ppv = static_cast<ICalculator*>(this);
            AddRef();
            return S_OK;
        }
        *ppv = nullptr;
        return E_NOINTERFACE;
    }
    
    ULONG __stdcall AddRef() override {
        return InterlockedIncrement(&refCount);
    }
    
    ULONG __stdcall Release() override {
        ULONG count = InterlockedDecrement(&refCount);
        if (count == 0) delete this;
        return count;
    }
    
    HRESULT __stdcall Add(int a, int b, int* result) override {
        *result = a + b;
        return S_OK;
    }
    
    HRESULT __stdcall Subtract(int a, int b, int* result) override {
        *result = a - b;
        return S_OK;
    }
};
```

### Step 3: Register the Server

To make the DLL accessible, we need to register it using `regsvr32`. We implement `DllRegisterServer`.

```cpp
STDAPI DllRegisterServer() {
    // Register CLSID and interfaces here (omitted for brevity)
    return S_OK;
}
```

Compile this as a DLL and register it using:

```sh
regsvr32 Calculator.dll
```

And that’s it! You now have a working in-process OLE server.

***

## Writing an Out-of-Process OLE Server (EXE)

Out-of-process servers are basically standalone applications that expose COM objects. We use the same `ICalculator` interface but implement it in an EXE.

```cpp
// CalculatorServer.cpp
#include <windows.h>
#include "Calculator_h.h"

class Calculator : public ICalculator { /* same as before */ };

int main() {
    CoInitialize(NULL);
    Calculator calc;
    // Register with COM runtime
    CoRegisterClassObject(__uuidof(Calculator), &calc, CLSCTX_LOCAL_SERVER, REGCLS_MULTIPLEUSE, &cookie);
    MessageBox(NULL, "Server running", "OLE Server", MB_OK);
    CoUninitialize();
    return 0;
}
```

Compile and register it with:

```sh
CalculatorServer.exe /RegServer
```

***

## Calling the OLE Servers

From a client application, we can use `CoCreateInstance` to call either server.

```cpp
// Client.cpp
#include <windows.h>
#include "Calculator_h.h"

int main() {
    CoInitialize(NULL);
    ICalculator* calc;
    HRESULT hr = CoCreateInstance(__uuidof(Calculator), NULL, CLSCTX_ALL, IID_ICalculator, (void**)&calc);
    if (SUCCEEDED(hr)) {
        int result;
        calc->Add(3, 4, &result);
        printf("3 + 4 = %d\n", result);
        calc->Release();
    }
    CoUninitialize();
    return 0;
}
```

***

## Key Ideas

| Concept               | Explanation                                                          |
| --------------------- | -------------------------------------------------------------------- |
| OLE                   | Object Linking and Embedding, old-school inter-process communication |
| COM                   | The backbone of OLE, allows objects to interact                      |
| In-Process Server     | A DLL-based COM server                                               |
| Out-of-Process Server | An EXE-based COM server                                              |
| CoCreateInstance      | Function to instantiate a COM object                                 |

***

## References

* [Microsoft COM Documentation](https://docs.microsoft.com/en-us/windows/win32/com/)
* [Regsvr32 Documentation](https://docs.microsoft.com/en-us/windows/win32/stg/registration-functions)
* [Understanding OLE Servers](https://docs.microsoft.com/en-us/cpp/atl/)
