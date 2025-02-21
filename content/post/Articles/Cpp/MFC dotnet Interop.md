---
title: MFC Dot Net Interop Options in 2022
description: MFC Dot Net Interop Options in 2022
slug: mfc-dot-net-interop-options-in-2022
date: 2019-07-17
image: post/Articles/IMAGES/mfcclassic.pngng
categories:
  - Interop
  - MFC
  - DotNet
  - Win32
  - CSharp
  - CPP
  - WinAPI
tags:
  - Interop
  - Mfc
  - Dotnet
  - Win32
  - Csharp
  - Cpp
  - Legacy
  - Windows
draft: false
weight: 404
lastmod: 2025-02-20T22:12:49.640Z
---
# MFC Dot Net Interop Options in 2022

Ah, MFC and .NET interop—two things that probably shouldn't mix but somehow still do, like pineapple on pizza.

Whether you're maintaining some battle-worn Win32 application or just enjoy the pain of dealing with legacy code, you've come to the right place.

Let's take a trip down memory lane, look at Microsoft's various interop strategies, and explore the modern way to make MFC and .NET play nice.

## A Brief History of MFC and .NET Interop

### MFC: The Dinosaur That Refuses to Die

MFC (Microsoft Foundation Classes) first graced our lives in the early 1990s.

It was Microsoft's grand attempt to wrap Win32 API calls in a C++ object-oriented framework.

It had its quirks, sure, but it was *the* way to build Windows applications before .NET came along.

### Enter .NET: The Future (Or So We Thought)

When .NET showed up in the early 2000s, Microsoft basically said, "Hey, forget MFC, use this shiny new thing instead!" C# was fresh, garbage-collected, and had all sorts of goodies that made MFC devs cry tears of joy.

However, the real world is messy, and plenty of businesses already had massive MFC applications. They needed a way to bridge the old with the new.

### The Many Ways Microsoft Tried (and Failed) to Make This Easy

1. **C++/CLI** – The lovechild of C++ and .NET. You could write managed C++ code that talked to MFC and .NET. Effective? Yes. Fun? Not so much.
2. **P/Invoke** – A way for C# to call native functions via DLL imports. Great for simple calls but a headache for anything complex.
3. **COM Interop** – Register your MFC app as a COM server and talk to it from .NET. This method worked but required dealing with COM registration nightmares.
4. **Mixed-mode DLLs** – Using a combination of native and managed code in the same DLL. This worked well but made debugging feel like untangling spaghetti.

## The Modern Approach: C++/CLI as a Bridge

The best way to connect MFC and .NET today is still **C++/CLI**, as it allows direct calls between managed and unmanaged code.

Let’s look at an example where C# calls an MFC class, and then MFC calls back into C#.

### Example 1: C# Calls MFC

#### **Step 1: The MFC C++ Code (Exports a Function)**

```cpp
// MFCInterop.h
#pragma once
#include <afxwin.h>

class __declspec(dllexport) MFCInterop {
public:
    static CString GreetUser(const char* name);
};
```

```cpp
// MFCInterop.cpp
#include "MFCInterop.h"

CString MFCInterop::GreetUser(const char* name) {
    CString greeting;
    greeting.Format("Hello %s from MFC!", name);
    return greeting;
}
```

```cpp
// MFCInterop.def (For Explicit Export)
EXPORTS
    GreetUser @1
```

#### **Step 2: The C# Code (Calls the MFC Function)**

```csharp
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("MFCInterop.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern IntPtr GreetUser(string name);

    static void Main()
    {
        IntPtr ptr = GreetUser("John");
        string message = Marshal.PtrToStringAnsi(ptr);
        Console.WriteLine(message);
    }
}
```

### Example 2: MFC Calls a C# Function

Now let's flip things around. Here, MFC calls into C#.

#### **Step 1: The C# Code (Creates a .NET Class Library)**

```csharp
// DotNetLibrary.cs
using System;

namespace DotNetLibrary
{
    public class Greeter
    {
        public static string GetGreeting(string name)
        {
            return $"Hello {name}, from .NET!";
        }
    }
}
```

Compile this as a DLL, and make sure it's accessible from your MFC application.

#### **Step 2: The MFC Code (Calls the C# Function Using C++/CLI)**

```cpp
// DotNetInteropBridge.h
#pragma once

#include <msclr/marshal_cppstd.h>
using namespace System;
using namespace DotNetLibrary;

public ref class DotNetInteropBridge {
public:
    static std::string CallDotNetGreet(std::string name) {
        String^ managedName = gcnew String(name.c_str());
        String^ result = Greeter::GetGreeting(managedName);
        return msclr::interop::marshal_as<std::string>(result);
    }
};
```

```cpp
// MFC App Calling the Bridge
#include "DotNetInteropBridge.h"

void SomeMfcFunction() {
    std::string response = DotNetInteropBridge::CallDotNetGreet("Alice");
    AfxMessageBox(CString(response.c_str()));
}
```

## Pros and Cons of MFC-.NET Interop

| **Pros**                                           | **Cons**                                     |
| -------------------------------------------------- | -------------------------------------------- |
| Allows gradual migration from legacy MFC to .NET   | Debugging mixed-mode applications is painful |
| Can reuse existing business logic in MFC           | C++/CLI syntax is awkward                    |
| Bridges old Win32 applications with modern .NET UI | Deployment complexity (dependencies, DLLs)   |
| Performance is decent with P/Invoke or C++/CLI     | Increased maintenance burden                 |

## Conclusion

Interop between MFC and .NET isn't *pretty*, but it's very much possible. If you're maintaining an old MFC app, C++/CLI is your best bet. If you're starting fresh, do yourself a favor and avoid MFC altogether. Either way, you now have the tools to keep those legacy apps running for another decade (whether that's a blessing or a curse is up to you).

***

## Key Takeaways

* MFC is old but still alive in many legacy applications.
* .NET offers several interop options, but C++/CLI is the most practical.
* Calling MFC from .NET via P/Invoke is straightforward for simple functions.
* Calling .NET from MFC is best done with C++/CLI.
* Debugging mixed-mode applications can be painful but is sometimes necessary.

***

## References

1. [Microsoft Docs - C++/CLI](https://learn.microsoft.com/en-us/cpp/dotnet/walkthrough-mixed-mode-dlls)
2. [P/Invoke Best Practices](https://learn.microsoft.com/en-us/dotnet/standard/native-interop/pinvoke)
3. [MFC Documentation](https://learn.microsoft.com/en-us/cpp/mfc/mfc-desktop-applications)
