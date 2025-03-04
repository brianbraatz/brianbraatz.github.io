---
title: Bluetooth communication- Windows in C++ & C#
description: " C++ and C# Code Examples"
slug: bluetooth-communication-cpp-csharp
date: 2017-08-14
image: post/Articles/IMAGES/bluetooth.png
categories:
  - Bluetooth
  - C++
  - C#
  - Windows
  - Programming
  - CPP
  - CSharp
  - Device Drivers
tags:
  - Bluetooth
  - C++
  - C#
  - Windows
  - Networking
  - Wireless
draft: false
weight: 282
lastmod: 2025-03-04T10:29:51.652Z
---
<!-- 
# Bluetooth Communication in C++ and C# on Windows with Code Examples

Bluetooth. The magical technology that lets us connect our devices wirelessly, transfer files, and lose AirPods in record time.

If you’re looking to add Bluetooth functionality to your C++ or C# Windows application, you’ve come to the right place.

Buckle up, because we’re about to dive into the world of sockets, COM objects, and Windows APIs (brace yourself). -->

***

## Setting Up Bluetooth Communication in C++

<!-- C++ and Bluetooth on Windows can be a bit of a headache, but with the right approach, we can make it work without losing our sanity (mostly). -->

### Using Windows Sockets for Bluetooth (Winsock)

Microsoft provides the `winsock2.h` library for Bluetooth communication. Here’s a basic example of how to scan for Bluetooth devices using Winsock in C++:

```cpp
#include <winsock2.h>
#include <ws2bth.h>
#include <iostream>

#pragma comment(lib, "Ws2_32.lib")

int main() {
    WSADATA wsaData;
    WSAStartup(MAKEWORD(2, 2), &wsaData);

    SOCKET btSocket = socket(AF_BTH, SOCK_STREAM, BTHPROTO_RFCOMM);
    if (btSocket == INVALID_SOCKET) {
        std::cerr << "Failed to create Bluetooth socket!" << std::endl;
        return 1;
    }

    std::cout << "Bluetooth socket created successfully!" << std::endl;
    closesocket(btSocket);
    WSACleanup();
    return 0;
}
```

This initializes Winsock, creates a Bluetooth socket, and then closes it. Simple, right?

 <!-- Well, this is just the start. -->

### Sending Data Over Bluetooth in C++

Once you’ve found a Bluetooth device, you can connect and send data using `connect()` and `send()`.

```cpp
// Assume socket is already created and connected
const char* message = "Hello Bluetooth!";
send(btSocket, message, strlen(message), 0);
```

That’s all you need for a basic Bluetooth connection in C++! (Except for all the error handling you should be doing. But let’s not kill the vibe.)

The most important thing to rememebr-is this is just like a socket or serial port.

***

## Bluetooth Communication in C\#

C# makes Bluetooth development a little easier thanks to the Windows APIs and .NET libraries.

### Discovering Bluetooth Devices

Here’s how to list paired Bluetooth devices using C#:

```csharp
using System;
using System.Linq;
using Windows.Devices.Enumeration;
using Windows.Devices.Bluetooth;

class Program {
    static async System.Threading.Tasks.Task Main() {
        var devices = await DeviceInformation.FindAllAsync(BluetoothDevice.GetDeviceSelector());
        foreach (var device in devices) {
            Console.WriteLine($"Found: {device.Name}");
        }
    }
}
```

This snippet scans for Bluetooth devices and prints their names. Short, sweet, and to the point.

### Connecting and Sending Data in C\#

Let’s send data over Bluetooth using `RfcommDeviceService` (the C# equivalent of raw socket programming in C++ but without the suffering):

```csharp
using System;
using System.Text;
using System.IO;
using Windows.Devices.Bluetooth.Rfcomm;
using Windows.Storage.Streams;

class Program {
    static async System.Threading.Tasks.Task Main() {
        var device = await BluetoothDevice.FromIdAsync("YourDeviceIdHere");
        var service = await device.GetRfcommServicesAsync();
        var stream = service.Services[0].ConnectionHostName;
        
        using (DataWriter writer = new DataWriter(new MemoryStream().AsOutputStream())) {
            writer.WriteString("Hello, Bluetooth!");
            await writer.StoreAsync();
        }
    }
}
```

And just like that, we’re talking to Bluetooth devices without diving into raw sockets.

***

<!-- 
## Conclusion

Bluetooth communication in C++ is powerful but a bit painful, thanks to Winsock and manual socket handling.

C#, on the other hand, gives you a higher-level API that makes things easier but requires Windows 10+ APIs.

If you want control, go with C++. If you want ease of use, C# is your best bet.

Either way, Bluetooth is an exciting technology—when it actually works.

---

## Key Ideas

| Concept | Summary |
|---------|---------|
| Bluetooth in C++ | Uses Winsock for socket-based communication. |
| Bluetooth in C# | Uses Windows APIs and `RfcommDeviceService`. |
| Device Discovery | Both C++ and C# can list Bluetooth devices easily. |
| Data Transfer | Sockets in C++, Streams in C#. |
| Best Choice? | C++ for performance, C# for ease of use. |

---

## References

1. [Microsoft Docs - Bluetooth and Winsock](https://docs.microsoft.com/en-us/windows/win32/bluetooth/bluetooth-programming-with-winsock)
2. [C# UWP Blueto -->
