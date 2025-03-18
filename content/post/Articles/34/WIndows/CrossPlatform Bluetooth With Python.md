---
title: Cross-Platform Bluetooth Communication in Python
description: For Windows and Linux
slug: cross-platform-bluetooth-python
date: 2018-06-22
image: post/Articles/IMAGES/bluetooth.png
categories:
  - Bluetooth
  - Python
  - Windows
  - Linux
  - Programming
  - Device Drivers
tags:
  - Bluetooth
  - Python
  - Windows
  - Linux
  - Networking
  - Wireless
draft: false
weight: 231
categories_ref:
  - Bluetooth
  - Python
  - Windows
  - Linux
  - Programming
  - Device Drivers
slug_calculated: https://brianbraatz.github.io/p/cross-platform-bluetooth-python
lastmod: 2025-03-14T16:40:13.426Z
---
# Cross-Platform Bluetooth Communication in Python for Windows and Linux

<!-- Bluetooth. The technology that makes wireless connections simple (when it works) and debugging painful (when it doesn’t).

In our previous article, we explored Bluetooth communication in C++ and C# for Windows. But what if you need a cross-platform solution that runs on both Windows and Linux?

Python to the rescue!  -->

With the `pybluez` library, we can create Bluetooth applications that work seamlessly on both platforms.

<!-- Let’s dive in. -->

***

## Setting Up Bluetooth in Python

Python has a wonderful Bluetooth library called `pybluez`, which provides an easy-to-use API for scanning, connecting, and sending data over Bluetooth.

And it works the same on both platforms

### Installing PyBluez

Before we do anything, let’s install `pybluez`.

On **Windows**:

```sh
pip install pybluez
```

On **Linux**:

```sh
sudo apt-get install bluetooth libbluetooth-dev
pip install pybluez
```

***

## Discovering Bluetooth Devices

Now that we have `pybluez` installed, let’s scan for nearby Bluetooth devices.

```python
import bluetooth

def discover_devices():
    print("Scanning for Bluetooth devices...")
    devices = bluetooth.discover_devices(duration=8, lookup_names=True)
    
    for addr, name in devices:
        print(f"Found {name} at {addr}")

if __name__ == "__main__":
    discover_devices()
```

This script scans for Bluetooth devices for 8 seconds and prints their names and addresses.

***

## Connecting and Sending Data

Once we have found a Bluetooth device, we can connect to it and send data. Here’s how:

```python
import bluetooth

def send_data(target_address, message):
    port = 1  # RFCOMM default port
    sock = bluetooth.BluetoothSocket(bluetooth.RFCOMM)
    sock.connect((target_address, port))
    
    print(f"Connected to {target_address}, sending data...")
    sock.send(message)
    sock.close()

if __name__ == "__main__":
    target_address = "XX:XX:XX:XX:XX:XX"  # Replace with your device's Bluetooth address
    send_data(target_address, "Hello Bluetooth!")
```

This script creates an RFCOMM socket, connects to a Bluetooth device, sends a message, and then closes the connection.

***

## Receiving Data Over Bluetooth

To receive data from another device, we need to set up a Bluetooth server.

```python
import bluetooth

def bluetooth_server():
    server_sock = bluetooth.BluetoothSocket(bluetooth.RFCOMM)
    server_sock.bind(("", 1))  # Bind to port 1
    server_sock.listen(1)
    
    print("Waiting for connection...")
    client_sock, client_info = server_sock.accept()
    print(f"Connected to {client_info}")
    
    data = client_sock.recv(1024)
    print(f"Received: {data}")
    
    client_sock.close()
    server_sock.close()

if __name__ == "__main__":
    bluetooth_server()
```

This sets up a simple Bluetooth server that listens for incoming messages.

***

## Conclusion

Python makes cross-platform Bluetooth development easier than ever.

With `pybluez`, you can discover devices, send data, and even set up a Bluetooth server—all with a few lines of code.

<!-- Windows or Linux, Python has you covered! -->

<!-- 
---

## Key Ideas

| Concept | Summary |
|---------|---------|
| Bluetooth in Python | Uses `pybluez` for cross-platform support. |
| Device Discovery | Scans for nearby Bluetooth devices. |
| Sending Data | Uses RFCOMM sockets to send data. |
| Receiving Data | Sets up a server to accept incoming connections. |
| Cross-Platform | Works on both Windows and Linux. |

---

## References

1. [PyBluez Documentation](https://github.com/pybluez/pybluez)
2. [Bluetooth Programming with Python](https://people.csail.mit.edu/albert/bluez-intro/)
3. [RFCOMM Protocol](https://en.wikipedia.org/wiki/RFCOMM)
 -->
