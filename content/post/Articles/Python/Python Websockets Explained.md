---
title: Python Websockets Explained
description: An informal exploration of Python WebSockets including example
slug: websockets-sockets-python
date: 2015-12-15
image: post/Articles/IMAGES/24.jpg
categories:
  - Python
  - Python-Websockets
  - Websockets
  - Networking
tags:
  - WebSockets
  - Sockets
  - SignalR
  - Python
  - Real-Time
  - Communication
draft: false
weight: 30
lastmod: 2025-02-09T23:16:55.224Z
---
<!--
# WebSockets, Sockets, and SignalR: A Fun Dive into Real-Time Communication with Python

Hey there, fellow nerd! 

Ready to embark on a journey through the wild world of real-time communication? 

Grab your virtual backpack, and let's explore WebSockets, Sockets, and SignalR. We'll even get our hands dirty with a Python WebSocket server example. And don't worry, we'll keep things light with some jokes and funny images along the way!
-->

## What's the Deal with WebSockets?

Imagine you're at a tennis match.

The ball (data) is constantly moving back and forth between players (client and server) without any delays.

That's pretty much how WebSockets work! They provide a full-duplex (two-way) communication channel over a single TCP connection, allowing data to flow freely between client and server in real-time.

No more waiting for your turn to speak

## Buzzword Cracking: WebSockets vs. Sockets vs. SignalR

* **Sockets**: Think of sockets as the fundamental building blocks for network communication. They allow devices to send and receive data over a network. It's like having a phone line where you dial in, have a chat, and hang up when you're done.

* **WebSockets**: These are a specialized type of socket designed for the web. They start as a regular HTTP connection but then upgrade to a persistent, full-duplex communication channel. It's like starting a conversation with a "Hello" and then switching to walkie-talkies for continuous chatter.

* **SignalR**: This is a high-level library that abstracts the complexities of real-time communication. It uses WebSockets under the hood when available but can fall back to other techniques if necessary. It's like having a universal translator that ensures your message gets across, no matter the language barrier.

For the purpose of this article - and to help you be less confused, just remember that SignalR sits on top of Websockets..

This Article is about websockets.. so thats all for SignalR in this post..

and we carry on...

For a deeper dive into the differences, check out this comparison: [SignalR vs. WebSocket: A Depth Comparison You Should Know](https://apidog.com/blog/signalr-vs-websocket/)

## Coroutines: The Unsung Heroes of Asynchronous Programming

Before we jump into the code, let's talk about coroutines.

In Python, coroutines are special functions that allow you to pause and resume execution, making them perfect for handling asynchronous tasks like managing multiple WebSocket connections.

Think of them as the multitaskers of the programming world, juggling multiple tasks without breaking a sweat.

For more on coroutines, check out the [Python documentation on coroutines](https://docs.python.org/3/library/asyncio-task.html#coroutines).

## Writing a Python WebSocket Server

Time to write a simple WebSocket server in Python.

We'll use the `websockets` library, which makes handling WebSockets easy as cake! ( to this day I still dont understand that expression).

First, install the library:

```bash
pip install websockets
```

Now, let's write the server:

```python
import asyncio
import websockets

async def echo(websocket, path):
    async for message in websocket:
        print(f"Received message: {message}")
        await websocket.send(f"Echo: {message}")

async def main():
    async with websockets.serve(echo, "localhost", 8765):
        await asyncio.Future()  # Run forever

if __name__ == "__main__":
    asyncio.run(main())
```

In this script:

* We define an asynchronous function `echo` that listens for messages from the client and sends back an "Echo" response.
* The `main` function sets up the server to listen on `localhost` at port `8765`.
* We use `asyncio.run(main())` to start the server.

To create a Python client that connects to a WebSocket server, you can use the `websockets` library, which provides a straightforward and efficient way to handle WebSocket communications asynchronously.

**Write the WebSocket Client:**

Here's a sample Python client that connects to a WebSocket server, sends a message, and awaits a response:

```python
import asyncio
import websockets

async def communicate():
    uri = "ws://localhost:8765"  # Replace with your server's URI
    async with websockets.connect(uri) as websocket:
        # Send a message to the server
        message = "Hello, Server!"
        await websocket.send(message)
        print(f"Sent to server: {message}")

        # Await a response from the server
        response = await websocket.recv()
        print(f"Received from server: {response}")

# Run the communicate coroutine
asyncio.run(communicate())
```

**Explanation:**

* **Import Statements:** We import `asyncio` for asynchronous operations and `websockets` for WebSocket functionalities.

* **`communicate` Coroutine:** This asynchronous function handles the connection and communication with the WebSocket server.
  * **Connection:** It connects to the server specified by `uri`.
  * **Sending a Message:** Sends a greeting message to the server.
  * **Receiving a Response:** Waits for and prints the server's response.

* **Running the Coroutine:** `asyncio.run(communicate())` starts the asynchronous communication.

**Note:** Ensure that the WebSocket server is running and accessible at the specified URI (`ws://localhost:8765` in this example) before running the client.

For more detailed information and advanced usage, you can refer to the [websockets library documentation](https://websockets.readthedocs.io/en/stable/intro/tutorial1.html).

<!-- To test this server, you can use a WebSocket client or even write a simple one in Python. For more details, check out the 
-->

[websockets library documentation](https://pypi.org/project/websockets/).

## Key Ideas

| Concept                     | Description                                                                                      |
| --------------------------- | ------------------------------------------------------------------------------------------------ |
| **WebSockets**              | Full-duplex communication channels over a single TCP connection for real-time data exchange.     |
| **Sockets**                 | Endpoints for sending and receiving data across a network.                                       |
| **SignalR**                 | A library that abstracts real-time communication complexities, using WebSockets when available.  |
| **Coroutines**              | Special Python functions that enable asynchronous programming by pausing and resuming execution. |
| **Python WebSocket Server** | An example using the `websockets` library to create a simple echo server.                        |

## Related Resources

* [Writing WebSocket client applications](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_client_applications)
* [SignalR vs. WebSocket: A Depth Comparison You Should Know](https://apidog.com/blog/signalr-vs-websocket/)
* [Python WebSockets Example](https://pythonexamples.org/python-websockets-example/)
* [websockets - PyPI](https://pypi.org/project/websockets/)
