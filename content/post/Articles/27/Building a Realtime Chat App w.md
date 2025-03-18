---
title: Build a Chat App with Node.js + Socket.IO
description: 
slug: building-realtime-chat-nodejs
date: 2018-07-22
image: post/Articles/IMAGES/nodejs.png
categories:
  - Node.js
  - Socket.IO
  - WebSockets
  - Chat Application
tags:
  - Node.js
  - Socket.io
  - Websockets
  - Chat
  - Real-time Communication
draft: "False"
weight: "386"
categories_ref:
  - Node.js
  - Socket.IO
  - WebSockets
  - Chat Application
slug_calculated: https://brianbraatz.github.io/p/building-realtime-chat-nodejs
lastmod: 2025-03-14T16:40:10.714Z
---
## 🚀 Let's Build a Realtime Chat App with Node.js and Socket.IO

Ah, chat apps.

The backbone of our modern communication, second only to yelling across the house.

But what if I told you that you could build your own real-time chat app in just a few lines of code?

Enter **Node.js** and **Socket.IO**—the dynamic duo of the web-socket world!

Today, we'll build a simple chat app where users enter their names, join a chat, and send messages that update instantly for everyone online.

No refreshing, no polling, just beautiful, smooth, real-time communication.

## 🤔 What is WebSockets?

WebSockets are like those walkie-talkies you had as a kid.

Once connected, both sides can send messages without repeatedly knocking on the server’s door (*cough* AJAX polling *cough*).

It’s a persistent, full-duplex communication channel over a single TCP connection.

## 🔌 Meet Socket.IO

Socket.IO is a library that makes using WebSockets **ridiculously** easy.

It provides an abstraction over raw WebSockets and handles reconnections, fallbacks, and other nightmares you’d rather avoid.

### 🆚 WebSockets vs. SignalR

* **WebSockets**: The raw, unfiltered real-time magic.

Works well but requires you to manage a lot.

* **Socket.IO**: A layer over WebSockets that makes life easier.
* **SignalR**: Microsoft's version, great for .NET applications but a bit overkill if you just need a simple chat app.

Alright, enough talk.

Let's build!

***

## 🛠️ Setting Up Our Node.js Chat Server

First things first, make sure you have **Node.js** installed.

Then, create a project folder and install dependencies:

```sh
mkdir realtime-chat && cd realtime-chat
npm init -y
npm install express socket.io
```

Now, create a file called `server.js` and throw this in:

```js
const express = require("express");
const http = require("http");
const socketIo = require("socket.io");

const app = express();
const server = http.createServer(app);
const io = socketIo(server);

app.use(express.static("public"));

io.on("connection", (socket) => {
    console.log("A user connected");
    
    socket.on("setUsername", (username) => {
        socket.username = username;
        io.emit("userJoined", username);
    });

    socket.on("message", (msg) => {
        io.emit("message", { user: socket.username, text: msg });
    });

    socket.on("disconnect", () => {
        console.log("A user disconnected");
    });
});

server.listen(3000, () => {
    console.log("Server running on http://localhost:3000");
});
```

***

## 🎨 Frontend: The Chat UI

Now, inside the `public/` folder, create an `index.html` file and add:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Chat App</title>
    <script src="/socket.io/socket.io.js"></script>
    <script>
        const socket = io();
        let username = "";

        function setUsername() {
            username = prompt("Enter your name:");
            socket.emit("setUsername", username);
        }

        function sendMessage() {
            const msg = document.getElementById("message").value;
            socket.emit("message", msg);
            document.getElementById("message").value = "";
        }

        socket.on("userJoined", (user) => {
            const messages = document.getElementById("messages");
            messages.innerHTML += `<p><strong>${user}</strong> joined the chat</p>`;
        });

        socket.on("message", (data) => {
            const messages = document.getElementById("messages");
            messages.innerHTML += `<p><strong>${data.user}:</strong> ${data.text}</p>`;
        });

        window.onload = setUsername;
    </script>
</head>
<body>
    <h1>Realtime Chat</h1>
    <div id="messages"></div>
    <input id="message" type="text" placeholder="Type a message...">
    <button onclick="sendMessage()">Send</button>
</body>
</html>
```

***

## 🔥 Running the Chat App

Now, fire up the server:

```sh
node server.js
```

Then open <http://localhost:3000> in your browser.

Open multiple tabs to see the magic! 🎩✨

## 🎉 How It Works

1. The user enters their name, which is sent to the server.
2. When the user sends a message, it gets emitted to the server.
3. The server takes that message and broadcasts it to all connected users.
4. The frontend updates live for everyone. No refresh needed!

## 🏆 Conclusion

Boom!

You just built a real-time chat app with **Node.js** and **Socket.IO**.

Not bad, huh?

Now, go ahead and style it, add rooms, or even build your own chat platform!

If you feel extra fancy, try adding database storage, message history, or even emojis! (Because what's a chat app without emojis? 🤩🔥)

***

## 🔑 Key Ideas

| Topic            | Summary                                           |
| ---------------- | ------------------------------------------------- |
| WebSockets       | Persistent connection for real-time communication |
| Socket.IO        | Makes WebSockets easier to use in Node.js         |
| Node.js Server   | Manages chat messages and connections             |
| Frontend Chat UI | Displays chat messages in real-time               |
| Broadcasting     | Sends messages to all connected users             |

***

## 📚 References

* [Socket.IO Documentation](https://socket.io/docs/)
* [Node.js Official Site](https://nodejs.org/)
* [WebSockets MDN](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API)
