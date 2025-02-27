---
title: Node.js 10-Worker Thread Messages
description: 
slug: worker-threads-how-they-c
date: 2019-05-17
image: post/Articles/IMAGES/35.jpg
categories:
  - Node.js
  - Worker Threads
  - Concurrency
tags:
  - Node.js
  - Worker Threads
  - Multithreading
  - Message Passing
  - Concurrency
  - Inter-Thread Communication
draft: "False"
weight: "428"
lastmod: 2025-02-27T15:08:21.107Z
---
<!-- ## Worker Threads: How They Communicate with Messages

So, you’ve dipped your toes into **Worker Threads** in Node.js and figured out how to create them.

But now you're wondering: *How do they actually talk to each other?* Do they pass notes in class?

Send emails?

Maybe even use carrier pigeons?

Nope!

They use **message passing**—and today, we’re going to break it down.

--- -->

### Message Passing: The Basics

Since **Worker Threads** don’t share the same execution context, they need to communicate using **messages**.

This is done via the `postMessage()` and `message` event listener.

Each worker has a **message port** that allows it to send and receive data from the main thread.

Think of it like a walkie-talkie—except instead of shouting, we send structured JSON objects.

***

### Sending and Receiving Messages

Let’s start with a simple example of message passing between a main thread and a worker.

#### **Main Thread (index.js)**

```javascript
const { Worker } = require('worker_threads');

const worker = new Worker('./worker.js');

worker.on('message', (message) => {
    console.log(`Main thread received: ${message}`);
});

worker.postMessage('Hello, Worker!');
```

#### **Worker Thread (worker.js)**

```javascript
const { parentPort } = require('worker_threads');

parentPort.on('message', (message) => {
    console.log(`Worker received: ${message}`);
    parentPort.postMessage('Hello, Main Thread!');
});
```

#### **Output**

```
Worker received: Hello, Worker!

Main thread received: Hello, Main Thread!

```

See?

They’re getting along just fine.

✨

***

### Sending Complex Data Structures

Worker Threads support structured cloning, meaning you can send objects, arrays, and even **TypedArrays**!

#### **Main Thread Example**

```javascript
worker.postMessage({ task: 'processData', payload: [1, 2, 3, 4, 5] });
```

#### **Worker Thread Example**

```javascript
parentPort.on('message', (message) => {
    console.log(`Worker processing:`, message.payload);
    parentPort.postMessage({ result: message.payload.map(num => num * 2) });
});
```

This allows for more complex and meaningful interactions between threads.

***

### Sharing Memory Between Threads

Sometimes, sending data via messages can be slow for large datasets.

Instead, you can use **SharedArrayBuffer** to allow direct memory access across threads.

#### **Main Thread Example**

```javascript
const sharedBuffer = new SharedArrayBuffer(1024);
const intArray = new Int32Array(sharedBuffer);
worker.postMessage({ buffer: sharedBuffer });
```

#### **Worker Thread Example**

```javascript
parentPort.on('message', ({ buffer }) => {
    const intArray = new Int32Array(buffer);
    intArray[0] = 42;
    parentPort.postMessage('Memory updated!');
});
```

With **SharedArrayBuffer**, threads can modify the same memory region without the overhead of message passing.

***

### Handling Multiple Workers

What if you have **multiple workers**?

You can assign different tasks to each worker and handle their responses separately.

#### **Main Thread Example**

```javascript
const worker1 = new Worker('./worker.js');
const worker2 = new Worker('./worker.js');

worker1.postMessage('Task for Worker 1');
worker2.postMessage('Task for Worker 2');

worker1.on('message', msg => console.log(`Worker 1: ${msg}`));
worker2.on('message', msg => console.log(`Worker 2: ${msg}`));
```

This setup lets you efficiently distribute workloads across multiple threads.

***

### When to Use Message Passing vs.

Shared Memory

| Use Case                   | Best Approach                 |
| -------------------------- | ----------------------------- |
| Small data or JSON objects | Message passing (postMessage) |
| Large datasets             | SharedArrayBuffer             |
| Multiple independent tasks | Separate workers              |
| High-speed memory access   | Shared memory                 |

***

## Conclusion

* **Worker Threads communicate via message passing** using `postMessage()` and `message` event listeners.

* They **support structured data** like objects, arrays, and buffers.

* For **faster data sharing**, use `SharedArrayBuffer` to avoid serialization overhead.

* **Multiple workers** can process tasks in parallel, improving performance for CPU-intensive workloads.

<!-- 
With these tools in your belt, you can make the most of **multithreading in Node.js** and build high-performance applications.

Happy coding!

🚀 -->

***

## References

* [Node.js Worker Threads](https://nodejs.org/api/worker_threads.html)
* [MDN Web Docs: Web Workers](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API)
* [SharedArrayBuffer Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer)
