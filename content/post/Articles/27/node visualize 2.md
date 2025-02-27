---
title: Visualizing Node.js Scaling
description: Part 2
slug: visualizing-nodejs-scaling-2
date: 2018-07-14
image: post/Articles/IMAGES/27.jpg
categories:
  - Node.js
  - Scaling
  - Monitoring
tags:
  - Node.js
  - Scaling
  - Monitoring
  - Load
  - Balancing
  - Clustering
draft: "False"
weight: "342"
lastmod: 2025-02-27T15:04:50.221Z
---
\*Note: This is Part Two of our series.

<!-- 
If you missed the first part, where we dived into Kubernetes load balancing and monitoring, check it out [here](https://brianbraatz.github.io/p/testing-kubernetes-load-balancing-and-monitoring-orderbot-output/). -->

## Introduction

Welcome back, intrepid developer!

So, you've conquered the wilds of Kubernetes with your trusty OrderBot.

But what if you're sailing the seas of Node.js?

Fear not!

Today, we're diving into the nitty-gritty of scaling and monitoring your Node.js OrderBot.

Grab your favorite caffeinated beverage, and let's get this show on the road!

## Step 1: Unleashing the Power of Node.js Clustering

Node.js is like that one friend who insists on doing everything single-threaded.

Admirable, but sometimes you need a bit more oomph.

Enter the **cluster module**—Node.js's way of saying, "Why not use all the CPU cores?"

### Assembling the Cluster

Here's how you can rally your CPU cores to work together:

```javascript
const cluster = require('cluster');
const os = require('os');
const http = require('http');

if (cluster.isMaster) {
  const numCPUs = os.cpus().length;
  console.log(`Master ${process.pid} is running`);

  // Fork workers
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`Worker ${worker.process.pid} died. Time to spawn a new one!`);
    cluster.fork();
  });
} else {
  // Workers share the TCP connection
  http.createServer((req, res) => {
    res.writeHead(200);
    res.end('Hello from OrderBot!\n');
  }).listen(8080);

  console.log(`Worker ${process.pid} started`);
}
```

In this script:

* The **master process** plays the role of the conductor, spawning worker processes for each CPU core.
* Each **worker process** is like a musician, handling incoming requests in harmony.

With this setup, your OrderBot is ready to handle more orders than a barista on free coffee day.

## Step 2: Putting Your Cluster to the Test

What's the point of having a supercharged OrderBot if you don't test its mettle?

Let's simulate a caffeine rush with a load test.

### Crafting the Load Test

We'll use a simple Node.js script to bombard our OrderBot with requests:

```javascript
const http = require('http');

const options = {
  hostname: 'localhost',
  port: 8080,
  path: '/',
  method: 'GET',
};

for (let i = 0; i < 50; i++) {
  const req = http.request(options, (res) => {
    console.log(`STATUS: ${res.statusCode}`);
  });

  req.on('error', (e) => {
    console.error(`Problem with request: ${e.message}`);
  });

  req.end();
}
```

Run this script, and watch your OrderBot handle the onslaught like a pro.

If all goes well, your terminal should be abuzz with status messages.

## Step 3: Keeping an Eye on Your Workers

Even the best workers need supervision.

Let's set up some basic monitoring to ensure our OrderBots are in tip-top shape.

### Logging Worker Activity

Modify your worker code to include some logging:

```javascript
http.createServer((req, res) => {
  console.log(`Worker ${process.pid} is handling request`);
  res.writeHead(200);
  res.end('Order received!\n');
}).listen(8080);
```

Now, each request will log which worker handled it.

It's like giving each OrderBot a name tag.

### Monitoring with PM2

For more robust monitoring, consider using [PM2](https://pm2.keymetrics.io/), a process manager that offers:

* Automatic restarts
* Load balancing
* Detailed metrics

Install PM2 globally:

```bash
npm install pm2 -g
```

Start your app with PM2:

```bash
pm2 start your-app.js -i max
```

The `-i max` flag tells PM2 to run as many instances as there are CPU cores.

To monitor your processes, use:

```bash
pm2 monit
```

Now you have a dashboard that makes you feel like the captain of the Starship Enterprise.

## Key Takeaways

| Concept          | Description                                                                 |
| ---------------- | --------------------------------------------------------------------------- |
| **Clustering**   | Utilizing Node.js's cluster module to leverage multiple CPU cores.          |
| **Load Testing** | Simulating high traffic to ensure your application can handle the pressure. |
| **Monitoring**   | Keeping tabs on your application's health and performance.                  |
| **PM2**          | A process manager that simplifies clustering and monitoring.                |

## Reference Links

* [Node.js Cluster Documentation](https://nodejs.org/api/cluster.html)
* [PM2 Process Manager](https://pm2.keymetrics.io/)
* [Original Kubernetes Load Balancing Article](https://brianbraatz.github.io/p/testing-kubernetes-load-balancing-and-monitoring-orderbot-output/)

```
```
