---
title: Node.js and IoT-  JavaScript and Blinking LEDs
description: Building the Future with JavaScript and Blinking LEDs
slug: nodejs-iot-building
date: 2017-06-14
image: post/Articles/IMAGES/nodejs.png
categories:
  - Node.js
  - IoT
  - JavaScript
  - Hardware
  - Embedded
  - Embedded Systems
tags:
  - Node.js
  - IoT
  - JavaScript
  - Hardware
  - Sensors
  - Microcontrollers
  - Networking
draft: "False"
weight: "482"
categories_ref:
  - Node.js
  - IoT
  - JavaScript
  - Hardware
  - Embedded
  - Embedded Systems
lastmod: 2025-03-14T15:45:04.318Z
---
Alright, so you’ve heard of **Node.js**—the JavaScript runtime that refuses to quit.

But did you know that you can use it to control **real-life hardware**?

Yep, not just websites and APIs.

We’re talking LEDs, sensors, and even entire smart home setups.

This is the magic of the **Internet of Things (IoT)**—where devices talk to each other and make us feel like we're living in a sci-fi movie.

And the best part?

You don’t need to be an embedded systems engineer to get started.

If you know JavaScript, you can hack your way into IoT with Node.js!

***

## 🚀 Why Node.js for IoT?

Why use **JavaScript** for something as serious as **hardware**?

Because:

* **It’s easy** – If you know JavaScript, you’re halfway there.
* **Event-driven magic** – Node.js is built for handling real-time events, perfect for IoT.
* **Huge ecosystem** – Tons of libraries help you connect sensors, motors, and even your coffee machine to the internet.
* **Runs on tiny devices** – You can install Node.js on things like the Raspberry Pi.

***

## 🛠️ Getting Started: What You Need

Before we get our hands dirty with code, let’s make sure we have the right stuff:

### Hardware

* A **Raspberry Pi** or **Arduino** (We’ll go with Raspberry Pi since it runs Node.js natively)
* A **LED** (because no IoT tutorial is complete without blinking something)
* A **Resistor** (so we don’t fry our poor LED)
* Some **wires** (yes, you need these)
* A breadboard (optional, but makes life easier)

### Software

* **Node.js** (obviously)
* **Johnny-Five** (a cool library for working with hardware in JavaScript)
* **onoff** (for controlling GPIO pins on Raspberry Pi)

***

## 🔥 Hello World in IoT: Blinking an LED

If this is your first IoT project, we’re going old school—**blinking an LED**.

Because nothing says "I built this" like making a tiny light turn on and off.

### Step 1: Set Up Node.js on Raspberry Pi

If you haven’t installed Node.js on your Raspberry Pi, do it like this:

```sh
curl -sL https://deb.nodesource.com/setup_16.x | sudo bash -
sudo apt install -y nodejs
```

### Step 2: Install Dependencies

We’ll use the `onoff` library to control the GPIO pins.

```sh
npm install onoff
```

### Step 3: Wire Up the LED

Connect:

* **LED’s long leg (anode) to GPIO pin 17**
* **Short leg (cathode) to GND**
* **A 330Ω resistor between GPIO 17 and the LED** to avoid magic smoke.

### Step 4: Write the Code

Create a file called `blink.js`:

```js
const Gpio = require('onoff').Gpio;
const led = new Gpio(17, 'out');

setInterval(() => {
    led.writeSync(led.readSync() ^ 1); // Toggle LED state
    console.log("LED toggled!");
}, 1000);

process.on('SIGINT', () => {
    led.unexport(); // Clean up
    console.log("\nBye bye!");
    process.exit();
});
```

### Step 5: Run It

```sh
node blink.js
```

Boom! 🎉 Your LED should now be blinking like it’s in a 90s disco.

***

## 🌎 IoT + Node.js: Connecting to the Internet

Okay, blinking an LED is cool, but what if you want to **control it over the internet**?

Let’s make a simple **web-controlled LED** using Node.js and Express.

### Install Express

```sh
npm install express
```

### Write the Server Code

Create `server.js`:

```js
const express = require('express');
const Gpio = require('onoff').Gpio;
const app = express();
const led = new Gpio(17, 'out');

app.get('/on', (req, res) => {
    led.writeSync(1);
    res.send('LED is ON');
});

app.get('/off', (req, res) => {
    led.writeSync(0);
    res.send('LED is OFF');
});

app.listen(3000, () => console.log('Server running on http://localhost:3000'));
```

### Run It:

```sh
node server.js
```

Now open your browser and go to:

* `http://localhost:3000/on` → LED turns ON
* `http://localhost:3000/off` → LED turns OFF

Welcome to the **future**, where even your lightbulbs have an IP address. 🔥

***

## 🏡 Taking It Further: IoT Ideas

Now that you’ve got the basics, here are some crazy IoT ideas you can build:

* **Smart Doorbell** – Detect when someone is at the door and send a notification.
* **Temperature Monitor** – Use a sensor to measure temperature and send alerts.
* **Automated Plant Watering** – Water your plants when the soil gets dry.
* **IoT Coffee Maker** – Brew coffee when you wake up (because waking up is hard).
* **Cat Feeder** – Feed your cat with the press of a button (or let the cat do it).

***

## 🚀 Wrapping Up

Node.js + IoT is a **powerful** combination that lets you control the world with JavaScript.

Whether it’s turning on an LED or building a full-fledged **smart home**, the possibilities are endless.

Now go forth, connect everything, and don’t forget to blink some LEDs along the way! ✨

***

## 📌 Key Ideas

| Topic              | Summary                                          |
| ------------------ | ------------------------------------------------ |
| Node.js for IoT    | Node.js is great for real-time IoT applications. |
| Hardware Needed    | Raspberry Pi, LED, wires, and some patience.     |
| Blinking LED       | Use the `onoff` library to control GPIO pins.    |
| Web-Controlled LED | Use Express to control an LED over the web.      |
| Future Projects    | Smart doorbells, IoT coffee makers, and more.    |

***

## 🔗 References

* [Node.js Official Site](https://nodejs.org/)
* [Johnny-Five Library](http://johnny-five.io/)
* [onoff Library](https://www.npmjs.com/package/onoff)
* [Raspberry Pi GPIO Guide](https://www.raspberrypi.org/documentation/usage/gpio/)
* [Express.js Guide](https://expressjs.com/)
