---
title: "Ethernet Unplugged: A Journey Through the Wires"
description: High level explain of Ethernet at the Hardware layer
slug: ethernet-unplugged-journey-through-wires
date: 2023-07-15
image: post/Articles/IMAGES/ethernet.jpg
categories:
  - History
  - Algorithms
  - Networking
  - Network Protocols
tags:
  - Ethernet
  - Networking
  - CSMA/CD
  - Hubs
  - Switches
  - ARCNET
  - HardwareProtocols
  - Devices
  - EmbeddedSystems
  - Cloud
draft: false
weight: 342
lastmod: 2025-02-14T16:23:15.746Z
---
# Ethernet Unplugged: A Journey Through the Wires

## Introduction

Back in the day, while my friends were geeking out over comic books and video games, I was diving deep into the writings of Douglas Comer.

His books, like [*Internetworking with TCP/IP*](https://www.cs.purdue.edu/homes/comer/netbooks.html) and [*The Internet Book*](https://www.porchlightbooks.com/product/internet-book-everything-you-need-to-know-about-computer-networking-and-how-the-internet-works--douglas-e-comer), were my bedtime stories.

They unraveled the mysteries of computer networks and fueled my passion for understanding the nuts and bolts of how our devices communicate.

## The Basics: Ethernet and the Wire

At its core, Ethernet is like a sophisticated party line. Imagine a bunch of people (devices) connected by a single wire. When one wants to speak (transmit data), they check if anyone else is talking.

If the coast is clear, they start chatting.

But what happens if two people start talking at the same time?

You get a collision—a jumble of words where no one is understood.

## Collision Course: CSMA/CD

To handle these inevitable verbal pile-ups, Ethernet employs a method called **Carrier Sense Multiple Access with Collision Detection (CSMA/CD)**. Here's a breakdown:

1. **Carrier Sense**: Before speaking, a device listens to ensure no one else is talking.
2. **Multiple Access**: Multiple devices share the same communication medium.
3. **Collision Detection**: If two devices talk simultaneously, they detect the collision.

When a collision occurs, both devices stop, wait for a random period (to avoid another simultaneous start), and then attempt to retransmit.

This method ensures that the network remains orderly, even when multiple devices are vying for attention.

## Hubs vs. Switches: The Network Traffic Cops

In the early days, networks used **hubs**. A hub is like a megaphone: when one device sends a message, the hub broadcasts it to all connected devices, regardless of the intended recipient.

This often led to unnecessary traffic and, you guessed it, more collisions.

Enter the **switch**—the smarter, more discerning cousin of the hub. A switch keeps track of the devices connected to it and directs incoming data only to the intended recipient.

This targeted approach reduces collisions and improves network efficiency.

But wait, there's more! **Managed switches** take it up a notch.

They offer advanced features like traffic monitoring, VLAN configuration, and enhanced security settings, giving network administrators greater control over data flow.

**Comparison Table: Hub vs. Switch vs. Managed Switch**

| Feature            | Hub                       | Switch                         | Managed Switch                 |
| ------------------ | ------------------------- | ------------------------------ | ------------------------------ |
| Data Transmission  | Broadcasts to all devices | Sends data to specific devices | Sends data to specific devices |
| Collision Handling | Higher collision rates    | Reduced collisions             | Reduced collisions             |
| Control Features   | None                      | Basic                          | Advanced (e.g., VLANs, QoS)    |
| Cost               | Low                       | Moderate                       | Higher                         |

## A Blast from the Past: ARCNET

Before Ethernet became the networking superstar, there was **ARCNET** (Attached Resource Computer NETwork). Developed in 1976 by Datapoint Corporation, ARCNET was the first widely available networking system and was used extensively in the 1980s for office automation. Unlike Ethernet's collision-based approach, ARCNET used a token-passing protocol, where a token (a small data packet) is passed around the network. Only the device holding the token can transmit data, effectively eliminating collisions.

**Comparison Table: Ethernet vs. ARCNET**

| Feature       | Ethernet                              | ARCNET                                           |
| ------------- | ------------------------------------- | ------------------------------------------------ |
| Access Method | CSMA/CD (Collision-based)             | Token-passing (Collision-free)                   |
| Data Rate     | Initially 10 Mbps; now up to 100 Gbps | Initially 2.5 Mbps; later versions up to 20 Mbps |
| Topology      | Bus or Star                           | Star                                             |
| Popularity    | Dominant LAN technology               | Largely obsolete                                 |

<!-- 
## Conclusion

Ethernet has come a long way from its humble beginnings, evolving into the backbone of modern networking. While technologies like ARCNET had their moment in the spotlight, Ethernet's adaptability and robustness have ensured its lasting prominence. And as we continue to build and expand our networks, the foundational principles laid out by pioneers like Douglas Comer remain as relevant as ever.
-->

## Key Ideas

| Concept            | Explanation                                                                                  |
| ------------------ | -------------------------------------------------------------------------------------------- |
| **CSMA/CD**        | A method to manage data collisions in Ethernet networks.                                     |
| **Hub vs. Switch** | Hubs broadcast data to all devices, while switches send data only to the intended recipient. |
| **Managed Switch** | Offers advanced features like traffic monitoring and VLAN configuration.                     |
| **ARCNET**         | An early networking technology using token-passing to avoid collisions.                      |

## References

* [Douglas Comer's Networking Books](https://www.cs.purdue.edu/homes/comer/netbooks.html)
* [The Internet Book by Douglas E. Comer](https://www.porchlightbooks.com/product/internet-book-everything-you-need-to-know-about-computer-networking-and-how-the-internet-works--douglas-e-comer)
* [Carrier Sense Multiple Access with Collision Detection (CSMA/CD)](https://www.youtube.com/watch?v=XrimgDtk34s)
* [ARCNET Overview](https://www.arcnet.cc/abtarc.htm)
