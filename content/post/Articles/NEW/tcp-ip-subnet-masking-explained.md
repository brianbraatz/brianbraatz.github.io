---
title: TCP/IP Subnet Masking Explained
description: 
slug: tcp-ip-subnet-masking-explained
date: 2022-08-05
image: post/Articles/IMAGES/venetianmasks.png
categories:
  - Internet standards
  - Networking
  - Protocols
  - Network Protocols
tags:
  - TCP/IP
  - Subnetting
  - Networking
  - IP
  - Addressing
  - CIDR
draft: false
weight: 45
categories_ref:
  - Internet standards
  - Networking
  - Protocols
  - Network Protocols
slug_calculated: https://brianbraatz.github.io/p/tcp-ip-subnet-masking-explained
lastmod: 2025-03-14T16:40:22.755Z
---
The Venetian carnival tradition is most famous for its distinctive masks. Photograph by [Frank Kovalchek](https://en.wikipedia.org/wiki/File:Venice_Carnival_-_Masked_Lovers_\(2010\).jpg) via Wikimedia Commons (CC BY 2.0).

[History of Venetian Carnival Masks](https://www.dailyartmagazine.com/history-of-venetian-carnival-masks/)

# TCP/IP Subnet Masking Explained

## A Brief  History of TCP/IP

Back in the ancient days of the internet (a.k.a. the 1970s), a bunch of brilliant people at DARPA were trying to connect computers across different networks.

They came up with TCP/IP, a communication protocol that made sure computers could talk to each other without getting confused.

Think of it as the internet’s universal translator.

Now, as networks grew, engineers realized that assigning a unique IP address to every device on Earth was going to be a nightmare (and IPv6 hadn’t saved us yet).

So, subnetting was introduced to break large networks into smaller, more manageable chunks. Enter **subnet masks**—the unsung heroes of the networking world.

[More about TCP/IP on Wikipedia](https://en.wikipedia.org/wiki/Internet_protocol_suite)

## What’s a Subnet Mask? (And Why Should You Care?)

A **subnet mask** is like an address filter that determines which part of an IP address belongs to the **network** and which part belongs to the **host** (a fancy way of saying "device").

It helps organize networks, improves security, and reduces network congestion.

A typical subnet mask looks something like this:\
`255.255.255.0`

But what does that actually mean? Time to break it down.

## Understanding Octets (No, Not an Alien Species)

An **IP address** is made up of **four octets** (groups of 8 bits), separated by dots. Example:

```
192.168.1.10
```

Each octet can have a value between `0` and `255` because **8 bits = 2^8 = 256** possible values.

Now, a subnet mask tells us **which part** of the address is the "network" and which part is the "host." Example:

```
IP:      192.168.1.10
Subnet:  255.255.255.0
```

* The `255` parts mean "this belongs to the network."
* The `0` part means "this belongs to the host."

In this case, the **network** is `192.168.1.0`, and all addresses from `192.168.1.1` to `192.168.1.254` belong to this network.

[Subnetting on Wikipedia](https://en.wikipedia.org/wiki/Subnetwork)

## Example: Why Does Subnetting Matter?

Let’s say you run a **cyber café** (because apparently, it’s 2005 again), and you have 100 computers. You could:

1. Get a gigantic network where every computer sees every other computer’s traffic (terrible for security).
2. Use subnetting to divide them into smaller groups, like:

   * `192.168.1.0/25` → For wired PCs
   * `192.168.1.128/25` → For Wi-Fi users

This keeps things clean and efficient.

## CIDR Notation (Because Dots Weren’t Confusing Enough)

Instead of writing subnet masks like `255.255.255.0`, we use **CIDR (Classless Inter-Domain Routing) notation**:

```
192.168.1.0/24
```

The `/24` means **24 bits** are reserved for the **network**. The remaining bits (8 bits) are for hosts.

### Common CIDR Blocks

| CIDR | Subnet Mask     | Hosts per Subnet |
| ---- | --------------- | ---------------- |
| /8   | 255.0.0.0       | 16,777,214       |
| /16  | 255.255.0.0     | 65,534           |
| /24  | 255.255.255.0   | 254              |
| /30  | 255.255.255.252 | 2                |

So, if you see something like `/30`, that means you only have **2 usable IP addresses** (great for point-to-point connections).

## Real-World Subnetting Example

Let’s say your ISP gives you a **public IP block**:\
`200.100.50.0/28`

That `/28` tells us:

* **Network:** `200.100.50.0`
* **Subnet mask:** `255.255.255.240`
* **Total addresses:** 16
* **Usable addresses:** 14 (2 are reserved for network & broadcast)

You can assign these **14 IPs** to your web servers, routers, or secret hacking lab.

## Conclusion

Subnet masking is one of those things that **seems boring** until you realize it’s what keeps the internet running smoothly. Whether you're setting up a home network or managing a corporate data center, knowing how subnet masks work will save you from network chaos.

***

## Key Ideas

| Concept         | Explanation                                                                |
| --------------- | -------------------------------------------------------------------------- |
| TCP/IP          | The core communication protocol of the internet                            |
| Subnet Mask     | A filter that determines network and host portions of an IP address        |
| Octets          | Groups of 8 bits in an IP address (e.g., 192.168.1.1)                      |
| CIDR Notation   | A shorthand way to write subnet masks (e.g., /24 instead of 255.255.255.0) |
| Why Subnetting? | Organizes networks, improves security, and reduces congestion              |

***

## Reference Links

* [Internet Protocol Suite (TCP/IP)](https://en.wikipedia.org/wiki/Internet_protocol_suite)
* [Subnetting](https://en.wikipedia.org/wiki/Subnetwork)
* [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)
