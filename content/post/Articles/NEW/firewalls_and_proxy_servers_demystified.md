---
title: Firewalls and Proxy Servers Demystified
description: Firewalls and Proxy Servers Demystified
slug: firewalls-and-proxy-servers-demystified
date: 2021-04-21
image: post/Articles/IMAGES/brickwall3.jpg
categories:
  - Firewalls
  - Proxy Servers
  - Networking
  - Linux
  - Unix
tags:
  - Firewalls
  - Proxy
  - Servers
  - Networking
  - Cybersecurity
  - STUN
  - WebSockets
  - SignalR
  - Windows
  - Linux
  - REST
  - API
  - GoLang
  - Cloud
draft: false
weight: 59
categories_ref:
  - Firewalls
  - Proxy Servers
  - Networking
  - Linux
  - Unix
lastmod: 2025-03-14T15:45:11.137Z
---
# Firewalls and Proxy Servers Demystified

Welcome to the wonderful world of firewalls and proxy servers—those magical gatekeepers standing between your computer and the scary abyss of the internet.

## A Brief History of Firewalls (No, Not the Ones in Castles)

Back in the early days of the internet, people thought, “Hey, wouldn’t it be great if computers could just talk to each other freely without restrictions?” ..

Then hackers said, “Yeah, that *would* be great.”

And just like that, security nightmares were born.

Firewalls started gaining prominence in the late 1980s when network security became a serious issue.

The idea was simple: **control the flow of traffic**.

Like a bouncer at a club, a firewall checks each packet and decides whether to let it in or kick it to the curb.

## How Firewalls Work (Or How They Keep the Bad Guys Out)

Firewalls come in different flavors:

* **Packet Filtering Firewalls** – These check the source, destination, and type of packet before allowing or denying it. Think of it as a security guard checking IDs.
* **Stateful Firewalls** – More advanced, these keep track of active connections and ensure only expected traffic gets through.
* **Application Layer Firewalls** – They inspect packets for specific application behaviors. Basically, they can tell if an innocent-looking email is actually a phishing attempt.
* **Next-Gen Firewalls (NGFWs)** – These combine traditional firewall features with advanced security like intrusion detection and deep packet inspection. They’re like firewalls on steroids.

### Configuring a Firewall: Windows vs. Linux

Both Windows and Linux have built-in firewall solutions, but configuring them is a bit different.

#### Windows Firewall (The GUI Way)

1. Open **Windows Defender Firewall**
2. Click **Advanced Settings**
3. Define **Inbound and Outbound Rules**
4. Specify ports, protocols, or applications to allow/block

#### Linux Firewall (Because Real Hackers Use the CLI)

Linux users rely on `iptables` or `firewalld`. Example of allowing SSH traffic:

```sh
sudo iptables -A INPUT -p tcp --dport 22 -j ACCEPT
```

Or using `firewalld`:

```sh
sudo firewall-cmd --permanent --add-service=ssh
sudo firewall-cmd --reload
```

## REST API Calls and Firewalls

Let’s say Bob is behind a firewall and tries to make a simple REST API request to a server on the internet.

### Client Side

1. The request is sent from Bob’s browser or app.
2. The firewall checks if outgoing requests on that port (e.g., 443 for HTTPS) are allowed.
3. If allowed, the request is forwarded to the destination server.

### Server Side

1. The server receives the request.
2. If it has a firewall, it ensures incoming connections on the API port are allowed.
3. It processes the request and sends the response back to Bob.

## STUN: The Unsung Hero of NAT Traversal

STUN (Session Traversal Utilities for NAT) is a protocol that helps devices behind a firewall or NAT (Network Address Translation) figure out their **public IP address**. Without STUN, peer-to-peer communication would be nearly impossible.

[STUN on Wikipedia](https://en.wikipedia.org/wiki/STUN)

## WebSockets and SignalR: The Cool Kids of Real-Time Communication

WebSockets were invented in 2011 to provide full-duplex communication over a single TCP connection.

SignalR, introduced by Microsoft, builds on WebSockets to make real-time messaging easier.

### How Do They Work Behind Firewalls?

* WebSockets try to establish a persistent connection, but some firewalls block them.
* If blocked, SignalR can fall back to polling methods like **long polling** or **server-sent events** to maintain communication.

## Key Takeaways

| Topic                      | Summary                                                     |
| -------------------------- | ----------------------------------------------------------- |
| Firewalls                  | Control incoming/outgoing network traffic                   |
| Windows vs. Linux Firewall | Windows uses GUI, Linux uses CLI (`iptables`, `firewalld`)  |
| REST API Calls             | Firewalls can block or allow traffic based on rules         |
| STUN Protocol              | Helps devices discover their public IP when behind NAT      |
| WebSockets & SignalR       | Enable real-time communication, but firewalls can interfere |

## References

* [STUN Protocol](https://en.wikipedia.org/wiki/STUN)
* [WebSockets](https://en.wikipedia.org/wiki/WebSocket)
* [SignalR](https://en.wikipedia.org/wiki/SignalR)
* [Windows Firewall](https://docs.microsoft.com/en-us/windows/security/threat-protection/windows-firewall/)
* [Linux Firewalld](https://firewalld.org/)
* [iptables Guide](https://netfilter.org/)

There you have it—firewalls, proxies, STUN, WebSockets, and SignalR, all demystified. Now go forth and browse the internet safely! 🚀
