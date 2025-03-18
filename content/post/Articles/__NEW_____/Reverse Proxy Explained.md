---
title: Reverse Proxies for  Performance and Scalability in Web Apps
description: Exploring How Reverse Proxies work
slug: reverse-proxies-perf
date: 2019-05-18
image: post/Articles/IMAGES/phoneboothtoomany.png
categories:
  - Reverse Proxy
  - Security
  - Web Applications
  - Performance
  - Scalability
  - Load Balancing
  - Nginx
  - Apache
  - Docker
  - Kubernetes
  - Forward Proxy
tags:
  - Reverse
  - Proxy
  - Security
  - Web
  - Applications
  - Performance
  - Scalability
  - Load
  - Balancing
  - Nginx
  - Apache
  - Docker
  - Kubernetes
  - Forward
  - Proxy
draft: false
weight: 44
categories_ref:
  - Reverse Proxy
  - Security
  - Web Applications
  - Performance
  - Scalability
  - Load Balancing
  - Nginx
  - Apache
  - Docker
  - Kubernetes
  - Forward Proxy
slug_calculated: https://brianbraatz.github.io/p/reverse-proxies-perf
lastmod: 2025-03-14T16:40:33.589Z
---
Phonebooth Stuffing:

[Phonebooth Stuffing - Wikipedia](https://en.wikipedia.org/wiki/Phonebooth_stuffing#:~:text=Phonebooth%20stuffing%20is%20a%20sporadic,are%20no%20more%20individuals%20available)

For a code example of visualizing Reverse Proxies see these articles:

[Reverse Proxies Visually Explained](/post/Articles/__NEW_____/Reverse%20Proxies%20Visually%20Explained.md)\
[Reverse Proxies Scaling](/post/Articles/__NEW_____/Reverse%20Proxies%20Scaling.md)

# Understanding How Reverse Proxies Improve Performance and Scalability in Web Applications

<!--
## Introduction

Ah, reverse proxies. The unsung heroes of the web. They're like bouncers at a club, making sure traffic flows smoothly, nobody gets in without permission, and VIPs get faster service. But what exactly *is* a reverse proxy, and why do we love it so much?

In this article, we’ll break down:

- What a reverse proxy is and why it’s better than your grandma’s DNS round robin.
- A (very brief and entertaining) history of proxy servers.
- The differences between forward and reverse proxies.
- How to set up a reverse proxy on both Windows and Linux.
- How reverse proxies play well with Docker and Kubernetes.
- A bunch of code examples to get you started.

Buckle up, it's proxy time! 🚀

---
-->

## A Quick History of Proxy Servers

Proxy servers been around since the dawn of networked computers, acting as middlemen between clients and servers.

If you want the nitty-gritty details, check out [Wikipedia’s proxy server history](https://en.wikipedia.org/wiki/Proxy_server). But here’s the gist:

* The earliest proxies were just gateways for passing requests between networks.
* Forward proxies became popular to help clients access restricted content (hello, workplace YouTube restrictions).
* Reverse proxies emerged to help websites handle massive amounts of traffic efficiently.

### When Did Reverse Proxies Start Being Used?

Reverse proxies became a thing when websites started melting down under heavy traffic.

Some genius realized that instead of making one server handle everything, why not spread the load across multiple servers?

**!!!BOOM!!!**

Reverse proxies.

Now, before reverse proxies, people tried **Round Robin DNS** to distribute traffic.

This basically rotates requests among multiple IPs assigned to a domain. Sounds cool, right? Well, kinda. It has issues, like:

| Feature           | Reverse Proxy | Round Robin DNS |
| ----------------- | ------------- | --------------- |
| Load Balancing    | ✅ Yes         | 🚧 Limited      |
| Failover Support  | ✅ Yes         | ❌ No            |
| Caching           | ✅ Yes         | ❌ No            |
| Security Features | ✅ Yes         | ❌ No            |
| SSL Termination   | ✅ Yes         | ❌ No            |

Reverse proxies win. DNS round robin is like a spinning wheel of fortune—you never know if the server you land on is dead or alive. 😅

***

## Forward vs. Reverse Proxies

There are two main types of proxies:

1. **Forward Proxies** – These are used by clients to access the internet. Think of it as an internet middleman for your browser.
2. **Reverse Proxies** – These sit in front of web servers to handle incoming requests efficiently.

Reverse proxies do the heavy lifting for servers, handling:

* Load balancing (spreading traffic across multiple servers)
* Caching frequently requested content
* SSL termination (handling HTTPS so your servers don’t have to)
* Security filtering (blocking malicious traffic)

***

## Setting Up a Reverse Proxy

Now let’s get our hands dirty. We'll set up a reverse proxy using Nginx on both **Windows** and **Linux**.

### Setting Up Nginx Reverse Proxy on Windows

1. Download Nginx from the [official site](https://nginx.org/en/download.html).
2. Extract it somewhere (like `C:\\nginx`).
3. Open `conf/nginx.conf` and add:

```nginx
server {
    listen 80;
    server_name mysite.com;

    location / {
        proxy_pass http://localhost:3000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

4. Run `nginx.exe` to start the server.

### Setting Up Nginx Reverse Proxy on Linux

```sh
sudo apt update && sudo apt install nginx -y

sudo nano /etc/nginx/sites-available/reverse_proxy
```

Paste the following:

```nginx
server {
    listen 80;
    server_name mysite.com;

    location / {
        proxy_pass http://localhost:3000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

Then:

```sh
sudo ln -s /etc/nginx/sites-available/reverse_proxy /etc/nginx/sites-enabled/
sudo systemctl restart nginx
```

Boom. You’re reverse proxying! 🚀

***

## Reverse Proxies with Docker and Kubernetes

### Using Nginx Reverse Proxy with Docker

Docker + Reverse Proxies = 💖. Here’s a simple `docker-compose.yml`:

```yaml
version: '3'
services:
  app:
    image: myapp
    ports:
      - "3000:3000"

  nginx:
    image: nginx
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
    ports:
      - "80:80"
```

### Using Nginx Reverse Proxy in Kubernetes

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: my-ingress
spec:
  rules:
    - host: mysite.com
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: my-service
                port:
                  number: 3000
```

Apply it:

```sh
kubectl apply -f ingress.yaml
```

Boom. K8s handles the rest. 🎉

***

## Key Ideas Table

| Concept             | Explanation                                                                                   |
| ------------------- | --------------------------------------------------------------------------------------------- |
| Reverse Proxy       | A server that sits in front of web servers to improve performance, security, and scalability. |
| Load Balancing      | Distributes traffic across multiple servers to prevent overload.                              |
| Caching             | Stores frequently accessed resources to improve response times.                               |
| SSL Termination     | Handles HTTPS encryption so backend servers don’t have to.                                    |
| Docker & Kubernetes | Reverse proxies integrate well for managing containers and microservices.                     |

***

## References

* [Proxy Server - Wikipedia](https://en.wikipedia.org/wiki/Proxy_server)
* [Reverse Proxy - Wikipedia](https://en.wikipedia.org/wiki/Reverse_proxy)
* [Nginx Docs](https://nginx.org/en/docs/)
