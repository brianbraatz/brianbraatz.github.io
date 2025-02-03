---
title: Docker Firewall Demystified-Ingress Egress
description: Docker Firewall Demystified-Ingress Egress
slug: docker-firewall-demystified-ingress-egress
date: 2026-12-21
image: post/Articles/IMAGES/brickwall2.jpg
categories: 
tags:
  - Docker
  - Firewall
  - Networking
  - Containers
  - Ingress
  - Egress
  - Reverse
  - Firewall
  - Websockets
  - SignalR
  - Virtual
  - Networking
  - Kubernetes
draft: false
weight: 209
lastmod: 2025-02-03T13:23:47.566Z
---
# Docker Firewall Demystified: Ingress & Egress

If you've ever wondered how Docker's firewall works and why it seems like some mystical force blocking (or allowing) your containers' network traffic, you’re in the right place. We're going to break it down, have some laughs, and maybe—just maybe—prevent you from rage-quitting your Docker setup.

## A (Very) Brief History of Docker and Why It Has a Firewall

Docker started in 2013 as a way to package and ship applications in containers. Fast-forward, and now it's the de facto standard for deploying everything from tiny hobby projects to enterprise-scale applications. But with great power comes great... security concerns!

By default, Docker sets up some network rules to prevent your container from going rogue and hacking the Pentagon (or just accidentally exposing your database to the world). The firewall is implemented using **iptables**, Linux’s built-in firewall tool.

## How Docker's Firewall Works

Docker's firewall is all about controlling **Ingress (incoming)** and **Egress (outgoing)** traffic.

* **Ingress**: Controls traffic coming **into** the container. If your app listens on port 8080, but Docker doesn’t expose it, nobody gets in. (Except maybe Chuck Norris.)
* **Egress**: Controls traffic going **out** of the container. By default, Docker allows outbound connections because, let's be honest, containers need the internet.

## Configuring the Docker Firewall (With Examples!)

### 1. Exposing a Port on a Running Container

Run a container and expose port 8080:

```sh
docker run -d -p 8080:80 nginx
```

This maps **port 8080 on the host** to **port 80 inside the container**.

### 2. Blocking Egress Traffic (Preventing Containers from Calling Home)

You can stop a container from accessing the internet with:

```sh
iptables -I DOCKER-USER -d 0.0.0.0/0 -j DROP
```

### 3. Allowing Only Specific IPs to Access a Container

If you only want **192.168.1.100** to access your container:

```sh
iptables -A INPUT -p tcp --dport 8080 -s 192.168.1.100 -j ACCEPT
iptables -A INPUT -p tcp --dport 8080 -j DROP
```

## How Firewall Relates to Pods & Containers

If you’re using Kubernetes, **pods** are groups of containers that share the same network namespace. This means the firewall rules apply at the pod level, not the individual container level.

## Virtual Networking & Docker

Docker creates a **virtual network interface (veth pair)** for each container. These interfaces are connected to a bridge network by default. This setup allows containers to talk to each other while keeping their traffic isolated from the host.

## External Firewalls & Reverse Firewalls

### What is a Reverse Firewall?

A **reverse firewall** blocks **incoming** traffic unless explicitly allowed. It's like a bouncer that lets in VIPs but keeps out random strangers.

### Handling External Firewalls with Docker

If your Docker container is behind an external firewall, you may need to allow traffic through:

```sh
sudo ufw allow 8080/tcp
```

Or if you’re using AWS Security Groups:

```sh
aws ec2 authorize-security-group-ingress --group-id sg-12345 --protocol tcp --port 8080 --cidr 0.0.0.0/0
```

## WebSockets, SignalR, and Docker Firewall Considerations

If you're using **WebSockets** or **SignalR**, ensure that:

1. **Persistent connections** are allowed through your firewall.
2. **Keep-alive packets** are not getting blocked.

For Nginx reverse proxy:

```nginx
location /ws/ {
    proxy_pass http://your-backend;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "Upgrade";
}
```

## Dockerfiles for Different Configurations

### Example 1: Dockerfile with Open Ports

```Dockerfile
FROM nginx
EXPOSE 8080
```

### Example 2: Dockerfile Restricting Outbound Connections

```Dockerfile
FROM alpine
RUN iptables -A OUTPUT -p tcp --dport 80 -j DROP
```

## Summary Table

| Topic                         | Key Takeaways                                                          |
| ----------------------------- | ---------------------------------------------------------------------- |
| Docker Firewall Basics        | Uses `iptables` to control ingress/egress traffic                      |
| Exposing Ports                | Use `-p host_port:container_port`                                      |
| Blocking Outbound Connections | Modify `iptables` rules                                                |
| Pods & Containers             | Pods share network namespace, so firewall rules apply at the pod level |
| Virtual Networking            | Docker uses virtual interfaces & bridge networks                       |
| Reverse Firewalls             | Block all incoming traffic by default                                  |
| WebSockets & SignalR          | Requires persistent connections & keep-alive handling                  |

## References

* Docker Networking Overview: <https://docs.docker.com/network/>
* iptables Documentation: <https://linux.die.net/man/8/iptables>
* Kubernetes Networking: <https://kubernetes.io/docs/concepts/cluster-administration/networking/>
* AWS Security Groups: <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/security-group-rules-reference.html>
* WebSockets: <https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API>
