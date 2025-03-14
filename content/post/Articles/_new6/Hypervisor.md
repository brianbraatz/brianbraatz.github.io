---
title: What's a Hypervisor? And Why Does My Docker Need One???
description: Hypervisors Explained..
slug: what's-a-hypervisor-and-why-does-my-docker-need-one
date: 2019-10-31
image: post/Articles/IMAGES/hypervlogo.png
categories:
  - Docker
  - Cloud
  - Virtual Machines
  - Containers
tags:
  - Hypervisor
  - Docker
  - Virtualization
  - Containers
  - Kvm
  - Vmware
  - Virtual
  - Machines
  - Cloud
  - Computing
  - Linux
  - Windows
draft: false
weight: 151
categories_ref:
  - Docker
  - Cloud
  - Virtual Machines
  - Containers
lastmod: 2025-03-14T15:45:22.297Z
---
# What's a Hypervisor? And Why Does My Docker Need One???  

## Introduction  

So, you’re running **Docker** on Windows or Mac, and suddenly someone tells you:  

*"You need a hypervisor to run Docker."*  

And you're like... **what even is a hypervisor?!**  

  <!-- 

In this article, we’ll break it down:  

  

- **What a hypervisor is** and why it exists.  

- **The history of virtualization and hypervisors**.  

- **Why Docker on Windows/Mac requires a hypervisor**.  

- **How hypervisors compare to other virtualization technologies**.  

- **Examples and practical usage of hypervisors**.  

  

Let’s jump in! 🚀  
-->

\---  

## What Even is a Hypervisor?  

A **hypervisor** is a **fancy piece of software** that lets you **run virtual machines (VMs)** on your computer.  

It **creates, manages, and runs virtual machines** while keeping them separate from your main system.  

Think of it like:  

🔹 A **zookeeper** managing multiple animals (**virtual machines**).  

🔹 A **ringmaster** controlling multiple performers (**operating systems**).  

🔹 A **hallway of isolated rooms** where each VM lives its own life.  

Without a hypervisor, you’d have **no way** to create **virtual computers** inside your real one.  

\---  

## A Brief History of Hypervisors  

| Year  | Development              | Notes                                      |
| ----- | ------------------------ | ------------------------------------------ |
| 1960s | IBM Mainframes           | Early VM concepts for multi-user computing |
| 1990s | VMware launches ESXi     | Virtualization takes off for enterprises   |
| 2000s | VirtualBox, Hyper-V, KVM | More consumer-friendly hypervisors         |
| 2010s | Containers emerge        | Docker disrupts the VM world               |
| Today | Cloud-based Hypervisors  | AWS, Azure, and Google Cloud dominate      |

Hypervisors were **originally created for mainframes** but became **mainstream** in the **late 1990s and early 2000s** with **VMware, Hyper-V, and KVM**.  

\---  

## Why Does Docker Need a Hypervisor?  

### **Docker on Linux → No Hypervisor Needed**  

If you run **Docker on Linux**, everything **just works**. Why? Because **containers** natively use **Linux kernel features** like **namespaces and cgroups**.  

### **Docker on Windows/Mac → Needs a Hypervisor**  

But on **Windows and macOS**, Docker **can't** talk to the Linux kernel directly. Instead, it runs a **small virtual machine** using a **hypervisor** like:  

* **Hyper-V (Windows)**  

* **VirtualBox (Older Windows/Mac versions)**  

* **Apple’s Hypervisor Framework (macOS)**  

This is why **Docker Desktop** **automatically installs a hypervisor** to make everything work.  

💡 If you're on **Windows or Mac**, Docker runs inside a **tiny Linux VM**, and that VM needs a **hypervisor** to exist.  

\---  

## Hypervisors vs. Other Virtualization Technologies  

| Feature                  | Type 1 Hypervisor         | Type 2 Hypervisor              | Containers                |
| ------------------------ | ------------------------- | ------------------------------ | ------------------------- |
| **Runs on Bare Metal?**  | ✅ Yes                     | ❌ No                           | ❌ No                      |
| **Performance**          | ✅ Fastest                 | ❌ Slower                       | ✅ Super Fast              |
| **Resource Usage**       | ✅ Optimized               | ❌ More Overhead                | ✅ Very Low                |
| **Example Technologies** | VMware ESXi, Hyper-V, KVM | VirtualBox, VMware Workstation | Docker, LXC               |
| **Best For**             | Datacenters, Cloud        | Personal Use, Testing          | Microservices, Cloud Apps |

💡 **Verdict:** Hypervisors **run entire OS instances**, while **containers share the host OS**, making them **lighter and faster**.  

\---  

## Key Takeaways  

* **Hypervisors allow you to run virtual machines** inside your computer.  

* **Docker needs a hypervisor on Windows/macOS** because it requires a **Linux kernel**.  

* **Type 1 hypervisors (KVM, Hyper-V, ESXi) are faster** than Type 2 (VirtualBox, VMware Workstation).  

* **Containers like Docker are NOT hypervisors**, but they rely on them in non-Linux environments.  

\---  

## References  

1. [Hypervisor Wikipedia](https://en.wikipedia.org/wiki/Hypervisor)  

2. [Microsoft Hyper-V Documentation](https://learn.microsoft.com/en-us/virtualization/hyper-v-on-windows/)  

3. [Docker Desktop and Hypervisors](https://docs.docker.com/desktop/)  

4. [KVM Virtualization](https://www.linux-kvm.org/)  

5. [VMware vs. VirtualBox](https://www.vmware.com/products/workstation.html)
