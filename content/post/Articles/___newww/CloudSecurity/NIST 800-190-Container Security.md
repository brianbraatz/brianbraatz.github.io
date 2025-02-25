---
title: "NIST 800-190: Keeping Your Containers from Becoming Security Dumpster Fires"
description: Summary of NIST 800-190 which descirbes how to secure containers
slug: nist-800-190-keeping-your-containers-from-becoming-security-dumpster-fires
date: 2024-04-23
image: post/Articles/IMAGES/Nist-800-190.png
categories:
  - Nist 800-190
  - Container security
  - Docker
  - Kubernetes
  - Cybersecurity
  - Best practices
  - Vulnerability scanning
tags:
  - Nist 800-190
  - Container security
  - Docker
  - Kubernetes
  - Cybersecurity
  - Best practices
  - Vulnerability scanning
draft: false
weight: 306
lastmod: 2025-02-25T12:26:21.873Z
---
# NIST 800-190: Keeping Your Containers from Becoming Security Dumpster Fires

## 🚀 Introduction

If you're running **Docker**, **Kubernetes**, or any other containerized magic, congratulations! You're riding the cloud-native wave. But before you get too comfy, let’s talk about **security**—because nobody wants their containers turning into **hacked piñatas** full of exposed secrets.

That's where **[NIST 800-190](https://csrc.nist.gov/publications/detail/sp/800-190/final)** comes in. It’s basically a survival guide for keeping your containers from becoming **security dumpster fires**.

<!--
**I have embedded the document for you here**
**NOTE:** 
The .Gov link seems to frequntly NOT be up frmo time to time- so I am hosting my latest copy on this site.

Here is an embedded viewer of my latest copy of this document .

NIST.SP.800-190.pdf

**Also** a bug on this site I have not had a chance to track down, is the pdf embeds dont always seem to work on mobile.
If you are on mobile, and the viewer doesnt work,- click my Full Page link below and it will direct link to the PDF, which usualy displays nicely in most browsers.



<embed src="NIST.SP.800-190.pdf" type="application/pdf" width="100%" height="600px">`
<div style="text-align: center;"> 
<a href="NIST.SP.800-190.pdf" style="text-align:center; text-decoration: underline">VIEW FULLPAGE-Download</a><br>

</div>

-->

***

## 🛑 Why Should You Care?

Containers are **awesome**, but they come with their own set of **security nightmares**. If you’re not careful, your entire system could be:

* **Compromised by a rogue container** running malicious code.
* **Overrun by vulnerabilities** hiding inside your container images.
* **Destroyed by an attacker** who found a way to escape the container and mess with your host system.

So, yeah, **you should care.** Let’s dive into the key security measures NIST 800-190 suggests.

***

## 🛠️ 1. Image Scanning: Stop Feeding Your Containers Junk Code

Before you run a container, **scan that thing** like it’s airport security on high alert. **[Image scanning](https://en.wikipedia.org/wiki/Container_image)** helps detect vulnerabilities **before** they can wreak havoc.

### **Best Practices:**

✔️ Use **trusted base images** (don’t just pull from random registries).\
✔️ Automate **container vulnerability scanning** (CI/CD integration is your friend).\
✔️ **Regularly update** images, because stale containers are full of old security holes.

If you **don’t** scan your images, you might as well hand over your root access to hackers. 🚨

***

## 🔒 2. Runtime Security: Just Because It Started Safe Doesn’t Mean It Stays Safe

Your container **starts out clean**, but what happens **after** it’s running? Attackers love to exploit containers **at runtime**, so you need real-time **monitoring**.

### **Best Practices:**

✔️ Use **least privilege** access—don’t let your containers run as **root**.\
✔️ Monitor **unexpected network traffic** (because containers should NOT be calling out to mystery servers).\
✔️ Implement **runtime security tools** like [Falco](https://falco.org/) or [Sysdig](https://sysdig.com/).

What happens in your container **does not** stay in your container. Monitor it! 👀

***

## 🔐 3. Access Control: No, Everyone Should NOT Have Root Access

Containers are **only as secure as your access controls**. Poorly configured **Identity and Access Management (IAM)** is an **open invitation** for attackers.

### **Best Practices:**

✔️ **Role-Based Access Control (RBAC)**—Give users only the permissions they need.\
✔️ Use **Secrets Management** tools instead of hardcoding credentials.\
✔️ Implement **Zero Trust** (assume every request is suspicious).

If you’re handing out **admin access** like Halloween candy, **stop**. 🎃🚫

***

## 🏆 Key Takeaways

| **Security Measure** | **Why It Matters**                                |
| -------------------- | ------------------------------------------------- |
| **Image Scanning**   | Prevents running vulnerable code.                 |
| **Runtime Security** | Detects attacks in real-time.                     |
| **Access Controls**  | Limits who can do what inside containers.         |
| **Least Privilege**  | Ensures containers don’t have unnecessary powers. |
| **Regular Updates**  | Keeps security patches applied.                   |

***

## 🔗 Reference Links

* [NIST 800-190 Official Document](https://csrc.nist.gov/publications/detail/sp/800-190/final)
* [Container Image Scanning](https://en.wikipedia.org/wiki/Container_image)
* [Role-Based Access Control (RBAC)](https://en.wikipedia.org/wiki/Role-based_access_control)
* [Falco - Runtime Security for Containers](https://falco.org/)
* [Sysdig - Secure Your Containers](https://sysdig.com/)
* [Zero Trust Security Model](https://en.wikipedia.org/wiki/Zero_trust_security_model)

***
