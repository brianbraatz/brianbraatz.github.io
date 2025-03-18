---
title: OpenLens goes commercial. Free version lost Docker Pod Shell.
description: Instructions on how to bring it back
slug: how-to-attach-a-shell-to-a-pod-in-openlens
date: 2022-06-13
image: post/Articles/IMAGES/openlenssmall.png
categories:
  - Docker
  - DevOps
  - OpenLens
  - Kubernetes
  - Cloud
tags:
  - Docker
  - Kubernetes
  - OpenLens
  - Pods
  - Shell
  - Kubectl
draft: false
weight: 110
categories_ref:
  - Docker
  - DevOps
  - OpenLens
  - Kubernetes
  - Cloud
slug_calculated: https://brianbraatz.github.io/p/how-to-attach-a-shell-to-a-pod-in-openlens
lastmod: 2025-03-14T16:40:20.097Z
---
# How to Attach a Shell to a Pod in OpenLens

## **Classic "Corporate ruins everything" situation**

### **Once Upon a Time in OpenLens Land...**

Back in the good ol’ days, OpenLens (the open-source version of Lens) was a fantastic tool.

It was like the Swiss Army knife of Kubernetes management—easy to use, powerful, and *best of all*, it let you attach a shell to pods right from the UI.

Life was good.

Shells and pods lived in harmony....

Then came **Mirantis**. *Dramatic music plays.*

### **The Great Commercialization of Lens**

Mirantis, a company known for OpenStack and other cloud wizardry, acquired Lens and decided, **"Hey, what if we made money off this?"** (which, let’s be fair, is what companies do).

They introduced a **commercial version of Lens**, started paywalling features, and in the process, stripped OpenLens of some goodies—including the **Attach Shell** feature.

That’s right. One day you’re happily right-clicking your pod and opening a shell like a Kubernetes wizard, and the next day—**poof!**—the option is gone.

### **Why Did They Do This?**

Because **money**. 💰

By removing features from OpenLens, they made the paid Lens version more attractive.

It’s the classic **"take something free, lock it up, sell it back"** move.

Some people call this **"feature gating,"** but honestly, it felt more like **"feature stealing."**

### **What Can You Do About It?**

* **Use a plugin** like `@alebcay/openlens-node-pod-menu` to bring back shell access (unless they block that too).
* **Go full hacker mode** and just use `kubectl exec -it <pod-name> -- /bin/bash` like the Kubernetes command-line ninja you were meant to be.
* **Complain loudly on the internet.** The louder you scream, the more likely open-source forks and alternatives will appear.

<!-- 
### **Final Thoughts**  
Lens was once the people's hero, but now it’s a corporate cash cow. 

OpenLens is still useful, but if you want shell access, you’ll have to jump through some hoops. 

Or just embrace the CLI life.  

**RIP Attach Shell in OpenLens** 🪦 *Gone but not forgotten.*
-->

## So what exactly is a Docker, a Kubernetes and an Openlens?

### Docker, Kubernetes, and OpenLens Walk Into a Bar...

Okay, they don’t actually walk into a bar, but if they did, Docker would be the friendly bartender, Kubernetes would be the overzealous manager, and OpenLens would be the chill customer trying to make sense of the chaos.

#### Docker ([Website](https://www.docker.com/), [Wikipedia](https://en.wikipedia.org/wiki/Docker_\(software\)))

Docker came first, and it revolutionized software deployment by creating containers—lightweight, portable environments that ensure "it works on my machine" actually means something.

#### Kubernetes ([Website](https://kubernetes.io/), [Wikipedia](https://en.wikipedia.org/wiki/Kubernetes))

Kubernetes (K8s for short, because who has time for syllables?) took things up a notch by orchestrating these containers across multiple machines.

It automates scaling, load balancing, and deployments, making sure your applications don’t crash and burn.

#### OpenLens ([Website](https://github.com/lensapp/lens), [Wikipedia](https://en.wikipedia.org/wiki/Lens_\(software\)))

OpenLens is a GUI for Kubernetes, making it easier to manage clusters, inspect logs, and—yes—attach a shell to pods without memorizing `kubectl` commands.

## Attaching a Shell to a Pod in OpenLens

### Installing the `@alebcay/openlens-node-pod-menu` Extension

If you want to attach a shell to a pod using OpenLens, you need the `@alebcay/openlens-node-pod-menu` extension.

Here’s how you do it:

1. Open OpenLens.
2. Navigate to **File → Extensions**.
3. Search for `@alebcay/openlens-node-pod-menu`.
4. Click **Install** and **Enable** it.

### Using the Menu to Attach a Shell

Once installed and enabled:

1. Go to **Workloads → Pods**.
2. Right-click on the pod.
3. Select **Attach Shell**.

If it’s not there, don’t panic! Here are some troubleshooting steps:

(it didnt work for me on the first time until i re-started openlens)

* **Check the Developer Console**: `View → Developer → Toggle Developer Tools`. Look for errors.
* **Verify Permissions**: Run `kubectl auth can-i exec pods --namespace <namespace>`. If it says "no," you need better Kubernetes privileges.
* **Restart OpenLens**: Because turning it off and on again works more often than you'd think.

## Alternative Methods (Because Sometimes GUI Just Won't Cut It)

If OpenLens is being stubborn, here are some other ways to get a shell into a pod:

### 1. Using `kubectl exec`

```bash
kubectl exec -it <pod-name> -- /bin/bash
```

If `/bin/bash` isn’t available, try `/bin/sh` instead.

### 2. Using `kubectl attach`

```bash
kubectl attach -it <pod-name> --container <container-name>
```

This method lets you attach to an already running process inside the pod.

### 3. Using `kubectl debug` (For Newer Kubernetes Versions)

```bash
kubectl debug -it <pod-name> --image=busybox
```

This creates a temporary debugging container inside the pod.

## Wrapping Up

Using OpenLens to attach a shell to a pod is convenient, but sometimes things don’t go as planned. If the `@alebcay/openlens-node-pod-menu` extension doesn’t work, fall back to good old `kubectl`.

## More Information

* Docker: <https://www.docker.com/>
* Kubernetes: <https://kubernetes.io/>
* OpenLens: <https://github.com/lensapp/lens>
* Kubectl Commands: <https://kubernetes.io/docs/reference/kubectl/>
