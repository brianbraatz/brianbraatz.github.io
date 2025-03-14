---
title: "Tutorial: Multiplayer Game with Unity 3D and Azure Cloud- Part 2"
description: Part 2-Set up Photon PUN with Azure and Unity. This guide covers Photon PUN’s history, sign-up process, pricing, and integrating it with Azure.
slug: unity-tutorial-multi-player-2
date: 2022-05-06
image: post/Articles/IMAGES/carmangria5_wide.jpg
categories:
  - Unity 3D Engine
  - CSharp
  - Cross Platform
  - Web Development
  - Mobile
  - IOS
  - iPhone
  - Android
  - Cloud
  - Cloud-Real Time Multi User
  - Azure
  - Azure Functions
  - Azure App Services
tags:
  - Cloud
  - Azure
  - Unity3D
  - CPP
  - CSharp
  - Cross-Platform
  - Lambda
weight: 31
published: 2001-01-01
draft: false
categories_ref:
  - Unity 3D Engine
  - CSharp
  - Cross Platform
  - Web Development
  - Mobile
  - IOS
  - iPhone
  - Android
  - Cloud
  - Cloud-Real Time Multi User
  - Azure
  - Azure Functions
  - Azure App Services
lastmod: 2025-03-14T15:45:30.697Z
---
<!-- 
(Article Image is from my Proto game "Carmangria" - more on that here [Unity 3D Multiplayer Game Experiments](post/unity3d/unity-proto-games/index.md) )

Image Below is from Babblecheckers (Same Article)
-->

![](/post/unity3d/unity-proto-games/bcheckers.png)

<!-- 
Previous Article is here:
[Tutorial: Multiplayer Game with Unity 3D and Azure Cloud- Part 1](post/unity3d/unity-proto-games/Unity3DwithMultiplayer1.md)
-->

# Setting Up Photon PUN with Azure and Unity

<!-- 
[Part 1](post/unity3d/unity-proto-games/Unity3DwithMultiplayer1.md) 
-->

## Introduction

In Part 1, we built a **real-time multiplayer game** using **Unity 3D, Azure Cloud, and OAuth authentication**.

Now, let’s take a deeper dive into **Photon PUN**—a powerful multiplayer networking solution—and explore how to integrate it with **Azure Cloud**.

We’ll cover:

* The history of **Photon PUN** and where to get it.
* How to **sign up** and **pricing details**.
* **Adding Photon PUN to Azure** for better scalability.

***

## A Brief History of Photon PUN

Photon PUN (Photon Unity Networking) is a **real-time multiplayer framework** developed by **Exit Games**. It’s one of the most popular networking solutions for Unity, offering features like:

* **Matchmaking & Room Management** 🚪
* **Lag Compensation & Syncing** 🔄
* **Cross-Platform Multiplayer Support** 📱💻
* **Dedicated Cloud Hosting** ☁️

## Personal Experience With Azure and PUN

I have watched Photon grow up over the years. The first version used was around 2011.\
The good news is- compared to then, internet connections have improved since then and the overall experience for players, and developers, have improved quite a bit.

In 2011, we had to "roll our own" security etc.. Usually a website where you have to sign up with user name and password etc.. Things are much easier now, as networking, Azure, and Photon have all improved..

### Where to Get Photon PUN

You can find Photon PUN on the **Unity Asset Store** or directly from **Exit Games**:

* 📥 [Photon PUN on the Unity Asset Store](https://assetstore.unity.com/packages/tools/network/pun-2-free-119922)
* 🌍 [Photon Engine Website](https://www.photonengine.com/)

***

## Step 5: Signing Up for Photon PUN

Before integrating Photon with Azure, let’s set up a **Photon account** and get our **App ID**.

### 5.1 Create a Photon Account

1. Go to the [Photon Engine website](https://www.photonengine.com/).
2. Click **Sign Up** and create a free account.
3. Once logged in, go to the **Dashboard**.
4. Click **Create a New App** and select **Photon PUN**.
5. Copy your **App ID**—you’ll need it for Unity.

### 5.2 Understanding Photon Pricing 💰

Photon PUN offers a **free tier** but has paid plans for scaling:

| Plan       | CCU (Concurrent Users) | Cost          |
| ---------- | ---------------------- | ------------- |
| Free       | 20 CCU                 | \$0           |
| Plus       | 100 CCU                | \$95/month    |
| Pro        | 500 CCU                | \$195/month   |
| Enterprise | Unlimited              | Contact sales |

For large-scale games, **Azure integration** can help offload server costs. Let’s do that next! 🚀

***

## Step 6: Adding Photon PUN to Azure

Now that we have Photon set up, let’s deploy it to **Azure Cloud** for better performance and scalability.

### 6.1 Deploying Photon to an Azure Virtual Machine

Photon’s **dedicated server mode** lets you host game sessions on **Azure Virtual Machines (VMs)**.

1. **Create an Azure VM**:
   * Go to **Azure Portal**.
   * Click **Create a resource > Virtual Machine**.
   * Select **Ubuntu or Windows Server**.
   * Choose a **Standard D2s v3 (2 vCPU, 8GB RAM)** instance.
   * Set up **networking** (allow TCP/UDP ports 5055-5058 for Photon).
   * Deploy and copy the **public IP address**.

2. **Install Photon Server**:
   * Download **Photon Server SDK** from [here](https://www.photonengine.com/Server).
   * Upload the Photon Server files to your **Azure VM**.
   * Run the **Photon Control Panel** and start the server.

3. **Connect Photon PUN to Azure**:
   * In Unity, go to **PhotonServerSettings**.
   * Change the **Server Type** to **Self-Hosted**.
   * Enter your **Azure VM’s public IP** as the Photon Server address.
   * Save and test the connection.

### 6.2 Scaling with Azure App Services

For larger games, you can **run Photon in Azure App Services** instead of VMs. This enables **auto-scaling**.

1. **Create an Azure Web App**:
   * Go to **Azure Portal > App Services**.
   * Click **Create Web App**.
   * Choose **Windows or Linux**.
   * Upload Photon PUN as a service.

2. **Enable Auto-Scaling**:
   * In the **Scale-Out** settings, configure automatic scaling.
   * Define rules based on CPU/memory usage.

This setup allows **Photon PUN** to scale dynamically, ensuring smooth gameplay for thousands of players. 🎯

***

<!-- 
## Conclusion

In this tutorial, we:
✅ Explored the **history** of **Photon PUN**
✅ Learned **how to sign up** and the **pricing model**
✅ Deployed **Photon PUN to Azure** using **Virtual Machines** and **App Services**

With this setup, your game is ready to handle massive multiplayer sessions with **low latency** and **high scalability**. 🚀

Got questions? Drop a comment below! Happy coding. 🎮🔥

-->

**NOTE**\
Unity3d is very versatile and can target many platforms. All the techniques here apply to iPhone-iOS, Android , as well as Windows and Mac.

Unity3d has the ability to target web pages- by compiling to web assembly. BUT the caveat is when you are running on a web page, your code can talk back to the server it came from , but cannot make calls out to other domains (cross origin issues).
