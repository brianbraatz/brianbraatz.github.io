---
title: Azure Notification Hubs in a Nutshell
description: Azure Notification Hubs
slug: azure-notification-hubs
date: 2016-08-27
image: post/Articles/IMAGES/azurenotificationhubs.png
categories:
  - Azure
tags:
  - Azure
  - Notification
  - Push
  - Mobile
  - Cloud
  - Messaging
draft: false
weight: 543
lastmod: 2025-03-23T03:10:34.231Z
---
# Azure Notification Hubs 🚀📱

<!-- 
Azure Notification Hubs is Microsoft’s supercharged push notification engine for sending cross-platform messages to millions of mobile devices with a single API call.

Yup — imagine you're sending updates to an iOS app, Android app, and a Windows app all at once… without duplicating code or managing multiple platforms.

Let’s dive into what makes it cool and how you can use it in real life (with code)! -->

***

## 🧠 What is Azure Notification Hubs?

At its core, Azure Notification Hubs (ANH) is a **platform-agnostic push notification service**. It abstracts away the complexities of dealing with different vendor push systems like:

* Apple Push Notification Service (APNS)
* Firebase Cloud Messaging (FCM)
* Windows Notification Service (WNS)

It supports *tagging*, *templating*, *broadcasts*, and *targeted messages*. It’s optimized for **high scale**, like sending *millions* of messages in a short time.

***

## 🛠️ Setup Overview

Before we write code, here’s what you’ll typically do:

1. **Create a Notification Hub** on Azure.
2. Configure platform credentials (like FCM key, APNS certificate).
3. Register your devices in your mobile apps.
4. Send push notifications via backend or Azure functions.

***

## 🏗️ Step 1: Create a Notification Hub

In the Azure Portal:

* Go to **Create a resource** → **Notification Hub**.
* Provide a namespace (like `mynotifynamespace`) and hub name (`myhub`).
* Link it to a resource group and region.
* After it’s created, configure platform settings like FCM and APNS credentials.

***

## 🤖 Code Example: Registering Android with FCM

Here’s how you register an Android device using FCM:

```kotlin
// In your Android app
val connectionString = "<Your Listen Connection String>"
val hubName = "<Your Hub Name>"

val hub = NotificationHub(hubName, connectionString, this)
val fcmToken = FirebaseInstanceId.getInstance().token

hub.register(fcmToken, "news", "sports") // Tags are optional
```

> This allows Azure to send messages specifically to devices tagged with `news` or `sports`.

***

## 🍏 Code Example: Sending Notification via .NET

Here’s how you send a message from your server:

```csharp
using Microsoft.Azure.NotificationHubs;

var hub = NotificationHubClient.CreateClientFromConnectionString(
    "<Full Access Connection String>", "<Hub Name>"
);

string payload = @"{
  ""data"": {
    ""message"": ""Hello from Azure!""
  }
}";

// Send to all Android devices
await hub.SendFcmNativeNotificationAsync(payload);
```

You can also send to **specific tags** like this:

```csharp
await hub.SendFcmNativeNotificationAsync(payload, "sports");
```

***

## 📨 Templated Notifications (💡 Bonus Feature)

Templates allow you to send **localized or customized** messages without crafting multiple payloads.

### Example Registration Template (on the client):

```json
{
  "body": {
    "data": {
      "message": "$(messageParam)"
    }
  }
}
```

Then from your server:

```csharp
var templateParams = new Dictionary<string, string>
{
    ["messageParam"] = "Templated Hello!"
};

await hub.SendTemplateNotificationAsync(templateParams, "news");
```

Boom — every client gets their own customized message based on their template!

***

## 📊 When to Use Azure Notification Hubs?

* Massive scale push notifications across platforms.
* Targeted user segments (via tags).
* Integration with backend systems or Azure Functions.
* Time-sensitive alerts or real-time updates.

***

## 🧪 Gotchas and Tips

* Use **tags** instead of creating too many hubs.
* Prefer **templates** for localization and dynamic content.
* Monitor **telemetry** via Azure Monitor or custom logs.
* Use **Access Policies** for proper security (Listen vs Full Access).

***

## 🎉 Wrap-Up

Azure Notification Hubs takes the pain out of building multi-platform push notification systems. Whether you’re building a sports app that blasts real-time scores or an e-commerce app that nudges users with sales — Notification Hubs has your back.

It’s scalable, flexible, and surprisingly easy to integrate once you get the hang of it.

***

## 🔑 Key Ideas

| Key Idea            | Summary                                  |
| ------------------- | ---------------------------------------- |
| What is it?         | Cross-platform push notification system  |
| Use Cases           | Alerts, updates, real-time notifications |
| Platforms Supported | iOS, Android, Windows, Kindle            |
| Tags & Templates    | Targeted and dynamic notifications       |
| Server SDKs         | .NET, Java, Node.js, REST                |
| Integration         | Azure Functions, Logic Apps, Mobile Apps |

***

> 🧠 **Pro Tip:** Think of Azure Notification Hubs as the air traffic controller for your push notifications — directing messages to the right device, in the right language, at the right time.

```
```
