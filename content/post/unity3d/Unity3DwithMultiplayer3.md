---
title: "Tutorial: Multiplayer Game with Unity 3D and Azure Cloud- Part 3"
description: Part 3- Hooking it all together
slug: unity-tutorial-multi-player-3
date: 2022-05-06
image: post/Articles/IMAGES/carmangria4_wide.jpg
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
weight: 41
published: 2001-01-01
draft: false
lastmod: 2025-03-03T12:24:03.066Z
---
# Multiplayer Unity 3D With Azure: Hooking It All Up and Making It Work

## Introduction

In our previous articles, we set up **Photon PUN** with **Azure** and **Unity 3D** to create a real-time multiplayer game. Now, it’s time to **hook everything up** and make it work seamlessly! 🎮

We’ll cover:

* **User authentication** (sign-up/sign-in)
* **Lobby mechanics** (how players join and interact)
* **Game session handling** (starting and managing games)
* **Smooth player movement** (interpolation, prediction, and lag compensation)
* **Photon’s different networking update models**
* **Anti-cheat strategies and Photon’s built-in protections**

***

## Step 7: User Authentication (Sign-Up & Sign-In)

### 7.1 Setting Up PlayFab Authentication with Photon

We’ll use **Azure PlayFab** to manage user authentication before connecting to **Photon**.

#### **Step 1: Initialize PlayFab in Unity**

Install the PlayFab SDK via the Unity Package Manager and create a **PlayFabManager.cs** script:

```csharp
using PlayFab;
using PlayFab.ClientModels;
using UnityEngine;

public class PlayFabManager : MonoBehaviour
{
    public void LoginWithCustomID(string customId)
    {
        var request = new LoginWithCustomIDRequest
        {
            CustomId = customId,
            CreateAccount = true
        };

        PlayFabClientAPI.LoginWithCustomID(request, OnLoginSuccess, OnLoginFailure);
    }

    private void OnLoginSuccess(LoginResult result)
    {
        Debug.Log("Login successful! PlayFab ID: " + result.PlayFabId);
    }

    private void OnLoginFailure(PlayFabError error)
    {
        Debug.LogError("Login failed: " + error.GenerateErrorReport());
    }
}
```

#### **Step 2: Connect PlayFab to Photon**

Once authenticated, we need to **connect Photon to PlayFab**:

```csharp
using Photon.Pun;
using Photon.Realtime;
using PlayFab;
using PlayFab.ClientModels;
using UnityEngine;

public class MultiplayerAuthenticator : MonoBehaviourPunCallbacks
{
    public void AuthenticateAndConnect()
    {
        PlayFabClientAPI.GetAccountInfo(new GetAccountInfoRequest(), result =>
        {
            string playFabId = result.AccountInfo.PlayFabId;
            PhotonNetwork.AuthValues = new AuthenticationValues(playFabId);
            PhotonNetwork.ConnectUsingSettings();
        }, error => Debug.LogError("Failed to retrieve PlayFab ID: " + error.GenerateErrorReport()));
    }
}
```

Now, users **authenticate with PlayFab** before joining a **Photon multiplayer session**. ✅

***

## Step 8: The Lobby System

A **lobby** is where players wait before joining a game. Photon supports:

1. **Default Lobby** - A simple list of open game rooms.
2. **Custom Matchmaking Lobbies** - Filtered searches (e.g., rank-based matchmaking).

### 8.1 Creating a Lobby UI

We need a **UI panel** listing available rooms. Here’s how we fetch **room lists**:

```csharp
using Photon.Pun;
using Photon.Realtime;
using UnityEngine;
using UnityEngine.UI;

public class LobbyManager : MonoBehaviourPunCallbacks
{
    public Text roomListText;

    public override void OnRoomListUpdate(List<RoomInfo> roomList)
    {
        roomListText.text = "Available Rooms:\n";
        foreach (var room in roomList)
        {
            roomListText.text += room.Name + " (" + room.PlayerCount + "/" + room.MaxPlayers + ")\n";
        }
    }
}
```

This updates the **lobby UI** dynamically as rooms are created.

***

## Step 9: Handling Game Start

Once players are in the **lobby**, we let them start a game:

```csharp
using Photon.Pun;
using UnityEngine;

public class GameLauncher : MonoBehaviour
{
    public void StartGame()
    {
        PhotonNetwork.LoadLevel("GameScene");
    }
}
```

***

## Step 10: Smooth Player Movement and Position Updates

Photon provides multiple ways to **synchronize player movement**:

1. **Photon Transform View** (simple but laggy)
2. **Manual Synchronization** (more control, less lag)
3. **State Interpolation & Prediction** (best for smoothness)

### 10.1 Using Photon Transform View (Easy Mode)

```csharp
using Photon.Pun;

public class PlayerSync : MonoBehaviourPun, IPunObservable
{
    public void OnPhotonSerializeView(PhotonStream stream, PhotonMessageInfo info)
    {
        if (stream.IsWriting)
        {
            stream.SendNext(transform.position);
        }
        else
        {
            transform.position = (Vector3)stream.ReceiveNext();
        }
    }
}
```

### 10.2 Advanced: Lag Compensation & Prediction

For better **smoothness**, use **interpolation**:

```csharp
private Vector3 networkPosition;
private float smoothing = 0.1f;

void Update()
{
    if (!photonView.IsMine)
    {
        transform.position = Vector3.Lerp(transform.position, networkPosition, smoothing);
    }
}
```

***

## Step 11: Anti-Cheat Strategies in Photon

Photon does **not** have built-in cheat detection but allows:

* **Server-side authoritative movement**
* **Encryption for network packets**
* **Custom anti-cheat logic**

### Pros and Cons of Photon Anti-Cheat

| Feature              | Pros                      | Cons               |
| -------------------- | ------------------------- | ------------------ |
| Client-side checks   | Easy to implement         | Hackers can bypass |
| Server-authoritative | Harder to hack            | More server load   |
| Encrypted packets    | Prevents packet injection | Not bulletproof    |

***

<!-- 
## Conclusion

We covered **everything** to get your **multiplayer game fully working**:
✅ **User authentication** with PlayFab & Photon
✅ **Lobby mechanics** and **matchmaking**
✅ **Starting a game session**
✅ **Smooth movement syncing**
✅ **Anti-cheat considerations**

Time to build your dream game! 🚀🎮
-->

**NOTE**\
Unity3d is very versatile and can target many platforms. All the techniques here apply to iPhone-iOS, Android , as well as Windows and Mac.

Unity3d has the ability to target web pages- by compiling to web assembly. BUT the caveat is when you are running on a web page, your code can talk back to the server it came from , but cannot make calls out to other domains (cross origin issues).
