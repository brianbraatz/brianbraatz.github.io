---
title: "Tutorial: Multiplayer Game with Unity 3D and Azure Cloud- Part 1"
description: Walk through the basics of a real-time multiplayer game in Unity 3D (C#) in Azure Cloud for hosting and OAuth for authentication. We will use Photon, Azure Functions, and Azure App Services
slug: unity-tutorial-multi-player-1
date: 2022-05-06
image: post/Articles/IMAGES/bw2.png
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
weight: 21
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
lastmod: 2025-03-14T15:45:30.654Z
---
<!-- 
Article Image is from my Proto game "Babblewar" - more on that here [Unity 3D Multiplayer Game Experiments](post/unity3d/unity-proto-games/index.md) 

[index](post/unity3d/unity-proto-games/index.md)
Image Below is from Carmangria (Same Article)
![](post/unity3d/unity-proto-games/carmangria3.png)
-->

# Tutorial: Make a Real-Time Multiplayer Game with Unity 3D and Azure Cloud- Part 1

<!-- 
Part 1 Walk through the basics of a real-time multiplayer game in Unity 3D (C#) in Azure Cloud for hosting and OAuth for authentication. We will use Photon, Azure Functions, and Azure App Services

Azure-Unity 3D-Multiplayer-Part 1
-->

## Introduction

Today, I will walk you through setting up a **real-time multiplayer game** using **Unity 3D**, **Azure Cloud**, and **OAuth authentication**.

This tutorial will guide you through the essential steps to build a multiplayer game with secure authentication and cloud hosting using **C#** and the **Unity engine**.

We’ll be using:

* **Unity 3D** for game development
* **Photon PUN** for real-time multiplayer networking
* **Azure Functions & App Services** for cloud backend
* **Azure PlayFab** for game services
* **OAuth (Microsoft Identity or Google OAuth)** for authentication

***

## Step 1: Setting Up Your Unity Project

### 1.1 Install Unity and Create a New Project

Ensure you have **Unity Hub** and **Unity 2022+** installed. Create a **3D** project and name it something cool like `AzureMultiplayerGame`.

### 1.2 Import Photon PUN for Multiplayer

Photon PUN (Photon Unity Networking) is a great way to get started with multiplayer games.

1. Open Unity and go to **Window > Package Manager**.
2. Click **Add package from git URL** and paste:
   ```
   https://github.com/ExitGames/Photon-Unity-Networking.git
   ```
3. Install the package and create a Photon account at [Photon Engine](https://www.photonengine.com/).
4. In Unity, go to **Photon > PUN Wizard** and enter your **App ID**.

Boom! Multiplayer setup ✅.

***

## Step 2: Implementing Authentication with OAuth (Azure AD)

### 2.1 Set Up an Azure AD Application

1. Go to [Azure Portal](https://portal.azure.com/).
2. Navigate to **Azure Active Directory > App Registrations > New Registration**.
3. Enter an app name (`UnityOAuthGame`) and set **Redirect URI** to `https://localhost/oauth2`.
4. Copy the **Client ID** and **Tenant ID**.

### 2.2 Implement OAuth in Unity (C#)

Install the **Microsoft Authentication Library (MSAL)** via NuGet or Unity Package Manager.

Create an `AuthManager.cs` script:

```csharp
using System;
using System.Threading.Tasks;
using Microsoft.Identity.Client;
using UnityEngine;

public class AuthManager : MonoBehaviour
{
    private IPublicClientApplication _clientApp;
    private string[] _scopes = { "User.Read" };
    private string _clientId = "YOUR_CLIENT_ID";
    private string _tenantId = "YOUR_TENANT_ID";

    void Start()
    {
        _clientApp = PublicClientApplicationBuilder.Create(_clientId)
                    .WithAuthority(AzureCloudInstance.AzurePublic, _tenantId)
                    .WithRedirectUri("https://localhost/oauth2")
                    .Build();
    }

    public async Task SignIn()
    {
        try
        {
            var result = await _clientApp.AcquireTokenInteractive(_scopes).ExecuteAsync();
            Debug.Log("Signed in: " + result.Account.Username);
        }
        catch (Exception ex)
        {
            Debug.LogError("OAuth Error: " + ex.Message);
        }
    }
}
```

Attach this script to a **GameObject** in Unity and call `SignIn()` to authenticate players. 🔑

***

## Step 3: Setting Up Azure PlayFab for Game Services

Azure PlayFab is a backend-as-a-service for games.

### 3.1 Register for PlayFab

1. Go to [PlayFab](https://playfab.com/) and create an account.
2. Create a new **Title** and copy your **Title ID**.

### 3.2 Integrate PlayFab in Unity

1. Install the **PlayFab SDK** via Unity Package Manager.
2. Create a `PlayFabManager.cs` script:

```csharp
using PlayFab;
using PlayFab.ClientModels;
using UnityEngine;

public class PlayFabManager : MonoBehaviour
{
    public void LoginWithOAuth(string authToken)
    {
        var request = new LoginWithCustomIDRequest
        {
            CustomId = authToken,
            CreateAccount = true
        };

        PlayFabClientAPI.LoginWithCustomID(request, OnLoginSuccess, OnLoginFailure);
    }

    private void OnLoginSuccess(LoginResult result)
    {
        Debug.Log("PlayFab login success! Player ID: " + result.PlayFabId);
    }

    private void OnLoginFailure(PlayFabError error)
    {
        Debug.LogError("PlayFab login failed: " + error.GenerateErrorReport());
    }
}
```

Call `LoginWithOAuth(authToken)` after signing in with OAuth.

***

## Step 4: Implementing Real-Time Multiplayer Logic

### 4.1 Create a Multiplayer Room

Modify `MultiplayerManager.cs` to create a room:

```csharp
using Photon.Pun;
using UnityEngine;

public class MultiplayerManager : MonoBehaviourPunCallbacks
{
    public void CreateRoom()
    {
        PhotonNetwork.CreateRoom("Room1", new Photon.Realtime.RoomOptions { MaxPlayers = 4 });
    }

    public override void OnJoinedRoom()
    {
        Debug.Log("Joined room: " + PhotonNetwork.CurrentRoom.Name);
    }
}
```

Call `CreateRoom()` when the player clicks **Start Game**.

### 4.2 Spawning Players

Modify `GameManager.cs` to instantiate a player:

```csharp
using Photon.Pun;
using UnityEngine;

public class GameManager : MonoBehaviour
{
    public GameObject playerPrefab;

    void Start()
    {
        PhotonNetwork.Instantiate(playerPrefab.name, new Vector3(0, 0, 0), Quaternion.identity);
    }
}
```

<!-- 
[Tutorial: Multiplayer Game with Unity 3D and Azure Cloud- Part 2](post/unity3d/unity-proto-games/Unity3DwithMultiplayer2.md)
-->

<!--
Boom! Your real-time multiplayer game is alive. 🎉

---

## Conclusion

You’ve built a **real-time multiplayer game** using **Unity 3D**, **Azure Cloud**, and **OAuth authentication**! We covered:

✅ Setting up **Photon PUN** for multiplayer
✅ Implementing **OAuth authentication** with **Azure AD**
✅ Using **PlayFab** for game services
✅ Spawning players and creating rooms

Time to expand your game! Add leaderboards, matchmaking, and more. 🚀

Got questions? Drop a comment below! Happy coding. 🎮🔥

-->

**NOTE**\
Unity3d is very versatile and can target many platforms. All the techniques here apply to iPhone-iOS, Android , as well as Windows and Mac.

Unity3d has the ability to target web pages- by compiling to web assembly. BUT the caveot is when you are running on a web page, your code can talk back to the server it came from , but cannot make calls out to other domains (cross origin issues).
