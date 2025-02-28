---
title: Graph API in a Nutshell
description: ""
slug: graphapi-nutshell
categories:
  - Sharepoint
  - Cloud
  - Graph API
  - Python
  - C#
  - CSharp
  - Office 365
  - Power Apps
tags:
  - Graph
  - API
  - Microsoft
  - Python
  - REST
  - API
  - Authentication
  - GraphAPI
date: 2023-10-17
image: post/Articles/IMAGES/gollumonering.png
draft: false
weight: 651
lastmod: 2025-02-28T20:58:05.451Z
---
<!-- 

# The History of the Graph API and How It Relates to Office and Active Directory

## 📜 Introduction
-->

The **Microsoft Graph API** is a unified interface that provides access to **Microsoft 365 services**, including **Office 365, Azure Active Directory (Azure AD), SharePoint, Teams, and more**.

Its like the "one ring" - but in the cloud :)

Before Graph API existed, we had to code against with **multiple APIs**, each specific to a product like **Exchange, SharePoint, or Active Directory**.

This frankly sucked.. :)

The GraphAPI brought all these old APIS into one place..

***

## 🏛 The Early Days: APIs Before Graph API

Before Microsoft introduced Graph API, developers had to use **separate APIs for different services**:

| **Legacy API**                  | **Purpose**                                     |
| ------------------------------- | ----------------------------------------------- |
| **Exchange Web Services (EWS)** | Access emails, calendars, contacts in Exchange  |
| **SharePoint REST API**         | Manage SharePoint documents and lists           |
| **Azure AD Graph API**          | Query and manage Azure Active Directory         |
| **Office 365 APIs**             | Work with Office services (deprecated)          |
| **Skype for Business API**      | Communicate with Skype for Business (now Teams) |

The **problem** with this approach? **Inconsistent authentication, different endpoints, and varied permissions** for each API.

Microsoft needed a **single API** to unify these services.

![](/post/Articles/___newww/Sharepoint/Pasted%20image%2020250228125535.png)

***

## 🚀 The Birth of Microsoft Graph API (2015)

In **November 2015**, Microsoft introduced the **Graph API** as a **RESTful, unified API** for Microsoft cloud services.

**Key Benefits of Graph API:**\
✅ **One API for all Microsoft 365 services**\
✅ **Consistent authentication using OAuth 2.0**\
✅ **Access multiple services (Office, AD, Teams, OneDrive, etc.)**\
✅ **Scalable and cloud-first design**

Instead of querying **Exchange, SharePoint, and Azure AD separately**, developers could now use **Graph API** for everything.

***

## 🏢 How Microsoft Graph API Relates to Office 365

**Office 365** is a **cloud-based productivity suite** that includes **Word, Excel, Outlook, Teams, SharePoint, OneDrive, and more**.

Graph API allows developers to **integrate with Office 365 data and automate tasks**:

| **Office 365 Feature**      | **Graph API Capabilities**                   |
| --------------------------- | -------------------------------------------- |
| **Outlook Mail**            | Read, send, and organize emails              |
| **Outlook Calendar**        | Schedule events, invite attendees            |
| **OneDrive**                | Upload, download, and manage files           |
| **Teams**                   | Create channels, post messages, manage users |
| **SharePoint**              | Retrieve documents, manage lists             |
| **Word, Excel, PowerPoint** | Edit and process Office files                |

**Example: Retrieving Emails Using Graph API**

```python
import requests

headers = {"Authorization": f"Bearer {token['access_token']}"}
response = requests.get("https://graph.microsoft.com/v1.0/me/messages", headers=headers)
print(response.json())
```

**C# Example**

```csharp
var response = await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/messages");
Console.WriteLine(await response.Content.ReadAsStringAsync());
```

***

## 🔐 How Microsoft Graph API Integrates with Azure Active Directory

**Azure Active Directory (Azure AD)** is **Microsoft’s cloud-based identity and access management service**. It controls authentication for **Office 365, Azure, and enterprise applications**.

### How Graph API Works with Azure AD:

1. **User Authentication** – Uses **OAuth 2.0** for secure logins.
2. **User & Group Management** – Manage **users, groups, and permissions**.
3. **Security & Compliance** – Retrieve **audit logs, security alerts, and compliance reports**.
4. **Enterprise Apps** – Integrate with **third-party applications**.

### Example: Retrieving User Details from Azure AD

**Python Example**

```python
requests.get("https://graph.microsoft.com/v1.0/users", headers=headers)
```

**C# Example**

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/users");
```

### Example: Managing Groups in Azure AD

**Python Example**

```python
requests.post("https://graph.microsoft.com/v1.0/groups", headers=headers, json={"displayName": "Developers", "mailNickname": "devs"})
```

**C# Example**

```csharp
var groupData = new { displayName = "Developers", mailNickname = "devs" };
var content = new StringContent(JsonConvert.SerializeObject(groupData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/groups", content);
```

***

## 📅 Key Milestones in the Evolution of Graph API

| **Year** | **Event**                                             |
| -------- | ----------------------------------------------------- |
| **2015** | Microsoft Graph API launched                          |
| **2017** | Added Teams, Planner, and Outlook support             |
| **2019** | Introduced Security & Compliance API                  |
| **2021** | Improved OneDrive, SharePoint, and Excel integrations |
| **2023** | AI-powered automation & Copilot integration           |

***

## 🔄 Transition from Legacy APIs to Graph API

### **Azure AD Graph API vs. Microsoft Graph API**

* **Azure AD Graph API** was the original API for **Active Directory**.
* **Microsoft Graph API** **replaced** it with **enhanced security, better performance, and more features**.

**Azure AD Graph API Deprecation Timeline**:

* **Deprecated in June 2023**.
* **Developers must migrate to Microsoft Graph API**.

### **Exchange Web Services (EWS) vs. Graph API**

* **EWS** was the old method to **access emails and calendars**.
* **Graph API is now recommended** for **Outlook data**.

***

<!-- 
## ⚡ Why Should Developers Use Microsoft Graph API?

✅ **Simplifies development** – No need for multiple APIs.  
✅ **Enhances security** – OAuth-based authentication.  
✅ **Improves performance** – Single API request for multiple services.  
✅ **Future-proof** – Regular updates and AI integration.  

---

## 🏁 Conclusion

The **Microsoft Graph API** has evolved from a simple **Office 365 API** to a **powerful platform** for integrating Microsoft 365 services.

🔹 It **unifies** access to **Office, Azure AD, Teams, SharePoint, and more**.  
🔹 It **replaces legacy APIs** like **Azure AD Graph and EWS**.  
🔹 It **powers automation and AI-driven workflows** in the Microsoft ecosystem.  

Whether you're working with **emails, users, files, security, or enterprise automation**, **Graph API is the future** of Microsoft 365 development.

-->
