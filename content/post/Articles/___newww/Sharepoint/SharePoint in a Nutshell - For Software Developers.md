---
title: SharePoint in a Nutshell - For Software Developers
description: ""
slug: sharepoint-nutshell-dev
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
  - CSharp
date: 2017-10-17
image: post/Articles/IMAGES/SharePoint.png
draft: false
weight: 41
lastmod: 2025-03-03T15:10:25.656Z
---
<!-- 
# SharePoint in a Nutshell - For Software Developers
-->

SharePoint is Microsoft’s Swiss Army knife for collaboration, document management, and business process automation.

Whether you love it, tolerate it, or avoid it like the plague, SharePoint has been a in enterprise software for over two decades.

<!-- 
In this guide, we’ll cover:

- The history of SharePoint
- Key features and release versions
- Comparing SharePoint to alternatives
- How the Microsoft Graph API fits in
- When SharePoint is a good solution (and when it’s not)
- Security and authentication (including OAuth)

Let’s dive in!
-->

***

## 📜 History of SharePoint

SharePoint was born in 2001 as a glorified document repository and collaboration tool.

Over the years, it evolved into a full-fledged content management system, enterprise intranet, and business automation platform.

### 📅 Table of Releases

| Version           | Release Year | Key Features                                               |
| ----------------- | ------------ | ---------------------------------------------------------- |
| SharePoint 2001   | 2001         | Basic document management                                  |
| SharePoint 2003   | 2003         | Web-based collaboration, document libraries                |
| SharePoint 2007   | 2007         | Workflows, improved UI, better integration with Office     |
| SharePoint 2010   | 2010         | Sandboxed solutions, Business Connectivity Services (BCS)  |
| SharePoint 2013   | 2013         | App model, improved search, cloud integration              |
| SharePoint 2016   | 2016         | Hybrid cloud features, better performance                  |
| SharePoint 2019   | 2019         | Modern UI, deeper integration with OneDrive and Teams      |
| SharePoint Online | Ongoing      | Cloud-native, Microsoft 365 integration, Graph API support |

***

## 🎯 Why Use SharePoint? (Motivation)

SharePoint is used by organizations to:

* Centralize document storage
* Manage permissions and compliance
* Automate business processes
* Create intranets and team sites
* Enable secure collaboration across teams

***

## 🔑 Key Features in Different SharePoint Versions

| Feature                   | SharePoint 2013 | SharePoint 2016 | SharePoint 2019 | SharePoint Online |
| ------------------------- | --------------- | --------------- | --------------- | ----------------- |
| On-Premises Support       | ✅               | ✅               | ✅               | ❌                 |
| Cloud Integration         | ❌               | ✅               | ✅               | ✅                 |
| Modern UI                 | ❌               | ❌               | ✅               | ✅                 |
| Microsoft 365 Integration | ❌               | ❌               | ✅               | ✅                 |
| AI-Powered Search         | ❌               | ❌               | ✅               | ✅                 |

***

## 🆚 Comparing SharePoint to Alternatives

| Feature                  | SharePoint | Google Drive | Confluence | Dropbox |
| ------------------------ | ---------- | ------------ | ---------- | ------- |
| Document Management      | ✅          | ✅            | ❌          | ✅       |
| Business Workflows       | ✅          | ❌            | ✅          | ❌       |
| Permissions & Compliance | ✅          | ❌            | ✅          | ❌       |
| Custom Apps & Extensions | ✅          | ❌            | ✅          | ❌       |

SharePoint is a **strong choice** for companies that need **secure collaboration, business workflows, and deep Microsoft 365 integration**.

If you are already in deep in Microsoft 365, Sharepoint is extra compelling..

But if your company is running on Dropbox or Google Drive, you will not get the same level of Microsoft eco-system integration .

For really simple sharing- arguably with less of a learning curve: tools like **Google Drive or Dropbox** might be more user-friendly.

***

## 🔍 The Microsoft Graph API

The **Graph API** is a RESTful API that allows developers to interact with SharePoint Online (and other Microsoft 365 services). You can use it to:

* **Manage Files & Folders**: Upload, download, and organize files.
* **Automate Workflows**: Trigger events when files change.
* **Query Lists & Data**: Fetch and update SharePoint lists.
* **User & Group Management**: Control permissions programmatically.

### Example: Retrieving SharePoint Files Using Graph API

```csharp
var driveItems = await graphClient.Sites["your-site-id"].Drive.Root.Children.Request().GetAsync();

foreach (var item in driveItems)
{
    Console.WriteLine($"Name: {item.Name}, ID: {item.Id}");
}
```

***

## ✅ Examples Where SharePoint is a Good Solution

1. **Company Intranet**
2. **HR Document Storage**
3. **Team Collaboration Sites**
4. **Workflow Automation for Approvals**
5. **Enterprise Content Management**
6. **Secure File Sharing with Permissions**
7. **Compliance and Audit Logging**
8. **Knowledge Management Portals**
9. **Departmental Project Management**
10. **Hybrid Cloud and On-Prem Document Syncing**

***

## ❌ 10 Examples Where SharePoint is NOT the Best Choice

1. **Real-Time Collaborative Editing (Google Docs is better)**
2. **Transactional Database Storage (Use SQL Server instead)**
3. **High-Performance Web Applications**
4. **Custom-Built SaaS Products**
5. **Public-Facing Websites**
6. **API-Heavy Workflows (Other services provide faster APIs)**
7. **Complex Business Logic Processing**
8. **Low-Code / No-Code Apps (Power Apps may be better)**
9. **High-Speed Data Processing Needs**
10. **Ad-Hoc Cloud Storage (Google Drive or OneDrive is simpler)**

***

## 🔐 Security & Authentication in SharePoint

### **Authentication Options**

* **Azure AD Authentication** (for cloud-based SharePoint Online)
* **Windows Authentication** (for on-premises SharePoint)
* **OAuth 2.0 & OpenID Connect** (for API access)
* **SAML-Based Authentication** (for enterprise integrations)

### **Can You Use OAuth with SharePoint?**

Yes! SharePoint Online supports **OAuth 2.0** via **Microsoft Graph API**. This allows secure, token-based authentication for apps accessing SharePoint resources.

### Example: Authenticating via OAuth 2.0

```csharp
var confidentialClient = ConfidentialClientApplicationBuilder
    .Create(clientId)
    .WithClientSecret(clientSecret)
    .WithAuthority($"https://login.microsoftonline.com/{tenantId}")
    .Build();

var authResult = await confidentialClient.AcquireTokenForClient(new[] { "https://graph.microsoft.com/.default" }).ExecuteAsync();

Console.WriteLine($"Access Token: {authResult.AccessToken}");
```

***

## So what does this have to do with Asp.net?

* Take a deep look at Sharepoint and Graph API
  * If you have a Corporate App: (sharepoint is not so great for public websites)
* Really ask yourself- soul searching- can you replace some of your boilerplate code with Sharepoint?
* The first time I did this (over 10 years ago), I was able to give the users a sharepoint site to administer the data we were collecting.
* I then made a public website - that could connect to that same data
* the best of both worlds
* Sharepoint is a powerful tool to know what it can do , and how it can best fit into your cloud application design

<!-- 
## 🏁 Conclusion

SharePoint is a **powerful, but often misunderstood** tool for developers. It’s great for **enterprise collaboration, document management, and workflows**, but **not ideal for high-speed transactional applications or real-time editing**.

With **Microsoft Graph API and OAuth support**, developers can extend SharePoint in **flexible and secure ways**.

If your business needs **secure document management, workflows, and integration with Microsoft 365**, **SharePoint is a great option**.

-->
