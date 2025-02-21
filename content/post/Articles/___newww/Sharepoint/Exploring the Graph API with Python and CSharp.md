---
title: Exploring the Graph API with Python and C#
description: Huge List of Code snippets for common tasks
slug: graphapi-python-cs-examples
categories:
  - Sharepoint
  - Cloud
  - Graph API
  - Python
  - C#
  - CSharp
  - Office 365
tags:
  - Graph
  - API
  - Microsoft
  - Python
  - REST
  - API
  - Authentication
  - CSharp
date: 2023-10-17
image: post/Articles/IMAGES/SharePoint.png
draft: false
weight: 423
lastmod: 2025-02-20T23:15:31.429Z
---
# Exploring the Graph API (Sharepoint) with Python and C#: 10 Things You Can Do

## 🔍 What is the Microsoft Graph API?

The **Microsoft Graph API** is a RESTful API that allows developers to interact with Microsoft 365 services like Outlook, SharePoint, OneDrive, Teams, and more. It provides a unified way to access data and automate workflows across Microsoft products.

### ✅ Pros of the Graph API

* **Unified API** – One API for multiple Microsoft 365 services.
* **Modern Authentication** – Supports OAuth 2.0 for secure authentication.
* **Extensive Data Access** – Access emails, calendars, files, users, groups, and more.
* **Integration-Friendly** – Works well with Python, C#, JavaScript, and other languages.
* **Cloud-First** – Optimized for Microsoft 365 cloud-based services.

### ❌ Cons of the Graph API

* **Rate Limits** – API calls are throttled, which can affect performance.
* **Complex Authentication** – OAuth 2.0 setup requires app registration in Azure.
* **Limited On-Prem Support** – Best suited for Microsoft 365, not legacy on-premises systems.
* **Learning Curve** – Requires understanding permissions and API structure.

***

# 🚀 Things You Can Do with Microsoft Graph API

## 1️⃣ Authenticate with Microsoft Graph API (OAuth 2.0)

### 🔹 Python Example

```python
import msal

client_id = "your-client-id"
client_secret = "your-client-secret"
tenant_id = "your-tenant-id"

authority = f"https://login.microsoftonline.com/{tenant_id}"
app = msal.ConfidentialClientApplication(client_id, client_secret, authority=authority)

token = app.acquire_token_for_client(scopes=["https://graph.microsoft.com/.default"])

if token:
    print("Access Token:", token["access_token"])
```

### 🔹 C# Example

```csharp
using Microsoft.Identity.Client;

var clientId = "your-client-id";
var clientSecret = "your-client-secret";
var tenantId = "your-tenant-id";

var confidentialClient = ConfidentialClientApplicationBuilder
    .Create(clientId)
    .WithClientSecret(clientSecret)
    .WithAuthority($"https://login.microsoftonline.com/{tenantId}")
    .Build();

var authResult = await confidentialClient.AcquireTokenForClient(new[] { "https://graph.microsoft.com/.default" }).ExecuteAsync();

Console.WriteLine($"Access Token: {authResult.AccessToken}");
```

***

## 2️⃣ Retrieve User Information

### 🔹 Python Example

```python
import requests

headers = {"Authorization": f"Bearer {token['access_token']}"}
response = requests.get("https://graph.microsoft.com/v1.0/me", headers=headers)
print(response.json())
```

### 🔹 C# Example

```csharp
using System.Net.Http;
using System.Net.Http.Headers;

var httpClient = new HttpClient();
httpClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", authResult.AccessToken);

var response = await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me");
var userData = await response.Content.ReadAsStringAsync();
Console.WriteLine(userData);
```

***

## List User Emails

### Python Example

```python
response = requests.get("https://graph.microsoft.com/v1.0/me/messages", headers=headers)
print(response.json())
```

### C# Example

```csharp
var response = await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/messages");
Console.WriteLine(await response.Content.ReadAsStringAsync());
```

***

## Send an Email via Microsoft Graph

### Python Example

```python
email_data = {
    "message": {
        "subject": "Hello from Graph API",
        "body": {"contentType": "Text", "content": "This is a test email."},
        "toRecipients": [{"emailAddress": {"address": "recipient@example.com"}}]
    }
}

requests.post("https://graph.microsoft.com/v1.0/me/sendMail", headers=headers, json=email_data)
```

### C# Example

```csharp
var emailContent = new
{
    message = new
    {
        subject = "Hello from Graph API",
        body = new { contentType = "Text", content = "This is a test email." },
        toRecipients = new[]
        {
            new { emailAddress = new { address = "recipient@example.com" } }
        }
    }
};

var content = new StringContent(JsonConvert.SerializeObject(emailContent), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/me/sendMail", content);
```

***

## Upload a File to OneDrive

### 🔹 Python Example

```python
file_path = "test.txt"
with open(file_path, "rb") as file:
    requests.put(f"https://graph.microsoft.com/v1.0/me/drive/root:/{file_path}:/content",
                 headers=headers, data=file)
```

### 🔹 C# Example

```csharp
var fileContent = File.ReadAllBytes("test.txt");
var byteContent = new ByteArrayContent(fileContent);
await httpClient.PutAsync("https://graph.microsoft.com/v1.0/me/drive/root:/test.txt:/content", byteContent);
```

***

## Get a List of SharePoint Files

```python
requests.get("https://graph.microsoft.com/v1.0/sites/{site-id}/drive/root/children", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/sites/{site-id}/drive/root/children");
```

***

## Manage Calendar Events

```python
requests.get("https://graph.microsoft.com/v1.0/me/calendar/events", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/calendar/events");
```

***

## Get Teams Information

```python
requests.get("https://graph.microsoft.com/v1.0/me/joinedTeams", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/joinedTeams");
```

***

## Manage SharePoint Lists

```python
requests.get("https://graph.microsoft.com/v1.0/sites/{site-id}/lists", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/sites/{site-id}/lists");
```

***

## Get Organization Users

```python
requests.get("https://graph.microsoft.com/v1.0/users", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/users");
```

***

## Manage Microsoft 365 Groups

Groups are essential for managing users in Teams, SharePoint, and Outlook.

### 🔹 Python Example: List Groups

```python
response = requests.get("https://graph.microsoft.com/v1.0/groups", headers=headers)
print(response.json())
```

### 🔹 C# Example: List Groups

```csharp
var response = await httpClient.GetAsync("https://graph.microsoft.com/v1.0/groups");
Console.WriteLine(await response.Content.ReadAsStringAsync());
```

***

## Add a User to a Group

### 🔹 Python Example

```python
group_id = "your-group-id"
user_id = "user-id"

requests.post(f"https://graph.microsoft.com/v1.0/groups/{group_id}/members/$ref",
              headers=headers, json={"@odata.id": f"https://graph.microsoft.com/v1.0/users/{user_id}"})
```

### 🔹 C# Example

```csharp
var groupId = "your-group-id";
var userId = "user-id";

var content = new StringContent(
    $"{{ "@odata.id": "https://graph.microsoft.com/v1.0/users/{userId}" }}",
    Encoding.UTF8, "application/json");

await httpClient.PostAsync($"https://graph.microsoft.com/v1.0/groups/{groupId}/members/$ref", content);
```

***

## Remove a User from a Group

```python
requests.delete(f"https://graph.microsoft.com/v1.0/groups/{group_id}/members/{user_id}/$ref", headers=headers)
```

```csharp
await httpClient.DeleteAsync($"https://graph.microsoft.com/v1.0/groups/{groupId}/members/{userId}/$ref");
```

***

## Create a New Team in Microsoft Teams

### 🔹 Python Example

```python
team_data = {
    "template@odata.bind": "https://graph.microsoft.com/v1.0/teamsTemplates('standard')",
    "displayName": "New Dev Team",
    "description": "Development team workspace"
}

requests.post("https://graph.microsoft.com/v1.0/teams", headers=headers, json=team_data)
```

### 🔹 C# Example

```csharp
var teamData = new
{
    template = new { "@odata.bind" = "https://graph.microsoft.com/v1.0/teamsTemplates('standard')" },
    displayName = "New Dev Team",
    description = "Development team workspace"
};

var content = new StringContent(JsonConvert.SerializeObject(teamData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/teams", content);
```

***

## Create a Task in Microsoft Planner

```python
task_data = {
    "planId": "your-plan-id",
    "title": "Finish Graph API Integration",
    "assignments": {}
}

requests.post("https://graph.microsoft.com/v1.0/planner/tasks", headers=headers, json=task_data)
```

```csharp
var taskData = new
{
    planId = "your-plan-id",
    title = "Finish Graph API Integration",
    assignments = new { }
};

var content = new StringContent(JsonConvert.SerializeObject(taskData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/planner/tasks", content);
```

***

### Get All OneDrive Shared Links

```python
requests.get("https://graph.microsoft.com/v1.0/me/drive/sharedWithMe", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/drive/sharedWithMe");
```

***

## Create a SharePoint Site

```python
site_data = {
    "displayName": "New SharePoint Site",
    "template": "STS#3"
}

requests.post("https://graph.microsoft.com/v1.0/sites", headers=headers, json=site_data)
```

```csharp
var siteData = new
{
    displayName = "New SharePoint Site",
    template = "STS#3"
};

var content = new StringContent(JsonConvert.SerializeObject(siteData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/sites", content);
```

***

## Retrieve User Presence in Microsoft Teams

```python
requests.get("https://graph.microsoft.com/v1.0/me/presence", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/presence");
```

***

## Manage OneNote Notebooks

```python
requests.get("https://graph.microsoft.com/v1.0/me/onenote/notebooks", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/onenote/notebooks");
```

***

## Send a Teams Channel Message

```python
message_data = {
    "body": {"content": "Hello from Graph API!"}
}

requests.post("https://graph.microsoft.com/v1.0/teams/{team-id}/channels/{channel-id}/messages", 
              headers=headers, json=message_data)
```

```csharp
var messageData = new
{
    body = new { content = "Hello from Graph API!" }
};

var content = new StringContent(JsonConvert.SerializeObject(messageData), Encoding.UTF8, "application/json");
await httpClient.PostAsync($"https://graph.microsoft.com/v1.0/teams/{teamId}/channels/{channelId}/messages", content);
```

***

## Retrieve Audit Logs from Microsoft 365

Microsoft Graph provides access to security logs for compliance and monitoring.

### 🔹 Python Example

```python
requests.get("https://graph.microsoft.com/v1.0/security/alerts", headers=headers)
```

### 🔹 C# Example

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/security/alerts");
```

***

## Manage Conditional Access Policies

Control security policies programmatically.

```python
requests.get("https://graph.microsoft.com/v1.0/identity/conditionalAccess/policies", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/identity/conditionalAccess/policies");
```

***

## Send Notifications to Users via Microsoft Graph

```python
notification_data = {
    "targetHostName": "outlook.office.com",
    "payload": {"message": "Security update required!"},
    "userId": "user-id"
}

requests.post("https://graph.microsoft.com/v1.0/me/sendActivityNotification", headers=headers, json=notification_data)
```

```csharp
var notificationData = new
{
    targetHostName = "outlook.office.com",
    payload = new { message = "Security update required!" },
    userId = "user-id"
};

var content = new StringContent(JsonConvert.SerializeObject(notificationData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/me/sendActivityNotification", content);
```

***

## Manage User Licenses in Microsoft 365

```python
requests.get("https://graph.microsoft.com/v1.0/users/{user-id}/licenseDetails", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/users/{user-id}/licenseDetails");
```

***

## Create an Azure AD Application Programmatically

```python
app_data = {
    "displayName": "My New App",
    "signInAudience": "AzureADMyOrg"
}

requests.post("https://graph.microsoft.com/v1.0/applications", headers=headers, json=app_data)
```

```csharp
var appData = new { displayName = "My New App", signInAudience = "AzureADMyOrg" };
var content = new StringContent(JsonConvert.SerializeObject(appData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/applications", content);
```

***

## Automate Role Assignments for Users

```python
requests.post(f"https://graph.microsoft.com/v1.0/users/{user_id}/assignLicense", headers=headers, json=license_data)
```

```csharp
await httpClient.PostAsync($"https://graph.microsoft.com/v1.0/users/{userId}/assignLicense", content);
```

***

## Get Microsoft 365 Service Health Reports

```python
requests.get("https://graph.microsoft.com/v1.0/admin/serviceAnnouncement/healthOverviews", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/admin/serviceAnnouncement/healthOverviews");
```

***

## Automate Meeting Room Booking

```python
event_data = {
    "subject": "Team Sync",
    "start": {"dateTime": "2025-02-20T10:00:00", "timeZone": "UTC"},
    "end": {"dateTime": "2025-02-20T11:00:00", "timeZone": "UTC"},
    "location": {"displayName": "Meeting Room 101"}
}

requests.post("https://graph.microsoft.com/v1.0/me/calendar/events", headers=headers, json=event_data)
```

```csharp
var eventData = new
{
    subject = "Team Sync",
    start = new { dateTime = "2025-02-20T10:00:00", timeZone = "UTC" },
    end = new { dateTime = "2025-02-20T11:00:00", timeZone = "UTC" },
    location = new { displayName = "Meeting Room 101" }
};

var content = new StringContent(JsonConvert.SerializeObject(eventData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/me/calendar/events", content);
```

***

## Archive a Microsoft Team

```python
requests.post("https://graph.microsoft.com/v1.0/teams/{team-id}/archive", headers=headers)
```

```csharp
await httpClient.PostAsync($"https://graph.microsoft.com/v1.0/teams/{teamId}/archive", null);
```

***

## Automate Data Retention Policies

```python
requests.get("https://graph.microsoft.com/v1.0/security/dataLossPreventionPolicies", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/security/dataLossPreventionPolicies");
```

***

## Retrieve and Manage Azure AD Sign-In Logs

Azure AD logs are crucial for security monitoring.

### 🔹 Python Example

```python
requests.get("https://graph.microsoft.com/v1.0/auditLogs/signIns", headers=headers)
```

### 🔹 C# Example

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/auditLogs/signIns");
```

***

## Manage Device Compliance Policies

```python
requests.get("https://graph.microsoft.com/v1.0/deviceManagement/deviceCompliancePolicies", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/deviceManagement/deviceCompliancePolicies");
```

***

## Reset a User's Password

```python
reset_data = {"passwordProfile": {"forceChangePasswordNextSignIn": True, "password": "NewSecurePass123!"}}

requests.patch("https://graph.microsoft.com/v1.0/users/{user-id}", headers=headers, json=reset_data)
```

```csharp
var resetData = new
{
    passwordProfile = new { forceChangePasswordNextSignIn = true, password = "NewSecurePass123!" }
};

var content = new StringContent(JsonConvert.SerializeObject(resetData), Encoding.UTF8, "application/json");
await httpClient.PatchAsync("https://graph.microsoft.com/v1.0/users/{user-id}", content);
```

***

## Manage Microsoft Defender Alerts

```python
requests.get("https://graph.microsoft.com/v1.0/security/alerts_v2", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/security/alerts_v2");
```

***

## Create a Guest User in Azure AD

```python
guest_data = {
    "accountEnabled": True,
    "displayName": "John Doe (Guest)",
    "mailNickname": "johndoe",
    "userPrincipalName": "johndoe@example.com",
    "userType": "Guest"
}

requests.post("https://graph.microsoft.com/v1.0/users", headers=headers, json=guest_data)
```

```csharp
var guestData = new
{
    accountEnabled = true,
    displayName = "John Doe (Guest)",
    mailNickname = "johndoe",
    userPrincipalName = "johndoe@example.com",
    userType = "Guest"
};

var content = new StringContent(JsonConvert.SerializeObject(guestData), Encoding.UTF8, "application/json");
await httpClient.PostAsync("https://graph.microsoft.com/v1.0/users", content);
```

***

## Retrieve Microsoft 365 Usage Reports

```python
requests.get("https://graph.microsoft.com/v1.0/reports/getOffice365ActiveUserDetail(period='D30')", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/reports/getOffice365ActiveUserDetail(period='D30')");
```

***

## Retrieve an Employee's Manager Information

```python
requests.get("https://graph.microsoft.com/v1.0/users/{user-id}/manager", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/users/{user-id}/manager");
```

***

## Manage Meeting Room Reservations

```python
room_list = requests.get("https://graph.microsoft.com/v1.0/me/findRooms", headers=headers)
print(room_list.json())
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/findRooms");
```

***

## Enable Multi-Factor Authentication (MFA) for a User

```python
mfa_data = {"strongAuthenticationMethods": [{"methodType": "PhoneAppNotification"}]}

requests.patch("https://graph.microsoft.com/v1.0/users/{user-id}", headers=headers, json=mfa_data)
```

```csharp
var mfaData = new { strongAuthenticationMethods = new[] { new { methodType = "PhoneAppNotification" } } };

var content = new StringContent(JsonConvert.SerializeObject(mfaData), Encoding.UTF8, "application/json");
await httpClient.PatchAsync("https://graph.microsoft.com/v1.0/users/{user-id}", content);
```

***

## Get a List of Recently Accessed Documents

```python
requests.get("https://graph.microsoft.com/v1.0/me/insights/used", headers=headers)
```

```csharp
await httpClient.GetAsync("https://graph.microsoft.com/v1.0/me/insights/used");
```

***
