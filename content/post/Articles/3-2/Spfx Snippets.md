---
title: SPFx Code Snippets
description: Collection of useful Spfx Snippets
slug: spfx-useful-code-examples
date: 2017-10-15
image: post/Articles/IMAGES/spfxlogo.png
categories:
  - SharePoint
  - Development
  - SPFx
tags:
  - SPFx
  - SharePoint
  - React
  - Microsoft
  - Graph
  - API
  - Power
  - Automate
draft: false
weight: 11
categories_ref:
  - SharePoint
  - Development
  - SPFx
slug_calculated: https://brianbraatz.github.io/p/spfx-useful-code-examples
lastmod: 2025-03-14T16:40:13.094Z
---
<!-- 
# **10 Useful Code Examples for SPFx Development** 🚀  

So, you're diving into **SharePoint Framework (SPFx)** development? Awesome! But let’s be honest—there’s nothing worse than **Googling the same code snippets over and over again**.  

No worries, my friend, because I’ve got **10 practical SPFx code snippets** that will save you **time, effort, and probably a little sanity**.  

Bookmark this, copy-paste like a pro, and let's make your SPFx development **a little less painful**.   -->

***

## **1️⃣ Fetch SharePoint List Items (The Classic)**

This is probably the first thing you'll ever need—getting data from a SharePoint list.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function getListItems() {
  const items = await sp.web.lists.getByTitle("MyList").items();
  console.log(items);
}
```

**🔥 Why it’s useful:**

* Fetches data from a SharePoint list **without REST headaches**.
* Uses **PnPJS**, making SharePoint API calls **way easier**.

***

## **2️⃣ Create a New List Item**

Need to add data to SharePoint? Here’s a clean way to do it.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function addListItem() {
  await sp.web.lists.getByTitle("MyList").items.add({
    Title: "New Item",
    Description: "Created via SPFx!"
  });
  console.log("Item added!");
}
```

**🔥 Why it’s useful:**

* Saves you from manually adding data.
* Works great in workflows and automated apps.

***

## **3️⃣ Get the Current User’s Info**

Want to personalize the experience? Get the current user's details easily.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function getCurrentUser() {
  const user = await sp.web.currentUser();
  console.log(user.Title, user.Email);
}
```

**🔥 Why it’s useful:**

* Useful for **personalized dashboards**.
* Helps control **access and permissions dynamically**.

***

## **4️⃣ Call Microsoft Graph API from SPFx**

Need access to Teams, Outlook, or OneDrive? **Graph API is your best friend**.

```typescript
import { MSGraphClient } from "@microsoft/sp-http";

async function getUserDetails(context) {
  const client = await context.msGraphClientFactory.getClient();
  const response = await client.api("/me").get();
  console.log(response);
}
```

**🔥 Why it’s useful:**

* Lets you fetch **user profile details, emails, and calendar events**.
* Works across **Microsoft 365 services**, not just SharePoint.

***

## **5️⃣ Add a Custom Command Button to SharePoint List**

Want a fancy button in your list toolbar? This is how you do it.

```typescript
import { BaseListViewCommandSet, Command } from "@microsoft/sp-listview-extensibility";

export default class MyCommandSet extends BaseListViewCommandSet {
  public onExecute(event: Command): void {
    if (event.itemId === "MY_COMMAND") {
      alert("Button clicked!");
    }
  }
}
```

**🔥 Why it’s useful:**

* Adds **custom actions** to SharePoint lists.
* Perfect for **workflow triggers and automation**.

***

## **6️⃣ Read a SharePoint File**

If your app works with documents, you’ll need this one.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function getFileContent() {
  const file = await sp.web.getFileByServerRelativeUrl("/Shared Documents/test.txt").getText();
  console.log(file);
}
```

**🔥 Why it’s useful:**

* Fetches **file content** easily.
* Works with **PDFs, Word, and Excel files** too.

***

## **7️⃣ Upload a File to a SharePoint Library**

Drag and drop? Nah, let’s do this with code.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function uploadFile(file) {
  await sp.web.getFolderByServerRelativeUrl("/Shared Documents").files.add(file.name, file, true);
  console.log("File uploaded!");
}
```

**🔥 Why it’s useful:**

* Automates **document management**.
* Works in **custom forms and workflows**.

***

## **8️⃣ Add a Web Part to a SharePoint Page**

Want to add a web part programmatically? Here’s how.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function addWebPart() {
  await sp.web.loadWebPart({
    serverRelativeUrl: "/sites/mySite/SitePages/myPage.aspx",
    webPartId: "my-webpart-id"
  });
}
```

**🔥 Why it’s useful:**

* Automates **web part deployment**.
* Useful in **site provisioning scripts**.

***

## **9️⃣ Check User Permissions on a SharePoint Site**

Security matters. Let’s check if the user can edit a site.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function checkPermissions() {
  const perms = await sp.web.getCurrentUserEffectivePermissions();
  console.log(perms);
}
```

**🔥 Why it’s useful:**

* Helps control **who can see or edit what**.
* Prevents **accidental access issues**.

***

## **🔟 Send an Email via Power Automate from SPFx**

Need to send emails? Instead of Graph API, let’s use **Power Automate**.

```typescript
import { HttpClient, HttpClientResponse } from "@microsoft/sp-http";

async function sendEmail(context, emailData) {
  const response = await context.httpClient.post(
    "https://prod-XX.westus.logic.azure.com/workflows/your-flow-id",
    HttpClient.configurations.v1,
    {
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(emailData)
    }
  );
  console.log("Email Sent!", response);
}
```

**🔥 Why it’s useful:**

* **No need for Graph API permissions**.
* Uses **Power Automate for automation**.

***

<!-- 
## **Final Thoughts**
SPFx is **powerful**, but finding the right code snippet can be **painful**. Hopefully, these 10 examples will **save you time, effort, and frustration**.  

Got any favorite SPFx tricks of your own? Drop them in the comments (or just keep them as your secret weapon—I won’t judge 😉).  

🚀 Want more deep dives into SPFx development? Let me know!  

---

## **🔹 Key Ideas**
| Topic | Summary |
|-------|---------|
| **SPFx Basics** | Code snippets to make SharePoint Framework development easier. |
| **Microsoft Graph API** | Fetch user data, Teams messages, and Outlook emails. |
| **Power Automate** | Send emails without Graph API permissions. |
| **File & List Management** | Read, write, and update SharePoint files and lists. |
| **Custom UI Actions** | Add buttons, command sets, and permissions checks. |

``` -->

<!-- 
# **10 More SPFx Code Examples You Need to Know** 🚀  

Alright, SPFx warriors, we’re back for **round two**!  

You loved the **first 10 SPFx snippets**, so here are **10 more** to make your SharePoint development **faster, smoother, and less painful**.  

Let’s **dive right in**—starting from **#11** so we keep the momentum going.  

--- -->

## **1️⃣1️⃣ Get SharePoint Site Details**

Want to grab some **basic info** about a SharePoint site?

```typescript
import { sp } from "@pnp/sp/presets/all";

async function getSiteDetails() {
  const site = await sp.web();
  console.log(site.Title, site.Url);
}
```

**🔥 Why it’s useful:**

* Gives you **site metadata** without API complexity.
* Great for **custom dashboards and reports**.

***

## **1️⃣2️⃣ Check If a User Belongs to a SharePoint Group**

Need to see if a user has access? Use this!

```typescript
import { sp } from "@pnp/sp/presets/all";

async function isUserInGroup(groupName) {
  const group = await sp.web.siteGroups.getByName(groupName).users();
  const currentUser = await sp.web.currentUser();
  return group.some(user => user.Email === currentUser.Email);
}
```

**🔥 Why it’s useful:**

* Controls **permissions dynamically**.
* Useful for **hiding/showing UI elements**.

***

## **1️⃣3️⃣ Add a Modern Dialog Box in SPFx**

Sometimes, you need a **clean popup** instead of alerts.

```typescript
import { Dialog } from "@microsoft/sp-dialog";

async function showDialog() {
  await Dialog.alert("Hello from SPFx!");
}
```

**🔥 Why it’s useful:**

* Looks **modern** and **user-friendly**.
* Replaces those **ugly browser alerts**.

***

## **1️⃣4️⃣ Cache API Calls for Better Performance**

Tired of slow API calls? Cache them in the browser.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function getCachedListItems() {
  const cacheKey = "myListData";
  let cachedData = sessionStorage.getItem(cacheKey);

  if (!cachedData) {
    const items = await sp.web.lists.getByTitle("MyList").items();
    sessionStorage.setItem(cacheKey, JSON.stringify(items));
    return items;
  }

  return JSON.parse(cachedData);
}
```

**🔥 Why it’s useful:**

* **Speeds up** your SPFx apps.
* **Reduces API calls**, saving bandwidth.

***

## **1️⃣5️⃣ Create a New SharePoint List via SPFx**

Automate list creation in **just a few lines**!

```typescript
import { sp } from "@pnp/sp/presets/all";

async function createList() {
  await sp.web.lists.add("NewList", "A custom list", 100);
  console.log("List created!");
}
```

**🔥 Why it’s useful:**

* Automates **site provisioning**.
* No need to manually create lists!

***

## **1️⃣6️⃣ Get User’s Manager Using Graph API**

Need to fetch someone’s boss? Use **Graph API**.

```typescript
import { MSGraphClient } from "@microsoft/sp-http";

async function getUserManager(context) {
  const client = await context.msGraphClientFactory.getClient();
  const response = await client.api("/me/manager").get();
  console.log(response.displayName);
}
```

**🔥 Why it’s useful:**

* Automates **approvals and workflows**.
* Great for **org charts** and **HR tools**.

***

## **1️⃣7️⃣ Add a Custom Action to the SharePoint Ribbon**

Let’s **spice up** the toolbar with a **custom action**.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function addRibbonButton() {
  await sp.web.userCustomActions.add({
    Title: "My Custom Action",
    Location: "CommandUI.Ribbon",
    ScriptSrc: "https://mycdn.com/script.js"
  });
}
```

**🔥 Why it’s useful:**

* Adds **custom buttons** to SharePoint UI.
* Helps **trigger scripts from the toolbar**.

***

## **1️⃣8️⃣ Update an Existing List Item in SharePoint**

Modify an item **without deleting and recreating it**.

```typescript
import { sp } from "@pnp/sp/presets/all";

async function updateListItem(itemId) {
  await sp.web.lists.getByTitle("MyList").items.getById(itemId).update({
    Title: "Updated Item"
  });
  console.log("Item updated!");
}
```

**🔥 Why it’s useful:**

* Saves **time and effort**.
* Essential for **data editing workflows**.

***

## **1️⃣9️⃣ Delete a List Item with Confirmation**

Be **extra careful** before deleting anything.

```typescript
import { sp } from "@pnp/sp/presets/all";
import { Dialog } from "@microsoft/sp-dialog";

async function deleteListItem(itemId) {
  const confirm = await Dialog.confirm("Are you sure?");
  if (confirm) {
    await sp.web.lists.getByTitle("MyList").items.getById(itemId).delete();
    console.log("Item deleted!");
  }
}
```

**🔥 Why it’s useful:**

* **Prevents accidental data loss**.
* Improves **user experience** with confirmation.

***

## **2️⃣0️⃣ Open a SharePoint Document in a New Tab**

Ever wanted to **force open** a document? Here’s how.

```typescript
function openDocument(url) {
  window.open(url, "_blank");
}
```

**🔥 Why it’s useful:**

* **Works with OneDrive and SharePoint**.
* Avoids annoying **same-tab navigation** issues.

***

<!-- 
## **Final Thoughts**
That’s **20 SPFx code snippets** in total (if you include [part 1](spfx-useful-code-examples)), and you’re now officially **an SPFx ninja**!  

**Which snippet was your favorite?** Got more tips? **Drop them in the comments!** 🚀  

Need a **deep dive** on any of these? Let me know! 😃  

---

## **🔹 Key Ideas**
| Topic | Summary |
|-------|---------|
| **SPFx Power Moves** | 10 more must-know code snippets for SharePoint Framework. |
| **Performance Optimization** | Cache API responses to speed up apps. |
| **Graph API** | Get user managers, permissions, and more. |
| **Custom Actions** | Add buttons and command sets to SharePoint UI. |
| **Automations** | Create lists, update items, and open documents dynamically. |

```
 -->
