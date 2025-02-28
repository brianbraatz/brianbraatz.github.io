---
title: Active Directory in a Nutshell
description: Code Examples in C# and Powershell
slug: active-directory-in-detail:-adsi-history-relationship-to-ldap-and-10-code-examples
date: 2020-11-02
image: post/Articles/IMAGES/activedirectory.png
categories:
  - Cloud
  - Azure Active Directory
  - CSharp
  - Powershell
  - Scripting
  - Biztalk
  - Microsoft Azure Cloud
  - Active Directory
tags:
  - Active
  - Directory
  - Ldap
  - Adsi
  - Windows
  - Server
  - Authentication
  - System
  - Administration
  - Network
  - Security
  - Directory
  - Services
draft: false
weight: 464
lastmod: 2025-02-28T04:16:08.488Z
---
<!--

# Active Directory in Detail: ADSI, History, Relationship to LDAP, and 10 Code Examples

## Introduction  

If you've ever dealt with **Windows servers**, chances are you've encountered **Active Directory (AD)**—Microsoft's **directory service** that controls **user authentication, security policies, and resource access** in enterprise environments.  

But have you ever wondered:  

- How does **Active Directory relate to LDAP**?  
- What is **ADSI (Active Directory Service Interfaces)**?  
- How can we interact with **Active Directory programmatically**?  

Well, you’re in for a treat! In this article, we’ll explore:  

- The **history and purpose** of Active Directory.  
- How it relates to **LDAP (Lightweight Directory Access Protocol)**.  
- **10 real-world code examples** for querying and managing AD.  

---
-->

## The History of Active Directory

Active Directory (AD) was introduced in **Windows 2000 Server** as a way to **centralize authentication and management** in Windows networks. Before AD, Microsoft used **NT Domains**, which were... well, **kind of a mess** compared to AD's hierarchical structure.

### **Why Was Active Directory Created?**

* **Simplify user and resource management** → Centralized **authentication and permissions**.
* **Scalability** → Allowed organizations to manage **millions of objects** in a network.
* **Integration with Internet Standards** → Added support for **LDAP, DNS, and Kerberos authentication**.

### **Key Features of Active Directory**

✅ **Hierarchical Structure** → Users, groups, computers, and policies are stored in a **tree-like structure**.\
✅ **Single Sign-On (SSO)** → Users log in once and access **multiple resources**.\
✅ **LDAP Integration** → AD **implements LDAP**, making it compatible with **non-Microsoft** systems.\
✅ **Group Policies (GPOs)** → Enforce **security and configuration settings** across an organization.

> **Further Reading:**
>
> * [Active Directory Wikipedia](https://en.wikipedia.org/wiki/Active_Directory)
> * [Microsoft Docs: Active Directory Overview](https://learn.microsoft.com/en-us/windows-server/identity/ad-ds/)

***

## Active Directory vs. LDAP

Active Directory is often **confused with LDAP**, but they are **not the same thing**.

| Feature            | Active Directory (AD)              | Lightweight Directory Access Protocol (LDAP)           |
| ------------------ | ---------------------------------- | ------------------------------------------------------ |
| **Developer**      | Microsoft                          | Open Standard (RFC 4511)                               |
| **Purpose**        | Directory service & authentication | Protocol for querying and modifying directory services |
| **Security Model** | Kerberos-based authentication      | No built-in authentication (relies on TLS/SSL)         |
| **Schema**         | Fixed, Windows-specific            | Flexible, open schema                                  |
| **Used By**        | Windows-based environments         | Multi-platform (Linux, macOS, Windows)                 |

💡 **Verdict:** LDAP is a **protocol**, while Active Directory is a **Microsoft directory service that uses LDAP**.

***

## Active Directory Code Examples

### **1. Query Active Directory Using PowerShell**

```powershell
Get-ADUser -Filter * | Select-Object Name, SamAccountName, Enabled
```

### **2. Create a New AD User (PowerShell)**

```powershell
New-ADUser -Name "John Doe" -GivenName "John" -Surname "Doe" -SamAccountName "jdoe" -UserPrincipalName "jdoe@domain.com" -Path "OU=Users,DC=domain,DC=com" -Enabled $true
```

### **3. Query Active Directory Using LDAP in Python**

```python
import ldap

conn = ldap.initialize("ldap://your-ad-server")
conn.simple_bind_s("admin@domain.com", "password")

search_base = "DC=domain,DC=com"
search_filter = "(objectClass=user)"
attributes = ["cn", "mail"]

result = conn.search_s(search_base, ldap.SCOPE_SUBTREE, search_filter, attributes)
for dn, entry in result:
    print(entry)
```

### **4. Query AD Using C# (System.DirectoryServices)**

```csharp
using System.DirectoryServices;

DirectorySearcher search = new DirectorySearcher(new DirectoryEntry("LDAP://domain.com"));
search.Filter = "(objectClass=user)";
foreach (SearchResult result in search.FindAll())
{
    Console.WriteLine(result.Properties["cn"][0]);
}
```

### **5. Check If a User Exists in AD (PowerShell)**

```powershell
Get-ADUser -Identity jdoe
```

### **6. Delete an AD User (PowerShell)**

```powershell
Remove-ADUser -Identity "jdoe" -Confirm:$false
```

### **7. Enable an AD User Account (PowerShell)**

```powershell
Enable-ADAccount -Identity "jdoe"
```

### **8. Disable an AD User Account (PowerShell)**

```powershell
Disable-ADAccount -Identity "jdoe"
```

### **9. Add a User to a Group (PowerShell)**

```powershell
Add-ADGroupMember -Identity "Administrators" -Members "jdoe"
```

### **10. Retrieve Group Memberships of a User (PowerShell)**

```powershell
Get-ADUser jdoe -Property MemberOf
```

***

## Key Takeaways

* **Active Directory is Microsoft's directory service, but it uses LDAP under the hood.**
* **ADSI (Active Directory Service Interfaces) allows programmatic access to AD in Windows environments.**
* **PowerShell, Python, and C# can all interact with Active Directory using LDAP or ADSI.**

***

## References

1. [Active Directory Wikipedia](https://en.wikipedia.org/wiki/Active_Directory)
2. [Microsoft Docs: Active Directory Overview](https://learn.microsoft.com/en-us/windows-server/identity/ad-ds/)
3. [LDAP Wikipedia](https://en.wikipedia.org/wiki/Lightweight_Directory_Access_Protocol)
4. [PowerShell AD Cmdlets](https://docs.microsoft.com/en-us/powershell/module/activedirectory/)
