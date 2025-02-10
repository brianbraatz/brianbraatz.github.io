---
title: Identity and Access Management-IAM in a Nutshell
description: "How the IAM Protocol Works: History, Relationship to Alternatives, and Examples"
slug: how-the-iam-protocol-works:-history-relationship-to-alternatives-and-10-code-examples
date: 2023-07-14
image: post/Articles/IMAGES/iamlogo.png
categories: 
tags:
  - Iam
  - Identity
  - Access
  - Management
  - Cybersecurity
  - Authentication
  - Authorization
  - Cloud
  - Security
  - Aws
  - Iam
  - Oauth
  - Rbac
  - Sso
draft: false
weight: 284
lastmod: 2025-02-10T15:28:44.545Z
---
<!--
# How the IAM Protocol Works: History, Relationship to Alternatives, and 10 Code Examples
-->

## Introduction

Ever wondered how companies **control who can access what** in their systems? That’s **IAM (Identity and Access Management)**—the backbone of **secure authentication and authorization** in cloud environments and enterprise networks.

<!--
In this article, we’ll cover:  

- The **history and evolution** of IAM.  
- How **IAM protocols work** and why they matter.  
- **IAM vs. other security models** like OAuth, RBAC, and Kerberos.  
- **10 real-world IAM code examples**.  
-->

***

## The History of IAM

IAM started as **simple username-password authentication**, but **modern IT environments** required **scalability, role-based access, and multi-factor authentication (MFA)**.

### **Key IAM Milestones**

| Year  | Development             | Notes                                |
| ----- | ----------------------- | ------------------------------------ |
| 1960s | Unix Permissions        | Simple file-based authentication     |
| 1990s | LDAP & Active Directory | Centralized user management          |
| 2000s | SAML, OAuth, OpenID     | Web authentication standards         |
| 2010s | AWS IAM, Zero Trust     | Cloud-based access control           |
| 2020s | Passwordless IAM        | Biometrics, FIDO2, and hardware keys |

💡 **Verdict:** IAM **evolved from simple logins to multi-layered security** for cloud and enterprise systems.

> **Further Reading:**
>
> * [IAM Wikipedia](https://en.wikipedia.org/wiki/Identity_and_access_management)
> * [AWS IAM Docs](https://docs.aws.amazon.com/IAM/latest/UserGuide/introduction.html)

***

## How IAM Works

IAM systems ensure **only authorized users** can access specific **applications, resources, or data**.

### **IAM Components**

1. **Identity Providers (IdP)** → Store and verify user identities (e.g., Azure AD, Okta).
2. **Authentication Mechanisms** → Validate users via **passwords, MFA, biometrics**.
3. **Authorization Policies** → Define **who can access what** (RBAC, ABAC, Zero Trust).
4. **Audit Logs & Compliance** → Track access and prevent unauthorized activities.

### **Step-by-Step IAM Flow**

1. **User logs in** → IAM system verifies identity (password, SSO, MFA).
2. **IAM grants an access token** → Based on **permissions and roles**.
3. **User accesses a resource** → System checks **authorization rules**.
4. **Audit logs track activity** → Ensures security and compliance.

This **ensures least privilege access**, reducing the risk of **data breaches and insider threats**.

***

## IAM vs. Other Security Models

| Feature                    | IAM   | OAuth | RBAC  | Kerberos |
| -------------------------- | ----- | ----- | ----- | -------- |
| **User Authentication**    | ✅ Yes | ✅ Yes | ❌ No  | ✅ Yes    |
| **Access Control**         | ✅ Yes | ❌ No  | ✅ Yes | ❌ No     |
| **Role-Based Permissions** | ✅ Yes | ❌ No  | ✅ Yes | ❌ No     |
| **Single Sign-On (SSO)**   | ✅ Yes | ✅ Yes | ❌ No  | ✅ Yes    |
| **Cloud & API Access**     | ✅ Yes | ✅ Yes | ❌ No  | ❌ No     |

💡 **Verdict:** IAM **combines multiple security principles**, while **OAuth, RBAC, and Kerberos focus on specific tasks**.

***

## 10 IAM Code Examples

### **1. Creating an AWS IAM User via CLI**

```bash
aws iam create-user --user-name dev_user
```

### **2. Assigning a Policy to an IAM User (AWS CLI)**

```bash
aws iam attach-user-policy --user-name dev_user --policy-arn arn:aws:iam::aws:policy/AmazonS3ReadOnlyAccess
```

### **3. Creating an IAM Role with JSON Policy (AWS CLI)**

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": "s3:*",
      "Resource": "*"
    }
  ]
}
```

### **4. Creating an IAM Role in Python (Boto3)**

```python
import boto3

iam = boto3.client('iam')
response = iam.create_role(
    RoleName='DeveloperRole',
    AssumeRolePolicyDocument='{"Version": "2012-10-17", "Statement": [{"Effect": "Allow", "Principal": {"Service": "ec2.amazonaws.com"}, "Action": "sts:AssumeRole"}]}'
)
print(response)
```

### **5. Checking IAM Policies in AWS CLI**

```bash
aws iam list-policies --scope AWS
```

### **6. Authenticating with OAuth (Python Flask Example)**

```python
from flask import Flask, redirect, request
from authlib.integrations.flask_client import OAuth

app = Flask(__name__)
oauth = OAuth(app)
oauth.register(
    "google",
    client_id="GOOGLE_CLIENT_ID",
    client_secret="GOOGLE_CLIENT_SECRET",
    authorize_url="https://accounts.google.com/o/oauth2/auth",
    access_token_url="https://oauth2.googleapis.com/token",
)

@app.route("/login")
def login():
    return oauth.google.authorize_redirect("http://localhost/callback")
```

### **7. Implementing Role-Based Access Control (RBAC) in Python**

```python
roles = {
    "admin": ["read", "write", "delete"],
    "user": ["read"]
}

def has_permission(role, action):
    return action in roles.get(role, [])

print(has_permission("admin", "delete"))  # True
print(has_permission("user", "delete"))  # False
```

### **8. Setting Up SSH Key-Based Authentication**

```bash
ssh-keygen -t rsa -b 2048 -C "user@example.com"
```

### **9. Checking IAM Access Logs (AWS CLI)**

```bash
aws iam list-access-keys --user-name dev_user
```

### **10. Enforcing Multi-Factor Authentication (MFA) in AWS IAM**

```bash
aws iam enable-mfa-device --user-name dev_user --serial-number arn:aws:iam::123456789012:mfa/dev_user --authentication-code-1 123456 --authentication-code-2 456789
```

***

## Key Takeaways

* **IAM is the foundation of modern access control and authentication.**
* **It supports user authentication, role-based access, and compliance tracking.**
* **IAM is essential for cloud security and API protection.**
* **Alternatives like OAuth, RBAC, and Kerberos complement IAM for specific use cases.**

***

## References

1. [IAM Wikipedia](https://en.wikipedia.org/wiki/Identity_and_access_management)
2. [AWS IAM Documentation](https://docs.aws.amazon.com/IAM/latest/UserGuide/introduction.html)
3. [OAuth vs IAM](https://oauth.net/articles/authentication/)
4. [RBAC vs ABAC](https://www.nist.gov/publications/role-based-access-control)
