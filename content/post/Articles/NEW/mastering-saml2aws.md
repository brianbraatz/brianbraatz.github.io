---
title: "SAML2AWS: The Best Dev Way to Authenticate to Amazon AWS Cloud"
description: How to use saml2aws to make cloud dev smoother
slug: mastering-saml2aws-the-best-way-to-authenticate-to-aws
date: 2024-06-19
image: post/Articles/IMAGES/31.jpg
categories: []
tags:
  - AWS
  - Authentication
  - SAML
  - Cloud Security
  - DevOps
  - CLoud
draft: false
weight: 101
lastmod: 2025-02-07T17:48:36.424Z
---
# Mastering SAML2AWS: The Best Way to Authenticate to AWS

<!-- 
## Introduction

If you work with AWS **but your company insists on using SAML authentication**, you've probably felt **the pain** of logging in manually through a browser every single time. 😩

Enter **SAML2AWS**, the ultimate CLI tool that makes AWS authentication **as smooth as butter**. 🧈

Today, we’re going to cover:
- **What SAML2AWS is**
- **Why it's better than the alternatives**
- **How to use it with plenty of code samples**

And of course, we’ll do it **without boring corporate-speak**. Let’s go! 🚀
-->

## What is SAML2AWS?

[SAML2AWS](https://github.com/Versent/saml2aws) is an **open-source CLI tool** that lets you authenticate to AWS via **SAML-based identity providers** (Okta, ADFS, OneLogin, PingFederate, etc.) and get temporary AWS credentials.

### Why is it Awesome?

* **No more logging into AWS manually** 🔑
* **Works with MFA (Multi-Factor Authentication)** 🔐
* **Fast & scriptable** (Use it in CI/CD pipelines!) ⚡
* **Cross-platform** (Windows, Mac, Linux—yes, even you, Windows users!) 🖥️

### Why is it Better than Other Methods?

| Method            | Pain Level | Why It Sucks                              |
| ----------------- | ---------- | ----------------------------------------- |
| **AWS Web Login** | 🔥🔥🔥🔥🔥 | Requires manual login every time          |
| **AWS SSO CLI**   | 🔥🔥🔥     | Requires config setup and AWS permissions |
| **SAML2AWS**      | ❄️         | Works instantly with your SAML provider   |

SAML2AWS is basically **the least annoying way to log into AWS with SAML**.

## Installation

### **Mac (Homebrew)**

```sh
brew install versent/taps/saml2aws
```

### **Linux**

```sh
curl -Lo saml2aws https://github.com/Versent/saml2aws/releases/latest/download/saml2aws-linux-amd64
chmod +x saml2aws
sudo mv saml2aws /usr/local/bin/
```

### **Windows (Scoop)**

```powershell
scoop install saml2aws
```

## Configuration

Before using SAML2AWS, **you need to configure it for your identity provider**.

### **Example: Configuring SAML2AWS for Okta**

```sh
saml2aws configure --idp okta
```

You'll be prompted for:

* **AWS Account Alias**
* **IDP Provider (Okta, ADFS, OneLogin, etc.)**
* **URL of your SAML provider**

Once configured, **logging into AWS is easy**!

## Logging into AWS with SAML2AWS

### **Standard Login**

```sh
saml2aws login
```

This will:

1. Open your SAML provider’s login prompt.
2. Authenticate using MFA (if required).
3. Generate temporary AWS credentials.

### **Login and Automatically Set AWS Environment Variables**

```sh
eval $(saml2aws login --exec-env)
```

Now you can run AWS CLI commands **without manually setting credentials**.

### **Login and Assume a Specific AWS Role**

```sh
saml2aws login --role arn:aws:iam::123456789012:role/PowerUser
```

### **Login Without MFA Prompt (if previously authenticated)**

```sh
saml2aws login --skip-mfa
```

### **Use in a Script for Automated AWS Auth**

```sh
#!/bin/bash

saml2aws login --quiet --role arn:aws:iam::123456789012:role/DevOps
aws s3 ls s3://my-bucket/
```

## Using SAML2AWS with AWS CLI

Once authenticated, SAML2AWS **stores credentials** in your AWS profile.

### **Example: Listing S3 Buckets After Login**

```sh
aws s3 ls
```

### **Use with a Specific Profile**

```sh
aws s3 ls --profile my-saml-profile
```

## Using SAML2AWS in CI/CD

If you need to **authenticate in a CI/CD pipeline**, you can do:

```sh
saml2aws login --password-env SAML_PASSWORD
```

Then set **SAML\_PASSWORD** as a CI/CD environment variable.

## Troubleshooting

| Issue                                 | Fix                                      |
| ------------------------------------- | ---------------------------------------- |
| **"Invalid SAML response"**           | Check if your SAML provider changed URLs |
| **"MFA prompt every time"**           | Use `--skip-mfa` or cache credentials    |
| **"AWS CLI not working after login"** | Run `eval $(saml2aws login --exec-env)`  |

## Key Ideas Table

| Concept           | Explanation                                    |
| ----------------- | ---------------------------------------------- |
| SAML2AWS          | CLI tool for logging into AWS via SAML         |
| AWS IAM Roles     | Assign permissions dynamically using roles     |
| MFA               | Multi-Factor Authentication for added security |
| AWS CLI           | Command-line interface for managing AWS        |
| CI/CD Integration | Automate AWS authentication in pipelines       |

## References

* https://github.com/Versent/saml2aws
* https://aws.amazon.com/iam/
* https://brew.sh/
* https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-sso.html
