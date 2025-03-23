---
title: Packer with Windows Images and Chocolatey
description: ""
slug: packer-windows-chocolatey
date: 2021-08-14
image: post/Articles/IMAGES/packer.png
categories:
  - DevOps
  - Automation
  - Infrastructure
tags:
  - Packer
  - Windows
  - Chocolatey
  - DevOps
  - Infrastructure as Code
draft: false
weight: 629
lastmod: 2025-03-21T04:15:26.988Z
---
# 🍫 More Packer Magic: Windows Images and Chocolatey Goodness

So you read [Packer in a Nutshell](../packer-in-a-nutshell/) and you're feeling spicy.\
So now you used Packer to build a sleek little Linux image with Nginx, tossed it in AWS, and you’re high-fiving yourself.

But now your boss walks in and says:

> "Hey, can you do this for a **Windows Server** image, with **Chocolatey** installed and pre-configured?"

<!-- 
And you panic.

Fear not, friend. We’re gonna tackle **Windows image baking** with **Packer** and **Chocolatey**, the Windows package manager that makes installing apps feel *slightly less painful than dental work*. -->

## Yes we can!

## ⚙️ What You’ll Need

Before we go full Windows wizardry, make sure you've got:

* **Packer** installed
* Access to AWS (or another builder platform)
* A base **Windows AMI** (e.g., Windows Server 2019)
* **RDP access** for testing (because SSH ain’t gonna cut it)
* 🍫 A deep love for Chocolatey

***

## 🪟 Example: Packer + Windows + Chocolatey on AWS

```hcl
# windows-choco.pkr.hcl

variable "aws_region" {
  default = "us-east-1"
}

source "amazon-ebs" "windows" {
  region             = var.aws_region
  source_ami_filter {
    filters = {
      name                = "Windows_Server-2019-English-Full-Base-*"
      virtualization-type = "hvm"
    }
    owners      = ["801119661308"] # Microsoft
    most_recent = true
  }
  instance_type      = "t3.medium"
  ami_name           = "windows-choco-{{timestamp}}"
  user_data_file     = "setup-choco.ps1"
  communicator       = "winrm"
  winrm_username     = "Administrator"
  winrm_insecure     = true
  winrm_use_ssl      = false
  winrm_timeout      = "10m"
}

build {
  sources = ["source.amazon-ebs.windows"]
}
```

### 💡 What’s Going On Here?

* We use the official **Windows Server 2019** base image.
* `user_data_file` runs a PowerShell script to install Chocolatey.
* We use **WinRM** for remote access, because Windows doesn’t speak SSH (it barely speaks English half the time).

***

## 🍫 Chocolatey Installer Script (setup-choco.ps1)

```powershell
# setup-choco.ps1

# Disable IE Enhanced Security (because it breaks installs)
Set-ItemProperty -Path "HKLM:\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap" -Name IEHarden -Value 0

# Download and install Chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12
iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# Install some stuff via Chocolatey
choco install -y googlechrome notepadplusplus git
```

💥 That script installs:

* Google Chrome (because IE is a cursed relic)
* Notepad++ (for actual editing)
* Git (because Git is life)

***

## 🔨 Build the Image

```bash
packer init .
packer validate windows-choco.pkr.hcl
packer build windows-choco.pkr.hcl
```

***

## 🧪 Testing It

Once it builds successfully, spin up an instance of your new image.

* Use RDP to connect.
* Login as `Administrator`.
* Check that Chocolatey and all your apps are installed.
* Bask in your glorious Windows DevOps power. ⚡

***

## 🧠 Extra Tips for Windows + Packer

| Tip                                        | Why It Matters                                                        |
| ------------------------------------------ | --------------------------------------------------------------------- |
| Use `t3.medium` or larger                  | Windows is thicc. Tiny instances will crawl.                          |
| Enable WinRM correctly                     | Packer needs to connect during provisioning.                          |
| Use `user_data_file` for boot-time scripts | Especially useful when you need Chocolatey ready before anything else |
| Disable Windows Defender during build      | It can slow things down a lot (re-enable after if needed)             |
| Snapshot your images                       | Store your goldens safely and use them in Terraform                   |

***

## 🤯 Bonus: Adding VS Code and Node.js

Just add to the PowerShell script:

```powershell
choco install -y vscode nodejs
```

And boom. You’ve got a dev-ready Windows machine image baked, seasoned, and served.

***

## 🧑‍🍳 The Bake-Off Continues...

Packer isn’t just a Linux bro — it’s fully bilingual. You can use it to create reliable, repeatable **Windows images**, automate your dev environments, and even standardize your infrastructure across teams.

Chocolatey + Packer is like peanut butter + jelly — or maybe more like **PowerShell + sanity**, which is rare but beautiful.

***

## 🔑 Key Ideas

| Concept                 | Summary                                                     |
| ----------------------- | ----------------------------------------------------------- |
| Packer supports Windows | Fully works with WinRM and PowerShell                       |
| Chocolatey integration  | Install apps easily during image build                      |
| PowerShell provisioning | Scripts can do setup, install, and config                   |
| Use case                | Great for dev VMs, remote workers, CI test images           |
| Extra tools             | Git, Chrome, Notepad++, VSCode, Node.js, and more via Choco |

***

🪄 Now go forth, build Windows images, and automate your life like it’s 2099.

Next up: Want to build Windows images **locally** with VirtualBox or **pipe them into Vagrant**? Stay tuned, it gets weirder (and cooler).

***

**Click the button below to copy this full article into your notes or dev docs:**

```markdown
<!-- Copy this full markdown content block from here to your site or editor -->
```

```
```
