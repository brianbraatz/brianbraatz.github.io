---
title: Understanding How SSH Works
description: and How to Set It Up on Windows vs Linux
slug: understanding-sshx
date: 2017-01-19
image: post/Articles/IMAGES/ssh.png
categories:
  - SSH
  - Networking
  - Linux
  - Windows
  - Security
tags:
  - SSH
  - Networking
  - Linux
  - Windows
  - Security
  - Authentication
  - Encryption
draft: false
weight: 339
lastmod: 2025-03-03T02:19:57.338Z
---
SSH (Secure Shell) is the magical spell that allows you to securely log into remote computers like a hacker in the movies (minus the dramatic typing and green text on black screens).

It’s the backbone of secure remote access, enabling sysadmins, developers, and the occasional curious geek to control servers without leaving their comfy chairs.

But how does it work? And how do you set it up?

***

## What Is SSH?

Imagine you need to control a computer in another city.

You could drive there, break into the office, and use the keyboard directly. But that would be highly illegal (and inefficient).

Instead, you can use SSH to securely connect to it over the internet.

SSH is a cryptographic network protocol that lets you access and manage remote machines securely. It encrypts all communication, preventing hackers from eavesdropping like digital Peeping Toms.

It’s used for:

* Logging into remote servers
* Transferring files securely
* Running commands remotely
* Tunneling traffic (because sometimes, you need to be sneaky)

***

## How SSH Works

SSH is based on a client-server model.

1. You (the client) use an SSH client to connect to a remote machine (the server).
2. The client and server exchange cryptographic keys to establish a secure, encrypted connection.
3. Once authenticated, you can run commands on the remote machine as if you were sitting right in front of it.

The secret sauce? **Public-key cryptography.**

When you use SSH keys, you generate a key pair: a **public key** (which you place on the server) and a **private key** (which stays on your local machine).

The server uses the public key to verify that your private key matches, granting access without needing a password.

This is **way more secure** than using passwords because:

* Hackers can’t brute-force your key like they can with passwords.
* You don’t have to remember long, complicated passwords.
* It’s just cooler.

***

## Setting Up SSH on Windows vs Linux

### Setting Up SSH on Linux (The Easier Way)

Linux users, you’ve got it easy. SSH is usually pre-installed.

#### **1. Check if SSH is installed**

Run this command:

```bash
ssh -V
```

If you see a version number, congrats! You have SSH.

If not, install it:

```bash
sudo apt update && sudo apt install openssh-client -y  # Debian/Ubuntu
sudo dnf install openssh-clients -y  # Fedora
sudo pacman -S openssh  # Arch
```

#### **2. Generate SSH Keys**

To create a new key pair, run:

```bash
ssh-keygen -t rsa -b 4096
```

This will create a public and private key in `~/.ssh/`.

#### **3. Copy Your Public Key to the Server**

Replace `user@yourserver` with your actual server username and IP:

```bash
ssh-copy-id user@yourserver
```

If `ssh-copy-id` isn’t available, manually copy your key:

```bash
cat ~/.ssh/id_rsa.pub | ssh user@yourserver "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys"
```

#### **4. Connect to the Server**

```bash
ssh user@yourserver
```

Boom. You’re in.

***

### Setting Up SSH on Windows (Slightly More Annoying but Doable)

Windows didn’t natively support SSH for years, so you had to use third-party tools like PuTTY.

Thankfully, things have changed.

#### **1. Install OpenSSH (If Needed)**

Windows 10+ has OpenSSH built-in. Check if it's installed:

```powershell
Get-WindowsCapability -Online | Where-Object Name -like 'OpenSSH*'
```

If it’s not installed, add it:

```powershell
Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0
```

#### **2. Generate SSH Keys**

Run:

```powershell
ssh-keygen -t rsa -b 4096
```

The keys will be stored in `C:\Users\YourUser\.ssh\`.

#### **3. Copy Your Public Key to the Server**

Use this command:

```powershell
type $env:USERPROFILE\.ssh\id_rsa.pub | ssh user@yourserver "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys"
```

Or copy it manually and paste it into the server’s `~/.ssh/authorized_keys` file.

#### **4. Connect to the Server**

```powershell
ssh user@yourserver
```

If all went well, you should be logged in.

***

## SSH Tips and Tricks

* **Use SSH Config Files**: Create `~/.ssh/config` (Linux/macOS) or `C:\Users\YourUser\.ssh\config` (Windows) to store connection settings. Example:

  ```ini
  Host myserver
      HostName 192.168.1.100
      User myuser
      IdentityFile ~/.ssh/id_rsa
  ```

  Now you can connect just by typing:

  ```bash
  ssh myserver
  ```

* **Enable SSH Agent**: Store decrypted private keys in memory so you don’t have to type your passphrase every time:

  ```bash
  eval $(ssh-agent)
  ssh-add ~/.ssh/id_rsa
  ```

* **Use SSH Tunnels**: Forward a remote port to your local machine:

  ```bash
  ssh -L 8080:localhost:80 user@yourserver
  ```

  This lets you access a remote web server on your local port 8080.

***

## Conclusion

SSH is one of the most powerful tools in a sysadmin or developer’s toolkit.

It lets you securely control remote systems, transfer files, and even tunnel traffic like a networking ninja.

Setting it up is a breeze on Linux and only slightly annoying on Windows.

Now go forth and SSH like a pro!

***

## Key Ideas

| Concept          | Explanation                                 |
| ---------------- | ------------------------------------------- |
| What is SSH?     | A secure protocol for remote access         |
| How SSH Works    | Uses encryption and public-key cryptography |
| SSH on Linux     | Pre-installed, easy to set up               |
| SSH on Windows   | Requires OpenSSH, but works fine            |
| SSH Keys         | More secure than passwords                  |
| SSH Config Files | Simplifies connection management            |
| SSH Tunnels      | Allows port forwarding for remote access    |

***

## References

1. [OpenSSH Official Documentation](https://www.openssh.com/)
2. [SSH on Windows](https://docs.microsoft.com/en-us/windows-server/administration/openssh/openssh_overview)
3. [SSH Key Authentication](https://www.ssh.com/academy/ssh/key)

```
```
