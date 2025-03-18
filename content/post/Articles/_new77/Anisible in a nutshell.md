---
title: Ansible in a nutshell
description: Ansible in a nutshell
slug: ansible-in-a-nutshell
date: 2019-11-11
image: post/Articles/IMAGES/ansible.png
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Ansible
  - CI\CD
tags: 
draft: false
weight: 219
categories_ref:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Ansible
  - CI\CD
slug_calculated: https://brianbraatz.github.io/p/ansible-in-a-nutshell
lastmod: 2025-03-14T16:40:31.399Z
---
# Ansible in a Nutshell

## What is Ansible?

Ansible is like a magic wand for system administrators.

You write some YAML, wave your SSH key, and boom!

Servers are configured, applications are deployed, and your DevOps team thinks you’re a genius (until they see your YAML indentation errors). (hahaha)

## A Brief History of Ansible (Or How We Got Here)

Ansible was created in 2012 by Michael DeHaan, who must have been tired of writing the same Bash scripts over and over.

Instead of spending hours debugging shell scripts, he decided to make a tool that could automate infrastructure easily, with no agents or extra dependencies. Thus, Ansible was born!

(Software engineers had tedious things - so we invent things.. sometimes people use our things and think our things are too tedious... so they invent more things.. the world is full of things... and this is why....)

In 2015, Red Hat acquired Ansible, proving that if you make a good enough open-source tool, eventually someone will throw money at you.

## Infrastructure as Code (IaC) - The Big Picture

If you’ve ever heard the phrase “treat your servers like cattle, not pets,” then you already get the gist of Infrastructure as Code. Instead of manually configuring each server like a delicate bonsai tree, IaC lets you define everything in code so that it can be automated, version-controlled, and replicated.

Ansible takes this a step further by making sure you don’t need a Ph.D. in YAML just to get things working (though it helps).

## How Does Ansible Compare to Other Tools?

### 🏆 Ansible vs. Puppet

* **Ansible**: Agentless, uses SSH, simple to set up.
* **Puppet**: Uses agents, requires a master node, more complex but powerful.

### 🏆 Ansible vs. Chef

* **Ansible**: Uses YAML (easy to read, but can be annoying to debug).
* **Chef**: Uses Ruby (which means, if you’re not a Ruby dev, good luck).

### 🏆 Ansible vs. Terraform

* **Ansible**: Great for configuration management and app deployment.
* **Terraform**: Great for infrastructure provisioning (they actually work well together!).

## Common Ansible Examples

### 1. Installing Packages

```yaml
- name: Install Nginx
  hosts: web_servers
  tasks:
    - name: Install Nginx
      apt:
        name: nginx
        state: present
```

### 2. Starting a Service

```yaml
- name: Start and Enable Nginx
  hosts: web_servers
  tasks:
    - name: Start Nginx
      service:
        name: nginx
        state: started
        enabled: yes
```

### 3. Copying a File

```yaml
- name: Copy Config File
  hosts: all
  tasks:
    - name: Copy nginx.conf
      copy:
        src: ./nginx.conf
        dest: /etc/nginx/nginx.conf
```

### 4. Running a Shell Command

```yaml
- name: Check Disk Usage
  hosts: all
  tasks:
    - name: Run df command
      command: df -h
```

### 5. Creating a User

```yaml
- name: Create a User
  hosts: all
  tasks:
    - name: Add User
      user:
        name: devops
        state: present
```

### 6. Deploying an App

```yaml
- name: Deploy Web App
  hosts: web_servers
  tasks:
    - name: Pull latest code
      git:
        repo: 'https://github.com/example/repo.git'
        dest: /var/www/html
```

### 7. Managing Firewall Rules

```yaml
- name: Open Port 80
  hosts: all
  tasks:
    - name: Allow HTTP traffic
      ufw:
        rule: allow
        port: 80
        proto: tcp
```

### 8. Configuring a Database

```yaml
- name: Configure MySQL
  hosts: db_servers
  tasks:
    - name: Ensure MySQL is installed
      apt:
        name: mysql-server
        state: present
```

### 9. Using Variables

```yaml
- name: Install Custom Package
  hosts: all
  vars:
    package_name: htop
  tasks:
    - name: Install package
      apt:
        name: "{{ package_name }}"
        state: present
```

### 10. Looping Over Tasks

```yaml
- name: Install Multiple Packages
  hosts: all
  tasks:
    - name: Install common tools
      apt:
        name: [vim, curl, git]
        state: present
```

## Key Ideas

| Topic                  | Summary                                                                |
| ---------------------- | ---------------------------------------------------------------------- |
| What is Ansible?       | A simple, agentless automation tool using YAML.                        |
| History                | Created in 2012, acquired by Red Hat in 2015.                          |
| Infrastructure as Code | Managing servers using code instead of manual setup.                   |
| Comparison             | Competes with Puppet, Chef, Terraform (but works well with Terraform). |
| Common Usage           | Configuration management, deployment, provisioning.                    |

## Reference Links

* https://www.ansible.com/
* https://docs.ansible.com/
* https://github.com/ansible/ansible
* https://www.redhat.com/en/technologies/management/ansible
