---
title: How  Bamboo and Octopus Work Together
description: 
slug: understanding-bitbucket-bamboo-octopus
date: 2020-11-30
image: post/Articles/IMAGES/bambooandoctopus.png
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Terraform
  - CI\CD
tags:
  - Bitbucket
  - Bamboo
  - Octopus
  - CI/CD
  - Version
  - Control
  - Automation
  - Deployment
draft: false
weight: 214
categories_ref:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Terraform
  - CI\CD
slug_calculated: https://brianbraatz.github.io/p/understanding-bitbucket-bamboo-octopus
lastmod: 2025-03-18T22:47:31.980Z
---
<!-- 
# Understanding Bitbucket + Bamboo + Octopus, Their History and How They Relate to Each Other with Example Code

## Introduction

Alright, buckle up, because we’re about to take a ride through the magical world of **Bitbucket, Bamboo, and Octopus Deploy**—the ultimate power trio of modern software deployment. If you’ve ever wondered how these three work together like a rock band of DevOps, this article is for you. 🎸🔥

We’ll cover:

- The **history** of each tool (because knowing where they came from makes you sound smart in meetings 😎).
- How they **connect** and work together.
- Example **CI/CD pipelines** using these tools.
- Why Octopus Deploy is **not** a seafood restaurant. 🐙

Let’s dive in!
-->

***

## The History of Each Tool

### **Bitbucket 🏗️ – Git Repo Hosting by Atlassian**

Once upon a time (2008, to be exact), Bitbucket was born as a **Mercurial**-based repository hosting service.

But, since nobody outside of a few stubborn devs really cared about Mercurial, Atlassian acquired it in **2010** and gave it a **Git makeover**. Now, it’s one of the big Git repo hosting services, standing alongside GitHub and GitLab.

👉 **Think of Bitbucket as:** The cool storage locker where all your code lives before it gets deployed.

More on Bitbucket: https://bitbucket.org/

### **Bamboo 🎋 – CI/CD for the Atlassian World**

Atlassian created **Bamboo** as an alternative to Jenkins because, let’s be honest, Jenkins is great but looks like it belongs in Windows 95.

Bamboo was built to integrate smoothly with Bitbucket, Jira, and other Atlassian tools. It provides **CI/CD pipelines** with a slick UI and **deep integrations** with source control.

👉 **Think of Bamboo as:** The conveyor belt in your software factory, making sure your code gets tested and built correctly before it ships.

More on Bamboo: https://www.atlassian.com/software/bamboo

### **Octopus Deploy 🐙 – The Deployment Powerhouse**

Founded in **2012**, Octopus Deploy is an **independent deployment automation tool**. While Bitbucket and Bamboo handle the **coding and building**,

Octopus takes care of the actual **deployment**. It specializes in multi-environment releases (like Dev, Staging, Production) and supports **auto-rollback, variable scoping, and approvals**.

👉 **Think of Octopus as:** The expert delivery guy making sure your software package is delivered safely—whether to Kubernetes, AWS, or a bunch of VMs.

More on Octopus Deploy: https://octopus.com/

***

## How They Work Together

Here’s a high-level overview of how Bitbucket, Bamboo, and Octopus **collaborate** in a CI/CD pipeline:

1. **Bitbucket** – Developers push their code to a Bitbucket repository.
2. **Bamboo** – Detects the new code, runs automated tests, and builds the application.
3. **Octopus Deploy** – Takes the successfully built artifact and deploys it to environments like Staging or Production.

***

## CI/CD Example: Bitbucket + Bamboo + Octopus

### **1. Bitbucket Pipelines (Optional)**

If you’re not using Bamboo yet, you can have Bitbucket Pipelines handle initial builds.

```yaml
image: mcr.microsoft.com/dotnet/sdk:6.0

pipelines:
  default:
    - step:
        script:
          - dotnet build
          - dotnet test
```

### **2. Bamboo Build Plan**

In Bamboo:

1. Create a **new plan** and connect it to your **Bitbucket repo**.
2. Add a **build step**:
   * Use a **Docker container** or **build script**.
   * Run tests and package the app.
3. **Artifact Sharing** – Save the build output so Octopus Deploy can use it.

### **3. Octopus Deploy Process**

1. Define **Environments** (e.g., Dev, Staging, Prod).
2. Create a **Release Pipeline**:
   * Pulls build artifacts from Bamboo.
   * Deploys to **Kubernetes, AWS ECS, Azure, or on-prem servers**.
3. Set up **Approval Steps** if needed.

***

## **Key Ideas Table**

| Step           | Description                                                                    |
| -------------- | ------------------------------------------------------------------------------ |
| Bitbucket      | Git-based repo hosting from Atlassian                                          |
| Bamboo         | CI/CD automation and build pipelines                                           |
| Octopus Deploy | Deployment automation and release management                                   |
| Integration    | Bitbucket triggers Bamboo; Bamboo builds and sends artifacts to Octopus Deploy |
| Example Code   | Bitbucket Pipelines YAML, Bamboo Build Plan, Octopus Deployment                |

***

## **Reference Links**

* https://bitbucket.org/
* https://www.atlassian.com/software/bamboo
* https://octopus.com/
* https://www.atlassian.com/git/tutorials/what-is-version-control

***

## **Final Thoughts**

Bitbucket, Bamboo, and Octopus work **best together** like peanut butter, jelly, and bread. Bitbucket stores your code, Bamboo makes sure it actually works, and Octopus gets it where it needs to go. If you’re looking for a **solid, battle-tested CI/CD setup**, this trio is a fantastic choice.

Now go forth and automate your deployments like a **DevOps wizard!** 🧙‍♂️✨
