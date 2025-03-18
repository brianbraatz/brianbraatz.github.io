---
title: Infrastructure as Code (IaC) Demystified
description: Thoughts on Infrastructure as Code..,,,
slug: infrastructure-as-code-iac-demystified
date: 2020-11-18
image: post/Articles/IMAGES/gustave_caillebotte_young_man_at_his_window_1876-Clipped.png
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Terraform
  - AWS CloudFormation
  - Google Cloud Deployment Manager
  - Azure Bicep
  - Ansible
  - Pulumi
  - Virtual Machines
tags:
  - Infrastructure
  - As
  - Code
  - IaC
  - Terraform
  - CloudFormation
  - CI/CD
  - DevOps
draft: false
weight: 40
categories_ref:
  - DevOps
  - Infrastructure as Code-IAC
  - Terraform
  - AWS CloudFormation
  - Google Cloud Deployment Manager
  - Azure Bicep
  - Ansible
  - Pulumi
  - Virtual Machines
slug_calculated: https://brianbraatz.github.io/p/infrastructure-as-code-iac-demystified
lastmod: 2025-03-14T16:40:21.660Z
---
<!-- 
post/Articles/IMAGES/50.jpg
# Infrastructure as Code (IaC) Demystified

-->

***

![](/post/Articles/IMAGES/gustave_caillebotte_young_man_at_his_window_1876.jpeg)

[Young Man at His Window - Gustave Caillebotte ](https://en.wikipedia.org/wiki/Young_Man_at_His_Window)

[Gustave Caillebotte](https://en.wikipedia.org/wiki/Gustave_Caillebotte)

***

## A Brief History of IaC

Once upon a time, in the wild west of software development, deploying infrastructure was a manual nightmare.

Engineers had to physically install servers, configure networks, and pray that nothing caught fire (literally and figuratively).

Then came virtualization, cloud computing, and finally, **Infrastructure as Code (IaC)**—a game-changer.

IaC emerged as an extension of **Continuous Integration (CI) and Continuous Delivery (CD)**.

If CI/CD is about automating software deployments, IaC is about automating the **infrastructure** that software runs on.

With IaC, you can define infrastructure using code, making deployments **repeatable, scalable, and not dependent on someone’s memory**.

## The Motivation Behind IaC

Why did we invent IaC? Because **humans are bad at doing things manually**, and automation is king. Key benefits include:

* **Consistency** – No more “it works on my server” excuses.
* **Speed** – Deploying infrastructure takes minutes, not weeks.
* **Version Control** – Infrastructure changes are tracked just like software.
* **Scalability** – Easily spin up hundreds of servers without breaking a sweat.

## The IaC Technology Stack: What’s in the Toolbox?

Here are some of the most popular tools in the IaC world:

* **Terraform** – The Swiss army knife of IaC. Works with almost every cloud provider.
* **AWS CloudFormation** – Amazon’s homegrown IaC tool.
* **Google Cloud Deployment Manager** – Google’s take on IaC.
* **Azure Bicep** – Microsoft’s new IaC language for Azure.
* **Ansible** – Configuration management + some IaC goodness.
* **Pulumi** – Write IaC using real programming languages (Python, JavaScript, etc.).

## IaC in Action: Setting Up Infrastructure for Different Projects

Here are some common ways IaC is used:

### 1. Deploying a Web App on AWS with Terraform

```hcl
provider "aws" {
  region = "us-east-1"
}

resource "aws_instance" "web" {
  ami           = "ami-123456"
  instance_type = "t2.micro"
}
```

### 2. Creating an Azure Virtual Machine with Bicep

```bicep
resource myVM 'Microsoft.Compute/virtualMachines@2021-07-01' = {
  name: 'myVM'
  location: resourceGroup().location
  properties: {
    hardwareProfile: {
      vmSize: 'Standard_D2s_v3'
    }
  }
}
```

### 3. Google Cloud Infrastructure with Deployment Manager

```yaml
resources:
  - name: my-vm
    type: compute.v1.instance
    properties:
      zone: us-central1-a
      machineType: zones/us-central1-a/machineTypes/n1-standard-1
```

## Comparing IaC Technologies

| Feature             | Terraform | AWS CloudFormation | Google Deployment Manager | Azure Bicep  |
| ------------------- | --------- | ------------------ | ------------------------- | ------------ |
| Cloud-Agnostic?     | ✅ Yes     | ❌ AWS Only         | ❌ Google Cloud Only       | ❌ Azure Only |
| Language            | HCL       | JSON/YAML          | YAML                      | Bicep        |
| Learning Curve      | Medium    | Medium             | Medium                    | Low          |
| Multi-Cloud Support | ✅ Yes     | ❌ No               | ❌ No                      | ❌ No         |

## Wrapping Up

IaC is the backbone of modern DevOps. Whether you use Terraform, CloudFormation, or Bicep, **automating your infrastructure saves time, reduces errors, and makes deployments way less painful**. So go forth and code your infrastructure—because nobody likes clicking buttons in a web console all day!

## Key Takeaways

* **IaC automates infrastructure** using code, making deployments faster and more reliable.
* **It integrates with CI/CD** to automate the entire software lifecycle.
* **Terraform is the most versatile tool** since it works across multiple clouds.
* **Cloud-specific tools (CloudFormation, Deployment Manager, Bicep) are great** if you’re locked into one provider.

## References

* [Infrastructure as Code on Wikipedia](https://en.wikipedia.org/wiki/Infrastructure_as_code)
* [Terraform](https://www.terraform.io/)
* [AWS CloudFormation](https://aws.amazon.com/cloudformation/)
* [Google Cloud Deployment Manager](https://cloud.google.com/deployment-manager)
* [Azure Bicep](https://learn.microsoft.com/en-us/azure/azure-resource-manager/bicep/)
