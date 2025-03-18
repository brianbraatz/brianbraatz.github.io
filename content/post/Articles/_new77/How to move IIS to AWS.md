---
title: Move a legacy IIS app to the AWS Cloud
description: with Beanstalk, App runner and docker
slug: how-to-move-a-legacy-iis-app-to-the-aws-cloud
date: 2024-01-14
image: post/Articles/IMAGES/iistoaws.png
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - IIS
  - Amazon App Runner
  - AWS Elastic Beanstalk
  - CI\CD
  - AWS Elastic Container Registry
  - Container Registry
tags: 
draft: false
weight: 51
categories_ref:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - IIS
  - Amazon App Runner
  - AWS Elastic Beanstalk
  - CI\CD
  - AWS Elastic Container Registry
  - Container Registry
slug_calculated: https://brianbraatz.github.io/p/how-to-move-a-legacy-iis-app-to-the-aws-cloud
lastmod: 2025-03-14T16:40:32.010Z
---
<!-- 
# How to Move a legacy IIS app to the AWS Cloud with Beanstalk, App runner and docker

## A Brief History of IIS and the Cloud Wars

Ah, IIS. The web server that powered countless enterprise apps, intranet portals, and that one weird HR site that only worked in Internet Explorer. Microsoft’s **Internet Information Services (IIS)** has been around since the '90s, back when Windows NT ruled the corporate world and the only "cloud" we knew about was the one storing our MP3s on Napster. Fast forward a couple of decades, and suddenly, on-premises servers are about as trendy as dial-up internet. Welcome to the cloud era!
-->

If you're still running an IIS app on some forgotten rack server in a basement, it’s time to bring it into the 21st century.

<!-- 
And what better way than by moving it to **AWS**? In this article, we’ll look at three ways to do it: **Elastic Beanstalk, App Runner, and Docker**. And, of course, we’ll throw in some code samples because what’s an article without some copy-paste magic?
--->

## Why Move IIS Apps to AWS?

Before we jump into how, let’s talk about **why**:

* **No more "server room" that doubles as a storage closet.** AWS takes care of infrastructure.
* **Scalability.** Your app can handle Black Friday traffic spikes without breaking a sweat.
* **Security.** AWS is much better at security than "Steve from IT" who still uses "password123".
* **Cost Savings.** No more hardware replacements every few years.

## Option 1: Elastic Beanstalk – The "I Want AWS to Do Everything for Me" Approach

Elastic Beanstalk is AWS’s **Platform-as-a-Service (PaaS)**. It handles deployment, scaling, monitoring, and even updates. You just upload your app, sit back, and pretend you did something complex.

### Steps to Deploy an IIS App with Beanstalk

1. Package your IIS app (as a **ZIP** file or **Docker image**).
2. Create an Elastic Beanstalk environment.
3. Deploy your app.

Here's an example of deploying a **Dockerized** IIS app:

```sh
eb init -p docker my-iis-app
eb create my-iis-env
eb deploy
```

**Pros:**\
✔️ AWS handles everything – networking, load balancing, autoscaling.\
✔️ Supports multiple environments (dev, staging, production).

**Cons:**\
❌ Less flexibility if you need custom configurations.\
❌ May cost more if you don’t optimize properly.

## Option 2: App Runner – The "I Just Want to Run My App" Approach

AWS **App Runner** is a fully managed service that lets you **run containerized applications** without dealing with infrastructure. Think of it like AWS’s version of **Heroku**.

### Steps to Deploy with App Runner

1. Containerize your IIS app.
2. Push the image to **Amazon Elastic Container Registry (ECR)**.
3. Create an App Runner service.

Example:

```sh
aws apprunner create-service --service-name my-iis-app \
  --source-configuration 'ImageRepositoryType=ECR,ImageIdentifier=123456789.dkr.ecr.us-east-1.amazonaws.com/my-iis-app:latest'
```

**Pros:**\
✔️ Super simple setup.\
✔️ Auto-scaling without complex configs.

**Cons:**\
❌ Limited customization options.\
❌ Only supports containerized apps.

## Option 3: Docker on ECS – The "I Like Control (But Not Too Much)" Approach

If you want more control over how your app runs but don’t want to deal with servers, AWS **Elastic Container Service (ECS)** with **Fargate** is a great choice. It lets you run Docker containers without managing servers.

### Steps to Deploy with ECS

1. **Containerize your IIS app:**

```dockerfile
FROM mcr.microsoft.com/windows/servercore/iis
COPY ./my-app /inetpub/wwwroot
```

2. **Push to ECR:**

```sh
aws ecr create-repository --repository-name my-iis-app
docker tag my-iis-app:latest 123456789.dkr.ecr.us-east-1.amazonaws.com/my-iis-app:latest
docker push 123456789.dkr.ecr.us-east-1.amazonaws.com/my-iis-app:latest
```

3. **Deploy with ECS Fargate:**

```sh
aws ecs create-cluster --cluster-name my-iis-cluster
aws ecs create-service --cluster my-iis-cluster --service-name my-iis-app \
  --task-definition my-iis-task --launch-type FARGATE
```

**Pros:**\
✔️ Full control over environment variables, networking, logging.\
✔️ No need to manage VMs.

**Cons:**\
❌ Slightly more complex than Beanstalk or App Runner.\
❌ Requires more AWS knowledge.

## Beanstalk vs. App Runner vs. ECS: Which One to Choose?

| Feature     | Elastic Beanstalk | App Runner                | ECS (Fargate)               |
| ----------- | ----------------- | ------------------------- | --------------------------- |
| Ease of Use | ⭐⭐⭐⭐              | ⭐⭐⭐⭐⭐                     | ⭐⭐⭐                         |
| Flexibility | ⭐⭐⭐               | ⭐⭐                        | ⭐⭐⭐⭐⭐                       |
| Scaling     | ⭐⭐⭐⭐              | ⭐⭐⭐⭐⭐                     | ⭐⭐⭐⭐                        |
| Best For    | Traditional apps  | Simple containerized apps | High-control container apps |

## Final Thoughts

Moving an old IIS app to AWS isn’t as scary as it sounds.

Whether you choose **Beanstalk, App Runner, or ECS**, you’ll be saving yourself from a future of on-prem server nightmares.

Take that leap into the cloud, and maybe, just maybe, retire that ancient server that’s still running on Windows Server 2003.

:)

***

### Key Ideas

| Concept           | Summary                                                     |
| ----------------- | ----------------------------------------------------------- |
| Elastic Beanstalk | Easiest way to deploy IIS apps with AWS managing everything |
| App Runner        | Best for containerized apps with minimal setup              |
| ECS Fargate       | Best for full control over the deployment process           |

***

### Reference Links

* https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/Welcome.html
* https://docs.aws.amazon.com/apprunner/latest/dg/what-is-apprunner.html
* https://docs.aws.amazon.com/AmazonECS/latest/developerguide/Welcome.html
