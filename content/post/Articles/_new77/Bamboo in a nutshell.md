---
title: Bamboo in a nutshell
description: Bamboo in a nutshell
slug: bamboo-in-a-nutshell
date: 2023-11-01
image: post/Articles/IMAGES/bamboologo.png
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Bamboo
  - CI\CD
  - Testing
  - Unit Testing
  - Container Registry
tags:
  - CICD
draft: false
weight: 332
categories_ref:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Bamboo
  - CI\CD
  - Testing
  - Unit Testing
  - Container Registry
lastmod: 2025-03-14T15:45:24.236Z
---
<!-- 
# Bamboo in a Nutshell
-->

## What is Bamboo?

Bamboo is Atlassian’s Continuous Integration (CI) and Continuous Deployment (CD) tool that makes sure your code doesn’t break every time Chad from QA pushes an update.

It’s like Jenkins but with a fancier UI and built-in Jira integration.

DevOps teams love it because it automates builds, tests, and releases, reducing the need for manual deployment drama.

## A Brief History of Bamboo (Or How We Got Here)

Bamboo was launched by Atlassian in 2007, back when people still thought Subversion was the future.

Bamboo Integrates well with Atlassian’s ecosystem (Jira, Bitbucket, Confluence, and probably a few other tools nobody fully understands).

## Infrastructure as Code (IaC) - The Big Picture

In the old days, setting up a new server meant physically plugging in machines, installing software manually, and praying to the IT gods.

Infrastructure as Code (IaC) changed that by letting us define infrastructure with code, so we can automate provisioning, configuration, and management.

Bamboo plays  in the IaC world by integrating with Terraform, Ansible, and Kubernetes.

## How Does Bamboo Compare to Other Tools?

### 🏆 Bamboo vs. Jenkins

* **Bamboo**: Sleek UI, native Jira/Bitbucket integration, and better support.
* **Jenkins**: Free and open-source but requires tons of plugins and setup.

### 🏆 Bamboo vs. GitLab CI/CD

* **Bamboo**: Stronger Jira/Bitbucket integration.
* **GitLab CI/CD**: Built directly into GitLab, no separate tool required.

### 🏆 Bamboo vs. CircleCI

* **Bamboo**: Great for Atlassian users.
* **CircleCI**: Strong cloud-based CI/CD with fast execution.

## 10 Common Bamboo Examples

### 1. Setting Up a Simple Build Plan

```yaml
plan:
  key: MYPROJ-BUILD
  name: Build My Project
stages:
  - name: Build
    jobs:
      - name: Compile
        tasks:
          - script: mvn clean install
```

### 2. Running Unit Tests

```yaml
jobs:
  - name: Test
    tasks:
      - script: mvn test
```

### 3. Deploying to Staging

```yaml
jobs:
  - name: Deploy Staging
    tasks:
      - script: kubectl apply -f deployment.yaml
```

### 4. Checking Out Code from Git

```yaml
repositories:
  - name: MyRepo
    url: https://bitbucket.org/myrepo.git
```

### 5. Building a Docker Image

```yaml
jobs:
  - name: Build Docker Image
    tasks:
      - script: docker build -t my-app:latest .
```

### 6. Pushing Docker Image to Registry

```yaml
jobs:
  - name: Push Docker Image
    tasks:
      - script: docker push my-app:latest
```

### 7. Running a Static Code Analysis

```yaml
jobs:
  - name: Code Quality
    tasks:
      - script: sonar-scanner
```

### 8. Sending Notifications to Slack

```yaml
jobs:
  - name: Notify Slack
    tasks:
      - script: curl -X POST -H "Content-Type: application/json" -d '{"text":"Build completed!"}' https://hooks.slack.com/services/XXX
```

### 9. Deploying to Production

```yaml
jobs:
  - name: Deploy Prod
    tasks:
      - script: ansible-playbook deploy.yml
```

### 10. Running a Performance Test

```yaml
jobs:
  - name: Load Test
    tasks:
      - script: locust -f load_test.py
```

## Key Ideas

| Topic                  | Summary                                                                  |
| ---------------------- | ------------------------------------------------------------------------ |
| What is Bamboo?        | A CI/CD tool by Atlassian for automating builds, tests, and deployments. |
| History                | Launched in 2007, optimized for Atlassian tools.                         |
| Infrastructure as Code | Automates infrastructure deployment alongside code.                      |
| Comparison             | Competes with Jenkins, GitLab CI/CD, and CircleCI.                       |
| Common Usage           | Automating builds, testing, deployment, and notifications.               |

## Reference Links

* https://www.atlassian.com/software/bamboo
* https://confluence.atlassian.com/bamboo
* https://bitbucket.org/product
* https://www.jenkins.io
* https://docs.gitlab.com/ee/ci/
