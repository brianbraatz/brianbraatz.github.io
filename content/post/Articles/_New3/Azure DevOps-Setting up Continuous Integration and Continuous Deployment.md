---
title: Azure DevOps-Continuous Integration & Continuous Deployment
description: Tutorial-Cheatsheet for Setup of Azure CI\CD
slug: setting-up-ci-cd-in-azure-devops
date: 2023-09-12
image: post/Articles/IMAGES/azure.png
categories:
  - Cloud
  - Azure Cloud
  - DevOps
  - CI\CD
  - CSharp
  - DotNet
tags:
  - Azure
  - Devops
  - CICD
  - Continuous
  - Integration
  - Jenkins
  - Pipelines
  - Deployment
  - Automation
  - DevOps
  - Continuous
  - Deployment
  - Cloud
  - Azure
  - CSharp
  - DotNet
draft: false
weight: 55
categories_ref:
  - Cloud
  - Azure Cloud
  - DevOps
  - CI\CD
  - CSharp
  - DotNet
lastmod: 2025-03-14T15:45:19.574Z
---
# Setting up CI/CD in Azure DevOps

> "Why did the developer break up with Git?\
> Because it had too many *commit*ment issues!"

Alright today we’re setting up CI/CD in **Azure DevOps**—because manually deploying code is so 2010, and we have better things to do (like debugging why our code doesn’t work in production).

This is basically the **Azure DevOps** version of [this Jenkins CI/CD guide](https://brianbraatz.github.io/p/jenkins-cicd/), but we’re replacing Jenkins with **Azure Pipelines** because... well, we like suffering slightly less.

***

## What is CI/CD?

CI/CD is Continuous Integration and Continuous Deployment (or Delivery). It’s the magical process that ensures your code moves smoothly from your laptop (where it "works perfectly") to production (where it immediately catches fire).

* **Continuous Integration (CI)** → Developers push code changes frequently, and automation ensures everything is tested before merging.
* **Continuous Deployment (CD)** → Once the code is tested, it's automatically deployed, so you can focus on more important things, like why your API suddenly has a 500 error.

***

## Setting Up Azure DevOps

First, you need an **Azure DevOps** account. If you don’t have one, go to [Azure DevOps](https://dev.azure.com/) and sign up—it’s free for small teams, and Microsoft hasn’t yet figured out how to charge us for breathing.

### Step 1: Create a New Project

1. In **Azure DevOps**, click **New Project**.
2. Give it a name, like `"MyCoolProject"` or `"YetAnotherBuggyApp"`.
3. Choose **Git** for version control (because what else would you use, TFVC?!).

***

## Setting Up the CI/CD Pipeline

Azure DevOps uses **Azure Pipelines** for CI/CD, which is basically Microsoft’s version of Jenkins but with more YAML and fewer plugins.

### Step 2: Create a Build Pipeline

A build pipeline automates the compilation, testing, and artifact creation of your project.

4. Go to **Pipelines** > **New Pipeline**.
5. Choose **GitHub** (or **Azure Repos Git** if you’re fancy).
6. Select your repo and configure the pipeline using **YAML** (Yes, it's YAML—brace yourself).

#### Example YAML for a .NET Project

```yaml
trigger:
  - main

pool:
  vmImage: 'ubuntu-latest'

steps:
  - task: UseDotNet@2
    inputs:
      packageType: 'sdk'
      version: '7.x'
  
  - script: dotnet build --configuration Release
    displayName: 'Build Project'

  - script: dotnet test
    displayName: 'Run Tests'

  - task: PublishBuildArtifacts@1
    inputs:
      pathToPublish: 'bin/Release/net7.0'
      artifactName: 'drop'
```

This will:\
✅ Run the build on **Ubuntu** (because we like free things).\
✅ Install **.NET SDK 7.x**.\
✅ Build the project.\
✅ Run tests (so you can see them fail).\
✅ Publish the artifacts for deployment.

***

## Setting Up the Release Pipeline

The release pipeline deploys your app after a successful build.

### Step 3: Create a Release Pipeline

7. Go to **Releases** > **New Pipeline**.
8. Use the **Empty job** template (because we’re starting fresh).
9. Add an **Artifact** source (pick the `drop` artifact from your build pipeline).

### Step 4: Add Deployment Stage

10. Click **Stage 1** and rename it to something like `"Deploy to Staging"` (or `"Deploy and Pray"`).
11. Add a deployment task.
12. If deploying to **Azure App Service**, use the `Azure App Service Deploy` task.
13. Configure it with your **Azure subscription**, **App Service name**, and **artifact location**.

#### Example Deployment YAML

```yaml
stages:
- stage: Deploy
  jobs:
  - job: DeployJob
    pool:
      vmImage: 'ubuntu-latest'
    steps:
    - task: AzureRmWebAppDeployment@4
      inputs:
        azureSubscription: 'MyAzureSubscription'
        appType: 'webApp'
        appName: 'my-cool-app'
        package: '$(Build.ArtifactStagingDirectory)/drop'
```

***

## Adding a Deployment Trigger

Since we don’t want to manually trigger deployments like cavemen, let’s make it automatic:

14. Click on your release pipeline.
15. Add a trigger to deploy on successful builds.
16. Enable **Continuous Deployment trigger**.

***

## Running the CI/CD Pipeline

Now, push a change to your `main` branch, sit back, and watch:

17. **Build starts** → Installs dependencies, compiles code, and runs tests.
18. **Artifact is created** → This is your deployable package.
19. **Release starts** → Deploys the artifact to your server.
20. **Something breaks in production** → You debug, cry, fix, repeat.

***

## Summary of Key Ideas

* **CI/CD automates builds, testing, and deployments**.
* **Azure DevOps Pipelines** replaces Jenkins for CI/CD.
* **YAML-based Pipelines** handle the entire process.
* **Deployment is automated** so you can focus on more debugging.

***

## References

* [Azure DevOps Documentation](https://learn.microsoft.com/en-us/azure/devops/)
* [Azure Pipelines YAML Syntax](https://learn.microsoft.com/en-us/azure/devops/pipelines/yaml-schema)
* [The Original Jenkins CI/CD Guide](https://brianbraatz.github.io/p/jenkins-cicd/)
