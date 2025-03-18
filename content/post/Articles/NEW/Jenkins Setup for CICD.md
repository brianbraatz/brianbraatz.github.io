---
title: Jenkins Setup-Continuous Integration and Deployment CICD
description: Includes C# sample with unit tests and docker deployment and setup
slug: jenkins-cicd
date: 2020-01-05
image: post/Articles/IMAGES/jenkinsloho.png
categories:
  - Jenkins
  - CI\CD
  - Cloud
  - DevOps
  - Testing
tags:
  - Jenkins
  - ContinuousIntegration
  - ContinuousDeployment
  - CICD
  - Automation
  - Docker
  - Unit
  - testing
  - Microservices
  - Git
  - Docker
  - CSharp
  - Microservice
  - DevOps
draft: false
weight: 61
categories_ref:
  - Jenkins
  - CI\CD
  - Cloud
  - DevOps
  - Testing
slug_calculated: https://brianbraatz.github.io/p/jenkins-cicd
lastmod: 2025-03-14T16:40:21.704Z
---
## A Brief History of Jenkins

Jenkins, the beloved but sometimes infuriating CI/CD tool, was born out of necessity.

Back in 2004, Kohsuke Kawaguchi, a Sun Microsystems engineer, was tired of breaking builds and getting yelled at by his team.

So, like any good developer, he built a tool to automate the pain away.

This tool was originally called **Hudson**, but after some corporate drama with Oracle, it was forked and renamed **Jenkins** in 2011.

Since then, it has become the go-to tool for continuous integration and deployment.

(Fun note- Hudson was the version I learned initially :) - Yes.. I am old )

## What is Continuous Integration and Continuous Deployment (CI/CD)?

Continuous Integration (CI) is like having a really strict but fair coding buddy who immediately runs tests whenever you push code.

If your code fails, it tells you instantly, saving you from merging a disaster into production.

Continuous Deployment (CD) takes it a step further and says, “Hey, your code passed?

Let’s deploy it to production automatically.” This ensures that software is always in a deployable state, with minimal human intervention.

In short: **CI catches problems early, and CD ensures you’re always ready to ship.**

## Setting Up a Sample Microservice

Before Jenkins can do anything useful, we need something to build and test.

Let's create a simple C# microservice that exposes a REST API to divide two numbers. If you try to divide by zero, it will yell at you—because math.

### C# Microservice Code

```csharp
using Microsoft.AspNetCore.Mvc;

[ApiController]
[Route("api/[controller]")]
public class MathController : ControllerBase
{
    [HttpGet("divide")]
    public IActionResult Divide(double a, double b)
    {
        if (b == 0)
        {
            return BadRequest("Division by zero is not allowed!");
        }
        return Ok(a / b);
    }
}
```

### Unit Tests for the Microservice

```csharp
using Xunit;
using Microsoft.AspNetCore.Mvc;

public class MathTests
{
    [Fact]
    public void Divide_ValidNumbers_ReturnsCorrectResult()
    {
        var controller = new MathController();
        var result = controller.Divide(10, 2) as OkObjectResult;

        Assert.NotNull(result);
        Assert.Equal(5.0, result.Value);
    }

    [Fact]
    public void Divide_ByZero_ReturnsBadRequest()
    {
        var controller = new MathController();
        var result = controller.Divide(10, 0) as BadRequestObjectResult;

        Assert.NotNull(result);
        Assert.Equal("Division by zero is not allowed!", result.Value);
    }
}
```

### Running Unit Tests from the Command Line

```sh
dotnet test
```

If all goes well, you’ll see some green checkmarks. If not, well, time to debug.

## Configuring Jenkins to Monitor Git and Build Code

1. Install Jenkins and necessary plugins (`Git`, `Pipeline`, `Docker Pipeline`).
2. Set up a new **Freestyle Project** or a **Pipeline Project**.
3. Link it to your Git repository.
4. Configure Jenkins to trigger a build when new code is pushed.

### Example Jenkinsfile for CI/CD

```groovy
pipeline {
    agent any

    stages {
        stage('Clone Repository') {
            steps {
                git 'https://github.com/your-repo/math-service.git'
            }
        }

        stage('Build') {
            steps {
                sh 'dotnet build'
            }
        }

        stage('Test') {
            steps {
                sh 'dotnet test'
            }
        }

        stage('Dockerize and Deploy') {
            steps {
                sh 'docker build -t math-service .'
                sh 'docker run -d -p 5000:80 math-service'
            }
        }

        stage('Post-Deployment Test') {
            steps {
                sh './test-api.sh'
            }
        }
    }
}
```

### What Happens if You Push Code Before the First Build Completes?

Chaos. Jenkins will either queue the new build or cancel the old one, depending on how it's configured. Either way, you’ll probably get an email from your team.

## Where Does Jenkins Put Built Files?

Jenkins stores built artifacts in `/var/lib/jenkins/workspace/<job-name>/` by default. You can configure it to store them elsewhere.

## Setting Up Failure Notifications

To notify a human when the build fails, configure **Email Notifications** or **Slack Webhooks**.

```groovy
post {
    failure {
        mail to: 'devs@example.com',
             subject: 'Jenkins Build Failed',
             body: 'Check Jenkins, something broke.'
    }
}
```

## Testing the API After Deployment

Create a simple test script:

```sh
#!/bin/bash

response=$(curl -s -o /dev/null -w "%{http_code}" "http://localhost:5000/api/math/divide?a=10&b=2")

if [ "$response" -eq 200 ]; then
    echo "API test passed!"
else
    echo "API test failed!"
    exit 1
fi
```

Make it executable:

```sh
chmod +x test-api.sh
```

## Deploying to Docker with Jenkins

Modify the Jenkins pipeline to build and run the Docker container, then execute `test-api.sh`.

```groovy
stage('Post-Deployment Test') {
    steps {
        sh './test-api.sh'
    }
}
```

## Cleaning Up After Docker Tests

```groovy
stage('Cleanup') {
    steps {
        sh 'docker stop math-service'
        sh 'docker rm math-service'
    }
}
```

## Retaining Files in Jenkins

Configure **Build Artifacts** in Jenkins to retain compiled files:

* **Keep artifacts for a certain number of builds.**
* **Use "Discard old builds" to prevent disk bloat.**

## Competing CI/CD Tools

Jenkins isn't the only player. Here are some alternatives:

* **GitHub Actions** – Simple, integrated into GitHub.
* **GitLab CI/CD** – Built-in CI for GitLab.
* **CircleCI** – Cloud-based and fast.
* **Travis CI** – Once great, now fading.
* **Azure DevOps** – Microsoft’s answer to Jenkins.
* **TeamCity** – Enterprise-grade CI/CD.
