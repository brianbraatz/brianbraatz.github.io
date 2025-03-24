---
title: AWS Code Pipelines in a Nutshell
description: Sample App + Full Pipeline Setup
slug: aws-codepipeline-nutshell
date: 2019-03-21
image: post/Articles/IMAGES/awscodepipeline.png
categories:
  - AWS
  - DevOps
  - CI/CD
tags:
  - Aws
  - Codepipeline
  - Codecommit
  - Codebuild
  - Codedeploy
  - Ci/Cd
draft: false
weight: 515
lastmod: 2025-03-23T22:49:00.449Z
---
## 🚀 Welcome to the Pipeline Party: Part 1

This is **Part 1** of our series *“AWS Code Pipelines in a Nutshell”*. We're gonna get our hands dirty with a **real sample app** and connect the dots between:

* **AWS CodeCommit** (our Git repo)
* **AWS CodeBuild** (to build stuff)
* **AWS CodeDeploy** (to push the stuff)
* **AWS CodePipeline** (to glue it all together)

So buckle up, grab your keyboard, and let’s make some DevOps magic happen!

***

## 🧁 Sample App: A Simple Express.js Web App

Here’s a tiny Node.js + Express app we’ll use:

### 📁 `app.js`

```javascript
const express = require("express");
const app = express();
const port = process.env.PORT || 3000;

app.get("/", (req, res) => {
  res.send("🎉 Hello from AWS CodePipeline! 🎉");
});

app.listen(port, () => {
  console.log(`App running at http://localhost:${port}`);
});
```

### 📁 `package.json`

```json
{
  "name": "codepipeline-demo",
  "version": "1.0.0",
  "main": "app.js",
  "scripts": {
    "start": "node app.js"
  },
  "dependencies": {
    "express": "^4.18.2"
  }
}
```

Push this to your Git repo (we’ll use CodeCommit shortly).

***

## 🗃 Step 1: AWS CodeCommit – Git in the Cloud

### 🛠️ Create a CodeCommit Repo

```bash
aws codecommit create-repository \
  --repository-name codepipeline-demo \
  --repository-description "Demo app for AWS CodePipeline"
```

### 🔗 Clone & Push the App

```bash
git clone https://git-codecommit.<region>.amazonaws.com/v1/repos/codepipeline-demo
cd codepipeline-demo
# Copy your app.js and package.json into this folder
git add .
git commit -m "Initial commit"
git push
```

Boom! Your app is now in CodeCommit.

***

## 🔧 Step 2: AWS CodeBuild – Build that Thing

### 🛠️ Create `buildspec.yml`

Add this to your root directory:

```yaml
version: 0.2

phases:
  install:
    runtime-versions:
      nodejs: 18
    commands:
      - echo "Installing dependencies..."
      - npm install
  build:
    commands:
      - echo "Build successful! 🎉"

artifacts:
  files:
    - '**/*'
```

### 🏗️ Create a CodeBuild Project

```bash
aws codebuild create-project \
  --name codepipeline-demo-build \
  --source type=CODECOMMIT,location=https://git-codecommit.<region>.amazonaws.com/v1/repos/codepipeline-demo \
  --artifacts type=NO_ARTIFACTS \
  --environment type=LINUX_CONTAINER,image=aws/codebuild/standard:7.0,computeType=BUILD_GENERAL1_SMALL \
  --service-role arn:aws:iam::<account-id>:role/CodeBuildServiceRole
```

***

## 📦 Step 3: AWS CodeDeploy – Shipping Time

Let’s deploy this thing to EC2.

### 🗂️ Create `appspec.yml`

Add this to your repo:

```yaml
version: 0.0
os: linux
files:
  - source: /
    destination: /home/ec2-user/app

hooks:
  AfterInstall:
    - location: scripts/install.sh
      timeout: 180
      runas: ec2-user
  ApplicationStart:
    - location: scripts/start.sh
      timeout: 180
      runas: ec2-user
```

### 📝 `scripts/install.sh`

```bash
#!/bin/bash
cd /home/ec2-user/app
npm install
```

### 📝 `scripts/start.sh`

```bash
#!/bin/bash
cd /home/ec2-user/app
nohup npm start > app.log 2>&1 &
```

> Don’t forget to make these executable:

```bash
chmod +x scripts/*.sh
```

### 🎯 Create an Application and Deployment Group

```bash
aws deploy create-application \
  --application-name codepipeline-demo-app \
  --compute-platform Server

aws deploy create-deployment-group \
  --application-name codepipeline-demo-app \
  --deployment-group-name codepipeline-demo-dg \
  --ec2-tag-filters Key=Name,Value=CodeDeployDemo,Type=KEY_AND_VALUE \
  --service-role-arn arn:aws:iam::<account-id>:role/CodeDeployServiceRole
```

Make sure your EC2 instance has:

* A tag `Name=CodeDeployDemo`
* The CodeDeploy agent installed

***

## 🧬 Step 4: AWS CodePipeline – Glue It All Together

### 🧪 Create the Pipeline

```bash
aws codepipeline create-pipeline --cli-input-json file://pipeline.json
```

### 📄 `pipeline.json`

```json
{
  "pipeline": {
    "name": "codepipeline-demo-pipeline",
    "roleArn": "arn:aws:iam::<account-id>:role/CodePipelineServiceRole",
    "artifactStore": {
      "type": "S3",
      "location": "my-codepipeline-bucket"
    },
    "stages": [
      {
        "name": "Source",
        "actions": [
          {
            "name": "SourceAction",
            "actionTypeId": {
              "category": "Source",
              "owner": "AWS",
              "provider": "CodeCommit",
              "version": "1"
            },
            "outputArtifacts": [
              {
                "name": "SourceOutput"
              }
            ],
            "configuration": {
              "RepositoryName": "codepipeline-demo",
              "BranchName": "main"
            },
            "runOrder": 1
          }
        ]
      },
      {
        "name": "Build",
        "actions": [
          {
            "name": "BuildAction",
            "actionTypeId": {
              "category": "Build",
              "owner": "AWS",
              "provider": "CodeBuild",
              "version": "1"
            },
            "inputArtifacts": [
              {
                "name": "SourceOutput"
              }
            ],
            "outputArtifacts": [
              {
                "name": "BuildOutput"
              }
            ],
            "configuration": {
              "ProjectName": "codepipeline-demo-build"
            },
            "runOrder": 1
          }
        ]
      },
      {
        "name": "Deploy",
        "actions": [
          {
            "name": "DeployAction",
            "actionTypeId": {
              "category": "Deploy",
              "owner": "AWS",
              "provider": "CodeDeploy",
              "version": "1"
            },
            "inputArtifacts": [
              {
                "name": "BuildOutput"
              }
            ],
            "configuration": {
              "ApplicationName": "codepipeline-demo-app",
              "DeploymentGroupName": "codepipeline-demo-dg"
            },
            "runOrder": 1
          }
        ]
      }
    ],
    "version": 1
  }
}
```

***

## 🎉 Final Result

You now have:

* A live **Node.js app**
* Versioned in **AWS CodeCommit**
* Built using **AWS CodeBuild**
* Deployed to EC2 with **AWS CodeDeploy**
* All automated via **AWS CodePipeline**

<!-- Next up in **Part 2**, we’ll dive deeper into:

- Monitoring & debugging pipelines
- Blue/Green deployments
- Customizing CodeBuild and CodeDeploy
- Cost optimization tips

---

## 🧠 Key Ideas

| Concept            | Summary                                       |
|--------------------|-----------------------------------------------|
| Sample App         | Node.js Express app for demo                  |
| CodeCommit         | Git repo for source control                   |
| CodeBuild          | Builds the app with `buildspec.yml`           |
| CodeDeploy         | Deploys to EC2 using `appspec.yml` and hooks |
| CodePipeline       | Orchestrates the CI/CD process                |

--- -->

<!-- ---
title: "AWS Code Pipelines in a Nutshell – Part 2: Monitoring, Blue/Green, and Custom Deploys"
description: "AWS Code Pipelines in a Nutshell – Part 2: Monitoring, Blue/Green, and Custom Deploys"
slug: aws-codepipeline-2
date: 2016-07-14
image: "post/Articles/IMAGES/37.jpg"
categories: ["AWS", "DevOps", "CI/CD"]
tags: ["Aws", "Codepipeline", "Monitoring", "Bluedeploy", "Codedeploy", "Buildspec"]
draft: false
weight: 447
--- -->

## Go Beyond the Basics

So you've got a simple pipeline up and running. Nice!

But now you're asking the big questions:

* How do I know if something goes wrong?
* What’s this blue/green deployment everyone keeps talking about?
* Can I do custom stuff during builds and deploys?

You're in luck—this part covers exactly that!

***

## Monitoring CodePipeline: Keeping an Eye on the Beast

### 🔍 View Pipeline Execution Status

You can check your pipeline’s state via the AWS Console or CLI:

```bash
aws codepipeline get-pipeline-execution \
  --pipeline-name codepipeline-demo-pipeline \
  --pipeline-execution-id <execution-id>
```

Or list all executions:

```bash
aws codepipeline list-pipeline-executions \
  --pipeline-name codepipeline-demo-pipeline
```

You’ll get:

* Start time
* Status (`Succeeded`, `Failed`, `InProgress`)
* Trigger info

### 🧯 Hook into CloudWatch Alarms

When a stage fails, AWS will automatically emit CloudWatch Events. Hook them up to SNS for email/text alerts or Lambda functions for automatic rollback.

```bash
aws sns create-topic --name codepipeline-alerts
aws sns subscribe --topic-arn <topic-arn> --protocol email --notification-endpoint your@email.com
```

Then add that SNS topic to a CloudWatch rule for `CodePipeline Pipeline Execution State Change`.

***

## 🌀 Blue/Green Deployment with CodeDeploy

Let’s level up our deploys with **zero-downtime deployments**.

### 🧠 What is Blue/Green?

* **Blue**: your currently running version
* **Green**: the new version you're deploying

CodeDeploy will:

1. Launch new instances or use a separate ASG
2. Test the new version
3. Shift traffic gradually or instantly
4. Roll back if something breaks

### 🎯 Update Your Deployment Group

Switch the deployment type to `BLUE_GREEN`:

```bash
aws deploy update-deployment-group \
  --application-name codepipeline-demo-app \
  --current-deployment-group-name codepipeline-demo-dg \
  --deployment-style deploymentType=BLUE_GREEN,deploymentOption=WITH_TRAFFIC_CONTROL
```

### 🏗️ Add Load Balancer Support

Make sure your EC2 instances are behind a Load Balancer (ALB preferred), then specify the target groups in your deployment group.

```bash
aws deploy update-deployment-group \
  --application-name codepipeline-demo-app \
  --current-deployment-group-name codepipeline-demo-dg \
  --blue-green-deployment-configuration file://bluegreen-config.json
```

#### `bluegreen-config.json`

```json
{
  "deploymentReadyOption": {
    "actionOnTimeout": "CONTINUE_DEPLOYMENT",
    "waitTimeInMinutes": 1
  },
  "greenFleetProvisioningOption": {
    "action": "COPY_AUTO_SCALING_GROUP"
  },
  "terminateBlueInstancesOnDeploymentSuccess": {
    "action": "TERMINATE",
    "terminationWaitTimeInMinutes": 5
  }
}
```

***

## 🔧 Advanced `buildspec.yml`: Customize All the Things

Want to run unit tests? Create zip files? Deploy to S3? You got it.

### 📁 Sample `buildspec.yml` with Tests & Artifact

```yaml
version: 0.2

phases:
  install:
    runtime-versions:
      nodejs: 18
    commands:
      - npm install
  pre_build:
    commands:
      - echo "Running unit tests..."
      - npm test || exit 1
  build:
    commands:
      - echo "Zipping build output"
      - zip -r app.zip .
artifacts:
  files:
    - app.zip
```

This will:

* Fail the build if tests fail
* Package the app into a zip file
* Upload it to CodePipeline as an artifact

***

## 🧙 Custom Scripts in CodeDeploy

### 🚀 More Hook Options in `appspec.yml`

Here’s a full list of lifecycle hooks:

```yaml
hooks:
  BeforeInstall:
  AfterInstall:
  ApplicationStart:
  ValidateService:
```

You can also use scripts like:

```bash
#!/bin/bash
pm2 restart app || node app.js &
```

Or run DB migrations, send Slack alerts, etc.

***

## 🧰 Handy CLI Tools & Tips

* **Check CodeDeploy agent** on EC2:
  ```bash
  sudo service codedeploy-agent status
  ```

* **Log locations**:
  * CodeDeploy logs: `/opt/codedeploy-agent/deployment-root/`
  * Hook logs: `/opt/codedeploy-agent/deployment-root/*/logs/scripts.log`

* **Manually trigger a pipeline**:
  ```bash
  aws codepipeline start-pipeline-execution \
    --name codepipeline-demo-pipeline
  ```

***

<!-- 
## 🧠 Key Ideas

| Concept                | Summary                                                          |
|------------------------|------------------------------------------------------------------|
| Monitoring             | Use CLI or CloudWatch to monitor pipeline state                 |
| Blue/Green Deployments | Safer updates using traffic shifting and new instances           |
| Advanced Build Specs   | Customize with test steps, zip artifacts, S3 deploys, etc.       |
| Custom Hooks           | Do more during deployment with pre/post install/start scripts    |

---

🎉 That’s a wrap on Part 2! You now know how to:
- Monitor your pipeline like a hawk 🦅
- Deploy with zero downtime using blue/green 💙💚
- Customize builds and deployments like a pro 🔧

In **Part 3**, we’ll go **serverless** with Lambda deploys, GitHub/GitLab source triggers, and multi-region deploy strategies. -->

<!-- ---
title: "AWS Code Pipelines in a Nutshell – Part 3: Serverless, GitHub Triggers, and Multi-Region Deploys"
description: "AWS Code Pipelines in a Nutshell – Part 3: Serverless, GitHub Triggers, and Multi-Region Deploys"
slug: aws-codepipeline-3
date: 2018-10-12
image: "post/Articles/IMAGES/48.jpg"
categories: ["AWS", "DevOps", "CI/CD"]
tags: ["Aws", "Codepipeline", "Lambda", "Github", "Multi-region", "Serverless"]
draft: false
weight: 702
--- -->

<!-- ## 🦄 Welcome to Part 3: Things Get Wild (and Serverless) -->

## Time to go Serverless

You've built your pipeline.\
You’ve got blue/green deployments like a boss.\
Now it’s time to go **serverless**, link up with **GitHub**, and do **multi-region deploys**—the fancy DevOps buffet! 🍱

***

## ⚡ Deploying Serverless Apps with CodePipeline + Lambda

Want to deploy Lambda functions without crying over zip files and cronjobs?\
We got you.

### 🧾 Example: Serverless Node.js Lambda

#### `index.js`

```javascript
exports.handler = async (event) => {
  return {
    statusCode: 200,
    body: "Hello from Lambda & CodePipeline! 🎉",
  };
};
```

#### `template.yaml` (SAM Template)

```yaml
AWSTemplateFormatVersion: '2010-09-09'
Transform: AWS::Serverless-2016-10-31
Resources:
  HelloLambda:
    Type: AWS::Serverless::Function
    Properties:
      Handler: index.handler
      Runtime: nodejs18.x
      CodeUri: .
      MemorySize: 128
      Timeout: 5
      Events:
        HelloAPI:
          Type: Api
          Properties:
            Path: /hello
            Method: get
```

### 📁 Add `buildspec.yml` for SAM Build & Deploy

```yaml
version: 0.2

phases:
  install:
    runtime-versions:
      nodejs: 18
    commands:
      - npm install -g aws-sam-cli
  build:
    commands:
      - sam build
  post_build:
    commands:
      - sam package --s3-bucket my-sam-artifacts-bucket --output-template-file packaged.yaml
artifacts:
  files:
    - packaged.yaml
```

### 💥 Setup Deploy Stage for Lambda

Change the deploy provider in your pipeline to `CloudFormation`:

```json
{
  "name": "DeployLambda",
  "actionTypeId": {
    "category": "Deploy",
    "owner": "AWS",
    "provider": "CloudFormation",
    "version": "1"
  },
  "configuration": {
    "ActionMode": "CREATE_UPDATE",
    "StackName": "lambda-pipeline-stack",
    "TemplatePath": "BuildOutput::packaged.yaml",
    "Capabilities": "CAPABILITY_IAM"
  },
  "inputArtifacts": [{ "name": "BuildOutput" }],
  "runOrder": 1
}
```

***

## 🔗 Using GitHub as a Source for CodePipeline

Tired of pushing to CodeCommit? Let’s hook up your GitHub repo.

### 🔑 Step 1: Connect GitHub to AWS

Go to **AWS Console > CodePipeline > Connections**\
Create a new **GitHub Connection** (via AWS CodeStar).

It’ll ask for GitHub OAuth permissions. Grant them.

### 🎛️ Step 2: Update Your Pipeline Source

Switch your source provider to GitHub:

```json
{
  "name": "Source",
  "actions": [
    {
      "name": "GitHubSource",
      "actionTypeId": {
        "category": "Source",
        "owner": "ThirdParty",
        "provider": "GitHub",
        "version": "1"
      },
      "outputArtifacts": [{ "name": "SourceOutput" }],
      "configuration": {
        "Owner": "your-github-username",
        "Repo": "your-repo-name",
        "Branch": "main",
        "OAuthToken": "****" 
      },
      "runOrder": 1
    }
  ]
}
```

> 🛑 PSA: If using GitHub source in new regions, use CodeStar connections instead of OAuth tokens—it's the cool kid now.

***

## 🌍 Multi-Region Deployment Like a Global DevOps Jedi

Let’s say you want your app deployed in `us-east-1` AND `eu-west-1`.

Here’s the plan:

* Build once
* Deploy the same artifact to both regions

### 💡 Step 1: Create a Cross-Region S3 Bucket

You need an S3 artifact bucket in each region:

```bash
aws s3 mb s3://my-codepipeline-artifacts-eu --region eu-west-1
```

### 🧠 Step 2: Update Your Pipeline with Cross-Region Deploy

Add a second deploy stage like this:

```json
{
  "name": "DeployToEU",
  "actionTypeId": {
    "category": "Deploy",
    "owner": "AWS",
    "provider": "CloudFormation",
    "version": "1"
  },
  "region": "eu-west-1",
  "configuration": {
    "ActionMode": "CREATE_UPDATE",
    "StackName": "my-stack-eu",
    "TemplatePath": "BuildOutput::packaged.yaml",
    "Capabilities": "CAPABILITY_IAM"
  },
  "inputArtifacts": [{ "name": "BuildOutput" }],
  "runOrder": 1
}
```

> Don't forget to give CodePipeline permission to use cross-region S3 buckets and services.

<!-- ---

## 🧠 Key Ideas

| Concept              | Summary                                                                |
|----------------------|------------------------------------------------------------------------|
| Lambda Deployments   | Use SAM + CodePipeline to automate serverless app releases             |
| GitHub Triggers      | Hook CodePipeline directly into GitHub via CodeStar                    |
| Multi-Region         | Use cross-region buckets and deploy stages for global reach            |
| CloudFormation Deploy| Swap CodeDeploy with CloudFormation for Lambda/Infra deployments       |

---

## 📦 What’s Next?

In **Part 4**, we'll tackle:

- Manual approvals (for enterprise-style workflows)
- Parallel actions and fan-out stages
- Conditional logic and approvals
- Secrets management (S3/KMS/SSM style)

We're talking full-blown **CI/CD wizardry** 🧙‍♀️

Want Part 4? Just say “✨ Deploy Me” and I’m on it.

``` -->
