---
title: How I Took a Legacy App from Bare Metal to AWS
description: A journey of refactoring a messy ASP.NET app
slug: legacy-app-from-bare-metal-to-aws
date: 2022-10-05
image: post/Articles/IMAGES/awsdatacenter.jpg
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Redis
  - Route 53
  - Cloudfront
  - AWS Elastic Beanstalk
  - Elastic Load Balancer-ELB
  - Amazon Auto Scaling Group-ASG
  - Amazon RDS
  - AWS CodePipeline
  - SQL
  - Microsoft Sql Server
  - Docker
  - Microservices
  - Amazon App Runner
  - IIS
  - CI\CD
tags:
  - AWS
  - Cloud
  - Migration
  - DevOps
  - Infrastructure
  - as
  - Code
draft: false
weight: 250
lastmod: 2025-03-03T00:12:26.259Z
---
# How I Took a Legacy App from Bare Metal to AWS

## The Nightmare I Walked Into

I got hired as a software engineer at this job, with years of experience with AWS, Azure and Google Cloud..

WRITING CLOUD CODE.. not doing a ton of DevOps

In the interview they asked me "Hey, can you move our internal app to AWS?".

I said yes, having no idea what kind of mess I was walking into.

Sure I ran old school websites (Early 2000's)  where we did crazy things to fix real time scaling business problem level issues...

The busiest site I worked on: I was Director of Engineering (and head coder ;) ) for a Meta\Dropbox Competitor.. Our Bandwidth bill was around \$100K a month... lots of users for us at least...

So we made alot of mistakes.. learned alot of lessons .. and used alot of Duct tape to solve problems..

We moved MySql into a RAM drive to make it go faster.. To buy us time to clean up the sql code..\
We did weird round robin tricks with client side code rotating app servers to try to scale..

Good painful learning experiences.....\
And these hacks worked.. until the next probelm cropped up..

I didnt get much sleep in those days..

but that aside.. this new job was for a large 100,000 employee technolgy company ...

And what the described in the interview didnt sound THAT bad... It just sounded like some old code and a slow computer..

Ill make a few code tweaks..

Its just some api changes right? Local files into S3.. Reinstall the SQL Server and data into a cloud managed system..

Fix it allup in an afternoon and be home for supper...\
(yes us old poeple say "supper"...)

SURE I can do this.. no problem...

hahah

So they hired me...

Then i learned more about what I was dealing with ...

This server was running on **a single server** in a Sacramento data center, complete with **notepad instructions on the desktop** for how the confiuration .. **MIGHT** be setup.....

The original dev had tried to "scale" it by adding IT provisioned Virtual MAchines as App servers.. ,\
but **the database and web server were running on the same ancient Celeron machine**.

AND the App Server VMs, were **MAKING CALLS BACK TO THE PHYSICAL SERVER FOR DATABASE ACCESS**

Umm.. ok...

Exactly how is that a goood idea?

I am still not sure..

Maybe its like if you mixed Horizontal Scaling with Quantum Mechanics... ????

***

Oh, and international users were complaining because, surprise surprise, Sacramento is *not* great for global access.

***

## Step 1: Cleaning Up the Code

The first issue? The codebase looked like it was written **by someone in a hurry, in the dark, during an earthquake**.

## Weird things I found:

### **Hardcoded Configurations Everywhere**

I found this:

```csharp
string connectionString = "Server=localhost;Database=MyDB;User Id=admin;Password=SuperSecret";
```

Seriously?!

#### **Fix:** Move everything to environment variables -later- AWS Secrets Manager.

```csharp
string connectionString = Environment.GetEnvironmentVariable("DB_CONNECTION_STRING");
```

### **Tightly Coupled Code**

Everything was jammed into a single class. Changing *anything* broke *everything*.

#### **Fix:** Dependency injection.

```csharp
public class ReportService {
   private readonly IDataRepository _repo;
   public ReportService(IDataRepository repo) {
      _repo = repo;
   }
}
```

Now I could actually test stuff without breaking the whole app.

### **Stateful Sessions**

Session data was being stored *on the web server*. This was not gonna scale too well..

#### **Fix:** Move sessions to **Redis**.

```csharp
_cache.Set("User", user, new MemoryCacheEntryOptions { SlidingExpiration = TimeSpan.FromMinutes(30) });
```

Now, users wouldn’t be tied to a single instance.

## Step 2: Moving to AWS

Now that the app was *less* of a dumpster fire, it was time to migrate.

After that  I HIT the Books with DevOps and AWS Scaling.....

### **Auto Scaling and Load Balancing**

I set up an **Elastic Load Balancer (ELB)** and **Auto Scaling Group (ASG)** so AWS could spin up new instances as needed.

```terraform
resource "aws_autoscaling_group" "my_app_asg" {
  launch_configuration = aws_launch_configuration.my_app_lc.id
  min_size             = 2
  max_size             = 10
  desired_capacity     = 2
}
```

No more single-server nightmares.

### **CloudFront for Performance?**

I tried adding **CloudFront** to cache static assets, but that didn't work on our Corporate Intranet.\
(Here is how you could do it if it can work for you.)

```terraform
resource "aws_cloudfront_distribution" "my_app_cf" {
  origin {
    domain_name = aws_s3_bucket.my_app_static.bucket_regional_domain_name
    origin_id   = "S3Origin"
  }
}
```

More on Cloudfront and why it doesnt play nicely with Intranets\
<https://stackoverflow.com/questions/70556875/how-to-use-cloudfront-only-on-the-internal-network>

<!-- 
### **Route 53 and CloudFront for Global Performance**
To fix the slow response times for users in India, I added **Route 53** (Private Hosted Zones) for geo-based routing.

I tried adding **CloudFront** to cache static assets, but that didnt work on our Corporate Intranet.
(Here is how you could do it if it can work for you.) 
~~~terraform
resource "aws_cloudfront_distribution" "my_app_cf" {
  origin {
    domain_name = aws_s3_bucket.my_app_static.bucket_regional_domain_name
    origin_id   = "S3Origin"
  }
}
~~~
More on Cloudfront and how it doesnt play nicely with Intranets
<https://stackoverflow.com/questions/70556875/how-to-use-cloudfront-only-on-the-internal-network>

Now, users in India hit the **Mumbai AWS region**, while North America users got **US West**.
-->

### **Moving the Database to AWS RDS**

I moved the **database off the web server** and into **AWS RDS (SQL Server)**, where it belonged.

```terraform
resource "aws_db_instance" "my_app_db" {
  allocated_storage = 50
  engine           = "sqlserver-se"
  instance_class   = "db.t3.medium"
}
```

Now I had backups, failover, and no more single points of failure.

### **CI/CD with AWS CodePipeline**

No more manual deployments! I set up **AWS CodePipeline** so everything deployed automatically.

```yaml
stages:
  - name: Build
    actions:
      - name: BuildApp
        provider: CodeBuild
  - name: Deploy
    actions:
      - name: DeployToAWS
        provider: CodeDeploy
```

## Migrating IIS and the Celeron Web Server

Now, the biggest challenge—moving the **ASP.NET app running on IIS** on that overworked Celeron machine.

### **Containerizing IIS**

First, I put the IIS-based ASP.NET application into a **Docker container** to make deployment more consistent.

```dockerfile
FROM mcr.microsoft.com/dotnet/framework/aspnet:4.8
WORKDIR /inetpub/wwwroot
COPY . .
```

This way, we could move it **anywhere** without worrying about system differences.

### **Running on AWS Elastic Beanstalk**

To avoid the headache of managing IIS servers manually, I deployed the container to **AWS Elastic Beanstalk**, which handles scaling and updates automatically.

```yaml
service: myapp
platform: Windows Server 2019 with IIS
option_settings:
  aws:elasticbeanstalk:container:dotnet:
    EnableApplicationHealthCheck: true
```

Now, AWS managed the underlying instances, and I didn’t have to worry about IIS configs.

### **Replacing IIS with AWS App Runner**

For long-term stability, I moved the app to **AWS App Runner**, which allows running web applications without dealing with servers.

```yaml
service:
  name: myapp-service
  instance_configuration:
    cpu: 2
    memory: 4GB
  deployment:
    image_uri: "<ECR_REPO_URI>:latest"
```

Now, the app was **fully managed**, **auto-scaled**, and **not dependent on a single server** anymore!

## The Final Results

After months of refactoring, debugging, and questioning my life choices, the app was **fully running in AWS**:

* It **auto-scales** when needed.
* Users in India and North America both get **fast response times**.
* No more single points of failure.

And most importantly—I **finally** started to understand Modern DevOps.

No more duct tape.. :)

## Key Takeaways

| Issue                     | Solution                                       |
| ------------------------- | ---------------------------------------------- |
| Hardcoded Config Values   | Used environment variables and Secrets Manager |
| Tightly Coupled Code      | Introduced dependency injection                |
| Stateful Sessions         | Used Redis for distributed session storage     |
| Single-Server Bottleneck  | Implemented AWS Auto Scaling and Load Balancer |
| Global Performance Issues | Used Route 53 and CloudFront for faster access |
| Database on Web Server    | Migrated to AWS RDS                            |
| Manual Deployments        | Set up CI/CD with AWS CodePipeline             |

## Reference Links

* https://aws.amazon.com/elastic-load-balancing/
* https://aws.amazon.com/rds/
* https://aws.amazon.com/cloudfront/
* https://aws.amazon.com/codepipeline/
