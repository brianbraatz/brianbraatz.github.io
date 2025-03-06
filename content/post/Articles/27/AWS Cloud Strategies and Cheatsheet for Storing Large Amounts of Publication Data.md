---
title: AWS Cloud Strategies -> Storing Large Amounts of Publication Data
description: Notes On Options
slug: aws-cloud-strategies-cheatsheet
date: 2017-08-19
image: post/Articles/IMAGES/manypapers.png
categories:
  - AWS
  - Cloud Storage
  - Big Data
  - DynamoDb
tags:
  - Aws
  - Cloud
  - storage
  - Big
  - data
  - S3
  - Glacier
  - Efs
  - Ebs
  - Dynamodb
  - Aurora
draft: false
weight: 81
lastmod: 2025-03-06T16:31:09.799Z
---
# AWS Cloud Strategies and Cheatsheet for Storing Large Amounts of Publication Data 📚☁️

So, you’ve got a metric ton of publication data—articles, research papers, reports, and probably a few thousand cat GIFs (no judgment).

How do you store all this madness without breaking the bank or losing your sanity? Welcome to the magical world of AWS, where storage options are plenty, but so are the potential pitfalls.

## AWS Storage Services You Can Use 🛠️

### 1. **Amazon S3 (Simple Storage Service) 🪣**

✅ **Best for:** General-purpose object storage, images, PDFs, backups, and logs.

🚀 **Pros:**

* Infinitely scalable.
* Pay only for what you use.
* Supports lifecycle policies to move data to cheaper storage.
* Strong durability (11 nines, meaning you’d have a better chance of getting hit by a meteor than losing data).

⚠️ **Cons:**

* Costs can spiral if not monitored (especially if you have a lot of GET requests).
* Retrieval times for infrequent access tiers can be slower.

***

### 2. **Amazon Glacier 🧊**

✅ **Best for:** Archival storage (think "cold storage" for ancient research papers you’ll need once a decade).

🚀 **Pros:**

* Super cheap (like couch-cushion-change cheap).
* Great for compliance and long-term retention.

⚠️ **Cons:**

* Retrieval can take hours (if you need it fast, be ready to pay up!).
* Complex pricing structure (one does not simply retrieve files for free).

***

### 3. **Amazon EBS (Elastic Block Store) 💽**

✅ **Best for:** Database storage, virtual machines, high-performance applications.

🚀 **Pros:**

* Super fast, SSD-backed storage.
* Snapshots make backups easy.

⚠️ **Cons:**

* Limited to a single EC2 instance.
* More expensive than object storage (S3).

***

### 4. **Amazon EFS (Elastic File System) 📂**

✅ **Best for:** Shared file storage for multiple EC2 instances.

🚀 **Pros:**

* Fully managed, scales automatically.
* Works across multiple instances.

⚠️ **Cons:**

* More expensive than S3.
* Performance is variable based on usage.

***

### 5. **Amazon DynamoDB 📚**

✅ **Best for:** Storing structured, high-speed, scalable metadata (think indexing publication data).

🚀 **Pros:**

* Managed NoSQL database that scales automatically.
* Low latency, high throughput.

⚠️ **Cons:**

* Costs can be unpredictable if you don’t manage read/write capacity properly.
* Limited query flexibility compared to SQL-based databases.

***

### 6. **Amazon Aurora 🚀**

✅ **Best for:** Storing structured relational publication data (think PostgreSQL or MySQL on steroids).

🚀 **Pros:**

* Faster and more scalable than traditional RDS.
* Automatic failover and replication.

⚠️ **Cons:**

* More expensive than RDS.
* Some vendor lock-in with AWS-specific optimizations.

***

## Cheat Sheet for AWS Storage Selection 📝

| Storage Option | Best For                       | Key Features                         | Cost          |
| -------------- | ------------------------------ | ------------------------------------ | ------------- |
| S3             | General-purpose object storage | Scalable, lifecycle policies         | \$\$          |
| Glacier        | Archival storage               | Ultra-low cost, long retrieval times | \$            |
| EBS            | Block storage for EC2          | Fast, SSD-backed                     | \$\$\$        |
| EFS            | Shared file storage            | Scalable, multi-instance support     | \$\$\$        |
| DynamoDB       | NoSQL database storage         | Fast, scalable, fully managed        | \$\$-\$\$\$\$ |
| Aurora         | High-performance SQL database  | Faster RDS, managed scaling          | \$\$\$\$      |

## Pro Tips for Cost Optimization 💰

1. **Use Lifecycle Policies** 📜 – Automatically move old data from S3 to Glacier to save money.
2. **Monitor Your Storage Costs** 📊 – AWS Cost Explorer is your best friend.
3. **Compress Data** 🗜️ – Reduce storage costs by compressing publication data before upload.
4. **Use Intelligent Tiering** 🧠 – Let AWS automatically move data to cheaper tiers based on access patterns.
5. **Set Budgets & Alerts** 🚨 – Avoid getting surprise AWS bills that make you cry.

## Final Thoughts 🤔

Choosing the right AWS storage service can feel overwhelming, but if you break it down based on your needs, it’s not so bad. If you need something quick and accessible, S3 is king. If you're hoarding old data like a digital dragon, Glacier is your treasure vault. Need shared storage? EFS is solid. Running a database? DynamoDB and Aurora have your back.

And remember—always, *always* keep an eye on your AWS bill.

***

## 🔗 References

* [AWS S3 Docs](https://aws.amazon.com/s3/)
* [AWS Glacier Docs](https://aws.amazon.com/glacier/)
* [AWS EBS Docs](https://aws.amazon.com/ebs/)
* [AWS EFS Docs](https://aws.amazon.com/efs/)
* [AWS DynamoDB Docs](https://aws.amazon.com/dynamodb/)
* [AWS Aurora Docs](https://aws.amazon.com/rds/aurora/)

***

## 🔑 Key Ideas

| Topic                 | Summary                                                                  |
| --------------------- | ------------------------------------------------------------------------ |
| **S3**                | Great for general-purpose storage with tiered pricing.                   |
| **Glacier**           | Dirt-cheap archival storage, but retrieval is slow.                      |
| **EBS**               | Fast SSD-backed storage for EC2, but pricey.                             |
| **EFS**               | Scalable shared file storage for multiple EC2 instances.                 |
| **DynamoDB**          | NoSQL database with high performance, but cost can be tricky.            |
| **Aurora**            | Managed SQL database that scales, but is expensive.                      |
| **Cost Optimization** | Use lifecycle policies, compression, and monitoring to avoid bill shock. |

***
