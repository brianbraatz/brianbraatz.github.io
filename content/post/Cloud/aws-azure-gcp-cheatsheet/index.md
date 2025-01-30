---
title: Amazon AWS, Microsoft Azure, Google Cloud Cheat Sheet
description: Comparison chart- Amazon AWS, Microsoft Azure, Google Cloud
slug: aws-azure-gcp-cheatsheet
date: 2024-02-06
image: cover.png
categories: 
tags:
  - AWS
  - Azure
  - GoogleCloud
  - Cloud
  - Cheatsheet
weight: 3
published: 2001-01-01
draft: false
lastmod: 2025-01-30T15:37:54.520Z
---
see also [DevOps Cloud Services Comparison](/post/Articles/DevOps%20Cloud%20Services%20Comparison/DevOps%20Cloud%20Services%20Comparison.md)

just a comparison and buzzword cracker :)

## **Amazon Web Services (AWS)** **Microsoft Azure** **Google Cloud Platform (GCP)** Cheat Sheet

| Feature/Parameter         | **Amazon Web Services (AWS)**                    | **Microsoft Azure**                                        | **Google Cloud Platform (GCP)**                        |
| ------------------------- | ------------------------------------------------ | ---------------------------------------------------------- | ------------------------------------------------------ |
| **Founded**               | 2006                                             | 2010                                                       | 2008                                                   |
| **Parent Company**        | Amazon                                           | Microsoft                                                  | Google                                                 |
| **Global Market Share**   | ~32% (2024)                                      | ~23% (2024)                                                | ~10% (2024)                                            |
| **Core Strengths**        | Extensive services, global reach, and ecosystem  | Enterprise integrations (Microsoft products), hybrid cloud | AI/ML capabilities, data analytics, cost-effectiveness |
| **Data Center Regions**   | 32 regions, 99 availability zones                | 60+ regions, 200+ data centers                             | 38 regions                                             |
| **Notable Customers**     | Netflix, Airbnb, NASA                            | Starbucks, HP, Adobe                                       | Twitter, PayPal, Spotify                               |
| **Pricing**               | Pay-as-you-go, savings plans, reserved instances | Pay-as-you-go, reserved pricing, hybrid benefits           | Pay-as-you-go, committed-use discounts                 |
| **Compute Services**      | EC2, Lambda (Serverless)                         | Virtual Machines (VMs), Azure Functions (Serverless)       | Compute Engine, Cloud Functions                        |
| **Storage Solutions**     | S3, EBS, Glacier                                 | Azure Blob Storage, Azure Files                            | Cloud Storage, Persistent Disk                         |
| **AI/ML Capabilities**    | Amazon SageMaker, Rekognition, Polly             | Azure Machine Learning, Cognitive Services                 | TensorFlow, Vertex AI, AutoML                          |
| **Hybrid Cloud**          | AWS Outposts                                     | Azure Arc, Azure Stack                                     | Anthos                                                 |
| **Kubernetes Support**    | Amazon EKS                                       | Azure Kubernetes Service (AKS)                             | Google Kubernetes Engine (GKE)                         |
| **Networking**            | Amazon VPC, Route 53, CloudFront                 | Azure Virtual Network, Traffic Manager                     | VPC, Cloud DNS, Cloud CDN                              |
| **Security & Compliance** | IAM, KMS, CloudTrail, extensive compliance       | Azure AD, Defender, and large compliance                   | IAM, Cloud Security Scanner, compliance                |
| **Developer Tools**       | CodePipeline, CodeBuild, Amplify                 | Azure DevOps, GitHub Actions                               | Cloud Build, Firebase                                  |
| **Big Data/Analytics**    | Redshift, EMR, QuickSight                        | Azure Synapse, HDInsight, Power BI                         | BigQuery, Dataflow, Looker                             |
| **Free Tier**             | 12-month free tier + always free services        | 12-month free tier + \$200 credit                          | 90-day free tier + \$300 credit                        |
| **Ideal For**             | Large-scale enterprises, startups                | Enterprise solutions, Microsoft ecosystems                 | AI/ML-focused projects, cost-conscious startups        |

## Detailed comparison of AWS, Azure, and GCP

| **Category**           | **AWS**                                                                                                            | **Azure**                                                                                                                                | **Google Cloud (GCP)**                                                                                                       |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| **Compute**            | **Amazon EC2**: Virtual servers  <br>**AWS Lambda**: Serverless functions                                          | **Azure Virtual Machines**: Virtual servers  <br>**Azure Functions**: Serverless functions                                               | **Google Compute Engine**: Virtual servers  <br>**Google Cloud Functions**: Serverless functions                             |
| **Containers**         | **Amazon ECS**: Managed containers  <br>**Amazon EKS**: Kubernetes service                                         | **Azure Kubernetes Service (AKS)**: Kubernetes service  <br>**Azure Container Instances**: Containers on demand                          | **Google Kubernetes Engine (GKE)**: Kubernetes service  <br>**Cloud Run**: Fully managed serverless containers               |
| **Storage**            | **Amazon S3**: Object storage  <br>**Amazon EBS**: Block storage  <br>**Amazon Glacier**: Archival storage         | **Azure Blob Storage**: Object storage  <br>**Azure Disk Storage**: Block storage  <br>**Azure Archive Storage**: Archival storage       | **Google Cloud Storage**: Object storage  <br>**Persistent Disk**: Block storage  <br>**Coldline Storage**: Archival storage |
| **Databases**          | **Amazon RDS**: Managed relational DB  <br>**DynamoDB**: NoSQL DB  <br>**Aurora**: High-performance DB             | **Azure SQL Database**: Managed relational DB  <br>**Cosmos DB**: NoSQL DB  <br>**Azure Database for MySQL/PostgreSQL**                  | **Cloud SQL**: Managed relational DB  <br>**Firestore**: NoSQL DB  <br>**Bigtable**: Scalable NoSQL DB                       |
| **Networking**         | **Amazon VPC**: Virtual private cloud  <br>**Route 53**: DNS  <br>**CloudFront**: CDN                              | **Azure Virtual Network (VNet)**: Virtual private cloud  <br>**Azure Traffic Manager**: DNS  <br>**Azure Front Door**: CDN               | **Google VPC**: Virtual private cloud  <br>**Cloud DNS**: DNS  <br>**Cloud CDN**: CDN                                        |
| **AI/ML**              | **Amazon SageMaker**: Build/train ML models  <br>**Rekognition**: Image recognition  <br>**Polly**: Text-to-speech | **Azure Machine Learning**: Build/train ML models  <br>**Cognitive Services**: Prebuilt AI APIs  <br>**Azure Bot Services**: Chatbots    | **Vertex AI**: Build/train ML models  <br>**AutoML**: Prebuilt ML solutions  <br>**Cloud Vision API**: Image recognition     |
| **Big Data/Analytics** | **Amazon Redshift**: Data warehouse  <br>**AWS Glue**: ETL service  <br>**Amazon EMR**: Big data processing        | **Azure Synapse Analytics**: Data warehouse  <br>**Azure Data Factory**: ETL service  <br>**HDInsight**: Big data processing             | **BigQuery**: Data warehouse  <br>**Dataflow**: ETL and stream processing  <br>**Dataproc**: Big data processing             |
| **Developer Tools**    | **AWS CodePipeline**: CI/CD  <br>**AWS Amplify**: Web and mobile apps  <br>**AWS Cloud9**: Cloud IDE               | **Azure DevOps**: CI/CD and version control  <br>**Visual Studio App Center**: Mobile apps  <br>**GitHub Actions**: CI/CD (GitHub-owned) | **Cloud Build**: CI/CD  <br>**Firebase**: Web and mobile apps  <br>**Cloud Source Repositories**: Git hosting                |
| **Hybrid/Edge**        | **AWS Outposts**: On-premises hardware  <br>**Local Zones**: Low-latency zones                                     | **Azure Arc**: Multi-cloud/hybrid management  <br>**Azure Stack**: On-premises hardware                                                  | **Anthos**: Multi-cloud and hybrid management  <br>**Google Distributed Cloud**: On-prem and edge                            |
| **IoT**                | **AWS IoT Core**: IoT device management  <br>**AWS Greengrass**: Edge computing                                    | **Azure IoT Hub**: IoT device management  <br>**Azure IoT Edge**: Edge computing                                                         | **Cloud IoT Core**: IoT device management  <br>**Edge TPU**: AI processing at the edge                                       |
| **Serverless**         | **AWS Lambda**: Serverless functions  <br>**Amazon Step Functions**: Workflow orchestration                        | **Azure Functions**: Serverless functions  <br>**Logic Apps**: Workflow orchestration                                                    | **Cloud Functions**: Serverless functions  <br>**Workflows**: Workflow orchestration                                         |
| **Content Delivery**   | **Amazon CloudFront**: CDN  <br>**Elastic Load Balancer (ELB)**: Traffic distribution                              | **Azure Front Door**: CDN  <br>**Azure Load Balancer**: Traffic distribution                                                             | **Cloud CDN**: CDN  <br>**Cloud Load Balancing**: Traffic distribution                                                       |
