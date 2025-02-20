---
title: Helm in a Nutshell
description: Nutshell Guide with Command and Config Samples
slug: helm-nutshell
date: 2021-03-14
image: post/Articles/IMAGES/helm.png
categories:
  - Kubernetes
  - Helm
  - DevOps
  - Cloud
tags:
  - Kubernetes
  - Helm
  - Pods
  - Package
  - Management
  - Automation
  - Cloud
draft: false
weight: 875
lastmod: 2025-02-20T12:21:16.415Z
---
<!-- 
# Helm Explained in Detail: A Complete Guide with Code Samples

If you’ve worked with **Kubernetes**, you know that deploying applications **can get complicated quickly**. Enter **Helm**, Kubernetes' **package manager**, which helps you deploy, manage, and upgrade applications effortlessly.

By the end of this guide, you’ll understand:
✅ **What Helm is and how it works**  
✅ **How Helm compares to other deployment methods**  
✅ **How to use Helm to deploy applications in Kubernetes**  
✅ **How to create and manage Helm charts**  
✅ **Advanced Helm configurations and best practices**  

Let’s dive in! 🚀
-->

***

## **1. What is Helm?**

[Helm](https://helm.sh/) is a **package manager for Kubernetes** that helps deploy applications as **charts**.

If you’ve worked with **Kubernetes**, you know that deploying applications **can get complicated quickly**.

### **Why Use Helm?**

* 📦 **Simplifies complex deployments**
* 🔄 **Manages upgrades & rollbacks easily**
* 📜 **Encapsulates Kubernetes YAML files into reusable charts**
* 🚀 **Enables application versioning**
* 💾 **Supports dependency management**

Helm **abstracts away** Kubernetes YAML complexity, making deployments **declarative and modular**.

***

## **2. Helm vs. Other Deployment Methods**

| Deployment Method | Pros                           | Cons                     |
| ----------------- | ------------------------------ | ------------------------ |
| **kubectl apply** | Simple, direct control         | Hard to manage updates   |
| **Kustomize**     | Patch-based, built-in          | No dependency management |
| **Helm**          | Reusable, versioned, templated | Learning curve           |

Helm is ideal when you need **reusability, upgrades, and rollback features**.

***

## **3. Installing Helm**

### **Step 1: Install Helm CLI**

For Linux/macOS:

```sh
curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash
```

For Windows (via Chocolatey):

```sh
choco install kubernetes-helm
```

Verify installation:

```sh
helm version
```

***

## **4. Deploying Applications with Helm**

### **Step 1: Add a Helm Repository**

```sh
helm repo add bitnami https://charts.bitnami.com/bitnami
helm repo update
```

### **Step 2: Install an Application (e.g., Nginx)**

```sh
helm install my-nginx bitnami/nginx
```

### **Step 3: Verify Installation**

```sh
kubectl get pods
```

To uninstall:

```sh
helm uninstall my-nginx
```

***

## **5. Creating a Helm Chart**

Helm charts are **pre-packaged Kubernetes applications**.

### **Step 1: Create a Chart Structure**

```sh
helm create mychart
cd mychart
```

This generates:

```
mychart/
│   Chart.yaml
│   values.yaml
│   templates/
│   ├── deployment.yaml
│   ├── service.yaml
│   ├── ingress.yaml
```

### **Step 2: Define the Chart.yaml**

```yaml
apiVersion: v2
name: mychart
description: A Helm chart for Kubernetes
type: application
version: 1.0.0
appVersion: 1.16.0
```

### **Step 3: Modify values.yaml**

```yaml
replicaCount: 2

image:
  repository: nginx
  tag: latest

service:
  type: ClusterIP
  port: 80
```

### **Step 4: Deploy Your Custom Chart**

```sh
helm install myrelease ./mychart
```

To upgrade:

```sh
helm upgrade myrelease ./mychart
```

***

## **6. Helm Advanced Features**

### **6.1 Managing Dependencies**

Define dependencies in `Chart.yaml`:

```yaml
dependencies:
  - name: postgresql
    version: "12.1.0"
    repository: "https://charts.bitnami.com/bitnami"
```

Then update dependencies:

```sh
helm dependency update
```

### **6.2 Using Helm Hooks**

Define lifecycle hooks:

```yaml
annotations:
  "helm.sh/hook": post-install
```

This runs the script **after installation**.

### **6.3 Rollbacks and Versioning**

To list all releases:

```sh
helm list
```

To rollback:

```sh
helm rollback myrelease 1
```

***

## **7. Helm in Production: Best Practices**

✅ **Use values.yaml to configure deployments instead of modifying templates directly**\
✅ **Enable Helm's built-in rollback features for safe deployments**\
✅ **Use Helmfile for managing multiple Helm releases**\
✅ **Regularly update Helm charts and repositories**\
✅ **Store Helm values in Git for version control**

***

<!-- 
## **Final Thoughts**

Helm is a **powerful tool** that simplifies Kubernetes application management.
-->

### **Key Takeaways**

✅ **Helm simplifies Kubernetes application deployment**\
✅ **Helm Charts package YAML files into reusable templates**\
✅ **Helm supports upgrades, rollbacks, and dependencies**\
✅ **Best practices help keep deployments secure and manageable**

***

## **Reference Links**

* [Helm Official Docs](https://helm.sh/docs/)
* [Helm Charts](https://artifacthub.io/packages/search?kind=helm)
* [Helm GitHub Repository](https://github.com/helm/helm)
* [Kubernetes Package Management](https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/)
