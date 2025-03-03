---
title: Setting Up IAM Inside a Pod
description: "Setting Up IAM Inside a Pod: A Complete Guide with Code Examples"
slug: setup-iam-inside-pod
date: 2022-08-22
image: post/Articles/IMAGES/iamfunny.png
categories:
  - Kubernetes
  - Security
  - IAM
  - DevOps
  - Cloud
tags:
  - Kubernetes
  - Security
  - IAM
  - AWS
  - GCP
  - Azure
  - Identity
  - Management
draft: false
weight: 28
lastmod: 2025-03-03T14:31:52.197Z
---
(\
props to :\
[IAM (International Association of Movers): What You Need to Know](https://sirelo.com/moving-associations/iam/)

for letting me use their logo..

well..

**maybe** they did..

I mean..

I wrote them an email..

well..

I have a plan to write them an email..

tomorrow..

....

maybe....\
)

<!-- 
# Setting Up IAM Inside a Pod: A Complete Guide with Code Examples

Identity and Access Management (**IAM**) is essential for **controlling access to resources in Kubernetes**, ensuring **least privilege**, and preventing **unauthorized access** inside a pod.

By the end of this guide, you’ll understand:
✅ **How IAM works in cloud environments (AWS, GCP, Azure)**  
✅ **How to configure IAM roles inside a Kubernetes pod**  
✅ **How to use IAM policies for fine-grained access control**  
✅ **Best practices for securing IAM inside Kubernetes**  

Let’s secure our workloads! 🔐

---
-->

## **1. What is IAM and Why Is It Important?**

[Identity and Access Management (IAM)](https://en.wikipedia.org/wiki/Identity_management) is a **framework for managing permissions** and **authenticating identities** in a system.

### **1.1 Why Use IAM Inside a Pod?**

* 🔐 **Control access to cloud services (AWS S3, GCP Storage, Azure Key Vault)**
* 🚀 **Enforce least privilege (only allow necessary permissions)**
* 🏗 **Secure API calls from applications running inside Kubernetes**
* 📜 **Ensure compliance (GDPR, HIPAA, PCI DSS)**

### **1.2 How IAM Works in Kubernetes**

IAM inside a pod relies on **cloud IAM policies and Kubernetes role-based access control (RBAC)**:

| Feature                      | Description                                         |
| ---------------------------- | --------------------------------------------------- |
| **Cloud IAM**                | Controls access to cloud services (AWS, GCP, Azure) |
| **Kubernetes RBAC**          | Controls access to Kubernetes resources             |
| **Service Accounts**         | Allows pods to assume IAM roles                     |
| **OIDC & Workload Identity** | Secure authentication inside clusters               |

Now, let’s configure **IAM inside Kubernetes pods**.

***

## **2. Setting Up IAM in AWS (EKS) Inside a Pod**

### **Step 1: Enable IAM Roles for Kubernetes Service Accounts**

Ensure your AWS **EKS cluster** supports **IAM roles for service accounts**.

```sh
eksctl utils associate-iam-oidc-provider     --region us-east-1     --cluster my-cluster     --approve
```

### **Step 2: Create an IAM Role for the Pod**

Create an **IAM policy**:

```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": ["s3:ListBucket", "s3:GetObject"],
            "Resource": ["arn:aws:s3:::my-bucket", "arn:aws:s3:::my-bucket/*"]
        }
    ]
}
```

Attach this policy to an **IAM role**:

```sh
aws iam create-role --role-name my-pod-role     --assume-role-policy-document file://trust-policy.json
aws iam attach-role-policy --role-name my-pod-role     --policy-arn arn:aws:iam::aws:policy/AmazonS3ReadOnlyAccess
```

### **Step 3: Associate IAM Role with a Kubernetes Service Account**

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: s3-reader
  annotations:
    eks.amazonaws.com/role-arn: "arn:aws:iam::123456789012:role/my-pod-role"
```

Apply:

```sh
kubectl apply -f service-account.yaml
```

### **Step 4: Use IAM Role Inside a Pod**

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: s3-pod
spec:
  serviceAccountName: s3-reader
  containers:
  - name: app
    image: amazonlinux
    command: ["sleep", "3600"]
```

Verify IAM permissions:

```sh
kubectl exec -it s3-pod -- aws s3 ls s3://my-bucket/
```

Now, the pod **can access AWS S3 securely without storing credentials**. 🚀

***

## **3. Setting Up IAM in GCP (GKE) Inside a Pod**

### **Step 1: Enable Workload Identity**

```sh
gcloud container clusters update my-cluster     --workload-pool=my-project.svc.id.goog
```

### **Step 2: Create a GCP IAM Service Account**

```sh
gcloud iam service-accounts create gke-service-account     --display-name "GKE Workload Identity SA"
```

Grant permissions:

```sh
gcloud projects add-iam-policy-binding my-project     --member "serviceAccount:gke-service-account@my-project.iam.gserviceaccount.com"     --role "roles/storage.objectViewer"
```

### **Step 3: Associate IAM Role with a Kubernetes Service Account**

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: gke-service-account
  annotations:
    iam.gke.io/gcp-service-account: "gke-service-account@my-project.iam.gserviceaccount.com"
```

Apply:

```sh
kubectl apply -f service-account.yaml
```

### **Step 4: Use IAM Role Inside a Pod**

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: gcs-pod
spec:
  serviceAccountName: gke-service-account
  containers:
  - name: app
    image: google/cloud-sdk
    command: ["sleep", "3600"]
```

Verify IAM permissions:

```sh
kubectl exec -it gcs-pod -- gsutil ls gs://my-bucket/
```

Now, the pod **can securely access GCP Storage without using service account keys**. 🚀

***

## **4. Setting Up IAM in Azure (AKS) Inside a Pod**

### **Step 1: Enable Managed Identity for AKS**

```sh
az aks update -g my-resource-group -n my-cluster --enable-managed-identity
```

### **Step 2: Assign an IAM Role to the AKS Pod Identity**

```sh
az identity create --name aks-pod-identity --resource-group my-resource-group
```

Grant permissions:

```sh
az role assignment create --assignee aks-pod-identity-id     --role "Reader" --scope "/subscriptions/my-subscription-id/resourceGroups/my-resource-group"
```

### **Step 3: Associate IAM Role with a Kubernetes Service Account**

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: aks-service-account
  annotations:
    azure.workload.identity/client-id: "aks-pod-identity-client-id"
```

Apply:

```sh
kubectl apply -f service-account.yaml
```

### **Step 4: Use IAM Role Inside a Pod**

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: azure-pod
spec:
  serviceAccountName: aks-service-account
  containers:
  - name: app
    image: mcr.microsoft.com/azure-cli
    command: ["sleep", "3600"]
```

Verify IAM permissions:

```sh
kubectl exec -it azure-pod -- az storage blob list --container-name mycontainer --account-name mystorageaccount
```

Now, the pod **can access Azure services securely**. 🚀

***

## **5. Best Practices for IAM Inside Kubernetes Pods**

✅ **Use IAM Roles instead of static credentials**\
✅ **Follow the Principle of Least Privilege (PoLP)**\
✅ **Use Kubernetes RBAC in combination with IAM**\
✅ **Rotate IAM credentials periodically**\
✅ **Monitor and audit IAM access logs**

***

<!-- 
## **Final Thoughts**

IAM is **crucial** for securing access **inside Kubernetes pods**.
-->

### **Key Ideas**

✅ **IAM ensures secure access to cloud services without storing credentials**\
✅ **AWS (EKS), GCP (GKE), and Azure (AKS) all support pod IAM roles**\
✅ **Use Kubernetes Service Accounts to assume IAM roles securely**\
✅ **Follow best practices for securing IAM in Kubernetes**

***

## **Reference Links**

* [AWS IAM Roles for Kubernetes](https://docs.aws.amazon.com/eks/latest/userguide/iam-roles-for-service-accounts.html)
* [GCP Workload Identity](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)
* [Azure Managed Identities](https://docs.microsoft.com/en-us/azure/aks/use-managed-identities)
