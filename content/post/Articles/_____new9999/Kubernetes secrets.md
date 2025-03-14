---
title: "Kubernetes Secrets: A Complete Guide with Code Examples"
description: "Kubernetes Secrets: A Complete Guide with Code Examples"
slug: kubernetes-secrets-complete-guide
date: 2022-02-10
image: post/Articles/IMAGES/bluesecret.png
categories:
  - Kubernetes
  - Security
  - DevOps
  - Cloud
tags:
  - Kubernetes
  - Security
  - Secrets
  - RBAC
  - Encryption
  - Best
  - Practices
draft: false
weight: 58
categories_ref:
  - Kubernetes
  - Security
  - DevOps
  - Cloud
lastmod: 2025-03-14T15:45:29.396Z
---
# Kubernetes Secrets: A Complete Guide with Code Examples

**Managing sensitive data in Kubernetes is critical.** Whether it's **API keys, database credentials, or TLS certificates**, Kubernetes **Secrets** provide a secure way to **store and manage sensitive information**.

By the end of this guide, you’ll understand:\
✅ **What Kubernetes Secrets are and why they matter**\
✅ **How to create, manage, and secure Kubernetes Secrets**\
✅ **Different types of Secrets and when to use them**\
✅ **Best practices for protecting sensitive data in Kubernetes**

Let’s get started! 🚀

***

## **1. What Are Kubernetes Secrets?**

[Kubernetes Secrets](https://kubernetes.io/docs/concepts/configuration/secret/) are **objects designed to store sensitive information**, such as:

* 🔑 **API Keys**
* 🗝 **Passwords**
* 🔒 **TLS Certificates**
* 🏦 **Database Credentials**

Secrets **prevent hardcoding sensitive data** in ConfigMaps, YAML files, or environment variables.

### **Why Use Secrets Instead of ConfigMaps?**

| Feature            | ConfigMaps          | Secrets                                   |
| ------------------ | ------------------- | ----------------------------------------- |
| **Data Type**      | Plaintext           | Base64-encoded (not encrypted)            |
| **Used For**       | App configs         | Sensitive data                            |
| **Access Control** | Standard            | More restrictive                          |
| **Mounted As**     | Volumes or Env Vars | Volumes or Env Vars                       |
| **Security Level** | Low                 | Medium (requires extra security measures) |

***

## **2. Creating and Managing Kubernetes Secrets**

### **2.1 Creating a Secret from a File**

First, create a **plaintext file**:

```sh
echo -n "supersecurepassword" > password.txt
```

Now, create a Kubernetes Secret:

```sh
kubectl create secret generic my-secret --from-file=password.txt
```

Verify:

```sh
kubectl get secrets
kubectl describe secret my-secret
```

### **2.2 Creating a Secret from Key-Value Pairs**

```sh
kubectl create secret generic my-db-secret   --from-literal=username=admin   --from-literal=password=supersecurepassword
```

Retrieve it:

```sh
kubectl get secret my-db-secret -o yaml
```

Note: **Data is base64-encoded, NOT encrypted!**

Decode the password:

```sh
echo "c3VwZXJzZWN1cmVwYXNzd29yZA==" | base64 --decode
```

### **2.3 Creating a Secret Using a YAML File**

Create `secret.yaml`:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: my-secret
type: Opaque
data:
  username: YWRtaW4=   # Base64 encoded "admin"
  password: c3VwZXJzZWN1cmVwYXNzd29yZA==  # Base64 encoded "supersecurepassword"
```

Apply it:

```sh
kubectl apply -f secret.yaml
```

***

## **3. Using Kubernetes Secrets in Pods**

### **3.1 Using Secrets as Environment Variables**

Modify `pod.yaml`:

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: secret-pod
spec:
  containers:
  - name: my-container
    image: nginx
    env:
    - name: DB_USERNAME
      valueFrom:
        secretKeyRef:
          name: my-db-secret
          key: username
    - name: DB_PASSWORD
      valueFrom:
        secretKeyRef:
          name: my-db-secret
          key: password
```

Apply:

```sh
kubectl apply -f pod.yaml
```

### **3.2 Mounting Secrets as Volumes**

Modify `pod-volume.yaml`:

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: secret-volume-pod
spec:
  containers:
  - name: my-container
    image: nginx
    volumeMounts:
    - name: secret-volume
      mountPath: "/etc/secret-data"
      readOnly: true
  volumes:
  - name: secret-volume
    secret:
      secretName: my-db-secret
```

Apply:

```sh
kubectl apply -f pod-volume.yaml
```

Access secrets inside the pod:

```sh
kubectl exec -it secret-volume-pod -- cat /etc/secret-data/password
```

***

## **4. Securing Kubernetes Secrets**

### **4.1 Enabling Role-Based Access Control (RBAC)**

Create an **RBAC policy** to restrict Secret access:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  namespace: default
  name: secret-reader
rules:
- apiGroups: [""]
  resources: ["secrets"]
  verbs: ["get", "list"]
```

Create a RoleBinding:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: secret-reader-binding
  namespace: default
subjects:
- kind: User
  name: developer
  apiGroup: rbac.authorization.k8s.io
roleRef:
  kind: Role
  name: secret-reader
  apiGroup: rbac.authorization.k8s.io
```

Apply:

```sh
kubectl apply -f rbac-secret.yaml
```

### **4.2 Encrypting Secrets at Rest**

By default, Secrets are **stored unencrypted in etcd**. Enable **encryption at rest**:

Modify `encryption-config.yaml`:

```yaml
apiVersion: apiserver.config.k8s.io/v1
kind: EncryptionConfiguration
resources:
  - resources:
      - secrets
    providers:
      - aescbc:
          keys:
            - name: key1
              secret: c3VwZXJzZWN1cmVwYXNzd29yZA==
      - identity: {}
```

Apply:

```sh
kubectl apply -f encryption-config.yaml
```

### **4.3 Using External Secret Management Systems**

Use **HashiCorp Vault** or **AWS Secrets Manager** for added security.

#### **Vault Integration Example**

```sh
vault kv put secret/myapp username=admin password=supersecurepassword
```

Retrieve secret:

```sh
vault kv get secret/myapp
```

***

## **5. Best Practices for Kubernetes Secrets**

✅ **Use RBAC to restrict access to Secrets**\
✅ **Enable encryption at rest for Secrets**\
✅ **Avoid storing Secrets in ConfigMaps or environment variables**\
✅ **Use a Secret Management System (Vault, AWS Secrets Manager)**\
✅ **Monitor access to Secrets using audit logs**\
✅ **Regularly rotate Secrets and enforce expiration policies**

***

## **Final Thoughts**

Kubernetes Secrets **help manage sensitive data securely**, but they **must be properly secured**.

### **Key Takeaways**

✅ **Secrets prevent hardcoding sensitive data in Pods**\
✅ **Use RBAC and encryption to protect Secrets**\
✅ **External secret managers enhance security**\
✅ **Monitor and audit Secret access in production**

With **proper management**, Kubernetes Secrets **enhance security while keeping configurations manageable**. 🚀

***

## **Reference Links**

* [Kubernetes Secrets Docs](https://kubernetes.io/docs/concepts/configuration/secret/)
* [Best Practices for Kubernetes Security](https://kubernetes.io/docs/concepts/security/overview/)
* [HashiCorp Vault Secrets Management](https://www.vaultproject.io/docs/secrets/)
* [AWS Secrets Manager](https://aws.amazon.com/secrets-manager/)
