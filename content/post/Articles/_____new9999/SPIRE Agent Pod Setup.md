---
title: SPIRE Agent Pod Setup
description: " SPIRE (Secure Production Identity Framework for Everyone) agent"
slug: setup-spire-agent-inside-pod
date: 2019-08-14
image: post/Articles/IMAGES/spiffe.png
categories:
  - Kubernetes
  - Security
  - SPIFFE
  - Cloud
  - DevOps
tags:
  - Kubernetes
  - Security
  - SPIFFE
  - SPIRE
  - Identity
  - Management
  - Zero
  - Trust
draft: false
weight: 789
lastmod: 2025-02-17T00:30:20.360Z
---
<!-- 
# Setting Up a SPIRE Agent Inside a Pod: A Complete Guide
-
Welcome to the **ultimate guide** on setting up a **SPIRE (Secure Production Identity Framework for Everyone) agent** inside a pod. If you’re serious about **zero trust security** in Kubernetes, **SPIRE** is an absolute game-changer.

-->

<!-- 
By the end of this tutorial, you’ll know how to:
✅ **Deploy SPIRE inside a Kubernetes pod**  
✅ **Use SPIRE to issue short-lived identity certificates**  
✅ **Secure workload identities with SPIFFE (Secure Production Identity Framework for Everyone)**  
✅ **Integrate SPIRE with a service mesh like Istio**  

Let’s get started! 🚀
-->

***

## **1. What is SPIRE and Why Do You Need It?**

### **SPIFFE & SPIRE Overview**

* **SPIFFE (Secure Production Identity Framework for Everyone)** ([Wikipedia](https://en.wikipedia.org/wiki/SPIFFE)) is a standard for **workload identity management**.
* **SPIRE (SPIFFE Runtime Environment)** ([GitHub](https://github.com/spiffe/spire)) is an **implementation of SPIFFE**, enabling automatic issuance of **X.509 certificates** and **JWTs** for services.

### **Why Use SPIRE?**

* 🔐 **Zero Trust Security** - Every workload must **prove its identity** before communicating.
* 🔄 **No Hardcoded Credentials** - Workloads get **short-lived identity certificates**, avoiding API keys.
* ☁️ **Multi-Cloud & Hybrid Ready** - Works across **AWS, GCP, Azure, on-prem, and Kubernetes**.

Now, let’s set it up inside a **Kubernetes pod!**

***

## **2. Installing SPIRE on Kubernetes**

### **Step 1: Deploy the SPIRE Server**

The **SPIRE Server** is the **central authority** that issues identities.

Create a Kubernetes deployment:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: spire-server
spec:
  replicas: 1
  selector:
    matchLabels:
      app: spire-server
  template:
    metadata:
      labels:
        app: spire-server
    spec:
      containers:
      - name: spire-server
        image: ghcr.io/spiffe/spire-server:1.5.0
        args:
        - "-config"
        - "/run/spire/config/server.conf"
        volumeMounts:
        - name: spire-config
          mountPath: /run/spire/config
      volumes:
      - name: spire-config
        configMap:
          name: spire-server-config
```

Create the **ConfigMap** for SPIRE Server:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: spire-server-config
data:
  server.conf: |
    server {
      log_level = "INFO"
      data_dir = "/run/spire/data"
      bind_address = "0.0.0.0"
      bind_port = "8081"
    }
```

Deploy the **SPIRE Server**:

```sh
kubectl apply -f spire-server.yaml
```

Verify it's running:

```sh
kubectl get pods | grep spire-server
```

***

## **3. Deploying the SPIRE Agent Inside a Pod**

Now, we need to run **SPIRE Agents** inside every pod that requires **identities**.

### **Step 1: Deploy the SPIRE Agent**

```yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: spire-agent
spec:
  selector:
    matchLabels:
      app: spire-agent
  template:
    metadata:
      labels:
        app: spire-agent
    spec:
      containers:
      - name: spire-agent
        image: ghcr.io/spiffe/spire-agent:1.5.0
        args:
        - "-config"
        - "/run/spire/config/agent.conf"
        volumeMounts:
        - name: spire-config
          mountPath: /run/spire/config
      volumes:
      - name: spire-config
        configMap:
          name: spire-agent-config
```

Create the **ConfigMap** for SPIRE Agent:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: spire-agent-config
data:
  agent.conf: |
    agent {
      server_address = "spire-server.default.svc.cluster.local"
      server_port = 8081
      socket_path = "/run/spire/sockets/agent.sock"
    }
```

Deploy the **SPIRE Agent**:

```sh
kubectl apply -f spire-agent.yaml
```

Check the logs to see if the agent is running:

```sh
kubectl logs -l app=spire-agent
```

***

## **4. Registering a Workload with SPIRE**

Now that **SPIRE Agents** are running inside our pods, we need to **register workloads**.

### **Step 1: Register a Kubernetes Pod with SPIRE**

Run this command:

```sh
kubectl exec -it $(kubectl get pods -l app=spire-server -o name) -- spire-server entry create     -parentID spiffe://example.org/spire/server     -spiffeID spiffe://example.org/my-service     -selector k8s:pod-label:app:my-app
```

This tells SPIRE:

* **Only pods labeled `app=my-app`** get the identity **spiffe://example.org/my-service**.

Now, your pod can **automatically receive short-lived certificates**! 🔐

***

## **5. Using SPIRE with Istio for Secure Service Mesh**

SPIRE integrates **seamlessly** with **Istio** for **secure mTLS communication**.

### **Step 1: Configure Istio to Use SPIRE**

```yaml
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: spire-authentication
  namespace: istio-system
spec:
  selector:
    matchLabels:
      istio: ingressgateway
  mtls:
    mode: STRICT
```

Apply it:

```sh
kubectl apply -f spire-authentication.yaml
```

Now, **all Istio services will use SPIRE for authentication**. 🚀

***

## **6. Verifying SPIRE is Working**

To verify, run:

```sh
kubectl exec -it $(kubectl get pods -l app=spire-server -o name) -- spire-server entry show
```

You should see the **SPIFFE IDs issued** to workloads.

You can also check your pod:

```sh
kubectl exec -it my-app-pod -- cat /run/spire/sockets/agent.sock
```

If you see **identity certificates**, SPIRE is **working correctly!** 🎉

***

## **Final Thoughts**

Setting up **SPIRE inside a pod** gives you **strong identity security** with **zero trust principles**.

### **Key Takeaways**

✅ **SPIRE issues workload identities dynamically**.\
✅ **No more hardcoded API keys!** 🎉\
✅ **Works with Istio for mTLS encryption**.\
✅ **Enables Zero Trust Security** in Kubernetes.

If you’re building **secure microservices**, **SPIRE is a must-have**! 🔥

***

## **Reference Links**

* [SPIFFE - Wikipedia](https://en.wikipedia.org/wiki/SPIFFE)
* [SPIRE GitHub](https://github.com/spiffe/spire)
* [Istio Security](https://istio.io/latest/docs/concepts/security/)
* [Zero Trust Security](https://en.wikipedia.org/wiki/Zero_trust_security_model)
