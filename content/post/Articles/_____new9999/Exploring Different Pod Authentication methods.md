---
title: Exploring Different Pod Authentication methods..
description: OAuth, JWT, API Gateways and Service Meshes
slug: setup-authentication-identity-management-pod
date: 2018-12-03
image: post/Articles/IMAGESpeapod.jpg
categories:
  - Kubernetes
  - Security
  - Authentication
  - Cloud
  - DevOps
tags:
  - Kubernetes
  - Security
  - Authentication
  - OAuth
  - Identity
  - Management
draft: false
weight: 812
lastmod: 2025-02-17T00:34:49.790Z
---
<!-- 
# Setting Up Authentication and Identity Management Inside a Pod

So, you've heard about **running authentication and identity management inside a pod**, but now you're thinking, **"Cool, but how do I actually do it?"** 

Well, buckle up, because we're about to go deep into **OAuth, JWT, API Gateways, Service Meshes, and Zero Trust**. By the end of this tutorial, you’ll know how to:

✅ Set up **authentication** inside a pod.  
✅ Use **OAuth2 Proxy** to handle logins.  
✅ Implement **JWT-based authentication**.  
✅ Secure services **using Istio** with identity management.  
✅ Integrate **cloud IAM services** for least privilege access.  

And of course, **we’ll have code examples**. Let’s get started! 🚀

---
-->

## **1. Why Authentication Inside a Pod?**

Before we dive in, let's **quickly review why** you'd even bother doing authentication inside a pod instead of just at the API gateway.

* 🔐 **Zero Trust Security** ([Wikipedia](https://en.wikipedia.org/wiki/Zero_trust_security_model)) - Every service **must authenticate** every request, even if it’s internal.
* 🎭 **Identity Propagation** - Services need **identity-aware access control**, like OAuth **tokens** or **mTLS**.
* 🚪 **Fine-grained access control** - You want **different pods to have different access levels**, instead of just relying on network policies.

**Now, let's implement it!**

***

## **2. Using OAuth2 Proxy Inside a Pod**

One of the easiest ways to add **authentication** inside a pod is by using **OAuth2 Proxy** ([GitHub](https://github.com/oauth2-proxy/oauth2-proxy)).

### **What is OAuth2 Proxy?**

* It acts as a **reverse proxy** in front of your app.
* It requires **users to authenticate via an OAuth provider** (e.g., Google, GitHub, Okta).
* It passes **authenticated requests** to your backend.

### **Step 1: Deploy OAuth2 Proxy**

We'll assume you're using **Google OAuth**. First, create an **OAuth client** in Google:

1. Go to [Google Cloud Console](https://console.cloud.google.com/).
2. Create **OAuth 2.0 credentials**.
3. Set the **redirect URI** to `https://your-app/oauth2/callback`.

Now, deploy **OAuth2 Proxy** inside a pod.

#### **oauth2-proxy Deployment (Kubernetes)**

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: oauth2-proxy
spec:
  replicas: 1
  selector:
    matchLabels:
      app: oauth2-proxy
  template:
    metadata:
      labels:
        app: oauth2-proxy
    spec:
      containers:
      - name: oauth2-proxy
        image: quay.io/oauth2-proxy/oauth2-proxy:v7.2.1
        args:
        - "--provider=google"
        - "--email-domain=*"
        - "--upstream=http://127.0.0.1:8080"
        - "--cookie-secret=$(COOKIE_SECRET)"
        - "--client-id=$(OAUTH_CLIENT_ID)"
        - "--client-secret=$(OAUTH_CLIENT_SECRET)"
        env:
        - name: OAUTH_CLIENT_ID
          valueFrom:
            secretKeyRef:
              name: oauth-secret
              key: client-id
        - name: OAUTH_CLIENT_SECRET
          valueFrom:
            secretKeyRef:
              name: oauth-secret
              key: client-secret
        - name: COOKIE_SECRET
          valueFrom:
            secretKeyRef:
              name: oauth-secret
              key: cookie-secret
```

***

## **3. Using JWT-Based Authentication**

If you need **stateless authentication**, **JWTs (JSON Web Tokens)** ([Wikipedia](https://en.wikipedia.org/wiki/JSON_Web_Token)) are the way to go.

### **Step 1: Generate JWTs with Keycloak**

Keycloak ([Official Site](https://www.keycloak.org/)) is a great open-source **identity provider**. You can deploy it in Kubernetes:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: keycloak
spec:
  replicas: 1
  selector:
    matchLabels:
      app: keycloak
  template:
    metadata:
      labels:
        app: keycloak
    spec:
      containers:
      - name: keycloak
        image: quay.io/keycloak/keycloak:latest
        args: ["start-dev"]
        env:
        - name: KEYCLOAK_ADMIN
          value: "admin"
        - name: KEYCLOAK_ADMIN_PASSWORD
          value: "admin"
```

***

## **4. Enforcing Authentication with Istio**

If you’re using **Istio** ([Wikipedia](https://en.wikipedia.org/wiki/Istio)), you can enforce **JWT authentication at the proxy level**.

### **Step 1: Create a JWT Policy in Istio**

```yaml
apiVersion: security.istio.io/v1beta1
kind: RequestAuthentication
metadata:
  name: jwt-auth
  namespace: default
spec:
  selector:
    matchLabels:
      app: my-app
  jwtRules:
  - issuer: "https://keycloak.example.com"
    jwksUri: "https://keycloak.example.com/protocol/openid-connect/certs"
```

Now, **any request without a valid JWT will be blocked!** 🚫

***

## **5. Using Cloud IAM for Workload Identity**

Instead of **hardcoding API keys**, you should use **cloud IAM roles**.

Example: If you're running in **GCP**, you can enable **Workload Identity** so that **pods automatically get IAM permissions**.

### **Step 1: Enable Workload Identity on a GKE Cluster**

```sh
gcloud container clusters update my-cluster   --workload-pool=my-project.svc.id.goog
```

### **Step 2: Link a Service Account to a Pod**

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: my-app-sa
  annotations:
    iam.gke.io/gcp-service-account: "my-gcp-sa@my-project.iam.gserviceaccount.com"
```

This lets your **pod access GCP services securely** without hardcoded credentials! 🎉

***

### **Key Takeaways**

✅ **OAuth2 Proxy** helps enforce **SSO authentication**.\
✅ **JWT authentication** enables **stateless security**.\
✅ **Istio** enforces **zero-trust security** with JWT policies.\
✅ **Cloud IAM** prevents **hardcoded credentials**.

***

## **Reference Links**

* [OAuth2 Proxy GitHub](https://github.com/oauth2-proxy/oauth2-proxy)
* [JSON Web Tokens - Wikipedia](https://en.wikipedia.org/wiki/JSON_Web_Token)
* [Keycloak](https://www.keycloak.org/)
* [Istio Security](https://istio.io/latest/docs/concepts/security/)
* [GCP Workload Identity](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)
