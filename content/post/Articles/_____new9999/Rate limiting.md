---
title: API Rate Limiting and Request Filtering in Kubernetes
description: Cheatsheets for Setup
slug: api-rate-limiting-request-filtering
date: 2022-10-05
image: post/Articles/IMAGES/tronmountains.png
categories:
  - Kubernetes
  - Security
  - API Gateway
  - Networking
  - DevOps
tags:
  - Kubernetes
  - Security
  - API
  - Rate
  - Limiting
  - Request
  - Filtering
  - Ingress
  - Docker
  - Istio
  - NGINX
  - Traefik
  - Cloud
draft: false
weight: 99
categories_ref:
  - Kubernetes
  - Security
  - API Gateway
  - Networking
  - DevOps
slug_calculated: https://brianbraatz.github.io/p/api-rate-limiting-request-filtering
lastmod: 2025-03-14T16:40:36.911Z
---
# API Rate Limiting and Request Filtering in Kubernetes: A Complete Guide

APIs are the meat of our apps...

but **without proper security**,

they can be exploited by **DDoS attacks, abuse, and excessive traffic**.

<!--
**Rate limiting and request filtering** ensure **API stability, security, and compliance**.

By the end of this guide, you’ll understand:
✅ **What API rate limiting and request filtering are**  
✅ **Why they are crucial for security and performance**  
✅ **How to implement rate limiting in Kubernetes using NGINX, Istio, and Traefik**  
✅ **How to apply request filtering to block malicious traffic**  
✅ **How these strategies relate to Kubernetes pods and Docker**  

Let’s get started! 🚀
-->

***

## **1. What is API Rate Limiting?**

[API Rate Limiting](https://en.wikipedia.org/wiki/Rate_limiting) is the process of **controlling the number of requests a client can send to an API within a specific time window**.

### **1.1 Why Use API Rate Limiting?**

* 🛡 **Prevents API abuse and denial-of-service (DDoS) attacks**
* 🚀 **Ensures fairly balanced usage and prevents excessive traffic from a single client**

<!-- - 🔥 **Optimizes backend resources and improves performance**  
- ⚖ **Complies with API usage policies (e.g., public APIs, SaaS platforms)**  
-->

### **1.2 Types of Rate Limiting**

| Type               | Description                                                   | Example                                         |
| ------------------ | ------------------------------------------------------------- | ----------------------------------------------- |
| **Fixed Window**   | Limits requests in a **fixed time period**                    | 100 requests per minute                         |
| **Sliding Window** | Tracks requests **in a rolling time frame**                   | Ensures smoother enforcement                    |
| **Token Bucket**   | Requests are **allowed until tokens run out**, then throttled | API keys get **100 tokens**, refilled over time |
| **Leaky Bucket**   | Requests are **processed at a fixed rate**, excess is dropped | Prevents sudden bursts of traffic               |

***

## **2. What is Request Filtering?**

Request filtering **blocks or modifies incoming API requests** based on predefined rules.

### **2.1 Why Use Request Filtering?**

* 🛡 **Blocks malicious requests (SQL injection, XSS, bot traffic, etc.)**
* 🔥 **Filters unauthorized IPs, user agents, and request methods**
* 🚀 **Improves API security and performance**

### **2.2 Examples of Request Filtering**

| Filter Type           | Description                          | Example                       |
| --------------------- | ------------------------------------ | ----------------------------- |
| **IP Filtering**      | Blocks requests from blacklisted IPs | Block `192.168.1.1`           |
| **Rate Limiting**     | Limits requests per client           | Allow max **10 req/sec**      |
| **Header Validation** | Requires valid headers               | Must have `User-Agent: MyApp` |
| **Method Filtering**  | Restricts request types              | Only allow `GET` and `POST`   |

Now, let’s **implement rate limiting and request filtering in Kubernetes pods**.

***

## **3. Cheatsheets for setup of API Rate Limiting in Kubernetes**

### **3.1 Using NGINX Ingress Controller for Rate Limiting**

#### **Step 1: Install NGINX Ingress Controller**

```sh
helm repo add ingress-nginx https://kubernetes.github.io/ingress-nginx
helm install nginx-ingress ingress-nginx/ingress-nginx
```

#### **Step 2: Deploy an API with Rate Limiting**

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: my-api-ingress
  annotations:
    nginx.ingress.kubernetes.io/limit-rpm: "60"  # 60 requests per minute
    nginx.ingress.kubernetes.io/limit-burst: "20"  # Allow bursts of 20 requests
spec:
  rules:
  - host: my-api.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: my-api-service
            port:
              number: 80
```

Apply:

```sh
kubectl apply -f my-api-ingress.yaml
```

Now, clients **can only send 60 requests per minute**.

***

### **3.2 Using Istio for Rate Limiting**

#### **Step 1: Install Istio**

```sh
istioctl install --set profile=demo -y
kubectl label namespace default istio-injection=enabled
```

#### **Step 2: Apply a Rate Limit Policy**

```yaml
apiVersion: security.istio.io/v1beta1
kind: RequestAuthentication
metadata:
  name: rate-limit-policy
spec:
  selector:
    matchLabels:
      app: my-api
  rules:
  - match:
      prefix: /
    quota:
      maxTokens: 100  # Allow 100 requests
      refillRate: 10   # Refill 10 requests per second
```

Apply:

```sh
kubectl apply -f istio-rate-limit.yaml
```

Now, **clients are rate-limited based on Istio’s policy**.

***

### **3.3 Using Traefik for Rate Limiting**

#### **Step 1: Install Traefik**

```sh
helm repo add traefik https://helm.traefik.io/traefik
helm install traefik traefik/traefik
```

#### **Step 2: Apply a Rate Limit Middleware**

```yaml
apiVersion: traefik.io/v1alpha1
kind: Middleware
metadata:
  name: rate-limit-middleware
spec:
  rateLimit:
    average: 50  # Allow 50 requests per second
    burst: 10
```

Apply:

```sh
kubectl apply -f traefik-rate-limit.yaml
```

Now, **requests to Traefik will be limited to 50 per second**.

***

## **4. Implementing Request Filtering in Kubernetes**

### **4.1 Block Malicious Requests Using NGINX**

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: my-api-ingress
  annotations:
    nginx.ingress.kubernetes.io/server-snippet: |
      if ($http_user_agent ~* (bot|crawler|spider)) {
        return 403;
      }
      if ($request_method !~ ^(GET|POST)$) {
        return 405;
      }
```

This blocks:

* 🚫 **Bots, crawlers, and scrapers**
* 🚫 **Disallows methods other than `GET` and `POST`**

Apply:

```sh
kubectl apply -f nginx-request-filter.yaml
```

***

<!-- 
## **5. Why Rate Limiting and Request Filtering Are Critical**

### **5.1 Dangers of Not Implementing Rate Limiting**

| Risk                       | Impact                                         |
| -------------------------- | ---------------------------------------------- |
| **DDoS Attacks**           | Overloads API, causing downtime                |
| **Resource Exhaustion**    | High CPU/memory usage, slows other services    |
| **Unauthorized API Abuse** | Users can brute-force or scrape sensitive data |
| **Billing Overages**       | SaaS APIs may exceed request limits            |

### **5.2 Dangers of Not Filtering Requests**

| Risk                           | Impact                                |
| ------------------------------ | ------------------------------------- |
| **SQL Injection (SQLi)**       | Hackers steal database data           |
| **Cross-Site Scripting (XSS)** | Injects malicious scripts             |
| **Brute Force Attacks**        | Repeated logins to hack credentials   |
| **Data Scraping**              | Competitors scrape sensitive API data |

**Implementing rate limiting and request filtering is essential to prevent these threats.**

---

## **6. Final Thoughts**

API security **is critical in Kubernetes environments**. **Rate limiting and request filtering** help prevent **DDoS, abuse, and malicious attacks**.

### **Key Takeaways**
✅ **Rate limiting protects APIs from overuse and abuse**  
✅ **Request filtering blocks bots, bad actors, and malicious requests**  
✅ **Use NGINX, Istio, and Traefik for enforcement**  
✅ **Without these protections, APIs are vulnerable to attacks**  

By implementing **API rate limiting and request filtering inside Kubernetes**, you **ensure security, stability, and compliance**. 🚀

---
-->

## **Reference Links**

* [API Rate Limiting - Wikipedia](https://en.wikipedia.org/wiki/Rate_limiting)
* [OWASP API Security Guide](https://owasp.org/www-project-api-security/)
* [NGINX Rate Limiting](https://docs.nginx.com/nginx/admin-guide/security-controls/controlling-access/)
* [Istio Traffic Management](https://istio.io/latest/docs/tasks/traffic-management/)
* [Traefik Middleware Configuration](https://doc.traefik.io/traefik/middlewares/ratelimit/)
