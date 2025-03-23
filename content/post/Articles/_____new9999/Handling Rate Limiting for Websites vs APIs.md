---
title: Rate Limiting for Websites vs APIs
description: Techniques Explored
slug: handling-rate-limiting-websites-vs-apis
date: 2022-01-15
image: post/Articles/IMAGES/waterfall.png
categories:
  - Web Development
  - Security
  - API Rate Limiting
  - DevOps
  - Cloud
tags:
  - Web
  - Development
  - Security
  - Rate
  - Limiting
  - API
  - Gateway
  - Cloud
  - DevOps
  - CDN
  - Microservices
  - Kubernetes
draft: false
weight: 44
categories_ref:
  - Web Development
  - Security
  - API Rate Limiting
  - DevOps
  - Cloud
slug_calculated: https://brianbraatz.github.io/p/handling-rate-limiting-websites-vs-apis
lastmod: 2025-03-23T15:55:09.710Z
---
<!--
# Handling Rate Limiting for Websites vs APIs: Best Practices and Code Examples

When handling **large user pools behind firewalls or NAT (e.g., colleges, businesses, mobile networks, entire countries)**, **rate limiting for websites vs APIs requires different approaches**. Websites rely on **browser interactions**, while APIs serve **machine-to-machine communication**.

By the end of this guide, you’ll understand:
✅ **How rate limiting challenges differ between websites and APIs**  
✅ **How to write website and server-side code to handle large shared IP pools**  
✅ **Best architecture patterns for web applications, microservices, and Kubernetes**  
✅ **How to use CDNs and caching to improve performance**  

Let’s break it down! 🚀

---
-->

## **1. Limiting Strategies  for Websites vs APIs**

| Factor                      | Websites                               | APIs                                          |
| --------------------------- | -------------------------------------- | --------------------------------------------- |
| **Users**                   | Human users (browsers)                 | Machines, mobile apps, scripts                |
| **Requests per User**       | 10-100 per session                     | 1,000+ per session                            |
| **State**                   | Sessions, cookies, authentication      | Stateless, token-based                        |
| **Concurrency**             | Multiple users per IP (firewalls, NAT) | Often 1:1 client-server                       |
| **Caching**                 | Heavy caching via CDN                  | Caching only applies to GET requests          |
| **Impact of Rate Limiting** | Users might get frustrated             | API clients may retry aggressively            |
| **Rate Limit Strategy**     | Per session, per user token            | Per API key, client IP, or device fingerprint |

🔹 **Key takeaway**: **API rate limiting must be stricter** since **bots, scrapers, and malicious actors** abuse APIs more often. **Websites can rely on caching and CDNs** to reduce load.

***

## **2. Handling Rate Limiting for Websites**

### **2.1 Web Client-Side Strategies**

To prevent users from hitting rate limits, we can use:

🔹 **JavaScript Throttling/Debouncing** – Prevents excessive requests from UI interactions.\
🔹 **Session-Based Limits** – Limits requests per logged-in user rather than by IP.\
🔹 **CDN Caching** – Offloads traffic by serving cached responses.

#### **Example: JavaScript Throttling with Lodash**

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.21/lodash.min.js"></script>
</head>
<body>
  <button id="fetchData">Fetch Data</button>
  <script>
    function fetchData() {
      fetch('/api/data')
        .then(response => response.json())
        .then(data => console.log(data));
    }
    
    // Throttle API calls (max 1 request per second)
    document.getElementById('fetchData').addEventListener(
      'click', _.throttle(fetchData, 1000)
    );
  </script>
</body>
</html>
```

🔹 **Why this works?** Prevents users from **spamming API requests** by **limiting clicks**.

***

### **2.2 Web Server-Side Strategies**

For web servers, we use:

* **Session-based limits** – Track request counts per user session.
* **Dynamic rate limits** – Adjust limits based on user behavior.
* **CDN caching** – Serve static data to reduce backend load.

#### **Example: Python Flask with Session-Based Rate Limiting**

```python
from flask import Flask, session, request, jsonify
from flask_limiter import Limiter

app = Flask(__name__)
app.secret_key = "super_secret_key"

limiter = Limiter(app, key_func=lambda: session.get("user_id", request.remote_addr))

@app.route("/api/data")
@limiter.limit("10 per minute")
def get_data():
    return jsonify({"message": "Request successful!"})

if __name__ == "__main__":
    app.run(debug=True)
```

🔹 **Why this works?** Limits **each logged-in user separately**, even if they share an IP.

***

## **3. Handling Rate Limiting for APIs**

### **3.1 API Server-Side Strategies**

For APIs, we use:

* **Token-based rate limiting** – Assign limits per API key or session token.
* **Geo-based exceptions** – Allow higher limits for countries behind firewalls.
* **Device fingerprinting** – Track unique devices instead of IPs.

#### **Example: Python Flask API with Token-Based Rate Limiting**

```python
from flask import Flask, request, jsonify
from flask_limiter import Limiter

app = Flask(__name__)
limiter = Limiter(app, key_func=lambda: request.headers.get("Authorization", request.remote_addr))

@app.route("/api/data")
@limiter.limit("100 per minute")
def get_data():
    return jsonify({"message": "Request successful!"})

if __name__ == "__main__":
    app.run(debug=True)
```

🔹 **Why this works?** Limits API usage **per token** rather than per IP.

***

### **3.2 API Gateway Rate Limiting**

Use **NGINX, Traefik, or Istio** for API gateway-based rate limiting.

#### **Example: NGINX Rate Limiting by API Key**

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: api-ingress
  annotations:
    nginx.ingress.kubernetes.io/limit-req-zone: "$http_x_api_key zone=api_limit:10m rate=50r/s"
    nginx.ingress.kubernetes.io/limit-req-burst: "20"
```

🔹 **Why this works?** Ensures **rate limits apply per API key**, not per IP.

***

## **4. Architecture Patterns for Scaling Web & API Rate Limiting**

### **4.1 Using CDNs for Websites**

For **static assets and cached API responses**, use **Cloudflare, AWS CloudFront, or Fastly**.

#### **Example: AWS CloudFront Caching API Responses**

```sh
aws cloudfront create-invalidation --distribution-id XYZ123 --paths "/api/*"
```

🔹 **Why this works?** Reduces API calls by **serving cached responses from edge locations**.

***

### **4.2 Microservices for Scalable Rate Limiting**

Break **monolithic applications** into **microservices** and limit rates per microservice.

#### **Example: Kubernetes Rate Limiting with Istio**

```yaml
apiVersion: security.istio.io/v1beta1
kind: RequestAuthentication
metadata:
  name: rate-limit-policy
spec:
  selector:
    matchLabels:
      app: api-service
  rules:
  - match:
      prefix: /
    quota:
      maxTokens: 500
      refillRate: 50
```

🔹 **Why this works?** Limits **each microservice separately**.

***

## **5. Final Thoughts: Key Takeaways**

| Strategy                       | Websites              | APIs                         |
| ------------------------------ | --------------------- | ---------------------------- |
| **Rate limit by IP**           | ❌ Bad for shared IPs  | ✅ Good for bot defense       |
| **Rate limit by user session** | ✅ Best practice       | ❌ Hard to track clients      |
| **Rate limit by API key**      | ❌ Not needed          | ✅ Best practice              |
| **Use CDNs**                   | ✅ Yes, for caching    | ✅ Only for GET requests      |
| **Use dynamic limits**         | ✅ Improves UX         | ✅ Prevents abuse             |
| **Use microservices**          | ✅ Reduces bottlenecks | ✅ Enables distributed limits |

### **Best Practices for Modern Cloud Applications**

✅ **Websites should cache, throttle UI actions, and use CDNs**\
✅ **APIs should rate limit by API key, use gateways, and track device fingerprints**\
✅ **Kubernetes microservices help distribute load and prevent bottlenecks**

***

## **Reference Links**

* [Rate Limiting - Wikipedia](https://en.wikipedia.org/wiki/Rate_limiting)
* [Cloudflare API Gateway](https://developers.cloudflare.com/api-shield/)
* [AWS API Gateway Throttling](https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-request-throttling.html)
