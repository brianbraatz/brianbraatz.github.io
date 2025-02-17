---
title: Squid Proxy Pod Setup
description: How to setup a proxy inside a Kubernetes pod
slug: setup-squid-proxy-inside-pod
date: 2020-02-18
image: post/Articles/IMAGES/squid.png
categories:
  - Kubernetes
  - Networking
  - Proxy
  - Cloud
  - DevOps
tags:
  - Kubernetes
  - Networking
  - Proxy
  - Squid
  - NGINX
  - HAProxy
  - Traefik
draft: false
weight: 821
lastmod: 2025-02-16T22:08:44.553Z
---
<!-- 
# Setting Up a Squid Proxy Inside a Pod: A Complete Guide

So, y-
So, WHY would you need a **proxy** inside a **Kubernetes pod**? 

Maybe you’re looking to **filter traffic**, enforce **security policies**, or just **cache requests** like. 
-->

These notes will help you setup  **Squid Proxy** ([Wikipedia](https://en.wikipedia.org/wiki/Squid_\(software\))).

<!--
By the end of this tutorial, you’ll know how to:
✅ **Deploy a Squid Proxy inside a Kubernetes pod**  
✅ **Configure Squid for filtering, caching, and security**  
✅ **Compare Squid with other proxies (NGINX, HAProxy, Traefik)**  
✅ **Secure and optimize your proxy for Kubernetes**  

Let’s dive in! 🚀

-----
-->

## **1. Why Use a Proxy Inside a Pod?**

A **proxy** acts as an intermediary between clients and servers. Running a **proxy inside a Kubernetes pod** lets you:

* 🔐 **Enforce security policies** (e.g., block certain domains, allowlist services).
* 🚀 **Improve performance** with **caching**.
* 🌍 **Control outbound internet access** (great for enterprise environments).
* 📊 **Monitor and log all outgoing traffic**.

Now, let’s deploy **Squid Proxy** inside a pod!

***

## **2. Deploying a Squid Proxy in Kubernetes**

### **Step 1: Create a ConfigMap for Squid Configuration**

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: squid-config
data:
  squid.conf: |
    http_port 3128
    cache deny all
    access_log stdio:/dev/stdout
    acl allowed_sites dstdomain .example.com .trusted.com
    http_access allow allowed_sites
    http_access deny all
```

This **Squid configuration**:

* Listens on port `3128`
* **Denies caching** (for security)
* **Allows traffic only** to `example.com` and `trusted.com`
* **Denies all other requests**

### **Step 2: Deploy Squid Proxy as a Pod**

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: squid-proxy
spec:
  replicas: 1
  selector:
    matchLabels:
      app: squid
  template:
    metadata:
      labels:
        app: squid
    spec:
      containers:
      - name: squid
        image: ubuntu/squid:latest
        ports:
        - containerPort: 3128
        volumeMounts:
        - name: config-volume
          mountPath: /etc/squid/squid.conf
          subPath: squid.conf
      volumes:
      - name: config-volume
        configMap:
          name: squid-config
```

### **Step 3: Expose the Proxy Service**

```yaml
apiVersion: v1
kind: Service
metadata:
  name: squid-service
spec:
  selector:
    app: squid
  ports:
    - protocol: TCP
      port: 3128
      targetPort: 3128
  type: ClusterIP
```

Deploy everything:

```sh
kubectl apply -f squid-config.yaml
kubectl apply -f squid-deployment.yaml
kubectl apply -f squid-service.yaml
```

Your Squid Proxy is now **running inside a pod**! 🎉

***

## **3. Using Squid Proxy Inside Your Kubernetes Cluster**

### **Configure Pods to Use the Proxy**

Update your pod’s **environment variables**:

```yaml
env:
  - name: HTTP_PROXY
    value: "http://squid-service:3128"
  - name: HTTPS_PROXY
    value: "http://squid-service:3128"
  - name: NO_PROXY
    value: "localhost,127.0.0.1,.trusted.com"
```

This ensures all outbound traffic **goes through Squid**.

### **Test the Proxy**

Exec into a pod:

```sh
kubectl exec -it my-app-pod -- bash
```

Run a request:

```sh
curl -x http://squid-service:3128 http://example.com
```

If `example.com` loads, **Squid is working!** 🎉

***

## **4. Comparing Squid with Other Proxies**

| Proxy       | Best For                   | Caching | Load Balancing | Security |
| ----------- | -------------------------- | ------- | -------------- | -------- |
| **Squid**   | Web filtering, caching     | ✅ Yes   | ❌ No           | ✅ Yes    |
| **NGINX**   | Reverse proxy, API gateway | ✅ Yes   | ✅ Yes          | ✅ Yes    |
| **HAProxy** | Load balancing             | ❌ No    | ✅ Yes          | ✅ Yes    |
| **Traefik** | Kubernetes-native proxy    | ✅ Yes   | ✅ Yes          | ✅ Yes    |

### **When to Use Each Proxy**

* **Use Squid** if you need **web filtering and security**.
* **Use NGINX** for **reverse proxy and API gateway functions**.
* **Use HAProxy** if you need **high-performance load balancing**.
* **Use Traefik** if you need **dynamic Kubernetes-native routing**.

***

## **5. Advanced Squid Configurations**

### **Blocking Websites**

Modify your **squid.conf**:

```yaml
acl blocked_sites dstdomain .facebook.com .youtube.com
http_access deny blocked_sites
```

Restart Squid:

```sh
kubectl delete pod -l app=squid
```

Now, requests to `facebook.com` and `youtube.com` will be **blocked**! 🚫

### **Enabling Logging**

Squid logs **all traffic** by default, but you can store logs persistently:

```yaml
access_log /var/log/squid/access.log
```

Mount a persistent volume:

```yaml
volumes:
  - name: logs
    emptyDir: {}
volumeMounts:
  - mountPath: /var/log/squid
    name: logs
```

Now, logs will **persist inside the pod**.

***

## **6. Securing Squid with Authentication**

You can **require users to authenticate** before using Squid:

### **Step 1: Install Authentication Package**

Modify the deployment:

```yaml
env:
  - name: SQUID_AUTH
    value: "basic"
```

### **Step 2: Configure Squid for Authentication**

Modify `squid.conf`:

```yaml
auth_param basic program /usr/lib/squid/basic_ncsa_auth /etc/squid/passwords
auth_param basic realm Proxy
acl authenticated proxy_auth REQUIRED
http_access allow authenticated
```

### **Step 3: Create Users**

Exec into the Squid pod:

```sh
kubectl exec -it squid-pod -- bash
```

Create a user:

```sh
htpasswd -c /etc/squid/passwords user1
```

Restart Squid:

```sh
service squid restart
```

Now, users **must log in** before using the proxy! 🔐

***

## **Final Thoughts**

Squid Proxy is a **powerful tool** for controlling **outbound network traffic** in Kubernetes.

### **Key Takeaways**

✅ **Squid Proxy helps control outbound traffic** in a cluster.\
✅ **You can block websites, cache requests, and enforce security policies.**\
✅ **Other proxies like NGINX, HAProxy, and Traefik have different use cases.**\
✅ **You can secure Squid with authentication.**

If you need **network control inside a pod**, Squid is **a great choice!** 🚀

***

## **Reference Links**

* [Squid Proxy - Wikipedia](https://en.wikipedia.org/wiki/Squid_\(software\))
* [Squid Official Docs](http://www.squid-cache.org/)
* [NGINX vs Squid - Comparison](https://www.nginx.com/)
* [HAProxy - Official Docs](http://www.haproxy.org/)
* [Traefik Proxy](https://traefik.io/)
