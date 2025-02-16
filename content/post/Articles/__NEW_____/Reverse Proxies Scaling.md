---
title: Kubernetes Load Balancing and Monitoring
description: How to test and visualize Kubernetes load balancing with dynamic scaling for Foobar OrderBot.
slug: testing-kubernetes-load-balancing-and-monitoring-orderbot-output
date: 2021-07-19
image: post/Articles/IMAGES/35.jpg
categories:
  - Foobar Examples
tags:
  - Kubernetes
  - Docker
  - Load Balancing
  - Python
  - Testing
  - Logging
draft: false
weight: 289
lastmod: 2025-02-15T23:04:27.728Z
---
See Part 1 here:

## Testing Kubernetes Load Balancing and Monitoring OrderBot Output

Alright, we’ve got **Foobar OrderBot running** in Kubernetes, scaling up and down like a champ. But now comes the big question:

**How do we actually see the output of these dynamically created and destroyed bots?**

And more importantly:

* **How do we test if Kubernetes is really load balancing?**
* **How do we create a test scenario that forces Kubernetes to scale up?**
* **How do we track which OrderBots handle which requests?**
* **How do we monitor when OrderBots are added and removed?**

Well, buckle up because we’re diving deep into testing, monitoring, and visualizing Kubernetes in action!

***

## Step 1: Setting Kubernetes to Use Low Resource Thresholds

To make scaling happen fast, we need to **lower the CPU and memory thresholds** that trigger new OrderBots to spin up.

### Update Your Deployment with Low CPU Requests

Modify your `deployment.yaml` to use **minimal CPU requests**:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: foobar-orderbot
spec:
  replicas: 1  # Start with only 1 bot
  selector:
    matchLabels:
      app: foobar-orderbot
  template:
    metadata:
      labels:
        app: foobar-orderbot
    spec:
      containers:
      - name: foobar-orderbot
        image: foobar_orderbot:latest
        ports:
        - containerPort: 8080
        resources:
          requests:
            cpu: "10m"  # Minimal CPU usage triggers scaling quickly
          limits:
            cpu: "50m"
```

Apply the changes:

```bash
kubectl apply -f deployment.yaml
```

Now, Kubernetes will **scale up quickly** when CPU usage increases.

***

## Step 2: Forcing Load Balancing with a Stress Test

Now, let’s force Kubernetes to load balance by **sending a massive flood of requests**.

### Create a Load Test Script (`load_test.py`)

This Python script will **hammer the OrderBots with requests**:

```python
import requests
import time

ORDERBOT_URL = "http://localhost/order"

for i in range(50):  # Send 50 requests
    response = requests.get(f"{ORDERBOT_URL}?quantity=5")
    print(f"Request {i + 1}: {response.text}")
    time.sleep(0.2)  # Short delay to spread load
```

Run the script:

```bash
python load_test.py
```

Since we set **low CPU limits**, Kubernetes should **scale up** new OrderBots dynamically!

***

## Step 3: Watching Kubernetes Scale Up and Down

Now let’s **see** Kubernetes adding and removing OrderBots in real time.

### Watch the Kubernetes Pods Live

```bash
kubectl get pods --watch
```

This will show output like:

```
foobar-orderbot-7bd84f789b-8whjd   Running   12s
foobar-orderbot-7bd84f789b-xz2b7   Running   10s
foobar-orderbot-7bd84f789b-pb6rm   Running   8s
```

You’ll see **new pods being created** as requests flood in.

### Watching Logs for Each OrderBot

To see which OrderBots are handling which requests, open a **separate terminal** and run:

```bash
kubectl logs -f deployment/foobar-orderbot
```

This will stream logs like:

```
Alice-12345: 5 foobars successfully ordered!
Bob-67890: 5 foobars successfully ordered!
Charlie-34567: 5 foobars successfully ordered!
```

Now, we can see that **different OrderBots are processing different requests**!

***

## Step 4: Monitoring Kubernetes Load Balancing with Metrics

To visualize load balancing, install **Metrics Server** for Kubernetes:

```bash
kubectl apply -f https://github.com/kubernetes-sigs/metrics-server/releases/latest/download/components.yaml
```

Then, monitor CPU usage in real time:

```bash
kubectl top pods
```

Example output:

```
NAME                              CPU(cores)   MEMORY(bytes)
foobar-orderbot-xyz12             25m         50Mi
foobar-orderbot-abc34             30m         55Mi
```

This confirms that **multiple OrderBots are sharing the workload**.

***

## Step 5: Testing Load Balancing on Localhost

### Run Kubernetes Locally with Minikube

If you’re using **Minikube**, start it with a LoadBalancer enabled:

```bash
minikube start --cpus=4 --memory=4g
```

Then expose your service:

```bash
minikube service foobar-orderbot-service --url
```

Now, point `ORDERBOT_URL` in your `load_test.py` script to Minikube’s URL.

***

## Step 6: How Do We Know It's Load Balancing?

✅ **Watching Pods**: `kubectl get pods --watch` confirms new bots are created and removed.

✅ **Checking Logs**: `kubectl logs -f deployment/foobar-orderbot` shows different bots processing requests.

✅ **Tracking CPU Usage**: `kubectl top pods` confirms multiple OrderBots are sharing load.

✅ **Running a Load Test**: Our `load_test.py` script forces Kubernetes to scale up dynamically.

✅ **Local Testing with Minikube**: Running services via `minikube service` proves load balancing works even on a dev machine.

***

## Conclusion

We’ve now **fully tested and visualized** Kubernetes load balancing in action! 🚀

| Step | Action                                                               |
| ---- | -------------------------------------------------------------------- |
| 1    | Lower CPU thresholds for rapid scaling                               |
| 2    | Run a Python script to flood OrderBots with requests                 |
| 3    | Watch pods dynamically scale up/down with `kubectl get pods --watch` |
| 4    | Track request distribution via `kubectl logs -f`                     |
| 5    | Monitor CPU load via `kubectl top pods`                              |
| 6    | Test load balancing locally using Minikube                           |

***

## Reference Links

* [Kubernetes HPA (Horizontal Pod Autoscaler)](https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/)
* [Kubernetes Metrics Server](https://github.com/kubernetes-sigs/metrics-server)
* [Minikube LoadBalancer](https://minikube.sigs.k8s.io/docs/)

Now, go forth and **watch Kubernetes do the heavy lifting!** 🚀🔥
