---
title: Foobar Orderbots-Part1-Visualizing Reverse Proxies with Python
description: Demonstrated And Visualizing with NGINX,IIS, mitmproxy and Docker
slug: foobar-how-reverse-proxies-visual
date: 2023-05-07
image: post/Articles/IMAGES/lostinspace.png
categories:
  - Foobar Examples
tags:
  - Python
  - Reverse
  - Proxy
  - Networking
  - Flask
  - Linux
  - Windows
  - Web
  - Servers
  - mitmproxy
  - IIS
  - NGINX
  - Docker
  - Kubernetes
  - Load
  - Balancing
draft: false
weight: 114
categories_ref:
  - Foobar Examples
lastmod: 2025-03-14T15:45:26.103Z
---
See Part 2 here:

[Reverse Proxies Scaling](/post/Articles/__NEW_____/Reverse%20Proxies%20Scaling.md)

Overview on Reverse Proxies:\
[Reverse Proxy Explained](/post/Articles/__NEW_____/Reverse%20Proxy%20Explained.md)

### The Company: Foobar Inc.

So, picture this: We have a company called **Foobar Inc.** And, shocker, we sell **foobars**.

Clever, huh? I thought so.

Now, our customers are hardcore foobar enthusiasts.

They don't want fancy web interfaces or mobile apps.. (sorry IPhone and Android users... :) )

No, sir! **They use Python command-line console apps** to order their beloved foobars.

Because if you’re REALLY into foobars, how ELSE would you order them?

### The Setup: Customer and OrderBot

We have two Python scripts:

* **`foobar_customer.py`**: Acts as the customer, sending REST calls to order foobars.
* **`foobar_orderbot.py`**: A Flask-based REST service that takes orders and confirms them.

Everything runs on **localhost**, so you can try this out on your own machine!

#### `foobar_customer.py` - The Customer Script

This script continuously prompts the user to order foobars. It:

1. Generates a random number (between 1 and 10) of foobars to order.
2. Waits for the user to press **Enter**.
3. Sends a REST request to `foobar_orderbot.py` to place the order.
4. Loops infinitely because foobars are life.

```python
import requests
import random

ORDERBOT_URL = "http://localhost:8080/order"

while True:
    quantity = random.randint(1, 10)
    input(f"Press Enter to order {quantity} foobars from the Foobar Orderbot...")
    response = requests.get(f"{ORDERBOT_URL}?quantity={quantity}")
    print(response.text)
```

#### `foobar_orderbot.py` - The OrderBot

This script:

1. Uses **Flask** to expose a REST endpoint.
2. Generates a random name on startup using `faker`.
3. Returns a message confirming the order.

```python
from flask import Flask, request
from faker import Faker
import time

app = Flask(__name__)
fake = Faker()
orderbot_name = fake.first_name() + "-" + str(int(time.time() * 1000) % 100000)

default_port = 8080

@app.route("/order", methods=["GET"])
def order_some_foobars():
    quantity = request.args.get("quantity", type=int, default=1)
    return f"{orderbot_name}: {quantity} foobars successfully ordered!"

if __name__ == "__main__":
    import sys
    port = int(sys.argv[1]) if len(sys.argv) > 1 else default_port
    print(f"{orderbot_name} - Ready for orders on port {port}!")
    app.run(host="0.0.0.0", port=port)
```

### Running the Scripts

1. Start the order bot:
   ```bash
   python foobar_orderbot.py
   ```
2. Run the customer script:
   ```bash
   python foobar_customer.py
   ```
3. Observe the interactions:

   **OrderBot Output:**

   ```
   Alice-45678 - Ready for orders on port 8080!
   127.0.0.1 - - [DATE] "GET /order?quantity=5 HTTP/1.1" 200 -
   ```

   **Customer Output:**

   ```
   Press Enter to order 5 foobars from the Foobar Orderbot...
   Alice-45678: 5 foobars successfully ordered!
   ```

### Setting Up a Reverse Proxy

#### Linux (Using Nginx)

1. Install Nginx:
   ```bash
   sudo apt install nginx
   ```
2. Configure the reverse proxy:
   ```bash
   sudo nano /etc/nginx/sites-available/foobar
   ```
   Add the following:
   ```nginx
   server {
       listen 80;
       location / {
           proxy_pass http://localhost:8080;
       }
   }
   ```
3. Enable the config and restart Nginx:
   ```bash
   sudo ln -s /etc/nginx/sites-available/foobar /etc/nginx/sites-enabled/
   sudo systemctl restart nginx
   ```

#### Windows (Using IIS)

1. Install **IIS** and the **Application Request Routing (ARR)** module.
2. Open **IIS Manager** → Select your server → Open **Application Request Routing Cache**.
3. Click **Server Proxy Settings** → Enable Proxy.
4. Add a Reverse Proxy rule:
   * **Condition:** `localhost`
   * **Rewrite URL:** `http://localhost:8080`

**(if you get ill in the stomach at bloating down your dev box with IIS.. i understand.. later in the article ill show how to do this without IIS... )**

### Running Multiple OrderBots

Now, let’s start multiple `foobar_orderbot.py` instances on different ports:

```bash
python foobar_orderbot.py 8081
python foobar_orderbot.py 8082
python foobar_orderbot.py 8083
```

Update your Nginx config to distribute traffic:

```nginx
upstream foobar_backend {
    server localhost:8081;
    server localhost:8082;
    server localhost:8083;
}
server {
    listen 80;
    location / {
        proxy_pass http://foobar_backend;
    }
}
```

Restart Nginx, and now requests get load-balanced across multiple bots!

### Key Ideas so Far

| Concept             | Explanation                                       |
| ------------------- | ------------------------------------------------- |
| Reverse Proxy       | Routes traffic to different backend servers       |
| Flask REST API      | Handles foobar ordering                           |
| Nginx Reverse Proxy | Load-balances requests across multiple order bots |
| Multiple Backends   | Running multiple instances of the order bot       |

<!--

---
title: "Setting Up a Reverse Proxy for Windows Development Without IIS"
description: "A guide on setting up a reverse proxy for Windows development without using IIS."
slug: "setting-up-reverse-proxy-for-windows-without-iis"
date: "2019-08-15"
image: "post/Articles/IMAGES/378.jpg"
categories: []
tags: ["Python", "Reverse Proxy", "Networking", "Windows", "Nginx", "Flask", "Development"]
draft: false
weight: 298
---
-->

## Setting Up a Reverse Proxy for Windows Development Without IIS

So far our **Foobar Ordering System** up and running using a **reverse proxy**.

We demonstrated how to set it up with **Nginx on Linux** and **IIS on Windows**.

But let's be real—**IIS setup on Windows dev machines can be a pain.**

Many companies **lock down development environments**, preventing installation of things like IIS.

So lets explore alternative ways to set up a reverse proxy on Windows **without using IIS**.

We will look at Python-based solutions, and even see if **Nginx on Windows** is viable.

### Quick Recap: What We Have so far

1. We created two scripts:
   * **`foobar_customer.py`**: A client that sends requests to order foobars.
   * **`foobar_orderbot.py`**: A Flask-based REST service that takes orders.
2. We set up a **reverse proxy** to distribute requests across multiple order bots.
3. We demonstrated **Nginx on Linux** and **IIS on Windows** as reverse proxies.
4. We showed multiple instances of `foobar_orderbot.py` running behind the proxy.

But now, let’s go **IIS-free** and explore **Python-based proxy solutions** and **Nginx on Windows**.

***

## Option 1: Setting Up a Reverse Proxy in Python

Luckily, Python has libraries that let us create a **reverse proxy** without breaking a sweat.

The `http.server` module in Python provides basic proxying functionality, but we’ll use `mitmproxy`, which is more powerful.

### Step 1: Install `mitmproxy`

```bash
pip install mitmproxy
```

### Step 2: Create `reverse_proxy.py`

```python
from mitmproxy import http
from mitmproxy.tools.dump import DumpMaster
from mitmproxy.addons import core

class ReverseProxy:
    def request(self, flow: http.HTTPFlow) -> None:
        if flow.request.host == "localhost":
            flow.request.host = "127.0.0.1"
            flow.request.port = 8080

addons = [ReverseProxy()]

def start_proxy():
    with DumpMaster(None, [core.Core(), *addons]) as m:
        m.run()

if __name__ == "__main__":
    start_proxy()
```

### Step 3: Run the Proxy

```bash
python reverse_proxy.py
```

This will intercept all requests to `localhost` and forward them to `127.0.0.1:8080`, where `foobar_orderbot.py` is running.

***

## Option 2: Running Nginx on Windows

Yes, you **can** run Nginx on Windows, and it works surprisingly well.

### Step 1: Download Nginx for Windows

1. Go to [Nginx's official website](https://nginx.org/en/download.html).
2. Download the Windows `.zip` file.
3. Extract it to `C:\nginx`.

### Step 2: Configure `nginx.conf`

Navigate to `C:\nginx\conf\nginx.conf` and edit it like this:

```nginx
worker_processes  1;
events {
    worker_connections  1024;
}
http {
    upstream foobar_backend {
        server 127.0.0.1:8081;
        server 127.0.0.1:8082;
        server 127.0.0.1:8083;
    }

    server {
        listen 80;
        location / {
            proxy_pass http://foobar_backend;
        }
    }
}
```

### Step 3: Start Nginx

Open **Command Prompt** and run:

```bash
cd C:\nginx
start nginx.exe
```

***

## Running Multiple OrderBots

Start multiple `foobar_orderbot.py` instances on different ports:

```bash
python foobar_orderbot.py 8081
python foobar_orderbot.py 8082
python foobar_orderbot.py 8083
```

With **Nginx on Windows** or our **Python-based proxy**, the `foobar_customer.py` script will now load-balance across the different bots!

***

## Example Output

### OrderBot Instances

```
Bob-12345 - Ready for orders on port 8081!
127.0.0.1 - - [DATE] "GET /order?quantity=3 HTTP/1.1" 200 -
```

```
Alice-67890 - Ready for orders on port 8082!
127.0.0.1 - - [DATE] "GET /order?quantity=7 HTTP/1.1" 200 -
```

### Customer Output

```
Press Enter to order 3 foobars from the Foobar Orderbot...
Bob-12345: 3 foobars successfully ordered!
```

```
Press Enter to order 7 foobars from the Foobar Orderbot...
Alice-67890: 7 foobars successfully ordered!
```

***

## What did we do?

* We ditched IIS and found **Python-based** and **Nginx-based** reverse proxy alternatives for Windows.
* We saw that `mitmproxy` can handle proxying **entirely in Python**.
* We learned that **Nginx runs just fine on Windows**, making it a solid choice.
* Our **Foobar Orderbot system now runs on any Windows dev box without admin rights!**

***

## Key Ideas So Far

| Concept              | Explanation                                      |
| -------------------- | ------------------------------------------------ |
| Python Reverse Proxy | `mitmproxy` handles request forwarding           |
| Nginx on Windows     | Lightweight and efficient reverse proxy solution |
| Load Balancing       | Distributes requests across multiple order bots  |
| No IIS Needed        | Avoids admin restrictions on Windows dev boxes   |

<!--
---
title: "Foobar Orderbots-Scaling Reverse Proxies with Docker and Kubernetes"
description: "Foobar Orderbots Move to the Cloud!"
slug: "foobar-how-reverse-proxies-work"
date: "2023-05-07"
image: "post/Articles/IMAGES/26.jpg"
categories: ["Foobar Examples"]
tags: ["Python", "Reverse Proxy", "Networking", "Flask", "Linux", "Windows", "Web Servers","mitmproxy","IIS","NGINX","Docker", "Kubernetes","Load Balancing"]
draft: false
weight: 114
---


title: "Foorbar Orderbots-Scaling Reverse Proxies with Docker and Kubernetes"




---
title: "Running Foobar OrderBot in Docker and Kubernetes"
description: "A guide on deploying Foobar OrderBot using Docker and Kubernetes, dynamically scaling based on demand."
slug: "running-foobar-orderbot-in-docker-and-kubernetes"
date: "2020-12-03"
image: "post/Articles/IMAGES/479.jpg"
categories: []
tags: ["Docker", "Kubernetes", "Python", "Reverse Proxy", "Load Balancing", "Development"]
draft: false
weight: 367
---
-->

## Running Foobar OrderBot in Docker and Kubernetes

Previously, we set up a **reverse proxy** to balance load across multiple instances of `foobar_orderbot.py`. But let’s be real—**manually launching instances isn’t ideal.**

Today, we’re leveling up by deploying **Foobar OrderBot** in **Docker** and **Kubernetes**, making it **dynamically scale** based on demand. We’ll set up **auto-scaling**, **load balancing**, and even **test it locally**.

***

## Docker vs. Kubernetes: What’s the Deal?

| Technology     | Role                                                                                     |
| -------------- | ---------------------------------------------------------------------------------------- |
| **Docker**     | Creates lightweight, portable containers that package our app with all its dependencies. |
| **Kubernetes** | Manages multiple Docker containers, enabling scaling, auto-healing, and networking.      |

In short:

* **Docker** gets our **Foobar OrderBot** running in isolated environments.
* **Kubernetes** manages many **OrderBots** efficiently.

***

## Step 1: Containerizing Foobar OrderBot with Docker

We need a **Dockerfile** to package our Flask-based order bot into a container.

### `Dockerfile`

```dockerfile
# Use an official Python runtime as base image
FROM python:3.11

# Set the working directory
WORKDIR /app

# Copy the app files into the container
COPY foobar_orderbot.py /app/

# Install dependencies
RUN pip install flask faker

# Expose the Flask port
EXPOSE 8080

# Define the entry point
CMD ["python", "foobar_orderbot.py"]
```

### Step 2: Build and Run the Docker Container

```bash
# Build the Docker image
docker build -t foobar_orderbot .

# Run a single instance on port 8080
docker run -p 8080:8080 foobar_orderbot
```

At this point, `foobar_orderbot.py` runs inside a Docker container, just like it would on your local machine. But let’s take it **one step further with Kubernetes!**

***

## Step 3: Deploying Foobar OrderBot on Kubernetes

### Step 3.1: Define the Kubernetes Deployment

We’ll create a **Kubernetes Deployment** to manage multiple instances of `foobar_orderbot.py`.

#### `deployment.yaml`

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: foobar-orderbot
spec:
  replicas: 3  # Start with 3 instances
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
```

### Step 3.2: Define the Kubernetes Service

To distribute traffic, we need a **Kubernetes Service**.

#### `service.yaml`

```yaml
apiVersion: v1
kind: Service
metadata:
  name: foobar-orderbot-service
spec:
  selector:
    app: foobar-orderbot
  ports:
  - protocol: TCP
    port: 80
    targetPort: 8080
  type: LoadBalancer
```

### Step 3.3: Deploy to Kubernetes

```bash
# Apply deployment and service
kubectl apply -f deployment.yaml
kubectl apply -f service.yaml
```

Now, Kubernetes spins up **3 OrderBots**, and traffic gets evenly distributed!

***

## Step 4: Dynamic Scaling with Kubernetes

Imagine traffic **spikes at lunchtime** when everyone orders foobars. We need **auto-scaling!**

### Step 4.1: Enable Auto-Scaling

```bash
kubectl autoscale deployment foobar-orderbot --cpu-percent=50 --min=3 --max=10
```

This command tells Kubernetes:

* Keep at least **3** instances running.
* If CPU usage goes above **50%**, scale up to **10** OrderBots.

***

## Step 5: Reverse Proxy with Kubernetes and Nginx

To load-balance requests across all OrderBots, we use **Nginx as a reverse proxy.**

#### `nginx-deployment.yaml`

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx-proxy
spec:
  replicas: 1
  selector:
    matchLabels:
      app: nginx-proxy
  template:
    metadata:
      labels:
        app: nginx-proxy
    spec:
      containers:
      - name: nginx-proxy
        image: nginx:latest
        ports:
        - containerPort: 80
        volumeMounts:
        - name: nginx-config
          mountPath: /etc/nginx/nginx.conf
          subPath: nginx.conf
      volumes:
      - name: nginx-config
        configMap:
          name: nginx-config
```

#### `nginx-config.yaml`

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: nginx-config

data:
  nginx.conf: |
    worker_processes  1;
    events {
        worker_connections  1024;
    }
    http {
        upstream foobar_backend {
            server foobar-orderbot-service:80;
        }
        server {
            listen 80;
            location / {
                proxy_pass http://foobar_backend;
            }
        }
    }
```

### Deploy Nginx to Kubernetes

```bash
kubectl apply -f nginx-config.yaml
kubectl apply -f nginx-deployment.yaml
```

Now, all requests go through **Nginx**, which load-balances across all OrderBots! 🎉

***

## Step 6: Testing Load Balancing Locally

If you're on **Minikube**, enable the LoadBalancer:

```bash
minikube service foobar-orderbot-service --url
```

Then, run the **customer script** against the Minikube IP:

```python
ORDERBOT_URL = "http://minikube_ip:port/order"
```

### Reference Links

* [mitmproxy Documentation](https://docs.mitmproxy.org/)
* [Nginx Windows Installation Guide](https://nginx.org/en/docs/windows.html)
* [Python HTTP Server](https://docs.python.org/3/library/http.server.html)

<!--
## Key Takeaways
| Concept                 | Explanation |
|-------------------------|-------------|
| Docker                 | Packages OrderBot into portable containers |
| Kubernetes Deployment  | Manages multiple OrderBot instances |
| Kubernetes Service     | Load-balances requests across OrderBots |
| Auto-Scaling          | Dynamically adjusts the number of OrderBots |
| Nginx Reverse Proxy   | Handles traffic distribution |

### Reference Links
- [Docker Documentation](https://docs.docker.com/)
- [Kubernetes Official Docs](https://kubernetes.io/docs/)
- [Nginx Reverse Proxy Guide](https://www.nginx.com/resources/admin-guide/reverse-proxy/)

That’s it! Now your Foobar OrderBot **auto-scales like a pro!** 🚀
-->
