---
title: HTTPS in a Nutshell
description: How HTTPS works
slug: https-nutshell
date: 2022-09-02
image: post/Articles/IMAGES/https.png
categories:
  - Web Development
  - Networking
  - Security
  - Protocols
  - Network Protocols
tags:
  - Https
  - Tls
  - Ssl
  - Cybersecurity
  - Encryption
  - Web
  - Security
  - Certificates
  - Networking
  - Http
  - Secure
  - Communication
draft: false
weight: 165
categories_ref:
  - Web Development
  - Networking
  - Security
  - Protocols
  - Network Protocols
slug_calculated: https://brianbraatz.github.io/p/https-nutshell
lastmod: 2025-03-14T16:40:29.473Z
---
<!--

# How the HTTPS Network Protocol Works: History, Relationship to Alternatives, and 10 Code Examples

## Introduction  

If you've ever typed `https://` into your browser, you've used **HTTPS (HyperText Transfer Protocol Secure)**—the **secure version of HTTP** that encrypts data between your browser and a website.  

But how does it work? And why is it so important?  

In this article, we’ll explore:  

- The **history and evolution** of HTTPS.  
- How HTTPS secures web traffic.  
- **HTTPS vs. HTTP and other security protocols**.  
- **10 practical HTTPS code examples**.  
-->

***

## The History of HTTPS

HTTPS was introduced in **1994 by Netscape** to **encrypt web traffic using SSL (Secure Sockets Layer)**. It evolved over the years with the introduction of **TLS (Transport Layer Security)**, replacing SSL.

### **Key HTTPS Milestones**

| Year | Development | Notes                                  |
| ---- | ----------- | -------------------------------------- |
| 1994 | SSL 2.0     | First attempt at encrypted web traffic |
| 1996 | SSL 3.0     | Improved security but still flawed     |
| 1999 | TLS 1.0     | Replaced SSL, more secure              |
| 2008 | TLS 1.2     | Stronger encryption, widely adopted    |
| 2018 | TLS 1.3     | Faster and even more secure            |

💡 **Verdict:** **SSL is dead. TLS 1.2+ is the modern standard.**

> **Further Reading:**
>
> * [TLS Wikipedia](https://en.wikipedia.org/wiki/Transport_Layer_Security)
> * [SSL vs TLS](https://www.cloudflare.com/learning/ssl/what-happened-to-ssl/)

***

## How HTTPS Works

HTTPS secures **data transmission** between **web browsers** and **web servers** using **TLS encryption**.

### **Step-by-Step HTTPS Connection**

1. **Client initiates a request** → `https://example.com`.
2. **Server sends an SSL/TLS certificate**.
3. **Browser verifies the certificate** against a **Certificate Authority (CA)**.
4. **TLS handshake happens** → Encryption keys are exchanged.
5. **Secure session is established** → Data is encrypted before transmission.

This **prevents eavesdropping, MITM attacks, and data manipulation**.

***

## HTTPS vs. Other Security Protocols

| Feature            | HTTPS                | HTTP            | SSH           | VPN         |
| ------------------ | -------------------- | --------------- | ------------- | ----------- |
| **Encryption**     | ✅ Yes                | ❌ No            | ✅ Yes         | ✅ Yes       |
| **Authentication** | ✅ Yes (Certificates) | ❌ No            | ✅ Yes         | ✅ Yes       |
| **Data Integrity** | ✅ Yes                | ❌ No            | ✅ Yes         | ✅ Yes       |
| **Speed**          | ✅ Fast               | ✅ Fast          | ❌ Slower      | ❌ Depends   |
| **Used By**        | Websites, APIs       | Legacy websites | Remote access | Enterprises |

💡 **Verdict:** **HTTPS is essential for web security, while SSH and VPNs serve different purposes.**

***

## 10 HTTPS Code Examples

### **1. Checking SSL Certificates in Linux (CLI)**

```bash
openssl s_client -connect example.com:443 -servername example.com
```

### **2. Generating a Self-Signed SSL Certificate (OpenSSL)**

```bash
openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -nodes
```

### **3. Using HTTPS in Python (Requests Library)**

```python
import requests

response = requests.get("https://example.com", verify=True)
print(response.text)
```

### **4. Enabling HTTPS in a Flask App**

```python
from flask import Flask

app = Flask(__name__)
app.run(ssl_context=('cert.pem', 'key.pem'))
```

### **5. Using HTTPS in C# (HttpClient)**

```csharp
using System.Net.Http;

HttpClient client = new HttpClient();
HttpResponseMessage response = await client.GetAsync("https://example.com");
Console.WriteLine(await response.Content.ReadAsStringAsync());
```

### **6. Using HTTPS in Java (HttpsURLConnection)**

```java
import java.net.URL;
import javax.net.ssl.HttpsURLConnection;

URL url = new URL("https://example.com");
HttpsURLConnection con = (HttpsURLConnection) url.openConnection();
con.connect();
System.out.println("Response Code: " + con.getResponseCode());
```

### **7. Using HTTPS in JavaScript (Node.js HTTPS Request)**

```javascript
const https = require("https");

https.get("https://example.com", (res) => {
    console.log("Status Code:", res.statusCode);
});
```

### **8. Enforcing HTTPS in an Nginx Server**

```nginx
server {
    listen 443 ssl;
    server_name example.com;

    ssl_certificate /etc/nginx/ssl/cert.pem;
    ssl_certificate_key /etc/nginx/ssl/key.pem;
}
```

### **9. Checking HTTPS Protocol Version (CLI)**

```bash
openssl s_client -connect example.com:443 -tls1_2
```

### **10. Using HTTPS in Golang**

```go
package main

import (
    "crypto/tls"
    "net/http"
)

func main() {
    tr := &http.Transport{
        TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
    }
    client := &http.Client{Transport: tr}
    resp, _ := client.Get("https://example.com")
    fmt.Println(resp.Status)
}
```

***

## Key Takeaways

* **HTTPS encrypts web traffic to protect against hackers and MITM attacks.**
* **TLS (not SSL) is the modern encryption standard for HTTPS.**
* **HTTPS is essential for APIs, websites, and secure communication.**
* **Alternatives like SSH and VPNs serve different security needs.**

***

## References

6. [TLS Wikipedia](https://en.wikipedia.org/wiki/Transport_Layer_Security)
7. [How HTTPS Works](https://howhttps.works/)
8. [SSL vs. TLS](https://www.cloudflare.com/learning/ssl/what-happened-to-ssl/)
9. [OpenSSL Commands](https://www.openssl.org/docs/manmaster/man1/openssl.html)
