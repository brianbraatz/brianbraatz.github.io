---
title: Migrating SOAP to REST Using the Strangler Pattern
description: Methods to gradually migrate large SOAP-based systems to REST using the Strangler Pattern.
slug: migrating-soap-to-rest-strangler-pattern
date: 2019-07-18
image: post/Articles/IMAGES/31.jpg
categories:
  - API
  - Web Services
  - Migration
tags:
  - SOAP
  - REST
  - Strangler Pattern
  - API Migration
  - Legacy Systems
draft: false
weight: 321
categories_ref:
  - API
  - Web Services
  - Migration
lastmod: 2025-03-14T15:45:26.670Z
---
# 10 Strategies for Migrating from SOAP to REST Using the Strangler Pattern

## Introduction

Migrating from SOAP to REST can feel like replacing the engine of a moving car. You have a massive, existing system that cannot afford downtime, yet you need to modernize it.

Instead of rewriting everything at once (which is a recipe for disaster), we use the **Strangler Pattern**—gradually replacing parts of the system while keeping it operational.

## What is the Strangler Pattern?

The **Strangler Pattern** allows you to replace legacy functionality piece by piece by **wrapping** old functionality and incrementally introducing new features.

Think of it as growing a new tree around an old one until the old tree is no longer needed.

### Key Benefits:

* No need for a risky "big bang" rewrite.
* Allows incremental testing and deployment.
* Keeps the system running while transitioning.
* Reduces complexity by handling one service at a time.

Now, let’s dive into **10 strategies** for migrating from SOAP to REST using the Strangler Pattern.

***

## 1. **Expose a RESTful Facade Over SOAP**

Start by building a REST API **wrapper** around your existing SOAP services. This allows REST clients to call the new API while SOAP services remain intact.

### Example:

```python
from flask import Flask, request, jsonify
import requests

app = Flask(__name__)
SOAP_ENDPOINT = "http://example.com/soap-service"

@app.route("/users/<user_id>", methods=["GET"])
def get_user(user_id):
    soap_request = f"""
    <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
       <soapenv:Body>
          <GetUserRequest>
             <userId>{user_id}</userId>
          </GetUserRequest>
       </soapenv:Body>
    </soapenv:Envelope>
    """
    response = requests.post(SOAP_ENDPOINT, data=soap_request, headers={"Content-Type": "text/xml"})
    return jsonify(parse_soap_response(response.text))

def parse_soap_response(soap_response):
    return {"userName": "John Doe", "email": "john.doe@example.com"}

if __name__ == "__main__":
    app.run()
```

***

## 2. **Use an API Gateway to Route Traffic**

Deploy an **API Gateway** (e.g., Kong, Apigee, AWS API Gateway) to route requests to either SOAP or REST endpoints as needed.

```yaml
paths:
  /users/{userId}:
    get:
      x-amazon-apigateway-integration:
        uri: "https://old-soap-service.com/users"
        httpMethod: "POST"
        type: "AWS"
```

***

## 3. **Implement a Feature Flag System**

Use feature flags to control whether a request goes to SOAP or REST dynamically.

```python
if feature_flag_enabled("use_rest_api"):
    response = call_rest_api(user_id)
else:
    response = call_soap_api(user_id)
```

***

## 4. **Gradually Migrate Endpoints One by One**

Start migrating less critical services first. For example:

* `GET /users` → Migrated to REST
* `POST /orders` → Still using SOAP

***

## 5. **Data Synchronization with Event-Driven Architecture**

Instead of fetching data from SOAP, **sync it in real-time** to a new REST-based service.

```python
@app.route("/users", methods=["POST"])
def create_user():
    data = request.json
    event_bus.publish("user_created", data)
    return {"message": "User creation event sent"}, 202
```

***

## 6. **Reverse Proxy with Nginx to Split Traffic**

Use **Nginx** as a reverse proxy to route traffic between SOAP and REST based on API paths.

```nginx
location /api/v1/ {
    proxy_pass http://new-rest-service;
}
location /api/v2/ {
    proxy_pass http://old-soap-service;
}
```

***

## 7. **GraphQL as an Intermediate Layer**

Instead of directly exposing REST or SOAP, introduce **GraphQL** as a middle layer that queries SOAP while REST endpoints are built.

```graphql
query {
  user(id: "12345") {
    name
    email
  }
}
```

***

## 8. **Parallel Running with Logging & Analytics**

Send requests to both SOAP and REST but only use REST responses if they match expectations.

```python
soap_response = call_soap_api(user_id)
rest_response = call_rest_api(user_id)
log_difference(soap_response, rest_response)
```

***

## 9. **Gradually Redirect Clients to REST**

Use HTTP 301 redirects to move clients to REST.

```python
@app.route("/old-endpoint")
def redirect_to_new():
    return redirect("/new-rest-endpoint", code=301)
```

***

## 10. **Final Cutover & Deprecation**

Once all SOAP endpoints have been replaced, shut down the old system.

```bash
systemctl stop soap_service
```

***

## Conclusion

Migrating from SOAP to REST doesn’t have to be a nightmare. By using the **Strangler Pattern**, you can slowly transition without breaking the system. Whether you use API gateways, feature flags, or reverse proxies, the key is **incremental change**.

## References

* [Strangler Fig Pattern - Martin Fowler](https://martinfowler.com/bliki/StranglerFigApplication.html)
* [API Gateway for Migration](https://aws.amazon.com/api-gateway/)
* [GraphQL as a Migration Layer](https://graphql.org/)
