---
title: "SOAP vs REST Explained: History, Comparison, and Migration Tips"
description: "SOAP vs REST Explained: History, Comparison, and Migration Tips"
slug: soap-vs-rest-explained-history-comparison-migration-tips
date: 2020-04-22
image: post/Articles/IMAGES/soap4.jpg
categories:
  - API
  - Web Services
  - Integration
tags:
  - SOAP
  - REST
  - API
  - Web Services
  - Integration
  - Migration
draft: false
weight: 257
categories_ref:
  - API
  - Web Services
  - Integration
slug_calculated: https://brianbraatz.github.io/p/soap-vs-rest-explained-history-comparison-migration-tips
lastmod: 2025-03-19T13:56:36.919Z
---
# SOAP vs REST Explained: History, Comparison, and Migration Tips

SOAP and REST have been developed to serve different purposes, as SOAP is a protocol, and REST is an architectural style that could be applied to a protocol.

## A Brief History Lesson

In the late 1990s, SOAP (Simple Object Access Protocol) emerged as a way to structure communication between applications over the web.

It relied on XML for data exchange and had strict rules, making it reliable but also heavy and verbose.

Then along came REST (Representational State Transfer) in the early 2000s, a more lightweight, flexible alternative.

REST is an architectural style rather than a strict protocol, and it operates naturally over HTTP using standard methods like GET, POST, PUT, and DELETE.

## SOAP: The Strict, Heavyweight Protocol

SOAP enforces a strict contract using WSDL (Web Services Description Language) and wraps all messages inside XML envelopes.

### Example WSDL Definition:

```xml
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:tns="http://example.com/soap-service"
             name="SoapService"
             targetNamespace="http://example.com/soap-service">
  <message name="GetUserRequest">
    <part name="userId" type="xsd:string"/>
  </message>
  <message name="GetUserResponse">
    <part name="userName" type="xsd:string"/>
    <part name="email" type="xsd:string"/>
  </message>
</definitions>
```

### Example SOAP Request:

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:web="http://example.com/soap-service">
   <soapenv:Body>
      <web:GetUserRequest>
         <web:userId>12345</web:userId>
      </web:GetUserRequest>
   </soapenv:Body>
</soapenv:Envelope>
```

### Example SOAP Response:

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:web="http://example.com/soap-service">
   <soapenv:Body>
      <web:GetUserResponse>
         <web:userName>John Doe</web:userName>
         <web:email>john.doe@example.com</web:email>
      </web:GetUserResponse>
   </soapenv:Body>
</soapenv:Envelope>
```

## REST: The Lightweight, Flexible Approach

REST is based on simple HTTP requests, often using JSON for data exchange.

### Example REST Request:

```
GET /users/12345 HTTP/1.1
Host: example.com
Accept: application/json
```

### Example REST Response:

```json
{
  "userName": "John Doe",
  "email": "john.doe@example.com"
}
```

No XML, no extra fluff—just the essentials.

## Translating REST into SOAP: 10 Examples

| REST Operation              | SOAP Equivalent           |
| --------------------------- | ------------------------- |
| `POST /users`               | `<CreateUserRequest>`     |
| `GET /users/12345`          | `<GetUserRequest>`        |
| `PUT /users/12345`          | `<UpdateUserRequest>`     |
| `DELETE /users/12345`       | `<DeleteUserRequest>`     |
| `GET /users`                | `<ListUsersRequest>`      |
| `POST /auth`                | `<AuthenticateRequest>`   |
| `GET /users/12345/orders`   | `<GetUserOrdersRequest>`  |
| `PUT /users/12345/password` | `<UpdatePasswordRequest>` |
| `GET /health`               | `<CheckHealthRequest>`    |
| `POST /logout`              | `<LogoutRequest>`         |

### Example REST-to-SOAP Conversion

REST API Call:

```
POST /users
```

REST JSON Request Body:

```json
{
  "userName": "Jane Doe",
  "email": "jane.doe@example.com"
}
```

SOAP Equivalent:

```xml
<CreateUserRequest>
  <userName>Jane Doe</userName>
  <email>jane.doe@example.com</email>
</CreateUserRequest>
```

## Can You Write an Adapter?

Yes! A REST-to-SOAP adapter can sit between the client and the SOAP service, translating JSON REST calls into XML SOAP requests.

### Example REST-to-SOAP Adapter (Python)

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

## Key Ideas Table

| Concept     | SOAP   | REST      |
| ----------- | ------ | --------- |
| Format      | XML    | JSON, XML |
| Verbosity   | High   | Low       |
| Flexibility | Low    | High      |
| Speed       | Slower | Faster    |
| Stateless   | No     | Yes       |

## References

* [SOAP vs REST: A Comparison](https://www.utupub.fi/)
* [W3C SOAP Standard](https://www.w3.org/TR/soap/)
* [Roy Fielding’s REST Dissertation](https://www.ics.uci.edu/~fielding/pubs/dissertation/top.htm)
