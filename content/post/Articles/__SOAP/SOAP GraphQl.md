---
title: "Deep Dive: Using GraphQL as a Middleware for Migrating SOAP to REST"
description: A detailed guide on leveraging GraphQL as a middleware to bridge SOAP and REST during migration using the Strangler Pattern.
slug: graphql-middleware-soap-migration
date: 2021-03-11
image: post/Articles/IMAGES/30.jpg
categories:
  - API
  - Web Services
  - GraphQL
  - Migration
tags:
  - GraphQL
  - SOAP
  - REST
  - Strangler Pattern
  - API Migration
  - Legacy Systems
draft: false
weight: 289
lastmod: 2025-02-15T12:27:02.817Z
---
# Deep Dive: Using GraphQL as a Middleware for Migrating SOAP to REST

## Introduction

Migrating a large, legacy SOAP-based system to REST is a daunting task. The **Strangler Pattern** provides a proven approach to gradually replace legacy functionality without disrupting the entire system. One of the best tools to achieve this gradual migration is **GraphQL**, acting as an intelligent middleware between the old SOAP services and the new REST endpoints.

This article takes a **deep dive** into using GraphQL as a middle layer to bridge SOAP and REST, allowing for incremental migration while keeping everything operational.

## What is the Strangler Pattern?

The **Strangler Pattern** is a software design approach where a new system is gradually built alongside the old one, slowly replacing components over time. Instead of a risky full-system rewrite, developers introduce new capabilities incrementally while ensuring the old system remains functional.

### How GraphQL Fits Into the Strangler Pattern

GraphQL can act as an **abstraction layer** over SOAP services while the new REST APIs are being developed. This allows clients to query GraphQL, which can fetch data from either SOAP or REST dynamically. Over time, SOAP dependencies are eliminated without requiring major rewrites.

### Advantages of Using GraphQL as a Middleware:

* **Unified API Interface**: Abstracts underlying differences between SOAP and REST.
* **Gradual Migration**: New REST endpoints replace SOAP calls incrementally.
* **Flexible Queries**: Clients can request only the data they need.
* **Performance Improvements**: Reduces over-fetching and under-fetching of data.

***

## Setting Up GraphQL as a Middleware

### 1. Defining the GraphQL Schema

First, define a GraphQL schema that maps both SOAP and REST fields.

```graphql
type User {
  id: ID!
  name: String
  email: String
}

type Query {
  getUser(id: ID!): User
}
```

***

### 2. Creating a Resolver That Calls SOAP Services

To fetch data from SOAP, we need a resolver that translates GraphQL requests into SOAP API calls.

```javascript
const axios = require('axios');
const xml2js = require('xml2js');

async function fetchUserFromSOAP(userId) {
  const soapRequest = `
    <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
       <soapenv:Body>
          <GetUserRequest>
             <userId>${userId}</userId>
          </GetUserRequest>
       </soapenv:Body>
    </soapenv:Envelope>
  `;
  
  const { data } = await axios.post('http://example.com/soap-service', soapRequest, {
    headers: { 'Content-Type': 'text/xml' },
  });
  
  const parsedData = await xml2js.parseStringPromise(data);
  return {
    id: userId,
    name: parsedData.Envelope.Body[0].GetUserResponse[0].userName[0],
    email: parsedData.Envelope.Body[0].GetUserResponse[0].email[0],
  };
}
```

***

### 3. Creating a Resolver That Calls REST Services

As REST endpoints are built, they replace SOAP calls in the GraphQL middleware.

```javascript
async function fetchUserFromREST(userId) {
  const { data } = await axios.get(`http://example.com/api/users/${userId}`);
  return data;
}
```

***

### 4. Combining SOAP and REST Calls in GraphQL

Now, we modify our GraphQL resolver to **dynamically call SOAP or REST** based on migration progress.

```javascript
const resolvers = {
  Query: {
    getUser: async (_, { id }) => {
      if (useRestEndpoint(id)) {
        return await fetchUserFromREST(id);
      } else {
        return await fetchUserFromSOAP(id);
      }
    },
  },
};
```

The function `useRestEndpoint(id)` determines whether to call REST or fallback to SOAP.

```javascript
function useRestEndpoint(id) {
  return id > 1000; // Example: Use REST for newer users, SOAP for old ones
}
```

***

## Migration Strategy with GraphQL

1. **Start with GraphQL as a wrapper** over SOAP services.
2. **Expose GraphQL to clients**, allowing them to fetch data in a unified way.
3. **Incrementally replace SOAP calls** with REST calls in GraphQL resolvers.
4. **Monitor usage metrics** to track which SOAP services are still being called.
5. **Deprecate SOAP services** once all dependencies are removed.
6. **Expose REST API directly** to clients once migration is complete.

***

## Performance Optimization

### **Batching and Caching**

Use DataLoader to batch multiple GraphQL requests to reduce SOAP overhead.

```javascript
const DataLoader = require('dataloader');
const userLoader = new DataLoader(keys => batchFetchUsers(keys));
```

### **GraphQL Federation**

Introduce **GraphQL Federation** to split legacy and modern services cleanly.

```graphql
type User @key(fields: "id") {
  id: ID!
  name: String
  email: String
}
```

***

## Conclusion

Using GraphQL as a middleware **enables a smooth migration from SOAP to REST** while keeping the system operational. By following the Strangler Pattern, you can modernize your architecture **without disrupting** existing clients.

## References

* [GraphQL Official Documentation](https://graphql.org/)
* [Strangler Pattern - Martin Fowler](https://martinfowler.com/bliki/StranglerFigApplication.html)
* [SOAP to REST API Migration](https://aws.amazon.com/api-gateway/)

# =====================================

\===============================================

***

title: "How to Use GraphQL as a Middleware for Migrating SOAP to REST"\
description: "A detailed guide comparing legacy SOAP code with new REST implementations using GraphQL as a middleware."\
slug: "graphql-middleware-migrating-soap-to-rest"\
date: 2020-10-05\
image: "post/Articles/IMAGES/32.jpg"\
categories: \["API", "Web Services", "GraphQL", "Migration"]\
tags: \["GraphQL", "SOAP", "REST", "Middleware", "API Migration"]\
draft: false\
weight: 315
-----------

# How to Use GraphQL as a Middleware for Migrating SOAP to REST

Migrating from SOAP to REST is a challenging task, especially when dealing with a massive legacy system. One effective way to ease the transition is to use **GraphQL as a middleware**, allowing clients to request data via GraphQL while the backend gradually shifts from SOAP to REST.

This article demonstrates **how to wrap SOAP calls with GraphQL**, migrate to REST, and compare the before-and-after code. We'll also discuss compatibility issues, datatype mismatches, and bad design patterns that can make this process difficult.

## What Did the Legacy SOAP Code Look Like?

A traditional SOAP-based system follows a **strictly structured XML format**. Here’s what a SOAP request and response for fetching a user might look like:

### SOAP Request:

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
   <soapenv:Body>
      <GetUserRequest>
         <userId>12345</userId>
      </GetUserRequest>
   </soapenv:Body>
</soapenv:Envelope>
```

### SOAP Response:

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
   <soapenv:Body>
      <GetUserResponse>
         <user>
            <id>12345</id>
            <name>John Doe</name>
            <email>john.doe@example.com</email>
         </user>
      </GetUserResponse>
   </soapenv:Body>
</soapenv:Envelope>
```

### SOAP Service Code (Legacy):

```java
@WebService
public class UserService {
    @WebMethod
    public User getUser(String userId) {
        return new User(userId, "John Doe", "john.doe@example.com");
    }
}
```

***

## Wrapping SOAP with GraphQL Middleware

To allow clients to query data **without being aware of SOAP**, we can introduce **GraphQL as a middleware**. This involves setting up GraphQL resolvers that fetch data from the existing SOAP services.

### GraphQL Schema Definition:

```graphql
type User {
  id: ID!
  name: String
  email: String
}

type Query {
  getUser(id: ID!): User
}
```

### GraphQL Resolver for SOAP:

```javascript
const axios = require('axios');
const xml2js = require('xml2js');

async function fetchUserFromSOAP(userId) {
  const soapRequest = `
    <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
       <soapenv:Body>
          <GetUserRequest>
             <userId>${userId}</userId>
          </GetUserRequest>
       </soapenv:Body>
    </soapenv:Envelope>
  `;

  const { data } = await axios.post('http://example.com/soap-service', soapRequest, {
    headers: { 'Content-Type': 'text/xml' },
  });

  const parsedData = await xml2js.parseStringPromise(data);
  return {
    id: userId,
    name: parsedData.Envelope.Body[0].GetUserResponse[0].user[0].name[0],
    email: parsedData.Envelope.Body[0].GetUserResponse[0].user[0].email[0],
  };
}
```

***

## Migrating to REST: The New Approach

Now, as we build out REST endpoints, we modify our GraphQL middleware to use REST instead of SOAP.

### REST API Request:

```
GET /users/12345 HTTP/1.1
Host: example.com
Accept: application/json
```

### REST API Response:

```json
{
  "id": "12345",
  "name": "John Doe",
  "email": "john.doe@example.com"
}
```

### REST API Controller:

```python
from flask import Flask, jsonify

app = Flask(__name__)

@app.route('/users/<user_id>', methods=['GET'])
def get_user(user_id):
    return jsonify({"id": user_id, "name": "John Doe", "email": "john.doe@example.com"})

if __name__ == '__main__':
    app.run()
```

### GraphQL Resolver for REST:

```javascript
async function fetchUserFromREST(userId) {
  const { data } = await axios.get(`http://example.com/api/users/${userId}`);
  return data;
}
```

### Switching GraphQL from SOAP to REST:

```javascript
const resolvers = {
  Query: {
    getUser: async (_, { id }) => {
      return await fetchUserFromREST(id);
    },
  },
};
```

***

## Compatibility Issues & Limitations

### 1. **Datatype Differences**

* SOAP returns **XML**, while REST commonly returns **JSON**.
* XML parsing can introduce overhead, while JSON is more lightweight.

### 2. **Field Name Differences**

* SOAP responses may have nested structures (`<user><name>John</name></user>`) which don’t directly map to JSON.
* Fields might have different casing (`userName` vs. `name`).

### 3. **Bad Practices in SOAP That Cause Migration Issues**

* **Tightly Coupled Services**: SOAP services often expose monolithic endpoints instead of granular resources.
* **Complex Authorization Layers**: SOAP frequently embeds security headers that are harder to translate into RESTful authentication models.
* **Stateful Services**: Some SOAP services maintain session-based state, which contradicts REST’s stateless nature.

***

## Conclusion

Using **GraphQL as a middleware** provides a seamless way to migrate from SOAP to REST. It abstracts the underlying complexity, allowing clients to request data from a single GraphQL interface while backend services transition from SOAP to REST. However, be aware of **compatibility issues** such as XML vs. JSON, nested structures, and tight coupling in SOAP that can make migration more challenging.

## References

* [GraphQL Official Documentation](https://graphql.org/)
* [SOAP to REST API Migration](https://aws.amazon.com/api-gateway/)
* [Strangler Pattern - Martin Fowler](https://martinfowler.com/bliki/StranglerFigApplication.html)

\==================

\===========================

\===========================

# betetr code examples?

***

title: "How to Use GraphQL as a Middleware for Migrating SOAP to REST"\
description: "A detailed guide comparing legacy SOAP code with new REST implementations using GraphQL as a middleware."\
slug: "graphql-middleware-migrating-soap-to-rest"\
date: 2020-10-05\
image: "post/Articles/IMAGES/32.jpg"\
categories: \["API", "Web Services", "GraphQL", "Migration"]\
tags: \["GraphQL", "SOAP", "REST", "Middleware", "API Migration"]\
draft: false\
weight: 315
-----------

# How to Use GraphQL as a Middleware for Migrating SOAP to REST

Migrating from SOAP to REST is a challenging task, especially when dealing with a massive legacy system. One effective way to ease the transition is to use **GraphQL as a middleware**, allowing clients to request data via GraphQL while the backend gradually shifts from SOAP to REST.

This article demonstrates **how to wrap SOAP calls with GraphQL**, migrate to REST, and compare the before-and-after code. We'll also discuss compatibility issues, datatype mismatches, and bad design patterns that can make this process difficult.

## What Did the Legacy SOAP Code Look Like?

A traditional SOAP-based system follows a **strictly structured XML format**. Here’s what a SOAP request and response for fetching a user might look like:

### SOAP Request (Old Client Code):

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
   <soapenv:Body>
      <GetUserRequest>
         <userId>12345</userId>
      </GetUserRequest>
   </soapenv:Body>
</soapenv:Envelope>
```

### SOAP Response:

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
   <soapenv:Body>
      <GetUserResponse>
         <user>
            <id>12345</id>
            <name>John Doe</name>
            <email>john.doe@example.com</email>
         </user>
      </GetUserResponse>
   </soapenv:Body>
</soapenv:Envelope>
```

### SOAP Client Code (Java Example):

```java
import javax.xml.soap.*;

public class SoapClient {
    public static void main(String[] args) throws Exception {
        SOAPConnectionFactory factory = SOAPConnectionFactory.newInstance();
        SOAPConnection connection = factory.createConnection();
        
        String url = "http://example.com/soap-service";
        SOAPMessage request = createSOAPRequest();
        SOAPMessage response = connection.call(request, url);
        response.writeTo(System.out);
    }

    private static SOAPMessage createSOAPRequest() throws Exception {
        MessageFactory messageFactory = MessageFactory.newInstance();
        SOAPMessage message = messageFactory.createMessage();
        SOAPPart soapPart = message.getSOAPPart();
        SOAPEnvelope envelope = soapPart.getEnvelope();
        SOAPBody body = envelope.getBody();
        body.addChildElement("GetUserRequest").addChildElement("userId").addTextNode("12345");
        message.saveChanges();
        return message;
    }
}
```

***

## Migrating to REST: The New Approach

Now, as we build out REST endpoints, we modify our GraphQL middleware to use REST instead of SOAP.

### REST API Request (New Client Code):

```
GET /users/12345 HTTP/1.1
Host: example.com
Accept: application/json
```

### REST API Response:

```json
{
  "id": "12345",
  "name": "John Doe",
  "email": "john.doe@example.com"
}
```

### REST Client Code (Python Example):

```python
import requests

response = requests.get("http://example.com/api/users/12345")
print(response.json())
```

***

## Compatibility Issues & Limitations

### 1. **Datatype Differences**

* SOAP returns **XML**, while REST commonly returns **JSON**.
* XML parsing can introduce overhead, while JSON is more lightweight.

### 2. **Field Name Differences**

* SOAP responses may have nested structures (`<user><name>John</name></user>`) which don’t directly map to JSON.
* Fields might have different casing (`userName` vs. `name`).

### 3. **Bad Practices in SOAP That Cause Migration Issues**

* **Tightly Coupled Services**: SOAP services often expose monolithic endpoints instead of granular resources.
* **Complex Authorization Layers**: SOAP frequently embeds security headers that are harder to translate into RESTful authentication models.
* **Stateful Services**: Some SOAP services maintain session-based state, which contradicts REST’s stateless nature.

***

## Conclusion

Using **GraphQL as a middleware** provides a seamless way to migrate from SOAP to REST. It abstracts the underlying complexity, allowing clients to request data from a single GraphQL interface while backend services transition from SOAP to REST. However, be aware of **compatibility issues** such as XML vs. JSON, nested structures, and tight coupling in SOAP that can make migration more challenging.

## References

* [GraphQL Official Documentation](https://graphql.org/)
* [SOAP to REST API Migration](https://aws.amazon.com/api-gateway/)
* [Strangler Pattern - Martin Fowler](https://martinfowler.com/bliki/StranglerFigApplication.html)
