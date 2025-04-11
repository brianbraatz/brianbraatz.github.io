---
title: Adding Swagger.json to GraphQL Middleware for Migrating SOAP to REST with NSwagStudio
description: Learn how to integrate Swagger.json into a GraphQL middleware to enable type-safe proxy generation using NSwagStudio.
slug: swagger-graphql-middleware-soap-rest
date: 2021-06-23
image: post/Articles/IMAGES/soap3.jpg
categories:
  - API
  - Web Services
  - GraphQL
  - Swagger
  - Migration
tags:
  - GraphQL
  - SOAP
  - REST
  - Swagger
  - NSwagStudio
  - API Proxy
draft: false
weight: 330
categories_ref:
  - API
  - Web Services
  - GraphQL
  - Swagger
  - Migration
slug_calculated: https://brianbraatz.github.io/p/swagger-graphql-middleware-soap-rest
lastmod: 2025-03-14T16:40:33.733Z
---
# Adding Swagger.json to GraphQL Middleware for Migrating SOAP to REST with NSwagStudio

Migrating from SOAP to REST can be a complex process, but **GraphQL as a middleware** provides a structured way to phase out SOAP services while keeping the system functional. To make this migration smoother, we can **generate a Swagger.json file** for the GraphQL middleware, allowing us to use **NSwagStudio** to generate **type-safe proxy classes** in **C#, Python, Go, or TypeScript**.

This approach helps **bury the SOAP complexity** while allowing UI and client code to interact with the RESTful GraphQL middleware using a generated proxy class.

***

## Why Add Swagger.json to GraphQL Middleware?

* **Enables automatic client SDK generation**: UI and client apps can interact with the API using strongly-typed proxies.
* **Abstracts SOAP complexity**: Developers work with a clean REST-like API instead of legacy SOAP XML.
* **Ensures consistency**: Swagger.json provides a standardized API description.
* **Allows for gradual migration**: Legacy SOAP calls get replaced with REST incrementally while maintaining the generated API contract.

***

## Step 1: Adding OpenAPI/Swagger Support to GraphQL Middleware

To generate a **Swagger.json** file for our **GraphQL middleware**, we need to expose the GraphQL queries and mutations via a RESTful OpenAPI specification.

### **1. Install Dependencies**

For a **Node.js/Express GraphQL middleware**, install **swagger-jsdoc** and **swagger-ui-express**:

```sh
npm install swagger-jsdoc swagger-ui-express
```

For a **Python FastAPI-based GraphQL middleware**:

```sh
pip install fastapi-graphql pydantic
```

***

### **2. Define OpenAPI Specification for GraphQL**

Create a `swagger.js` file to generate OpenAPI documentation:

```javascript
const swaggerJsDoc = require("swagger-jsdoc");
const swaggerUi = require("swagger-ui-express");

const swaggerOptions = {
  definition: {
    openapi: "3.0.0",
    info: {
      title: "GraphQL Middleware API",
      version: "1.0.0",
      description: "API gateway for transitioning SOAP to REST via GraphQL",
    },
    servers: [
      {
        url: "http://localhost:4000",
      },
    ],
  },
  apis: ["./routes/*.js"],
};

const swaggerDocs = swaggerJsDoc(swaggerOptions);
module.exports = { swaggerDocs, swaggerUi };
```

Then integrate it into an **Express.js GraphQL middleware**:

```javascript
const express = require("express");
const { graphqlHTTP } = require("express-graphql");
const { buildSchema } = require("graphql");
const { swaggerDocs, swaggerUi } = require("./swagger");

const app = express();
app.use("/api-docs", swaggerUi.serve, swaggerUi.setup(swaggerDocs));

const schema = buildSchema(`
  type Query {
    getUser(id: ID!): User
  }
  type User {
    id: ID!
    name: String
    email: String
  }
`);

app.use(
  "/graphql",
  graphqlHTTP({
    schema: schema,
    graphiql: true,
  })
);

app.listen(4000, () => console.log("Server running on port 4000"));
```

Now, hitting `http://localhost:4000/api-docs` displays the OpenAPI documentation.

***

## Step 2: Using NSwagStudio to Generate Proxy Classes

Once we have `swagger.json`, we can use **NSwagStudio** to generate **typed client proxies** for **C#, Python, Go, or TypeScript**.

### **1. Generate Swagger.json**

Export the Swagger definition using:

```sh
curl -o swagger.json http://localhost:4000/api-docs
```

### **2. Open NSwagStudio**

1. Download and install **NSwagStudio** from [NSwag GitHub](https://github.com/RicoSuter/NSwag).
2. Open **NSwagStudio** and load `swagger.json`.
3. Select the desired output format (C#, Python, Go, TypeScript).
4. Click **Generate Client Code**.

***

## Step 3: Using the Generated Proxy Classes

### **C# Proxy Class (Generated)**

```csharp
public class UserClient
{
    private readonly HttpClient _httpClient;
    public UserClient(HttpClient httpClient) { _httpClient = httpClient; }

    public async Task<User> GetUserAsync(int id)
    {
        return await _httpClient.GetFromJsonAsync<User>($"/users/{id}");
    }
}
```

### **Python Proxy Class (Generated)**

```python
import requests

class UserClient:
    def __init__(self, base_url):
        self.base_url = base_url

    def get_user(self, user_id):
        response = requests.get(f"{self.base_url}/users/{user_id}")
        return response.json()
```

### **TypeScript Proxy Class (Generated)**

```typescript
export class UserClient {
    constructor(private baseUrl: string) {}

    async getUser(id: number): Promise<User> {
        const response = await fetch(`${this.baseUrl}/users/${id}`);
        return await response.json();
    }
}
```

***

## Conclusion

By **adding Swagger.json to GraphQL middleware**, we enable **NSwagStudio** to generate **type-safe client proxies** for multiple programming languages. This approach allows:

1. **Abstracting SOAP complexity** behind GraphQL middleware.
2. **Preserving API contracts** while transitioning to REST.
3. **Using strongly-typed client code** instead of manual API requests.
4. **Enabling incremental SOAP-to-REST migration** without breaking existing clients.
<!-- 
## References

* [GraphQL Official Documentation](https://graphql.org/)
* [Swagger OpenAPI Specification](https://swagger.io/specification/)
* [NSwagStudio Documentation](https://github.com/RicoSuter/NSwag)

***

title: "Comparing Legacy SOAP Code with Swagger-Generated REST Proxies"\
description: "A side-by-side comparison of old SOAP calls vs. new RESTful client proxies generated by Swagger.json."\
slug: "comparing-soap-vs-swagger-generated-rest"\
date: 2021-09-15\
image: "post/Articles/IMAGES/36.jpg"\
categories: \["API", "Web Services", "GraphQL", "Swagger", "Migration"]\
tags: \["SOAP", "REST", "Swagger", "NSwagStudio", "API Proxy", "Migration"]\
draft: false\
weight: 345
-----------

# Comparing Legacy SOAP Code with Swagger-Generated REST Proxies

## Introduction

Migrating from SOAP to REST can be tricky, especially when dealing with **complex schemas** like `Person`, `Address`, `Order`, and `Order Items`. By generating **type-safe proxy classes** from **Swagger.json** using **NSwagStudio**, we can abstract the complexity of SOAP and move towards RESTful services more smoothly.

This article compares **10 SOAP calls** with their equivalent **Swagger-generated RESTful client code** and analyzes the pros and cons of each approach. We'll also discuss alternative migration strategies and whether this approach is the best fit for your project. -->

***

## Example 1: Fetching a Person by ID

### **SOAP Request:**

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
   <soapenv:Body>
      <GetPersonRequest>
         <personId>12345</personId>
      </GetPersonRequest>
   </soapenv:Body>
</soapenv:Envelope>
```

### **SOAP Response:**

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
   <soapenv:Body>
      <GetPersonResponse>
         <person>
            <id>12345</id>
            <name>John Doe</name>
            <age>30</age>
         </person>
      </GetPersonResponse>
   </soapenv:Body>
</soapenv:Envelope>
```

### **Swagger-Generated REST Proxy (C#):**

```csharp
var personClient = new PersonClient(httpClient);
Person person = await personClient.GetPersonAsync(12345);
Console.WriteLine(person.Name);
```

### **REST API Response (JSON):**

```json
{
  "id": "12345",
  "name": "John Doe",
  "age": 30
}
```

***

## Example 2: Fetching a Person with Address

### **SOAP Request:**

```xml
<GetPersonWithAddressRequest>
   <personId>12345</personId>
</GetPersonWithAddressRequest>
```

### **SOAP Response:**

```xml
<GetPersonWithAddressResponse>
   <person>
      <id>12345</id>
      <name>John Doe</name>
      <address>
         <street>123 Main St</street>
         <city>Springfield</city>
         <zip>12345</zip>
      </address>
   </person>
</GetPersonWithAddressResponse>
```

### **Swagger-Generated REST Proxy (C#):**

```csharp
var personClient = new PersonClient(httpClient);
Person person = await personClient.GetPersonWithAddressAsync(12345);
Console.WriteLine(person.Address.Street);
```

### **REST API Response (JSON):**

```json
{
  "id": "12345",
  "name": "John Doe",
  "address": {
    "street": "123 Main St",
    "city": "Springfield",
    "zip": "12345"
  }
}
```

***

## Example 3: Fetching an Order with Order Items

### **SOAP Request:**

```xml
<GetOrderRequest>
   <orderId>98765</orderId>
</GetOrderRequest>
```

### **SOAP Response:**

```xml
<GetOrderResponse>
   <order>
      <id>98765</id>
      <customerName>John Doe</customerName>
      <items>
         <item>
            <name>Laptop</name>
            <price>1000</price>
         </item>
         <item>
            <name>Mouse</name>
            <price>50</price>
         </item>
      </items>
   </order>
</GetOrderResponse>
```

### **Swagger-Generated REST Proxy (C#):**

```csharp
var orderClient = new OrderClient(httpClient);
Order order = await orderClient.GetOrderAsync(98765);
Console.WriteLine(order.CustomerName);
```

### **REST API Response (JSON):**

```json
{
  "id": "98765",
  "customerName": "John Doe",
  "items": [
    { "name": "Laptop", "price": 1000 },
    { "name": "Mouse", "price": 50 }
  ]
}
```

***

## Pros and Cons

### **SOAP Pros:**

* Strongly defined contracts via **WSDL**.
* Robust **security mechanisms** (WS-Security, WS-ReliableMessaging).
* Widely used in **enterprise** applications.

### **SOAP Cons:**

* **Verbose XML format** compared to JSON.
* **Slower performance** due to larger payloads.
* Requires **WSDL parsing and SOAP-specific libraries**.

### **REST with Swagger-Generated Clients Pros:**

* **Lightweight JSON responses** improve performance.
* **Easier to integrate** with modern applications.
* Can be used with **GraphQL as a middleware**.
* **NSwagStudio generates type-safe client code**.

### **REST with Swagger-Generated Clients Cons:**

* **Less security standardization** (compared to WS-Security).
* REST can have **less strict contracts** than SOAP.
* Requires **Swagger/OpenAPI documentation** maintenance.

***

## Alternative Approaches

| Approach                        | Pros                                    | Cons                         |
| ------------------------------- | --------------------------------------- | ---------------------------- |
| **GraphQL as Middleware**       | Flexible queries, incremental migration | Overhead of GraphQL layer    |
| **Direct SOAP to REST Rewrite** | Clean REST endpoints                    | High development effort      |
| **SOAP-to-REST Adapters**       | Allows old SOAP clients to call REST    | Requires adapter maintenance |

<!-- ***

## Conclusion

Using **Swagger.json** to generate **type-safe REST clients** is an excellent approach to migrating from SOAP, allowing for **incremental migration** while keeping existing functionality available. However, consider the **trade-offs**, such as security differences and contract enforcement.

For large enterprises with **high-security needs**, SOAP may still be relevant. However, for **modern applications**, REST and GraphQL with **Swagger-generated clients** provide a much **smoother developer experience** and **performance boost**. -->

## References

* [NSwagStudio Documentation](https://github.com/RicoSuter/NSwag)
* [GraphQL vs REST](https://graphql.org/)
* [SOAP vs REST](https://www.soapui.org/learn/api/soap-vs-rest/)
