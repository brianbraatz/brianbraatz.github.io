---
title: "GraphQL: Comparing Apollo Federation, Schema Stitching, and Monolithic GraphQL Servers"
description: "GraphQL: Comparing Apollo Federation, Schema Stitching, and Monolithic GraphQL Servers"
slug: graphql-comparing-Stitching
date: 2021-08-14
image: post/Articles/IMAGES/32.jpg
categories:
  - GraphQL
  - Apollo Federation
  - Schema Stitching
  - Monolithic GraphQL
  - API Design
  - Cloud
tags:
  - GraphQL
  - Apollo
  - Federation
  - Schema
  - Stitching
  - Monolithic
  - GraphQL
  - API
  - Design
draft: false
weight: 573
categories_ref:
  - GraphQL
  - Apollo Federation
  - Schema Stitching
  - Monolithic GraphQL
  - API Design
  - Cloud
slug_calculated: https://brianbraatz.github.io/p/graphql-comparing-apollo-federation-schema-stitching-and-monolithic-graphql-servers
lastmod: 2025-03-18T22:43:12.157Z
---
# GraphQL: Comparing Apollo Federation, Schema Stitching, and Monolithic GraphQL Servers

GraphQL is like a buffet of data—grab only what you need, leave the rest.

But how you serve it up matters, and that's where Apollo Federation, Schema Stitching, and the good ol’ Monolithic GraphQL server come into play.

Each approach has its own strengths, weaknesses, and quirks. Let’s dive into this battle royale of GraphQL architecture!

## The Contenders

### 1. Monolithic GraphQL Server 🏰

The simplest and most traditional setup. A single GraphQL server handles all requests, pulling data from databases, APIs, or microservices.

#### Pros:

* Simple to set up, easy to maintain
* No need to worry about stitching or federating multiple schemas
* Works well for small teams or projects

#### Cons:

* Can become a massive, unmanageable beast as the app grows
* Harder to scale with multiple teams working on different parts of the API

#### Example:

```javascript
const { ApolloServer, gql } = require('apollo-server');

const typeDefs = gql`
  type Query {
    hello: String
  }
`;

const resolvers = {
  Query: {
    hello: () => 'Hello, Monolith!'
  }
};

const server = new ApolloServer({ typeDefs, resolvers });

server.listen().then(({ url }) => {
  console.log(`🚀  Server ready at ${url}`);
});
```

***

### 2. Schema Stitching 🧵

Schema Stitching is like duct-taping multiple GraphQL APIs together into one unified API.

Each service exposes its own GraphQL API, and a central service stitches them into a single schema.

#### Pros:

* Allows for independent GraphQL services
* Easier to integrate third-party GraphQL APIs
* Good for legacy GraphQL services

#### Cons:

* Requires manual merging of schemas
* Can get messy with type conflicts and resolver coordination

#### Example:

```javascript
const { stitchSchemas } = require('@graphql-tools/stitch');
const { makeExecutableSchema } = require('@graphql-tools/schema');

const userSchema = makeExecutableSchema({
  typeDefs: `
    type User {
      id: ID!
      name: String!
    }
    type Query {
      user(id: ID!): User
    }
  `,
  resolvers: {
    Query: {
      user: (_, { id }) => ({ id, name: "Alice" })
    }
  }
});

const productSchema = makeExecutableSchema({
  typeDefs: `
    type Product {
      id: ID!
      name: String!
    }
    type Query {
      product(id: ID!): Product
    }
  `,
  resolvers: {
    Query: {
      product: (_, { id }) => ({ id, name: "Laptop" })
    }
  }
});

const stitchedSchema = stitchSchemas({
  subschemas: [userSchema, productSchema]
});

module.exports = stitchedSchema;
```

***

### 3. Apollo Federation 🚀

Apollo Federation is like Schema Stitching’s cooler, smarter cousin.

It allows services to define their own GraphQL schemas, but instead of merging them manually, it uses a special gateway to orchestrate everything.

#### Pros:

* Each service can own its part of the schema independently
* No need for manual schema merging
* Scales beautifully for microservices

#### Cons:

* More complex setup compared to monolithic GraphQL
* Requires Apollo's gateway service

#### Example:

##### Apollo Gateway

```javascript
const { ApolloGateway } = require('@apollo/gateway');
const { ApolloServer } = require('apollo-server');

const gateway = new ApolloGateway({
  serviceList: [
    { name: 'users', url: 'http://localhost:4001' },
    { name: 'products', url: 'http://localhost:4002' }
  ]
});

const server = new ApolloServer({ gateway, subscriptions: false });

server.listen().then(({ url }) => {
  console.log(`🚀 Gateway ready at ${url}`);
});
```

***

## Conclusion

| Approach           | Pros                                                   | Cons                                           |
| ------------------ | ------------------------------------------------------ | ---------------------------------------------- |
| Monolithic GraphQL | Simple, easy to manage                                 | Hard to scale, can become a bottleneck         |
| Schema Stitching   | Good for integrating multiple GraphQL APIs             | Requires manual merging, potential conflicts   |
| Apollo Federation  | Best for microservices, automatic schema orchestration | More setup complexity, requires Apollo Gateway |

In short:

* **Monolithic GraphQL** is great for small apps.
* **Schema Stitching** is handy for merging existing GraphQL services.
* **Apollo Federation** is the powerhouse for microservices.

Pick your fighter wisely!

***

## Key Ideas

| Concept            | Description                                                     |
| ------------------ | --------------------------------------------------------------- |
| Monolithic GraphQL | A single server handling all GraphQL requests                   |
| Schema Stitching   | Merging multiple GraphQL APIs into one schema                   |
| Apollo Federation  | A distributed approach using a gateway for service coordination |

***

## References

1. [Apollo Federation Docs](https://www.apollographql.com/docs/federation/)
2. [GraphQL Schema Stitching](https://www.graphql-tools.com/docs/schema-stitching)
3. [Building Monolithic GraphQL APIs](https://graphql.org/)
