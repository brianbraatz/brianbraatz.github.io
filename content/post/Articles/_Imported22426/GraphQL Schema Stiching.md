---
title: GraphQL Schema Stitching In a Nutshell
description: How GraphQL Schema Stitching Worked
slug: graphql-schema-stitching-nutshell
date: 2024-06-18
image: post/Articles/IMAGES/seweingsquares.png
categories:
  - GraphQL
  - API
  - Development
  - Cloud
tags:
  - Graphql
  - Schema
  - Stitching
  - Api
  - Development
  - Microservices
draft: false
weight: 190
lastmod: 2025-03-03T17:18:52.050Z
---
[How make a patchwork quilt](https://www.diaryofaquilter.com/how-to-make-a-quilt/)

GraphQL Schema Stitching—it's a bit like stitching together Frankenstein’s monster, but way less terrifying and significantly more useful.

## What Is Schema Stitching?

Schema stitching is the art (and occasional nightmare) of merging multiple GraphQL schemas into a single unified API.

Instead of making users query separate GraphQL services, you stitch them into one seamless schema, allowing queries across multiple sources.

Think of it as taking different LEGO sets, smashing them together, and somehow still ending up with a cohesive Death Star model.

## The History of Schema Stitching

Schema stitching made its grand entrance around **2017**, introduced by Apollo as a solution to GraphQL’s growing pains.

At that time, developers were struggling with GraphQL services that were fragmented across multiple microservices.

Instead of making users send multiple queries to different services, Apollo's schema stitching allowed developers to merge multiple schemas into a single API, making life easier for frontend devs and backend architects alike.

It was revolutionary! Until it wasn’t.

## Motivation: Why Was Schema Stitching Even a Thing?

Before schema stitching, GraphQL microservices had a real problem: each service had its own schema, and users had to make separate API calls to get data from different services. This was annoying and inefficient.

Schema stitching promised:

* A **unified API** experience.
* A way to **resolve types across services** (e.g., users from one service and orders from another).
* The ability to **extend types** across multiple services without needing a monolithic GraphQL server.

Basically, it sounded like a dream come true for teams building GraphQL-powered microservices.

## Pros of Schema Stitching

* **Unified API**: Clients can query across multiple services with a single request.
* **Scalability**: Teams can work on separate GraphQL services while still exposing a single API.
* **Flexibility**: You can combine different GraphQL services dynamically.
* **Extendable**: You can extend types across schemas without needing a monolith.

## Cons of Schema Stitching

* **Complexity**: Managing stitched schemas is not for the faint of heart.
* **Performance Issues**: It introduces overhead since resolvers often need to fetch data across multiple services.
* **Debugging Nightmares**: Error messages can be tricky because they often originate from stitched resolvers.
* **Deprecated in Apollo**: Apollo decided to abandon schema stitching in favor of **GraphQL Federation**, which solved many of its limitations.

## Is Schema Stitching Still Used Today?

Short answer: Not really, at least not in its original form.

Long answer: Apollo officially moved away from schema stitching in favor of **GraphQL Federation**, a more scalable and maintainable approach to combining GraphQL schemas.

However, some teams still use schema stitching, especially in legacy systems or when Federation isn’t an option. If you’re starting fresh today, **GraphQL Federation is the way to go**.

## Key Ideas

| Concept              | Description                                                                      |
| -------------------- | -------------------------------------------------------------------------------- |
| **Schema Stitching** | Merging multiple GraphQL schemas into one API.                                   |
| **History**          | Introduced by Apollo in 2017 to solve GraphQL microservices fragmentation.       |
| **Motivation**       | Aimed to provide a unified API, extend types, and make microservices friendlier. |
| **Pros**             | Unified API, scalable, flexible, and extendable.                                 |
| **Cons**             | Complex, performance-heavy, hard to debug, and now largely obsolete.             |
| **Current State**    | Replaced by GraphQL Federation but still exists in some legacy systems.          |

## References

1. [Apollo’s original schema stitching docs](https://www.apollographql.com/docs/graphql-tools/schema-stitching/)
2. [GraphQL Federation](https://www.apollographql.com/docs/federation/)
3. [The evolution of GraphQL API composition](https://graphql.org/blog/graphql-composition/)

***

And there you have it! Schema stitching—once a shining beacon of hope, now mostly a relic of the past. If you’re working with GraphQL today, you’re better off using Federation.
