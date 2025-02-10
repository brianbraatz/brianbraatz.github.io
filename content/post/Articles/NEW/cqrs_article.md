---
title: Dive into The CQRS Messaging Pattern
description: CQRS (Command Query Responsibility Segregation)
slug: cqrs-command-query-responsibility-segregation
date: 2024-12-15
image: post/Articles/IMAGES/23.jpg
categories:
  - Cloud
  - Docker
  - Microservices
  - Design Patterns
  - CQRS Pattern
  - MVC Pattern
tags:
  - CQRS
  - Commands
  - Queries
  - Design
  - Patterns
  - Martin
  - Fowler
  - MVC
  - Java
draft: false
weight: 347
lastmod: 2025-02-09T22:06:08.226Z
---
# CQRS: Making Software Less Confusing (or More, Depending on Your Perspective)

## What is CQRS?

CQRS, short for **Command Query Responsibility Segregation**, is a fancy way of saying:\
*"What if we separate the logic that changes data (commands) from the logic that reads data (queries)?"*

This pattern is **often linked to Martin Fowler** ([Wikipedia](https://en.wikipedia.org/wiki/Martin_Fowler_%28software_engineer%29)), though **Greg Young** also played a huge role in its popularization ([Wikipedia](https://en.wikipedia.org/wiki/Greg_Young_%28software_engineer%29)). The idea is simple: instead of having one model that does **both** reading and writing (which is what we usually do), we create **two** models—one for writes (commands) and one for reads (queries).

## Relationship to the Gang of Four Patterns

CQRS is **not** one of the original Gang of Four patterns ([Wikipedia](https://en.wikipedia.org/wiki/Design_Patterns)), but it draws inspiration from:

* **Command Pattern** ([Wikipedia](https://en.wikipedia.org/wiki/Command_pattern)): CQRS commands encapsulate a request to change state.
* **Observer Pattern** ([Wikipedia](https://en.wikipedia.org/wiki/Observer_pattern)): Often used with CQRS for event sourcing.
* **Mediator Pattern** ([Wikipedia](https://en.wikipedia.org/wiki/Mediator_pattern)): CQRS can use a mediator to manage communication between commands and queries.

***

## Commands, Queries, Results, and Envelopes

### Commands

A **command** represents an **action** that modifies the state of the system.

```java
public class CreateUserCommand {
    private final String username;
    
    public CreateUserCommand(String username) {
        this.username = username;
    }

    public String getUsername() {
        return username;
    }
}
```

### Queries

A **query** retrieves **data** without modifying anything.

```java
public class GetUserQuery {
    private final String userId;

    public GetUserQuery(String userId) {
        this.userId = userId;
    }

    public String getUserId() {
        return userId;
    }
}
```

### Results

A **result** is what comes back from a query.

```java
public class UserResult {
    private final String userId;
    private final String username;

    public UserResult(String userId, String username) {
        this.userId = userId;
        this.username = username;
    }

    public String getUserId() {
        return userId;
    }

    public String getUsername() {
        return username;
    }
}
```

### Envelopes

An **envelope** wraps commands or queries to add metadata.

```java
public class Envelope<T> {
    private final T payload;
    private final String correlationId;

    public Envelope(T payload, String correlationId) {
        this.payload = payload;
        this.correlationId = correlationId;
    }

    public T getPayload() {
        return payload;
    }

    public String getCorrelationId() {
        return correlationId;
    }
}
```

***

## CQRS vs. MVC: The Ultimate Battle

CQRS and MVC (Model-View-Controller) are often compared, so let's break it down:

| Feature                    | CQRS                                                     | MVC                                                              |
| -------------------------- | -------------------------------------------------------- | ---------------------------------------------------------------- |
| **Separation of Concerns** | Strong separation between commands and queries           | Everything handled in one model                                  |
| **Complexity**             | Higher (more moving parts)                               | Lower (easier to grasp)                                          |
| **Scalability**            | Very scalable, ideal for distributed systems             | Can be hard to scale when reads/writes compete                   |
| **Data Consistency**       | Eventual consistency (when combined with event sourcing) | Immediate consistency                                            |
| **Learning Curve**         | Steep                                                    | Easy (until you realize you've overcomplicated your controllers) |

***

## Key Takeaways

| Key Idea                                 | Explanation                                                     |
| ---------------------------------------- | --------------------------------------------------------------- |
| **CQRS separates read and write models** | Commands change state, queries fetch data                       |
| **CQRS is influenced by GoF patterns**   | Uses Command, Observer, and Mediator patterns                   |
| **Envelopes wrap data**                  | Adds metadata for tracking                                      |
| **CQRS vs. MVC**                         | CQRS is great for scalability, MVC is simpler but can get messy |

***

## References

* CQRS: <https://en.wikipedia.org/wiki/Command_query_responsibility_segregation>
* Martin Fowler: <https://en.wikipedia.org/wiki/Martin_Fowler_%28software_engineer%29>
* Gang of Four Patterns: <https://en.wikipedia.org/wiki/Design_Patterns>
* Command Pattern: <https://en.wikipedia.org/wiki/Command_pattern>
* Observer Pattern: <https://en.wikipedia.org/wiki/Observer_pattern>
* Mediator Pattern: <https://en.wikipedia.org/wiki/Mediator_pattern>
* MVC: [https://en.wikipedia.org/wiki/Model–view–controller](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller)
