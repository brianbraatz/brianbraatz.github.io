---
title: Cpp STL Cheatsheet
description: 
slug: cpp-stl-cheatsheet
date: 2014-05-06
image: post/Articles/IMAGES/6.jpg
categories: 
tags:
  - Cheatsheet
  - CPP
  - CPP-STL
weight: 30
draft: false
lastmod: 2025-01-30T12:59:27.860Z
---
## C++ Standard Template Library (STL) Cheatsheet

| **Concept**     | **Syntax/Example**                      | **Description**               |
| --------------- | --------------------------------------- | ----------------------------- |
| Vectors         | `vector<int> v = {1, 2, 3};`            | Creating a vector             |
| Iterators       | `vector<int>::iterator it = v.begin();` | Declaring an iterator         |
| Lists           | `list<int> myList = {1, 2, 3};`         | Creating a list               |
| Stacks          | `stack<int> myStack;`                   | Creating a stack              |
| Queues          | `queue<int> myQueue;`                   | Creating a queue              |
| Priority Queues | `priority_queue<int> pq;`               | Creating a priority queue     |
| Sets            | `set<int> mySet = {1, 2, 3};`           | Creating a set                |
| Maps            | `map<string, int> myMap;`               | Creating a map                |
| Algorithms      | `sort(v.begin(), v.end());`             | Sorting a vector              |
| `find`          | `find(v.begin(), v.end(), 2);`          | Finding an element            |
| `count`         | `count(v.begin(), v.end(), 2);`         | Counting occurrences          |
| `accumulate`    | `accumulate(v.begin(), v.end(), 0);`    | Summing elements              |
| `fill`          | `fill(v.begin(), v.end(), 0);`          | Filling elements with a value |

## STL Overview

```lua
STL
 ├── Containers
 │    ├── Sequence Containers
 │    │    ├── vector
 │    │    ├── list
 │    │    ├── deque
 │    │    └── array
 │    ├── Associative Containers
 │    │    ├── set
 │    │    ├── multiset
 │    │    ├── map
 │    │    └── multimap
 │    ├── Unordered Containers
 │    │    ├── unordered_set
 │    │    ├── unordered_multiset
 │    │    ├── unordered_map
 │    │    └── unordered_multimap
 │    └── Container Adapters
 │         ├── stack
 │         ├── queue
 │         └── priority_queue
 ├── Algorithms
 │    ├── Sorting
 │    │    └── sort
 │    ├── Searching
 │    │    └── find
 │    ├── Modifying
 │    │    └── copy
 │    ├── Non-modifying
 │    │    └── for_each
 │    └── Numeric
 │         └── accumulate
 ├── Iterators
 │    ├── Input Iterators
 │    ├── Output Iterators
 │    ├── Forward Iterators
 │    ├── Bidirectional Iterators
 │    └── Random Access Iterators
 └── Functors
      ├── Unary Functors
      ├── Binary Functors
      └── Function Objects
```

### Key Components:

* **Containers:** Hold collections of objects. Types include sequence containers (like `vector` and `list`), associative containers (like `set` and `map`), unordered containers (like `unordered_map`), and container adapters (like `stack` and `queue`).

* **Algorithms:** Operate on containers (e.g., `sort`, `find`, `copy`, `for_each`, `accumulate`).

* **Iterators:** Provide a way to traverse through the elements in a container (e.g., input, output, forward, bidirectional, random access iterators).

* **Functors (Function Objects):** Objects that can be called as if they were functions.
