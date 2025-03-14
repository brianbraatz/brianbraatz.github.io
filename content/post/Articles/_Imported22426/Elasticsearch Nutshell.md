---
title: Elasticsearch In A Nutshell
description: Elasticsearch with  Examples
slug: elasticsearch-nutshell
date: 2016-10-08
image: post/Articles/IMAGES/elastigirl.png
categories:
  - Search
  - Databases
  - Elasticsearch
  - Cloud
  - Apache
  - Apache Lucene
tags:
  - Search
  - Databases
  - Elasticsearch
  - Lucene
  - Text
  - Search
  - Data
  - Indexing
  - Full-Text
  - Search
  - Log
  - Analysis
draft: false
weight: 21
categories_ref:
  - Search
  - Databases
  - Elasticsearch
  - Cloud
  - Apache
  - Apache Lucene
lastmod: 2025-03-14T15:45:16.458Z
---
[Elastigirl](https://en.wikipedia.org/wiki/Elastigirl)

## A Quick History of Elasticsearch

Elasticsearch is built on **Apache Lucene**, a powerful full-text search library. While Lucene is great, it’s also complicated—like trying to assemble IKEA furniture without a manual.

In 2010, Shay Banon created Elasticsearch to make Lucene more user-friendly. Instead of dealing with complex configurations, Elasticsearch lets you send simple **JSON queries** over HTTP. And boom! You get lightning-fast search results.

Fast forward to today, Elasticsearch is used by companies like **Netflix, Uber, Slack, and Wikipedia** for real-time search, log analysis, and more.

## Why Use Elasticsearch?

### Pros

* **Speed Demon** – Handles millions of records in milliseconds.
* **Scalable** – Distributed architecture makes it easy to scale.
* **Schema-less** – Just throw JSON at it, and it figures things out.
* **Powerful Querying** – Full-text search, aggregations, filters—it's got it all.
* **RESTful API** – Easy to use over HTTP with JSON.

### Cons

* **Resource Hungry** – Eats RAM and CPU like it's at an all-you-can-eat buffet.
* **Cluster Complexity** – Managing clusters can be challenging.
* **Requires Tuning** – Defaults are okay, but real performance requires tweaking.
* **Not a Traditional Database** – Not designed for transactional workloads.

## 10 Examples of Elasticsearch in Action

### 1. Installing Elasticsearch

```bash
docker run -d --name elasticsearch -p 9200:9200 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:8.0.0
```

### 2. Indexing Data

```bash
curl -X POST "localhost:9200/movies/_doc/1" -H "Content-Type: application/json" -d'
{
  "title": "The Matrix",
  "year": 1999,
  "genre": ["Action", "Sci-Fi"]
}'
```

### 3. Searching for a Movie

```bash
curl -X GET "localhost:9200/movies/_search?q=title:Matrix&pretty"
```

### 4. Filtering Results

```json
{
  "query": {
    "range": {
      "year": { "gte": 2000 }
    }
  }
}
```

### 5. Fuzzy Search

```json
{
  "query": {
    "fuzzy": {
      "title": {
        "value": "Matrox"
      }
    }
  }
}
```

### 6. Aggregations

```json
{
  "aggs": {
    "genres_count": {
      "terms": { "field": "genre.keyword" }
    }
  }
}
```

### 7. Updating a Document

```bash
curl -X POST "localhost:9200/movies/_update/1" -H "Content-Type: application/json" -d'
{
  "doc": { "year": 1998 }
}'
```

### 8. Deleting a Document

```bash
curl -X DELETE "localhost:9200/movies/_doc/1"
```

### 9. Bulk Indexing

```bash
curl -X POST "localhost:9200/movies/_bulk" -H "Content-Type: application/json" -d'
{ "index": { "_index": "movies", "_id": "2" }}
{ "title": "Inception", "year": 2010, "genre": ["Sci-Fi", "Thriller"] }
{ "index": { "_index": "movies", "_id": "3" }}
{ "title": "Interstellar", "year": 2014, "genre": ["Sci-Fi", "Drama"] }
'
```

### 10. Deleting an Index

```bash
curl -X DELETE "localhost:9200/movies"
```

## Alternatives  Compared

| Method                          | Pros                                            | Cons                                  |
| ------------------------------- | ----------------------------------------------- | ------------------------------------- |
| **Elasticsearch**               | Fast, scalable, powerful search                 | Resource-intensive, complex to manage |
| **PostgreSQL Full-Text Search** | Built into PostgreSQL, good for structured data | Slower for large datasets             |
| **MySQL Full-Text Search**      | Simple, easy to use                             | Less flexible, limited features       |
| **Apache Solr**                 | Also built on Lucene, highly customizable       | More complex setup                    |
| **Algolia**                     | Extremely fast, easy to use                     | Expensive for large datasets          |

## Conclusion

Elasticsearch is a **powerful search engine** that’s great for full-text search, log analysis, and big data applications.

But it’s not a one-size-fits-all solution. If you need transactional consistency or a simple setup, alternatives like **PostgreSQL or MySQL full-text search** might be a better fit.

If you love speed, scalability, and JSON, Elasticsearch is your best friend. Just keep an eye on your cluster—because Elasticsearch **loves RAM more than you love coffee.**

## Key Ideas

| Topic             | Summary                                            |
| ----------------- | -------------------------------------------------- |
| **Elasticsearch** | A fast, scalable search engine based on Lucene     |
| **Pros & Cons**   | Fast and powerful but resource-hungry              |
| **10 Examples**   | Covers installation, indexing, searching, and more |
| **Alternatives**  | Compared with PostgreSQL, MySQL, Solr, and Algolia |

## References

* [Elasticsearch Official Docs](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)
* [Lucene Project](https://lucene.apache.org/)
* [PostgreSQL Full-Text Search](https://www.postgresql.org/docs/current/textsearch-intro.html)
* [Apache Solr](https://solr.apache.org/)
