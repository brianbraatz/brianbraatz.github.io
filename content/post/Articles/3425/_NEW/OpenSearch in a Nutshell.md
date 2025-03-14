---
title: OpenSearch in a Nutshell
description: ""
slug: opensearch-nutshell
date: 2022-08-14
image: post/Articles/IMAGES/opensearch.png
categories:
  - Opensearch
  - Elasticsearch
  - Search
  - Database
  - SQL
  - Cloud
tags:
  - Opensearch
  - Elasticsearch
  - Search
  - Database
draft: false
weight: 3293
categories_ref:
  - Opensearch
  - Elasticsearch
  - Search
  - Database
  - SQL
  - Cloud
lastmod: 2025-03-14T15:45:07.011Z
---
# OpenSearch in a Nutshell: A Fun and Informal Guide

## The History: How We Got Here

Once upon a time, there was a search engine called **Elasticsearch**. It was fast, scalable, and loved by many. But then, the evil corporate overlords (aka "That Company That Shall Not Be Named But Starts with an A") decided to take Elasticsearch in a more... *business-y* direction.

Enter **OpenSearch**—the people's champion! It was born when Amazon forked Elasticsearch 7.10.2 and said, "We shall make this open-source again!" And thus, in 2021, OpenSearch came to life, free from licensing restrictions and corporate greed.

Since then, OpenSearch has been on a mission to provide an **open-source, community-driven** search and analytics suite. It's like Elasticsearch, but with more freedom and fewer angry emails about licensing violations.

## Why OpenSearch?

Why should you care about OpenSearch? Good question! Here’s why:

* **It’s Free and Open-Source** – No sneaky licensing shenanigans!
* **Compatible with Elasticsearch** – You can use most Elasticsearch tools and APIs with OpenSearch.
* **Great for Logs, Analytics, and Searching** – It’s not just a search engine; it’s a full-fledged analytics powerhouse.
* **Scalable and Fast** – Can handle tons of data across distributed nodes.
* **Kibana? Nope! Meet OpenSearch Dashboards** – A friendly UI for visualizing all your data needs.

Now that we’ve covered the *why*, let’s get our hands dirty with some **code!**

## Getting Started with OpenSearch

### Installing OpenSearch

You can get OpenSearch running locally using Docker:

```sh
docker run -d --name opensearch -p 9200:9200 -p 9600:9600 \
  -e "discovery.type=single-node" \
  -e "OPENSEARCH_INITIAL_ADMIN_PASSWORD=admin" \
  opensearchproject/opensearch:latest
```

This will spin up an OpenSearch node on **http://localhost:9200**.

Now let’s make sure it’s working:

```sh
curl -u admin:admin -X GET "http://localhost:9200"
```

If everything goes well, you’ll see some JSON with details about your OpenSearch cluster.

### Indexing Data in OpenSearch

Let’s add some data! Say we have a collection of **cats** (because why not?).

```sh
curl -X POST "http://localhost:9200/cats/_doc/1" -H "Content-Type: application/json" -u admin:admin -d '{
  "name": "Whiskers",
  "age": 3,
  "color": "gray"
}'
```

Boom! You just added a cat to OpenSearch. Congratulations!

### Searching for Data

Now, let’s search for our furry friend:

```sh
curl -X GET "http://localhost:9200/cats/_search?q=name:Whiskers" -u admin:admin
```

And there you have it! Your OpenSearch cluster is now home to a fine feline.

## More Advanced OpenSearch Stuff

### Using OpenSearch Dashboards

OpenSearch Dashboards is the graphical interface for OpenSearch. You can run it using Docker as well:

```sh
docker run -d --name opensearch-dashboards -p 5601:5601 \
  -e "OPENSEARCH_HOSTS=[\"http://localhost:9200\"]" \
  opensearchproject/opensearch-dashboards:latest
```

Then, open **http://localhost:5601** and start exploring your data visually!

### Aggregations (Because Data Needs Stats)

Want to count how many cats you have by color? Easy!

```sh
curl -X POST "http://localhost:9200/cats/_search" -H "Content-Type: application/json" -u admin:admin -d '{
  "size": 0,
  "aggs": {
    "cats_by_color": {
      "terms": { "field": "color.keyword" }
    }
  }
}'
```

This gives you a breakdown of how many cats you have per color. Maybe it's time to diversify your cat collection?

## Wrapping It Up

So, OpenSearch is basically Elasticsearch’s cool, open-source sibling. It’s powerful, scalable, and free. If you’re dealing with logs, analytics, or search-heavy applications, OpenSearch is a fantastic choice.

And hey, if you ever get lost, just remember: **curl is your best friend**.

## Key Ideas

| Topic               | Summary                                                                |
| ------------------- | ---------------------------------------------------------------------- |
| **History**         | OpenSearch was forked from Elasticsearch in 2021 by Amazon.            |
| **Why OpenSearch?** | It’s open-source, scalable, and great for logs, search, and analytics. |
| **Installation**    | Easily set up using Docker.                                            |
| **Indexing Data**   | Add data using the REST API.                                           |
| **Searching**       | Use simple queries to retrieve data.                                   |
| **Aggregations**    | Perform stats and analytics on data.                                   |
| **Dashboards**      | OpenSearch Dashboards is the Kibana alternative.                       |

## References

* [OpenSearch Official Docs](https://opensearch.org/docs/)
* [OpenSearch GitHub](https://github.com/opensearch-project)
* [Docker OpenSearch](https://hub.docker.com/r/opensearchproject/opensearch)

***
