---
title: Building a Modern C++ Microservice with Docker and Kubernetes
description: For Fun and Profit!
slug: cpp-microservice-docker-kubernetes
date: 2017-06-15
image: post/Articles/IMAGES/cppblue.png
categories:
  - C++
  - Docker
  - Kubernetes
  - Microservices
  - Cloud
  - CPP
tags:
  - C++
  - Docker
  - Kubernetes
  - Microservices
  - REST API
  - Cross-platform
draft: false
weight: 52
categories_ref:
  - C++
  - Docker
  - Kubernetes
  - Microservices
  - Cloud
  - CPP
slug_calculated: https://brianbraatz.github.io/p/cpp-microservice-docker-kubernetes
lastmod: 2025-03-14T16:40:17.903Z
---
<!-- # Building a Modern C++ Microservice with Docker and Kubernetes -->

Alright, today we’re diving into something truly *epic*: building a C++ microservice with a REST API, wrapping it in a Docker container, deploying it to Kubernetes, and calling it from a test app.

And just to keep things spicy, we’re making this cross-platform so it runs on both Windows and Linux. Yes, you read that right.

## Step 1: The C++ Microservice

First, we’re going to create a simple C++ REST API using **cpp-httplib** (because Boost is still too heavy, fight me).

```cpp
#include <iostream>
#include "httplib.h"

void divide_handler(const httplib::Request& req, httplib::Response& res) {
    try {
        double a = std::stod(req.get_param_value("a"));
        double b = std::stod(req.get_param_value("b"));
        if (b == 0) {
            res.status = 400;
            res.set_content("Cannot divide by zero", "text/plain");
        } else {
            double result = a / b;
            res.set_content(std::to_string(result), "text/plain");
        }
    } catch (...) {
        res.status = 400;
        res.set_content("Invalid input", "text/plain");
    }
}

int main() {
    httplib::Server svr;
    svr.Get("/divide", divide_handler);
    std::cout << "Starting server on port 8080...\n";
    svr.listen("0.0.0.0", 8080);
}
```

## Step 2: Unit Test for Division

We can’t just write code without testing it (well, we *can*, but then we get paged at 3 AM). Let’s write a simple unit test using Google Test.

```cpp
#include <gtest/gtest.h>

TEST(DivisionTest, HandlesZeroDivision) {
    double a = 10.0;
    double b = 0.0;
    EXPECT_ANY_THROW({ double result = a / b; });
}

TEST(DivisionTest, ValidDivision) {
    double a = 10.0;
    double b = 2.0;
    EXPECT_EQ(a / b, 5.0);
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

## Step 3: Test Client for the REST API

Now, let’s write a simple C++ test client to call our microservice.

```cpp
#include <iostream>
#include "httplib.h"

int main() {
    httplib::Client cli("http://localhost:8080");
    auto res = cli.Get("/divide?a=10&b=2");
    if (res && res->status == 200) {
        std::cout << "Result: " << res->body << std::endl;
    } else {
        std::cout << "Error: " << (res ? res->status : 0) << std::endl;
    }
}
```

## Step 4: Running the Service Locally

To run the service locally:

```sh
g++ -o server server.cpp -pthread
g++ -o client client.cpp -pthread
./server
./client
```

## Step 5: Dockerizing the Microservice

Now, let’s move this into a Docker container.

**Dockerfile:**

```dockerfile
FROM ubuntu:20.04
WORKDIR /app
COPY server /app/server
CMD ["/app/server"]
```

Build and run the Docker container:

```sh
docker build -t cpp-microservice .
docker run -p 8080:8080 cpp-microservice
```

## Step 6: Running on Kubernetes

Let’s deploy this container to Kubernetes.

**Deployment YAML:**

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cpp-microservice
spec:
  replicas: 1
  selector:
    matchLabels:
      app: cpp-microservice
  template:
    metadata:
      labels:
        app: cpp-microservice
    spec:
      containers:
      - name: cpp-microservice
        image: cpp-microservice:latest
        ports:
        - containerPort: 8080
```

Apply to Kubernetes:

```sh
kubectl apply -f deployment.yaml
kubectl expose deployment cpp-microservice --type=LoadBalancer --port=8080
```

## Step 7: Calling the Service from Kubernetes

Now, let’s call our Kubernetes-hosted service from our test client.

```sh
docker run --network host cpp-client http://localhost:8080/divide?a=10&b=2
```

## Conclusion

Congratulations! You now have a C++ microservice running in Docker and Kubernetes, serving up division results like a math wizard. Just don’t let anyone divide by zero.

Or do. See what happens.

***

## Key Ideas

| Concept                 | Summary                                     |
| ----------------------- | ------------------------------------------- |
| C++ Microservice        | Built a REST API in C++ using cpp-httplib   |
| Unit Testing            | Used Google Test for division tests         |
| REST Client             | Wrote a C++ client to call the microservice |
| Running Locally         | Compiled and ran on both Windows and Linux  |
| Dockerization           | Created a Docker image for our microservice |
| Kubernetes Deployment   | Deployed the container to Kubernetes        |
| Calling from Kubernetes | Used a test client to call the service      |

***

## References

* [cpp-httplib](https://github.com/yhirose/cpp-httplib)
* [Google Test](https://github.com/google/googletest)
* [Docker Docs](https://docs.docker.com/)
* [Kubernetes Docs](https://kubernetes.io/docs/)
