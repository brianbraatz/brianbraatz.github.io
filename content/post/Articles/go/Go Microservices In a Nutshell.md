---
title: Go Microservices In a Nutshell
description: Quick dip into wrting Microservices in the Go Programming language
slug: go-microservcies-nutshell
date: 2023-02-06
image: post/Articles/IMAGES/dockergo.png
categories: 
tags:
  - Docker
  - DockerFile
  - WebDevelopment
  - GoLanguage
  - MicroServices
weight: 10
draft: false
lastmod: 2025-02-01T19:15:04.522Z
---
# Microservices with Go

Microservices architecture is a pattern where a system is broken down into smaller, independently deployable services.

Typically each service has its own database and communicates with others using HTTP or messaging queues.

(though i have worked on many production systems where a database is shared between the microservices.. but please don't tell anyone..

it will be our little secret...)

SO for fun and profit- lets build a simple microservices architecture using Go.

Its amazingly simple honestly..

As I Was working on this i rememeber the first time I got C++ DCOM working.. haha only took a few long days ...

this is much simpler..

so lets..

**GO** GET STARTED!~!!!!!

(I crack me up :) )

## Prerequisites - Stuff you need before we go camping

(or containering ??? )

* Go 1.18 or higher
* Basic understanding of Go and HTTP
* Docker (optional, for containerizing the services)

### Step 1: Setting up the Go Project

Create a project directory for your microservices:

```
mkdir go-microservices cd go-microservices
```

Inside this folder, now create directories for each microservice.

For example, let’s create `user-service` and `order-service`:

```
mkdir user-service 
mkdir order-service
```

### Step 2: Creating the User Service

In the `user-service` directory, create a Go file `main.go`:

```go
// user-service/main.go
package main

import (
	"encoding/json"
	"fmt"
	"net/http"
)

type User struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

var users = []User{
	{ID: "1", Name: "Alice"},
	{ID: "2", Name: "Bob"},
}

func getUsers(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(users)
}

func main() {
	http.HandleFunc("/users", getUsers)
	fmt.Println("User service listening on port 8080...")
	http.ListenAndServe(":8080", nil)
}

```

This is a simple HTTP server that returns a list of users when accessing the `/users` endpoint.

To run the service:

```
cd user-service go run main.go
```

**GO DO IT NOW!**

The service will be available at `http://localhost:8080/users`.

### Step 3: Creating the Order Service

Now, let's create an `order-service` in the `order-service` directory.

The order service will simulate creating an order and fetching orders for a specific user.

Create the `main.go` file in the `order-service` folder:

```go
// order-service/main.go
package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
)

type Order struct {
	ID     string `json:"id"`
	UserID string `json:"user_id"`
	Amount float64 `json:"amount"`
}

var orders = []Order{
	{ID: "1", UserID: "1", Amount: 100.50},
	{ID: "2", UserID: "1", Amount: 200.75},
}

func getOrders(w http.ResponseWriter, r *http.Request) {
	userID := r.URL.Query().Get("user_id")
	var filteredOrders []Order

	for _, order := range orders {
		if order.UserID == userID {
			filteredOrders = append(filteredOrders, order)
		}
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(filteredOrders)
}

func main() {
	http.HandleFunc("/orders", getOrders)
	fmt.Println("Order service listening on port 8081...")
	http.ListenAndServe(":8081", nil)
}

```

The `order-service` has an endpoint `/orders?user_id=1` to fetch orders for a specific user.

Run the service by navigating to the `order-service` folder and executing:\
**GO** **GO** **GO TEST IT NOW!!!!**

```
cd order-service 
go run main.go
```

Now the order service will be available at `http://localhost:8081/orders?user_id=1`.

### Step 4: Setting Up Communication Between Services

The user service provides data about users, and the order service uses the user ID to filter orders.

The proverbial "Hair CLub For Men" .. needs some clients...

Let’s modify the `order-service` to fetch the user details from the `user-service`.

Update the `getOrders` function in `order-service/main.go`:

```go
func getOrders(w http.ResponseWriter, r *http.Request) {
	userID := r.URL.Query().Get("user_id")
	var filteredOrders []Order

	// Fetch user details from user service
	userResp, err := http.Get("http://localhost:8080/users")
	if err != nil {
		http.Error(w, "Could not fetch users", http.StatusInternalServerError)
		return
	}
	defer userResp.Body.Close()

	var users []User
	if err := json.NewDecoder(userResp.Body).Decode(&users); err != nil {
		http.Error(w, "Could not decode users", http.StatusInternalServerError)
		return
	}

	// Find the user by userID
	var user *User
	for _, u := range users {
		if u.ID == userID {
			user = &u
			break
		}
	}

	// If user is found, filter orders by userID
	if user != nil {
		for _, order := range orders {
			if order.UserID == user.ID {
				filteredOrders = append(filteredOrders, order)
			}
		}
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(filteredOrders)
}
```

In this update, the `order-service` makes an HTTP request to the `user-service` to get all users and filter orders based on the `user_id`.

cool eh?

I love how small the code is honestly...

### Step 5: Running the Microservices

Run both services:

1. Start the `user-service`:

```bash
    cd user-service 
    go run main.go
```

2. Start the `order-service`:

```bash
    cd order-service 
    go run main.go
```

Now, you can request orders from the order service for a particular user. For example:

```bash
curl http://localhost:8081/orders?user_id=1`
```

This should return the list of orders for the user with ID `1` from the order service, which internally communicates with the user service.

(clap)(clap)(clap)

OPTIONAL:

### Step 6: Dockerizing the Microservices

You can containerize the microservices using Docker.

We need a  `Dockerfile` in both `user-service` and `order-service` directories.

For example, in `user-service/Dockerfile`:

```Dockerfile
FROM golang:1.18-alpine

WORKDIR /app
COPY . .
RUN go mod tidy
RUN go build -o user-service .

EXPOSE 8080

CMD ["./user-service"]
```

```Dockerfile
FROM golang:1.18-alpine

WORKDIR /app
COPY . .
RUN go mod tidy
RUN go build -o order-service .

EXPOSE 8081

CMD ["./order-service"]

```

Build and run the containers with Docker:

```bash
docker build -t user-service ./user-service
docker build -t order-service ./order-service
docker run -p 8080:8080 user-service
docker run -p 8081:8081 order-service

```

**NOW GO WRITE COME CODE!!!!!!!!!!!!!!!!!!!!**

:)

<!--

### Conclusion

In this guide, we created two simple Go-based microservices: `user-service` and `order-service`. These services communicate with each other over HTTP, and we used Docker to containerize them. This setup is a basic example, but you can extend it by adding more services, using a service discovery mechanism, and incorporating other tools such as Kubernetes for orchestration.

This should give you a good starting point for building Go-based microservices!


https://chatgpt.com/c/679d3220-c0ec-8008-b735-3adef62bf0b8

# Building Microservices with Go: A Simple Guide

Microservices architecture is a pattern where a system is broken down into smaller, independently deployable services. Each service has its own database and communicates with others using HTTP or messaging queues. In this article, we'll walk through how to build a simple microservices architecture using Go.

## Prerequisites

- Go 1.18 or higher
- Basic understanding of Go and HTTP
- Docker (optional, for containerizing the services)

### Step 1: Setting up the Go Project

Create a project directory for your microservices:

bash

CopyEdit

`mkdir go-microservices cd go-microservices`

Inside this folder, create directories for each microservice. For example, let’s create `user-service` and `order-service`:

bash

CopyEdit

`mkdir user-service mkdir order-service`

### Step 2: Creating the User Service

In the `user-service` directory, create a simple Go file `main.go`:

go

CopyEdit

``// user-service/main.go package main  import ( 	"encoding/json" 	"fmt" 	"net/http" )  type User struct { 	ID   string `json:"id"` 	Name string `json:"name"` }  var users = []User{ 	{ID: "1", Name: "Alice"}, 	{ID: "2", Name: "Bob"}, }  func getUsers(w http.ResponseWriter, r *http.Request) { 	w.Header().Set("Content-Type", "application/json") 	w.WriteHeader(http.StatusOK) 	json.NewEncoder(w).Encode(users) }  func main() { 	http.HandleFunc("/users", getUsers) 	fmt.Println("User service listening on port 8080...") 	http.ListenAndServe(":8080", nil) }``

This is a simple HTTP server that returns a list of users when accessing the `/users` endpoint. To run the service:

bash

CopyEdit

`cd user-service go run main.go`

The service will be available at `http://localhost:8080/users`.

### Step 3: Creating the Order Service

Now, let's create an `order-service` in the `order-service` directory. The order service will simulate creating an order and fetching orders for a specific user.

Create the `main.go` file in the `order-service` folder:

go

CopyEdit

``// order-service/main.go package main  import ( 	"encoding/json" 	"fmt" 	"net/http" 	"strconv" )  type Order struct { 	ID     string `json:"id"` 	UserID string `json:"user_id"` 	Amount float64 `json:"amount"` }  var orders = []Order{ 	{ID: "1", UserID: "1", Amount: 100.50}, 	{ID: "2", UserID: "1", Amount: 200.75}, }  func getOrders(w http.ResponseWriter, r *http.Request) { 	userID := r.URL.Query().Get("user_id") 	var filteredOrders []Order  	for _, order := range orders { 		if order.UserID == userID { 			filteredOrders = append(filteredOrders, order) 		} 	}  	w.Header().Set("Content-Type", "application/json") 	w.WriteHeader(http.StatusOK) 	json.NewEncoder(w).Encode(filteredOrders) }  func main() { 	http.HandleFunc("/orders", getOrders) 	fmt.Println("Order service listening on port 8081...") 	http.ListenAndServe(":8081", nil) }``

The `order-service` has an endpoint `/orders?user_id=1` to fetch orders for a specific user.

Run the service by navigating to the `order-service` folder and executing:

bash

CopyEdit

`cd order-service go run main.go`

Now the order service will be available at `http://localhost:8081/orders?user_id=1`.

### Step 4: Setting Up Communication Between Services

The user service provides data about users, and the order service uses the user ID to filter orders. Let’s modify the `order-service` to fetch the user details from the `user-service`.

Update the `getOrders` function in `order-service/main.go`:

go

CopyEdit

`func getOrders(w http.ResponseWriter, r *http.Request) { 	userID := r.URL.Query().Get("user_id") 	var filteredOrders []Order  	// Fetch user details from user service 	userResp, err := http.Get("http://localhost:8080/users") 	if err != nil { 		http.Error(w, "Could not fetch users", http.StatusInternalServerError) 		return 	} 	defer userResp.Body.Close()  	var users []User 	if err := json.NewDecoder(userResp.Body).Decode(&users); err != nil { 		http.Error(w, "Could not decode users", http.StatusInternalServerError) 		return 	}  	// Find the user by userID 	var user *User 	for _, u := range users { 		if u.ID == userID { 			user = &u 			break 		} 	}  	// If user is found, filter orders by userID 	if user != nil { 		for _, order := range orders { 			if order.UserID == user.ID { 				filteredOrders = append(filteredOrders, order) 			} 		} 	}  	w.Header().Set("Content-Type", "application/json") 	w.WriteHeader(http.StatusOK) 	json.NewEncoder(w).Encode(filteredOrders) }`

In this update, the `order-service` makes an HTTP request to the `user-service` to get all users and filter orders based on the `user_id`.

### Step 5: Running the Microservices

Run both services:

1. Start the `user-service`:
    
    bash
    
    CopyEdit
    
    `cd user-service go run main.go`
    
2. Start the `order-service`:
    
    bash
    
    CopyEdit
    
    `cd order-service go run main.go`
    

Now, you can request orders from the order service for a particular user. For example:

bash

CopyEdit

`curl http://localhost:8081/orders?user_id=1`

This should return the list of orders for the user with ID `1` from the order service, which internally communicates with the user service.

### Step 6: Dockerizing the Microservices (Optional)

You can containerize the microservices using Docker. Create a `Dockerfile` in both `user-service` and `order-service` directories.

For example, in `user-service/Dockerfile`:

Dockerfile

CopyEdit

`FROM golang:1.18-alpine  WORKDIR /app COPY . . RUN go mod tidy RUN go build -o user-service .  EXPOSE 8080  CMD ["./user-service"]`

And similarly for the `order-service/Dockerfile`:

Dockerfile

CopyEdit

`FROM golang:1.18-alpine  WORKDIR /app COPY . . RUN go mod tidy RUN go build -o order-service .  EXPOSE 8081  CMD ["./order-service"]`

Build and run the containers with Docker:

bash

CopyEdit

`docker build -t user-service ./user-service docker build -t order-service ./order-service docker run -p 8080:8080 user-service docker run -p 8081:8081 order-service`

### Conclusion

In this guide, we created two simple Go-based microservices: `user-service` and `order-service`. These services communicate with each other over HTTP, and we used Docker to containerize them. This setup is a basic example, but you can extend it by adding more services, using a service discovery mechanism, and incorporating other tools such as Kubernetes for orchestration.

This should give you a good starting point for building Go-based microservices!-->
