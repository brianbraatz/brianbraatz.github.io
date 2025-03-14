---
title: Implementing HTTP PATCH in C#, Python and Go
description: For efficient updates
slug: mastering-http-patch-method-fun-informal-guide
date: 2023-08-15
image: post/Articles/IMAGES/patchsmall.png
categories:
  - Cloud
  - HTTP
  - HTTP-Patch
  - GoLang
  - Python
  - CSharp
  - DotNet
tags:
  - HTTP
  - HTTPPATCH
  - Method
  - CSharp
  - Python
  - Go
  - RFCs
  - WebDevelopment
draft: false
weight: 342
categories_ref:
  - Cloud
  - HTTP
  - HTTP-Patch
  - GoLang
  - Python
  - CSharp
  - DotNet
lastmod: 2025-03-14T15:45:11.799Z
---
<!-- 
## Introduction

So, you've been cruising the web, minding your own business, when suddenly someone drops the term "HTTP PATCH method" into the conversation. You're left wondering, "Is this some kind of digital band-aid?" Well, buckle up, because we're about to dive into the world of HTTP, explore the PATCH method, and even sprinkle in some C#, Python, and Go code to keep things spicy.
-->

## A Brief History of HTTP

Before we get into PATCH, let's take a quick trip down memory lane with HTTP (HyperText Transfer Protocol). Think of HTTP as the language that web browsers and servers use to chat. It all started with:

* **HTTP/0.9 (1991):** The OG version, super simple, just GET requests.
* **HTTP/1.0 (1996):** Introduced in [RFC 1945](https://datatracker.ietf.org/doc/html/rfc1945). Added more methods, status codes, and headers.
* **HTTP/1.1 (1997):** Defined in [RFC 2068](https://datatracker.ietf.org/doc/html/rfc2068) and later updated in [RFC 2616](https://datatracker.ietf.org/doc/html/rfc2616). Brought persistent connections and chunked transfers.
* **HTTP/2 (2015):** Enhanced performance with multiplexing and header compression.
* **HTTP/3 (2020):** The latest and greatest, using QUIC for faster connections.

## Enter the PATCH Method

Now, let's talk about PATCH. No, it's not a software update or a pirate's accessory. The PATCH method was introduced in [RFC 5789](https://datatracker.ietf.org/doc/html/rfc5789) in 2010.

Its purpose? To allow partial updates to a resource.

Imagine you have a document, and you only want to change one sentence.

Instead of sending the whole document back and forth (like with PUT), PATCH lets you send just the changes. Efficient, right?

## PATCH vs. PUT

You might be thinking, "Wait, doesn't PUT handle updates?" Yes, but there's a twist:

* **PUT:** Replaces the entire resource with the data you send. It's like redecorating your entire house because you bought a new couch.
* **PATCH:** Updates only the specified parts of the resource. It's like just swapping out the old couch for the new one.

<!-- 
### 🚀 **PATCH in C#, Python, and Go (Server + Client)**
We'll cover:
1. **How to handle PATCH requests on the server**
2. **How to send PATCH requests from the client**

---
-->

## 🏗 **Server-Side PATCH Implementation**

This is how you **handle** incoming PATCH requests on the server.

***

### 🔹 **C# Server (ASP.NET Core)**

📌 **Install dependencies first**

```sh
dotnet add package Microsoft.AspNetCore.JsonPatch
```

📌 **C# Server Implementation**

```csharp
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.JsonPatch;
using System.Collections.Generic;
using System.Linq;

[ApiController]
[Route("api/users")]
public class UsersController : ControllerBase
{
    private static List<User> Users = new List<User>
    {
        new User { Id = 1, Name = "Alice", Email = "alice@example.com" },
        new User { Id = 2, Name = "Bob", Email = "bob@example.com" }
    };

    [HttpPatch("{id}")]
    public IActionResult PatchUser(int id, [FromBody] JsonPatchDocument<User> patchDoc)
    {
        var user = Users.FirstOrDefault(u => u.Id == id);
        if (user == null) return NotFound();

        patchDoc.ApplyTo(user, ModelState);

        if (!ModelState.IsValid) return BadRequest(ModelState);

        return Ok(user);
    }
}

public class User
{
    public int Id { get; set; }
    public string Name { get; set; }
    public string Email { get; set; }
}
```

💡 **What’s Happening?**

* We create an **in-memory list of users**.
* The `PATCH` method **finds the user**, applies the changes using `JsonPatchDocument`, and returns the updated user.

***

### 🐍 **Python Server (Flask)**

📌 **Install dependencies**

```sh
pip install flask jsonpatch
```

📌 **Python Server Implementation**

```python
from flask import Flask, request, jsonify
from jsonpatch import JsonPatch

app = Flask(__name__)

users = {
    1: {"id": 1, "name": "Alice", "email": "alice@example.com"},
    2: {"id": 2, "name": "Bob", "email": "bob@example.com"},
}

@app.route('/api/users/<int:user_id>', methods=['PATCH'])
def patch_user(user_id):
    if user_id not in users:
        return jsonify({"error": "User not found"}), 404

    patch_data = request.get_json()
    patch = JsonPatch(patch_data)

    users[user_id] = patch.apply(users[user_id])

    return jsonify(users[user_id])

if __name__ == '__main__':
    app.run(debug=True)
```

💡 **What’s Happening?**

* The `PATCH` method **looks for the user, applies changes using `JsonPatch`, and updates the user.**

***

### 🦫 **Go Server (Gin)**

📌 **Install dependencies**

```sh
go get -u github.com/gin-gonic/gin
go get -u github.com/evanphx/json-patch/v5
```

📌 **Go Server Implementation**

```go
package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"github.com/gin-gonic/gin"
	"github.com/evanphx/json-patch/v5"
)

type User struct {
	ID    int    `json:"id"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

var users = map[int]*User{
	1: {ID: 1, Name: "Alice", Email: "alice@example.com"},
	2: {ID: 2, Name: "Bob", Email: "bob@example.com"},
}

func PatchUser(c *gin.Context) {
	id := c.Param("id")

	var patchData []byte
	if err := c.BindJSON(&patchData); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Invalid JSON"})
		return
	}

	user, exists := users[id]
	if !exists {
		c.JSON(http.StatusNotFound, gin.H{"error": "User not found"})
		return
	}

	userData, _ := json.Marshal(user)
	patchedData, err := jsonpatch.MergePatch(userData, patchData)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Invalid PATCH data"})
		return
	}

	json.Unmarshal(patchedData, &user)
	users[id] = user
	c.JSON(http.StatusOK, user)
}

func main() {
	r := gin.Default()
	r.PATCH("/api/users/:id", PatchUser)
	r.Run(":8080")
}
```

💡 **What’s Happening?**

* It **binds the JSON Patch request**, merges it with the existing user data, and updates the record.

***

## 💻 **Client-Side PATCH Implementation**

This is how you **send** PATCH requests from a client.

***

### 🔹 **C# Client (Sending a PATCH request)**

```csharp
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;

class Program
{
    static async Task Main()
    {
        var patchData = new[]
        {
            new { op = "replace", path = "/email", value = "new.email@example.com" }
        };

        var content = new StringContent(JsonConvert.SerializeObject(patchData), Encoding.UTF8, "application/json-patch+json");

        using (var client = new HttpClient())
        {
            var response = await client.PatchAsync("http://localhost:5000/api/users/1", content);
            var responseText = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseText);
        }
    }
}
```

***

### 🐍 **Python Client (Sending a PATCH request)**

```python
import requests
import json

url = "http://localhost:5000/api/users/1"
headers = {"Content-Type": "application/json-patch+json"}

patch_data = [
    {"op": "replace", "path": "/email", "value": "new.email@example.com"}
]

response = requests.patch(url, headers=headers, data=json.dumps(patch_data))

print(response.json())
```

***

### 🦫 **Go Client (Sending a PATCH request)**

```go
package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
)

func main() {
	url := "http://localhost:8080/api/users/1"

	patchData := []map[string]string{
		{"op": "replace", "path": "/email", "value": "new.email@example.com"},
	}

	jsonData, _ := json.Marshal(patchData)

	req, _ := http.NewRequest(http.MethodPatch, url, bytes.NewBuffer(jsonData))
	req.Header.Set("Content-Type", "application/json-patch+json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer resp.Body.Close()

	fmt.Println("Status:", resp.Status)
}
```

***

## 🎯 **Conclusion**

* The **PATCH** method is useful for **partial updates** without resending the whole resource.
* We saw **how to handle PATCH requests on the server** in **C#, Python, and Go**.
* We also **sent PATCH requests from a client** in **C#, Python, and Go**.

📌 **Next time someone asks, "Why use PATCH?" you can confidently reply, "Because sending only what’s needed saves bandwidth and resources!" 🚀**

***

## 🏆 **Key Ideas Table**

| Concept             | Explanation                                                          |
| ------------------- | -------------------------------------------------------------------- |
| **PATCH vs. PUT**   | PATCH updates only part of a resource; PUT replaces the whole thing. |
| **C# Server**       | Uses `JsonPatchDocument` to apply changes.                           |
| **Python Server**   | Uses Flask and `jsonpatch` to modify resources.                      |
| **Go Server**       | Uses Gin framework and `jsonpatch` for updates.                      |
| **Client Requests** | PATCH requests are sent using HTTP libraries in C#, Python, and Go.  |

***

## 📚 **References**

* [RFC 5789: PATCH Method for HTTP](https://datatracker.ietf.org/doc/html/rfc5789)
* [JSON Patch Format](https://tools.ietf.org/html/rfc6902)
