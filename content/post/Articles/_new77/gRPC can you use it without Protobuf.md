---
title: gRPC can you use it without Protobuf?
description: Exploring gRPC with JSON - Code Examples in Python, C# and GO
slug: using-grpc-with-and-without-protobuf
date: 2022-08-14
image: post/Articles/IMAGES/grpc.webp
categories:
  - Networking
  - Network Protocols
  - gRPC
  - Protobuf
  - CSharp
  - Python
  - GoLang
tags:
  - gRPC
  - Protobuf
  - Python
  - C#
  - Go
  - Microservices
  - Networking
  - RPC
  - Serialization
draft: false
weight: 134
categories_ref:
  - Networking
  - Network Protocols
  - gRPC
  - Protobuf
  - CSharp
  - Python
  - GoLang
slug_calculated: https://brianbraatz.github.io/p/using-grpc-with-and-without-protobuf
lastmod: 2025-03-14T16:40:31.732Z
---
<!-- 
# 🚀 Using gRPC with and without Protobuf: 10 Code Examples in Python, C#, and Go

Welcome, brave coder! Today, we’re diving into the wacky, wonderful world of gRPC, where services talk to each other like secret agents on walkie-talkies. We'll look at how gRPC works with and without Protobuf, and as always—plenty of code! 🎯
-->

## 🕰️ A Brief History (Because All Good Stories Need One)

Back in 2015, Google launched gRPC to let services communicate easily over the web. It uses HTTP/2 for faster and more reliable connections and Protobuf for efficient, binary serialization. More on this [here](https://grpc.io/).

But what if you don’t want Protobuf? Enter gRPC with alternative serialization....

## 🤔 Why Protobuf vs Without Protobuf?

| Feature              | With Protobuf              | Without Protobuf                  |
| -------------------- | -------------------------- | --------------------------------- |
| **Performance**      | Blazing fast ⚡️            | Slower (e.g., JSON serialization) |
| **Type Safety**      | Strong typing with schemas | Loose typing, more errors         |
| **Interoperability** | Cross-language compatible  | Language-specific quirks          |
|                      |                            |                                   |

## This article is mostly for Fun and Learning..

## There is likely not a good reason to use JSON.. but you CAN! And thats the point..

## 🛠️ Let’s Get to the Code!

### 1. Python: Protobuf gRPC Server

```python
import grpc
from concurrent import futures
import example_pb2
import example_pb2_grpc

class Greeter(example_pb2_grpc.GreeterServicer):
    def SayHello(self, request, context):
        return example_pb2.HelloReply(message=f"Hello, {request.name}!")

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    example_pb2_grpc.add_GreeterServicer_to_server(Greeter(), server)
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()

serve()
```

### 2. Python: Protobuf gRPC Client

```python
import grpc
import example_pb2
import example_pb2_grpc

channel = grpc.insecure_channel('localhost:50051')
stub = example_pb2_grpc.GreeterStub(channel)
response = stub.SayHello(example_pb2.HelloRequest(name="Pythonista"))
print(response.message)
```

### 3. Python: gRPC with JSON

```python
import grpc
import json
from concurrent import futures
from grpc.experimental import json_format

class GreeterService:
    def SayHello(self, request, context):
        data = json.loads(request.data)
        return json_format.MessageToJson({"message": f"Hello, {data['name']}!"})

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()

serve()
```

### 4. C#: Protobuf gRPC Server

```csharp
public class GreeterService : Greeter.GreeterBase
{
    public override Task<HelloReply> SayHello(HelloRequest request, ServerCallContext context)
    {
        return Task.FromResult(new HelloReply { Message = $"Hello, {request.Name}!" });
    }
}
```

### 5. C#: Protobuf gRPC Client

```csharp
var channel = GrpcChannel.ForAddress("http://localhost:50051");
var client = new Greeter.GreeterClient(channel);
var response = client.SayHello(new HelloRequest { Name = "C# Guru" });
Console.WriteLine(response.Message);
```

### 6. C#: gRPC with JSON

```csharp
HttpClient client = new HttpClient();
var content = new StringContent("{\"name\":\"C# Guru\"}", Encoding.UTF8, "application/json");
var response = await client.PostAsync("http://localhost:50051/greet", content);
var result = await response.Content.ReadAsStringAsync();
Console.WriteLine(result);
```

### 7. Go: Protobuf gRPC Server

```go
package main

import (
	"context"
	"fmt"
	"google.golang.org/grpc"
	"net"
	pb "example"
)

type server struct{}

func (s *server) SayHello(ctx context.Context, req *pb.HelloRequest) (*pb.HelloReply, error) {
	return &pb.HelloReply{Message: "Hello, " + req.Name + "!"}, nil
}

func main() {
	lis, _ := net.Listen("tcp", ":50051")
	s := grpc.NewServer()
	pb.RegisterGreeterServer(s, &server{})
	s.Serve(lis)
}
```

### 8. Go: Protobuf gRPC Client

```go
package main

import (
	"context"
	"fmt"
	"google.golang.org/grpc"
	pb "example"
)

func main() {
	conn, _ := grpc.Dial("localhost:50051", grpc.WithInsecure())
	defer conn.Close()
	c := pb.NewGreeterClient(conn)
	resp, _ := c.SayHello(context.Background(), &pb.HelloRequest{Name: "Go Dev"})
	fmt.Println(resp.Message)
}
```

### 9. Go: gRPC with JSON

```go
package main

import (
	"bytes"
	"fmt"
	"net/http"
)

func main() {
	data := `{"name":"Go Dev"}`
	resp, _ := http.Post("http://localhost:50051/greet", "application/json", bytes.NewBuffer([]byte(data)))
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	fmt.Println(string(body))
}
```

### 10. Protobuf Definition (Proto3)

```proto
syntax = "proto3";
service Greeter {
  rpc SayHello (HelloRequest) returns (HelloReply);
}
message HelloRequest {
  string name = 1;
}
message HelloReply {
  string message = 1;
}
```

## 🔑 Key Ideas

| Concept            | Explanation                             |
| ------------------ | --------------------------------------- |
| **gRPC**           | RPC framework using HTTP/2 & Protobuf   |
| **Protobuf**       | Binary serialization for performance    |
| **JSON in gRPC**   | Alternative to Protobuf for flexibility |
| **Cross-platform** | Works across multiple languages         |

## 📚 References

* [gRPC Official Site](https://grpc.io/)
* [Protocol Buffers Guide](https://developers.google.com/protocol-buffers/)
* [Python gRPC Tutorial](https://grpc.io/docs/languages/python/)
* [C# gRPC Tutorial](https://learn.microsoft.com/en-us/aspnet/core/grpc/)
* [Go gRPC Docs](https://grpc.io/docs/languages/go/)
