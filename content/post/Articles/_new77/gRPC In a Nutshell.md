---
title: gRPC in a Nutshell
description: Code Examples in Python, C# and GO
slug: grpc-in-depth-with-10-code-examples-in-python-csharp-and-go
date: 2021-03-22
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
  - Python
  - C#
  - Go
  - Microservices
  - Networking
  - RPC
  - Protobuf
draft: false
weight: 287
lastmod: 2025-02-14T16:47:57.688Z
---
## 📜 A Quick History Lesson

Back in 2015, Google said, "Let there be gRPC," and boom 💥—we got a powerful framework built on HTTP/2, Protobuf, and a sprinkle of Google magic.

**(BOOM!)**

It was designed to make microservices talk like best friends.

OR at least friends that like to talk to each other...

**efficiently**

For the nerdy details, check out [gRPC.io](https://grpc.io/) or dive into [Google's blog](https://developers.google.com/protocol-buffers/).

## 🧠 What is gRPC Anyway?

gRPC stands for Google Remote Procedure Call.

It's like calling a method on a remote server as if it was your local function.

Think of it as teleportation for function calls.

| Feature       | Description                           |
| ------------- | ------------------------------------- |
| Protocol      | HTTP/2                                |
| Serialization | Protocol Buffers (Protobuf)           |
| Language      | Polyglot (supports tons of languages) |
| Streaming     | Bidirectional and unary streaming     |

## 🛠️ Code Examples

### 1. Python: Protobuf Definition

```proto
syntax = "proto3";
message Person {
  string name = 1;
  int32 age = 2;
  string email = 3;
}
```

### 2. Python: gRPC Service Definition

```proto
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

### 3. Python: Server Implementation

```python
import grpc
from concurrent import futures
import helloworld_pb2
import helloworld_pb2_grpc

class Greeter(helloworld_pb2_grpc.GreeterServicer):
    def SayHello(self, request, context):
        return helloworld_pb2.HelloReply(message=f"Hello {request.name}!")

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    helloworld_pb2_grpc.add_GreeterServicer_to_server(Greeter(), server)
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()

serve()
```

### 4. Python: Client Code

```python
import grpc
import helloworld_pb2
import helloworld_pb2_grpc

channel = grpc.insecure_channel('localhost:50051')
stub = helloworld_pb2_grpc.GreeterStub(channel)
response = stub.SayHello(helloworld_pb2.HelloRequest(name='Pythonista'))
print(response.message)
```

### 5. C#: Protobuf Definition

```proto
syntax = "proto3";
message User {
  string username = 1;
  int32 id = 2;
}
```

### 6. C#: Server Implementation

```csharp
public class GreeterService : Greeter.GreeterBase {
    public override Task<HelloReply> SayHello(HelloRequest request, ServerCallContext context) {
        return Task.FromResult(new HelloReply { Message = $"Hello {request.Name}" });
    }
}
```

### 7. C#: Client Code

```csharp
var channel = GrpcChannel.ForAddress("http://localhost:50051");
var client = new Greeter.GreeterClient(channel);
var reply = client.SayHello(new HelloRequest { Name = "C# Wizard" });
Console.WriteLine(reply.Message);
```

### 8. Go: Protobuf Definition

```proto
syntax = "proto3";
message Task {
  string description = 1;
  bool completed = 2;
}
```

### 9. Go: Server Code

```go
import (
	"context"
	"fmt"
	"google.golang.org/grpc"
	"net"
)
type server struct{}
func (s *server) SayHello(ctx context.Context, in *pb.HelloRequest) (*pb.HelloReply, error) {
	return &pb.HelloReply{Message: "Hello " + in.Name}, nil
}
func main() {
	lis, _ := net.Listen("tcp", ":50051")
	s := grpc.NewServer()
	pb.RegisterGreeterServer(s, &server{})
	s.Serve(lis)
}
```

### 10. Go: Client Code

```go
import (
	"context"
	"fmt"
	"google.golang.org/grpc"
	"log"
	"time"
)
func main() {
	conn, _ := grpc.Dial("localhost:50051", grpc.WithInsecure())
	defer conn.Close()
	c := pb.NewGreeterClient(conn)
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	resp, _ := c.SayHello(ctx, &pb.HelloRequest{Name: "Go Guru"})
	fmt.Println(resp.Message)
}
```

## 🔑 Key Ideas

| Concept        | Explanation                               |
| -------------- | ----------------------------------------- |
| **gRPC**       | Remote Procedure Call framework by Google |
| **Protobuf**   | Serialization format used by gRPC         |
| **HTTP/2**     | Underlying protocol enabling streaming    |
| **Polyglot**   | Supports multiple programming languages   |
| **Efficiency** | More efficient than REST with JSON        |

## 📚 References

* [gRPC Official Docs](https://grpc.io/docs/)
* [Protocol Buffers Guide](https://developers.google.com/protocol-buffers/)
* [Python gRPC Tutorial](https://grpc.io/docs/languages/python/)
* [C# gRPC Guide](https://learn.microsoft.com/en-us/aspnet/core/grpc/)
* [Go gRPC Examples](https://grpc.io/docs/languages/go/)
