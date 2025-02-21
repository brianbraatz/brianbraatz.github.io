---
title: gRPC COmpared with WCF and DCOM
description: Code Examples in Python, C# and GO
slug: grpc-in-depth-comparison-with-wcf-and-dcom
date: 2020-05-18
image: post/Articles/IMAGES/grpc.webp
categories:
  - Networking
  - Network Protocols
  - gRPC
  - Protobuf
  - CSharp
  - Python
  - GoLang
  - WinApi
tags:
  - gRPC
  - WCF
  - DCOM
  - Python
  - Go
  - Microservices
  - Networking
  - RPC
draft: false
weight: 412
lastmod: 2025-02-21T01:11:03.583Z
---
## 🕰️ A Little Backstory (because context matters)

### DCOM (Distributed Component Object Model)

* Created by Microsoft in the 1990s (think: grunge music and dial-up internet).
* Built to enable communication between software components across machines.
* More info [here](https://learn.microsoft.com/en-us/windows/win32/com/component-object-model).

### WCF (Windows Communication Foundation)

* Introduced with .NET Framework 3.0 in 2006.
* Made for building service-oriented applications.
* More [here](https://learn.microsoft.com/en-us/dotnet/framework/wcf/).

### gRPC (Google Remote Procedure Call)

* Launched by Google in 2015.
* Uses HTTP/2, Protobuf, and it's blazing fast.
* More [here](https://grpc.io/).

## 🚀 Head-to-Head Comparison

| Feature           | DCOM         | WCF                   | gRPC              |
| ----------------- | ------------ | --------------------- | ----------------- |
| **Protocol**      | COM/DCOM     | SOAP, HTTP, TCP, MSMQ | HTTP/2            |
| **Serialization** | Binary       | XML, Binary, JSON     | Protobuf          |
| **Platform**      | Windows-only | Windows-centric       | Cross-platform    |
| **Streaming**     | Nope         | Limited               | Fully supported   |
| **Performance**   | Slowish      | Decent                | Lightning fast ⚡️ |

## 🛠️ Code Examples

### 1. Python: Protobuf Definition

```proto
syntax = "proto3";
message Greeting {
  string message = 1;
}
```

### 2. Python: gRPC Server

```python
import grpc
from concurrent import futures
import greeting_pb2
import greeting_pb2_grpc

class Greeter(greeting_pb2_grpc.GreeterServicer):
    def SayHello(self, request, context):
        return greeting_pb2.Greeting(message=f"Hello {request.name}!")

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    greeting_pb2_grpc.add_GreeterServicer_to_server(Greeter(), server)
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()

serve()
```

### 3. Python: gRPC Client

```python
import grpc
import greeting_pb2
import greeting_pb2_grpc

channel = grpc.insecure_channel('localhost:50051')
stub = greeting_pb2_grpc.GreeterStub(channel)
response = stub.SayHello(greeting_pb2.HelloRequest(name='Pythonista'))
print(response.message)
```

### 4. C#: WCF Service

```csharp
[ServiceContract]
public interface IGreetingService {
    [OperationContract]
    string SayHello(string name);
}

public class GreetingService : IGreetingService {
    public string SayHello(string name) => $"Hello {name}!";
}
```

### 5. C#: WCF Client

```csharp
var factory = new ChannelFactory<IGreetingService>(new BasicHttpBinding(), new EndpointAddress("http://localhost:8000/GreetingService"));
var client = factory.CreateChannel();
Console.WriteLine(client.SayHello("C# Wizard"));
```

### 6. Go: gRPC Server

```go
import (
	"context"
	"fmt"
	"google.golang.org/grpc"
	"net"
)
type server struct{}
func (s *server) SayHello(ctx context.Context, req *pb.HelloRequest) (*pb.HelloReply, error) {
	return &pb.HelloReply{Message: "Hello " + req.Name}, nil
}
func main() {
	lis, _ := net.Listen("tcp", ":50051")
	s := grpc.NewServer()
	pb.RegisterGreeterServer(s, &server{})
	s.Serve(lis)
}
```

### 7. Go: gRPC Client

```go
import (
	"context"
	"fmt"
	"google.golang.org/grpc"
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

### 8. Python: DCOM Setup (Simplified)

```python
import win32com.client
app = win32com.client.Dispatch("Excel.Application")
app.Visible = True
print("DCOM Excel instance launched! ON ANOTHER COMPUTER")
```

### 9. C#: Simple DCOM Client

```csharp
Type excelType = Type.GetTypeFromProgID("Excel.Application");
dynamic excel = Activator.CreateInstance(excelType);
excel.Visible = true;
Console.WriteLine("DCOM Excel instance launched!  ON ANOTHER COMPUTER");
```

### 10. Go: Basic HTTP Call (WCF-style)

```go
resp, err := http.Get("http://localhost:8000/GreetingService?name=GoGuru")
if err != nil {
	log.Fatal(err)
}
defer resp.Body.Close()
body, _ := ioutil.ReadAll(resp.Body)
fmt.Println(string(body))
```

## 🔑 Key Ideas

| Concept            | Explanation                                        |
| ------------------ | -------------------------------------------------- |
| **gRPC**           | Modern RPC framework with HTTP/2 and Protobuf      |
| **WCF**            | Windows-centric, versatile communication framework |
| **DCOM**           | Legacy component communication from the 90s        |
| **Streaming**      | Fully supported in gRPC                            |
| **Cross-platform** | Only gRPC supports true cross-platform calls       |

## 📚 References

* [gRPC Official Docs](https://grpc.io/docs/)
* [WCF Documentation](https://learn.microsoft.com/en-us/dotnet/framework/wcf/)
* [DCOM Basics](https://learn.microsoft.com/en-us/windows/win32/com/component-object-model)
* [Python gRPC Guide](https://grpc.io/docs/languages/python/)
* [C# gRPC Guide](https://learn.microsoft.com/en-us/aspnet/core/grpc/)
* [Go gRPC Examples](https://grpc.io/docs/languages/go/)

💡 **Pro tip:** If you hear weird terms like "IDL" or "marshaling," just nod knowingly and Google them later. 😉
