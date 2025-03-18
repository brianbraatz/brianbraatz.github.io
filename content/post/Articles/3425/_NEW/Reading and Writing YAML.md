---
title: Reading and Writing YAML
description: In C++, C#, Go, Python, and Rust
slug: reading-and-writing-yaml-cpp-csharp-go-python-rust
date: 2017-07-19
image: post/Articles/IMAGES/43.jpg
categories:
  - Yaml
  - Programming
  - CPP
  - CSharp
  - Go
  - Python
  - Rust
tags:
  - Yaml
  - Programming
  - Cpp
  - Csharp
  - Go
  - Python
  - Rust
draft: false
weight: 172
categories_ref:
  - Yaml
  - Programming
  - CPP
  - CSharp
  - Go
  - Python
  - Rust
slug_calculated: https://brianbraatz.github.io/p/reading-and-writing-yaml-cpp-csharp-go-python-rust
lastmod: 2025-03-14T16:40:17.479Z
---
<!-- # Reading and Writing YAML in C++, C#, Go, Python, and Rust -->

YAML: the beautiful, human-readable, indentation-sensitive cousin of JSON that somehow manages to cause equal amounts of joy and frustration.

If you've ever dealt with configuration files, you’ve probably encountered YAML. And if you’ve ever been tasked with reading or writing YAML in different programming languages, you know the struggle of finding the right library, avoiding dependency hell, and figuring out why your indentation keeps breaking things.

## C++: The Land of Headers and Templates

C++ doesn’t come with YAML support out of the box (big surprise), but luckily, some great libraries exist to make our lives easier.

### **YAML-CPP**

[YAML-CPP](https://github.com/jbeder/yaml-cpp) is the de facto YAML library for C++.

#### **Installation:**

```sh
git clone https://github.com/jbeder/yaml-cpp.git
cd yaml-cpp
mkdir build && cd build
cmake ..
make && make install
```

#### **Reading YAML in C++:**

```cpp
#include <iostream>
#include "yaml-cpp/yaml.h"

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    std::cout << "Title: " << config["title"].as<std::string>() << std::endl;
    return 0;
}
```

#### **Writing YAML in C++:**

```cpp
YAML::Emitter out;
out << YAML::BeginMap;
out << YAML::Key << "name" << YAML::Value << "C++ Programmer";
out << YAML::EndMap;
std::cout << out.c_str();
```

It’s clean, efficient, and mostly pain-free—until you inevitably mess up CMake.

## C#: The Comfort of .NET

C# developers get to use the excellent **YamlDotNet** library, which is well-maintained and integrates smoothly into .NET projects.

### **Installation:**

```sh
dotnet add package YamlDotNet
```

#### **Reading YAML in C#:**

```csharp
using System;
using YamlDotNet.Serialization;
using System.IO;

class Program {
    static void Main() {
        var deserializer = new DeserializerBuilder().Build();
        var yaml = File.ReadAllText("config.yaml");
        var obj = deserializer.Deserialize<dynamic>(yaml);
        Console.WriteLine(obj["title"]);
    }
}
```

#### **Writing YAML in C#:**

```csharp
var serializer = new SerializerBuilder().Build();
var yaml = serializer.Serialize(new { name = "C# Developer" });
Console.WriteLine(yaml);
```

With LINQ and dynamic objects, C# makes YAML handling a breeze.

## Go: The Simple and Elegant Way

Go has a fantastic YAML library called [gopkg.in/yaml.v3](https://pkg.go.dev/gopkg.in/yaml.v3).

### **Installation:**

```sh
go get gopkg.in/yaml.v3
```

#### **Reading YAML in Go:**

```go
package main

import (
    "fmt"
    "gopkg.in/yaml.v3"
    "os"
)

type Config struct {
    Title string `yaml:"title"`
}

func main() {
    file, _ := os.ReadFile("config.yaml")
    var config Config
    yaml.Unmarshal(file, &config)
    fmt.Println("Title:", config.Title)
}
```

#### **Writing YAML in Go:**

```go
config := Config{Title: "Gopher Life"}
out, _ := yaml.Marshal(&config)
fmt.Println(string(out))
```

Go keeps it straightforward—no surprises here.

## Python: YAML, But Make It Pythonic

Python developers get the joy of using [PyYAML](https://pyyaml.org/), which makes YAML handling almost too easy.

### **Installation:**

```sh
pip install pyyaml
```

#### **Reading YAML in Python:**

```python
import yaml

with open("config.yaml", "r") as f:
    config = yaml.safe_load(f)
    print("Title:", config["title"])
```

#### **Writing YAML in Python:**

```python
config = {"name": "Pythonista"}
with open("output.yaml", "w") as f:
    yaml.dump(config, f)
```

Python, as always, makes things simple and intuitive.

## Rust: The Safe and Speedy Choice

Rust’s package ecosystem includes [serde\_yaml](https://docs.rs/serde_yaml/latest/serde_yaml/), which integrates seamlessly with Serde.

### **Installation:**

```sh
cargo add serde_yaml serde --features derive
```

#### **Reading YAML in Rust:**

```rust
use serde::Deserialize;
use serde_yaml;
use std::fs;

#[derive(Debug, Deserialize)]
struct Config {
    title: String,
}

fn main() {
    let yaml = fs::read_to_string("config.yaml").unwrap();
    let config: Config = serde_yaml::from_str(&yaml).unwrap();
    println!("Title: {}", config.title);
}
```

#### **Writing YAML in Rust:**

```rust
use serde::Serialize;
use serde_yaml;

#[derive(Serialize)]
struct Config {
    name: String,
}

fn main() {
    let config = Config { name: "Rustacean".to_string() };
    let yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", yaml);
}
```

<!-- Rust, while strict, rewards you with blazing-fast, memory-safe YAML processing. -->

## Key Ideas

| Key Idea                 | Summary                                     |
| ------------------------ | ------------------------------------------- |
| C++ with YAML-CPP        | A clean and efficient C++ library for YAML. |
| C# with YamlDotNet       | Simple and intuitive YAML handling in .NET. |
| Go with gopkg.in/yaml.v3 | A lightweight and elegant YAML library.     |
| Python with PyYAML       | The easiest way to handle YAML.             |
| Rust with serde\_yaml    | Safe, fast, and integrates well with Serde. |

## References

* [YAML-CPP GitHub](https://github.com/jbeder/yaml-cpp)
* [YamlDotNet GitHub](https://github.com/aaubry/YamlDotNet)
* [Go YAML v3](https://pkg.go.dev/gopkg.in/yaml.v3)
* [PyYAML Documentation](https://pyyaml.org/)
* [Serde YAML Docs](https://docs.rs/serde_yaml/latest/serde_yaml/)
