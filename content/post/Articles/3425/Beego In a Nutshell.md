---
title: Beego In a Nutshell
description: Full-fledged web framework for Go
slug: beego-in-a-nutshell
date: 2017-07-14
image: post/Articles/IMAGES/Go-Logo_Blue.png
categories:
  - Go
  - Beego
  - Web Framework
  - MVC
  - ORM
tags:
  - Go
  - Beego
  - Web Framework
  - MVC
  - Routing
  - ORM
draft: false
weight: 534
lastmod: 2025-03-06T12:30:28.432Z
---
# Beego In a Nutshell

<!-- So, you’ve stumbled upon Beego, huh? Maybe you’re tired of wrestling with bloated web frameworks, or perhaps you just like the Go programming language and want to build web apps with it. Either way, you’re in for a treat because Beego is like Go’s chill, no-nonsense web framework that gets stuff done.

Let’s break it all down. -->

***

## What Is Beego?

Beego is a full-fledged web framework for Go that follows the Model-View-Controller (MVC) pattern. It’s lightweight, fast, and perfect for building web applications and RESTful APIs. Unlike some other frameworks, Beego comes with a lot of batteries included: an ORM, built-in caching, session handling, and even task scheduling.

It’s like Django, but for Go.

***

## Why Should You Care?

Here’s why Beego is worth checking out:

* **Speed:** It’s written in Go, so it’s crazy fast.
* **Simplicity:** Minimal setup, maximum productivity.
* **Auto-routing:** Beego can automatically handle routing based on controller methods.
* **Built-in ORM:** No need for third-party libraries.
* **Task Scheduling:** Like cron jobs, but in your Go app.
* **REST API Ready:** Makes building APIs a breeze.
* **Logging and Monitoring:** Comes with built-in logging and performance monitoring tools.

***

## Installing Beego

First things first, you need Go installed. Once that’s out of the way, install Beego using:

```sh
go get github.com/beego/beego/v2
```

And if you want the Beego command-line tool (optional but handy):

```sh
go install github.com/beego/bee/v2@latest
```

Boom! You’re ready to roll.

***

## Creating Your First Beego App

Let’s build a simple Beego web application.

```sh
bee new hello_beego
cd hello_beego
bee run
```

Visit `http://localhost:8080` in your browser, and you’ll see Beego’s default welcome page.

***

## Routing in Beego

Beego makes routing stupidly easy.

Define routes in `main.go` like this:

```go
package main

import (
    "github.com/beego/beego/v2/server/web"
)

func main() {
    web.Router("/", &MainController{})
    web.Run()
}

type MainController struct {
    web.Controller
}

func (c *MainController) Get() {
    c.Ctx.WriteString("Hello, Beego!")
}
```

Run the app, and when you visit `http://localhost:8080/`, you’ll see `Hello, Beego!` displayed.

***

## Beego ORM (Object-Relational Mapping)

Beego comes with its own ORM called `orm`. Here’s how you define a model:

```go
package models

import (
    "github.com/beego/beego/v2/client/orm"
)

type User struct {
    Id   int    `orm:"auto"`
    Name string `orm:"size(100)"`
}

func init() {
    orm.RegisterModel(new(User))
}
```

To interact with the database:

```go
o := orm.NewOrm()
user := User{Name: "John Doe"}
o.Insert(&user)
```

Easy peasy.

***

## Beego vs Other Frameworks

How does Beego stack up against other Go frameworks?

| Feature        | Beego   | Gin        | Fiber      | Echo       |
| -------------- | ------- | ---------- | ---------- | ---------- |
| Full MVC       | ✅       | ❌          | ❌          | ❌          |
| Built-in ORM   | ✅       | ❌          | ❌          | ❌          |
| Auto Routing   | ✅       | ❌          | ❌          | ❌          |
| REST API       | ✅       | ✅          | ✅          | ✅          |
| Middleware     | ✅       | ✅          | ✅          | ✅          |
| Performance    | 🚀 Fast | 🚀 Fastest | 🚀 Fastest | 🚀 Fastest |
| Task Scheduler | ✅       | ❌          | ❌          | ❌          |

***

## Beego Task Scheduling

Need to run periodic tasks? Beego’s got you covered.

```go
package main

import (
    "github.com/beego/beego/v2/task"
    "fmt"
)

func myTask() error {
    fmt.Println("Running scheduled task...")
    return nil
}

func main() {
    t := task.NewTask("myTask", "@every 10s", myTask)
    task.AddTask("myTask", t)
    task.StartTask()
    select {}
}
```

This will run `myTask` every 10 seconds. No more messy cron jobs!

***

<!-- 
## Conclusion

Beego is an awesome web framework for Go developers who want a complete package with minimal fuss. It’s got everything you need to build web apps and APIs efficiently while keeping your code clean and maintainable.

So, if you’re into Go and need a powerful yet simple web framework, give Beego a shot! -->

***

## Key Ideas

| Concept            | Summary                                                   |
| ------------------ | --------------------------------------------------------- |
| **Beego**          | A full-stack MVC web framework for Go.                    |
| **Routing**        | Automatic routing based on controller methods.            |
| **ORM**            | Built-in ORM for database management.                     |
| **Task Scheduler** | Run periodic tasks inside your app.                       |
| **Comparison**     | Beego is more feature-complete than Gin, Fiber, and Echo. |

***

## References

* [Beego Official Docs](https://beego.me/docs)
* [Beego GitHub Repo](https://github.com/beego/beego)
* [Go Programming Language](https://golang.org)
