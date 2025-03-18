---
title: SOLID in a Nutshell
description: Code examples in Go and Rust
slug: solid-in-a-nutshell-go-rust
date: 2013-09-14
image: post/Articles/IMAGES/metalcloseup.png
categories:
  - Programming
  - Software Design
  - SOLID
  - Go
  - Rust
tags:
  - Programming
  - Software design
  - Solid
  - Go
  - Rust
  - Clean code
draft: false
weight: 612
categories_ref:
  - Programming
  - Software Design
  - SOLID
  - Go
  - Rust
slug_calculated: https://brianbraatz.github.io/p/solid-in-a-nutshell-go-rust
lastmod: 2025-03-14T16:40:14.441Z
---
# SOLID in Go and Rust: A Pragmatic Approach to Clean Code

<!-- Alright, Go and Rust developers, you didn’t think I’d leave you hanging, did you?

Go is all about simplicity, and Rust is all about safety. But can these two languages handle SOLID principles? Absolutely! Let’s dive in. -->

## The SOLID Principles in Go and Rust

### 1. **Single Responsibility Principle (SRP)**

*One job per struct, please.*

#### **Bad Example (Go):**

```go
package main

type Report struct {}

func (r Report) Generate() {
    // Generate report
}

func (r Report) SaveToFile() {
    // Save report to file
}
```

The `Report` struct is doing too much. Let’s split it up.

#### **Good Example (Go):**

```go
package main

type ReportGenerator struct {}

func (r ReportGenerator) Generate() {
    // Generate report
}

type ReportSaver struct {}

func (r ReportSaver) Save(report ReportGenerator) {
    // Save to file
}
```

Now, each struct has its own responsibility. Much better!

#### **Bad Example (Rust):**

```rust
struct Report;

impl Report {
    fn generate(&self) {
        // Generate report
    }
    fn save_to_file(&self) {
        // Save report
    }
}
```

#### **Good Example (Rust):**

```rust
struct ReportGenerator;

impl ReportGenerator {
    fn generate(&self) {
        // Generate report
    }
}

struct ReportSaver;

impl ReportSaver {
    fn save(&self, report: &ReportGenerator) {
        // Save report
    }
}
```

Separation of concerns FTW!

***

### 2. **Open/Closed Principle (OCP)**

*A struct should be open for extension, but closed for modification.*

#### **Bad Example (Go):**

```go
package main

type DiscountService struct {}

func (d DiscountService) Apply(price float64, discountType string) float64 {
    if discountType == "Christmas" {
        return price * 0.9
    } else if discountType == "BlackFriday" {
        return price * 0.8
    }
    return price
}
```
