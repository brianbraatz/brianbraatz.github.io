---
title: F# Code Snippets
description: Collected Useful F# Code Snippets
slug: fsharp-snippets
date: 2016-09-14
image: post/Articles/IMAGES/fsharp.png
categories:
  - F#
  - Functional Programming
  - .NET
  - Programming
  - DotNet
  - FSharp
tags:
  - F#
  - Functional Programming
  - .NET
  - Programming
draft: false
weight: 564
categories_ref:
  - F#
  - Functional Programming
  - .NET
  - Programming
  - DotNet
  - FSharp
lastmod: 2025-03-14T15:45:07.700Z
---
# Common F# Code Snippets

 <!-- You Should Know

So you've dipped your toes into F# and want to see some real-world snippets? Good news—I've got you covered! Here are 10 super useful F# snippets, each explained simply. Let’s roll! -->

## 1. Printing to the Console

```fsharp
printfn "Hello, world!"
```

No need for `Console.WriteLine`, just a simple `printfn` and you're done!

## 2. Defining a Function

```fsharp
let square x = x * x
printfn "%d" (square 5) // Outputs: 25
```

Functions are first-class citizens in F#, and defining one is a breeze.

## 3. Piping Data (`|>` Operator)

```fsharp
let square x = x * x
let result = 5 |> square |> string
printfn "%s" result // Outputs: "25"
```

Piping makes function composition elegant and clean.

## 4. Pattern Matching

```fsharp
let describeNumber x =
    match x with
    | 0 -> "Zero"
    | 1 -> "One"
    | _ -> "Something else"

printfn "%s" (describeNumber 42) // Outputs: "Something else"
```

Forget those clunky `if-else` chains. Pattern matching is the way to go!

## 5. List Comprehensions

```fsharp
let numbers = [1..5]
let squaredNumbers = [for n in numbers -> n * n]
printfn "%A" squaredNumbers // Outputs: [1; 4; 9; 16; 25]
```

Simple, readable, and efficient.

## 6. Recursive Functions

```fsharp
let rec factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)

printfn "%d" (factorial 5) // Outputs: 120
```

Because recursion is the functional way!

## 7. Tuples and Destructuring

```fsharp
let person = ("Alice", 30)
let (name, age) = person
printfn "%s is %d years old." name age
```

Tuple unpacking makes handling grouped data a breeze.

## 8. Records (Lightweight Data Structures)

```fsharp
type Person = { Name: string; Age: int }
let bob = { Name = "Bob"; Age = 25 }
printfn "%A" bob // Outputs: { Name = "Bob"; Age = 25 }
```

Simple, structured data without the C# verbosity.

## 9. Option Type (No More Nulls!)

```fsharp
let divide a b =
    if b = 0 then None
    else Some (a / b)

match divide 10 2 with
| Some result -> printfn "Result: %d" result
| None -> printfn "Cannot divide by zero!"
```

F# says "nope" to nulls, and we love it.

## 10. Asynchronous Code

```fsharp
open System.Threading.Tasks

let asyncExample = async {
    do! Async.Sleep 1000
    printfn "Task Complete!"
}

Async.Start asyncExample
```

<!-- 
Asynchronous programming made simple and elegant.

---

## Key Ideas

| Concept               | Summary |
|-----------------------|------------------------------------------------|
| Printing             | `printfn` for easy console output |
| Functions            | Clean and simple function definitions |
| Piping               | `|>` for smooth function composition |
| Pattern Matching     | A powerful alternative to `if-else` |
| Lists               | Comprehensions for easy transformations |
| Recursion           | Functional approach to loops |
| Tuples & Records    | Structured, immutable data |
| Option Type         | Eliminates `null` values |
| Async Programming   | Lightweight, elegant async operations |

---

## References

- [F# Official Docs](https://learn.microsoft.com/en-us/dotnet/fsharp/)
- [F# for Fun and Profit](https://fsharpforfunandprofit.com/)
- [F# Snippets](https://fssnip.net/)
 -->

<!-- 
---
title: "10 More Common F# Code Snippets You Should Know"
description: "10 More Common F# Code Snippets You Should Know"
slug: "10-more-common-fsharp-snippets"
date: 2018-06-07
image: "post/Articles/IMAGES/37.jpg"
categories: ["F#", "Functional Programming", "Code Snippets"]
tags: ["F#", "Functional Programming", "Code Snippets", ".NET"]
draft: false
weight: 723
---

# 10 More Common F# Code Snippets You Should Know

You thought 10 snippets were enough? Nah, let's keep going! Here are 10 more awesome F# snippets, picking up right where we left off. Get ready to level up! -->

## 11. Sequences (Lazy Evaluation)

```fsharp
let infiniteNumbers = Seq.initInfinite (fun x -> x * x)
printfn "%A" (infiniteNumbers |> Seq.take 5 |> Seq.toList) // Outputs: [0; 1; 4; 9; 16]
```

Need an infinite list but don't want to destroy your RAM? Sequences to the rescue!

## 12. Higher-Order Functions

```fsharp
let applyTwice f x = f (f x)
let double x = x * 2
printfn "%d" (applyTwice double 3) // Outputs: 12
```

Functions that take functions and return functions? Welcome to functional programming!

## 13. Discriminated Unions

```fsharp
type Shape =
    | Circle of float
    | Rectangle of float * float

let area shape =
    match shape with
    | Circle r -> System.Math.PI * r * r
    | Rectangle (w, h) -> w * h

printfn "Circle area: %f" (area (Circle 5.0)) // Outputs: Circle area: 78.54
```

A clean and powerful way to model data variations.

## 14. Using the `map` Function

```fsharp
let numbers = [1..5]
let squared = numbers |> List.map (fun x -> x * x)
printfn "%A" squared // Outputs: [1; 4; 9; 16; 25]
```

Apply a function to every element of a list with zero fuss.

## 15. Folding (Reduce in Other Languages)

```fsharp
let sum = List.fold (fun acc x -> acc + x) 0 [1..5]
printfn "%d" sum // Outputs: 15
```

Folding is just a fancy way of saying "reduce everything to a single value."

## 16. Working with Dictionaries

```fsharp
let dict = dict ["one", 1; "two", 2; "three", 3]
printfn "%d" (dict.["two"]) // Outputs: 2
```

Need key-value pairs? Dictionaries got you covered.

## 17. Using `try...with` for Exception Handling

```fsharp
let safeDivide a b =
    try
        Some (a / b)
    with
    | :? System.DivideByZeroException -> None

match safeDivide 10 0 with
| Some result -> printfn "Result: %d" result
| None -> printfn "Oops! Division by zero."
```

Because sometimes, life (and division) throws you an error.

## 18. Using `async` Workflows

```fsharp
let asyncJob = async {
    do! Async.Sleep 1000
    printfn "Done sleeping!"
}

Async.Start asyncJob
```

Non-blocking, super clean asynchronous programming.

## 19. Partial Application

```fsharp
let multiply x y = x * y
let double = multiply 2
printfn "%d" (double 10) // Outputs: 20
```

Fix arguments ahead of time for ultimate function reusability!

## 20. Using `Seq.filter` for Filtering Data

```fsharp
let numbers = [1..10]
let evens = numbers |> List.filter (fun x -> x % 2 = 0)
printfn "%A" evens // Outputs: [2; 4; 6; 8; 10]
```

Elegant, readable, and functional filtering.

<!-- 
---

## Key Ideas

| Concept               | Summary |
|-----------------------|------------------------------------------------|
| Sequences            | Lazy evaluation for memory efficiency |
| Higher-Order Functions | Functions that operate on other functions |
| Discriminated Unions  | Define complex types in a simple way |
| Mapping & Folding     | Transform and reduce collections elegantly |
| Dictionaries         | Easy key-value pair handling |
| Exception Handling   | `try...with` for catching errors |
| Async Workflows      | Elegant, non-blocking programming |
| Partial Application  | Bind arguments early for flexibility |
| Filtering Data       | Use `filter` to extract what you need |

---

## References

- [F# Official Docs](https://learn.microsoft.com/en-us/dotnet/fsharp/)
- [F# for Fun and Profit](https://fsharpforfunandprofit.com/)
- [F# Snippets](https://fssnip.net/)
 -->

<!-- 
---
title: "10 Advanced F# Code Snippets You Should Know"
description: "10 Advanced F# Code Snippets You Should Know"
slug: "10-advanced-fsharp-snippets"
date: 2019-02-18
image: "post/Articles/IMAGES/29.jpg"
categories: ["F#", "Functional Programming", "Advanced Code Snippets"]
tags: ["F#", "Functional Programming", "Code Snippets", ".NET", "Advanced"]
draft: false
weight: 612
---

# 10 Advanced F# Code Snippets You Should Know

Alright, we’ve gone through 20 essential F# snippets already. Still with me? Good! Now, let’s get into some more advanced F# goodness. Buckle up! -->

## 21. Function Composition (`>>` Operator)

```fsharp
let add2 x = x + 2
let multiply3 x = x * 3
let addThenMultiply = add2 >> multiply3

printfn "%d" (addThenMultiply 4) // Outputs: 18
```

Think of `>>` as piping without explicit arguments. Super clean!

## 22. Defining Custom Operators

```fsharp
let (++) x y = x + y * 2
printfn "%d" (5 ++ 3) // Outputs: 11
```

Why use boring old operators when you can make your own?

## 23. Using the `yield` Keyword in Sequences

```fsharp
let numbers = seq {
    yield 1
    yield 2
    yield! [3; 4; 5]
}
printfn "%A" (numbers |> Seq.toList) // Outputs: [1; 2; 3; 4; 5]
```

`yield!` lets you expand another sequence inside your own.

## 24. Type Providers for Easy Data Access

```fsharp
#r "nuget: FSharp.Data"
open FSharp.Data

let csv = CsvProvider<"Name,Age\nAlice,30\nBob,25">
let data = csv.GetSample()

for row in data.Rows do
    printfn "%s is %d years old" row.Name row.Age
```

Type providers give you strongly-typed access to external data. No manual parsing required!

## 25. Asynchronous Parallel Computation

```fsharp
open System.Threading.Tasks

let task1 = async { return 5 }
let task2 = async { return 10 }
let combined = Async.Parallel [task1; task2]

Async.RunSynchronously combined |> printfn "%A" // Outputs: [|5; 10|]
```

Run multiple async tasks in parallel like a pro.

## 26. Memoization (Caching Function Results)

```fsharp
let memoize f =
    let cache = System.Collections.Generic.Dictionary<_, _>()
    fun x ->
        if cache.ContainsKey(x) then cache.[x]
        else
            let result = f x
            cache.[x] <- result
            result

let expensiveComputation = memoize (fun x -> x * x * x)
printfn "%d" (expensiveComputation 10) // Outputs: 1000
```

Memoization = automatic caching for expensive functions.

## 27. Active Patterns for Smart Matching

```fsharp
let (|Even|Odd|) x = if x % 2 = 0 then Even else Odd

let describeNumber x =
    match x with
    | Even -> "Even number"
    | Odd -> "Odd number"

printfn "%s" (describeNumber 5) // Outputs: Odd number
```

Active patterns let you create custom match cases for cleaner logic.

## 28. Infinite Lazy Sequences

```fsharp
let rec fibs a b = seq {
    yield a
    yield! fibs b (a + b)
}
printfn "%A" (fibs 0 1 |> Seq.take 10 |> Seq.toList)
```

Lazy evaluation lets you generate infinite sequences without crashing your machine.

## 29. Using `MailboxProcessor` for Actor-Based Concurrency

```fsharp
let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop () = async {
        let! msg = inbox.Receive()
        printfn "Received: %s" msg
        return! loop()
    }
    loop()
)

agent.Post "Hello, F#"
```

MailboxProcessor is a great way to handle message-based concurrency.

## 30. Dependency Injection with Partial Application

```fsharp
let log message = printfn "Log: %s" message
let add x y = x + y
let addWithLogging = fun x y ->
    log (sprintf "Adding %d and %d" x y)
    add x y

printfn "%d" (addWithLogging 3 7) // Outputs: Log: Adding 3 and 7, then 10
```

Inject dependencies (like logging) into your functions using partial application.

<!-- ---

## Key Ideas

| Concept               | Summary |
|-----------------------|------------------------------------------------|
| Function Composition | `>>` for composing functions seamlessly |
| Custom Operators     | Define your own operators for better readability |
| Yield in Sequences   | `yield!` for working with sequence expressions |
| Type Providers       | Strongly-typed access to external data |
| Parallel Computation | `Async.Parallel` for running tasks concurrently |
| Memoization         | Cache expensive function results |
| Active Patterns      | Custom pattern matching cases |
| Lazy Sequences      | Generate infinite sequences safely |
| MailboxProcessor    | Actor-based concurrency for message handling |
| Dependency Injection | Use partial application for injecting dependencies | -->

***

## References

* [F# Official Docs](https://learn.microsoft.com/en-us/dotnet/fsharp/)
* [F# for Fun and Profit](https://fsharpforfunandprofit.com/)
* [F# Snippets](https://fssnip.net/)
