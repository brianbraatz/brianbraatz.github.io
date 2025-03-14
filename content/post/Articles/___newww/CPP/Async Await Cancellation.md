---
title: Windows Async-Async Revisited
description: Contemplating Cancellation...
slug: windows-async-async-revisited-what-about-cancellation
date: 2025-12-03
image: post/Articles/IMAGES/asyncawait.png
categories:
  - Concurrency
  - Multithreading
  - ASP.NET
  - Async
tags:
  - Windows
  - Async
  - Cancellation
  - Threads
  - Concurrency
draft: false
weight: 327
categories_ref:
  - Concurrency
  - Multithreading
  - ASP.NET
  - Async
lastmod: 2025-03-14T15:45:27.380Z
---
## Why Do We Even Need Cancellation?

Imagine you’re downloading a file the size of your entire Netflix watch history (don’t lie, we all have one).

Halfway through, you realize it's the wrong file—oops, you wanted the cat videos, not the corporate PowerPoint presentations.

Now you want the download to stop. This, my friends, is where *cancellation* steps in like a digital superhero.

Without it, your app keeps grinding away, downloading data you’ll never use, eating bandwidth like a hungry hippo. So yeah, cancellation is kinda important.

## The Old-School Problem

Back in the day, async code in Windows didn't exactly make cancellation easy.

It was like trying to explain TikTok trends to your grandparents—confusing, frustrating, and occasionally ending in disaster.

The issue? Once you fired off an async operation, it was like launching a rocket: no turning back. If you wanted to stop it, you had to cross your fingers and hope it respected a cancellation token—assuming you remembered to add one.

## The New & Improved Approach

Enter modern async patterns! These days, we've got cancellation baked into APIs like grandma's secret cookie recipe. Cancellation tokens are passed around like backstage passes at a concert, giving you control over when operations should *gracefully bow out*.

Here's a quick sample of how you might use a cancellation token in C#:

```csharp
var cts = new CancellationTokenSource();
var token = cts.Token;

var task = Task.Run(async () => {
    while (!token.IsCancellationRequested) {
        Console.WriteLine("Working hard... or hardly working?");
        await Task.Delay(1000);
    }
    Console.WriteLine("Operation cancelled!");
}, token);

// Cancel the task after 3 seconds
await Task.Delay(3000);
cts.Cancel();
await task;
```

Boom! After three seconds, the task checks the token, realizes it's been given the digital death sentence, and gracefully shuts down.

## But Wait—Not All Operations Play Nice

Now, here's the kicker: not every async operation respects cancellation. Some tasks are like stubborn toddlers—they just keep going regardless of your requests.

For example, old-school Windows I/O APIs might just ignore your precious tokens, leaving your code awkwardly hanging around like a bad Tinder date.

The fix? Make sure you're using modern APIs that respect cancellation or wrap legacy calls in timeouts or custom cancellation logic. Microsoft has done a decent job modernizing core libraries, but if you're dealing with ancient code, prepare for some creative debugging.

## Lessons Learned: Cancellation Isn't Optional

The takeaway? Cancellation isn’t just a *nice-to-have*; it’s essential for robust, responsive applications. Plus, adding it from the get-go saves you from painful rewrites later.

So, next time you're writing async code, remember: Cancellation tokens are your friends. Treat them well, and your app (and your sanity) will thank you.

***

## Key Ideas

| Concept             | Explanation                                       |
| ------------------- | ------------------------------------------------- |
| Cancellation Tokens | Essential for stopping async tasks gracefully     |
| Legacy API Issues   | Older APIs might ignore cancellation requests     |
| Modern Practices    | Use newer libraries that support cancellation     |
| Token Passing       | Pass tokens to async calls early and consistently |
| Debugging Headaches | Lack of cancellation can cause performance issues |

## References

* [Original Article](https://devblogs.microsoft.com/oldnewthing/20250212-00/?p=110857)
* [Microsoft Documentation on Cancellation Tokens](https://docs.microsoft.com/en-us/dotnet/standard/threading/cancellation-in-managed-threads)
* [Raymond Chen's Blog](https://devblogs.microsoft.com/oldnewthing/)
