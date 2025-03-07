---
title: Lessons Learned on a Huge Legacy MIgration
description: Lessons Learned from a Massive Legacy Conversion with Using Dashboards, the Strangler Pattern, and QE Metrics
slug: lessons-learned-legacy-conversion-strangler-pattern-qe-metrics
date: 2018-07-14
image: post/Articles/IMAGES/55.jpg
categories:
  - Legacy Modernization
  - Software Engineering
  - QE Metrics
  - ServiceNow
  - Strangler Pattern
  - Quality Engineering (QE) Metrics
tags:
  - Legacy Modernization
  - Strangler Pattern
  - Software Engineering
  - QE Metrics
  - Testing
draft: false
weight: 547
lastmod: 2025-03-07T15:23:12.819Z
---
<!-- 
# Lessons Learned from a Massive Legacy Conversion with the Strangler Pattern and QE Metrics

## The Nightmare That Was Our Legacy System -->

I was hired to inherit an internal company system used by 40,000–60,000 people(employees) yearly, patched together over decades, with parts built in different technologies, no automated tests, and an architecture that looked more like a spaghetti monster than an actual structured system.

<!-- 
Sound fun? No. -->

<!-- 
But this was the reality we were dealing with. A massive, mission-critical application that nobody fully understood. If something broke, you’d better hope the one guy who wrote that piece in 2003 was still around to remember how to fix it. (Spoiler: he probably wasn’t.) -->

## Where Do You Even Start? (Hint: A Dashboard)

The biggest challenge in modernizing a legacy system isn't rewriting code. It’s figuring out **what the heck is even happening in the first place**.

So, instead of jumping in with big changes and breaking everything, I started small: **a simple dashboard**.

The first version pulled data from our internal ticketing system (ServiceNow) to track support issues. It wasn’t much, but at least we knew what was breaking and how often.

## Enter the Strangler Pattern (Or, "How Not to Burn Everything Down")

If you’ve never heard of the **Strangler Pattern**, it’s a method for modernizing systems without a big-bang rewrite.

Instead of replacing everything at once (and watching chaos unfold), you **gradually build new functionality alongside the old system**, slowly phasing out the old parts over time.

We followed this approach religiously. First, we hooked up ServiceNow to our dashboard.

Then, we connected Git. Then, we added test coverage metrics. Then, we tied in deployment data.

Little by little, our dashboard became the single source of truth for everything happening in the system.

## QE Metrics: The "Aha!" Moment

Somewhere along the way, I got inspired by **Quality Engineering (QE) Metrics**.

### What Are QE Metrics?

QE Metrics are a set of measurements used to **track software quality, reliability, and efficiency** over time.

These aren’t just "nice-to-have" stats; they’re essential for understanding whether your software is improving or turning into a dumpster fire.

They were popularized by **Jez Humble and Nicole Forsgren** (of "Accelerate" fame), and they focus on things like:

* **Lead Time for Changes** (How fast can you go from code commit to production?)
* **Deployment Frequency** (How often are you releasing?)
* **Mean Time to Recovery (MTTR)** (How long does it take to recover from failure?)
* **Change Failure Rate** (What percentage of changes cause incidents?)

### Applying QE Metrics to Our Legacy Modernization

We gradually built these metrics into our dashboard:

* **Support tickets?** Already there from ServiceNow.
* **Git activity?** Hooked in.
* **Deployment times?** Added from our CI/CD pipelines.
* **Test coverage?** Slowly improving as we modernized the codebase.
* **Incident recovery times?** Extracted from system logs.

Over six years (yes, **six years**), this thing evolved into a powerhouse of data.

We could track system health, identify problem areas, and justify modernization work with **actual numbers** instead of gut feelings.

## The Biggest Lesson: **Just Start.**

Here’s the deal: **you’re never going to have all the data you need right away.**

When we started, we had zero unit tests and no structured metrics.

But by **starting with something simple** (a basic dashboard) and iterating over time, we built a system that gave us real insight into our modernization progress.

So if you're facing a terrifying legacy system:

* **Start tracking anything you can.** Even basic support ticket data is better than nothing.
* **Don’t wait for perfect data.** Just get something useful and improve it over time.
* **Keep adding more.** Test coverage, deployment metrics, failure rates—whatever helps you see the bigger picture.
* **Use QE Metrics to measure progress.** They give you a way to prove (to yourself and management) that things are actually getting better.

Modernizing a legacy system is **a long, painful, but totally doable journey**. Just start. And don’t stop.

***

## Key Ideas

| Topic                   | Summary                                                               |
| ----------------------- | --------------------------------------------------------------------- |
| **Legacy System Chaos** | No tests, patchwork of old tech, and 40k–60k users annually.          |
| **Start Small**         | Built a dashboard to track basic support metrics first.               |
| **Strangler Pattern**   | Gradually replaced old parts instead of rewriting everything at once. |
| **QE Metrics**          | Inspired by software quality metrics to track progress.               |
| **Six-Year Journey**    | Dashboard evolved to cover deployments, tests, and incident recovery. |
| **Biggest Lesson**      | Start somewhere, even if it's small. Improve over time.               |

***

<!-- 
## References

- [The Strangler Fig Application - Martin Fowler](https://martinfowler.com/bliki/StranglerFigApplication.html)
- [Accelerate: The Science of Lean Software and DevOps](https://itrevolution.com/book/accelerate/)
- [What are DORA Metrics?](https://www.devops-research.com/research.html) -->
