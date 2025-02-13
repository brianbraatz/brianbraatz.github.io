---
title: Early AI- How LISP and Prolog Led to Modern AI
description: The future was gonna be cool... Or so it seemed..
slug: ai-early
date: 2020-12-03
image: post/Articles/IMAGES/hal5.jpg
categories:
  - Artificial Intellgence
  - History
  - Languages
  - Machine Learning
  - LISP
  - Prolog
tags:
  - AI
  - LISP
  - Prolog
  - Artificial
  - Intelligence
  - History
  - of
  - AI
  - Logic
  - Programming
  - Machine
  - Learning
  - Neural
  - Networks
  - Expert
  - Systems
draft: false
weight: 487
lastmod: 2025-02-13T16:08:03.583Z
---
## The Birth of AI: Enter LISP and Prolog

Before ChatGPT, Alexa, and robots that can flip pancakes, there was a time when AI was all about logic and rules.

And back then, the cool kids on the AI block were **LISP** and **Prolog** — two programming languages that laid the groundwork for modern artificial intelligence.

Why these two languages?

Well, because in the early days, AI researchers weren't chasing viral tweets.

They were after something much more ambitious: understanding and mimicking human reasoning.

## Why LISP? The Language of Symbolic AI

### A Bit of History

LISP, short for *LISt Processing*, was created in 1958 by John McCarthy at MIT.

He wanted a language that could manipulate symbols rather than just crunch numbers — perfect for AI, which involves processing language, logic, and abstract concepts.

### Key Features

* **Homoiconicity**: Code and data share the same structure (lists).
* **Garbage Collection**: Yep, LISP had it way before Java.
* **Recursion and Symbolic Processing**: It excelled at tasks like parsing sentences or solving puzzles.

### Simple LISP Example

Here's a LISP program that defines a recursive factorial function:

```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print (factorial 5))  ; Outputs 120
```

### LISP's Role in Early AI

LISP powered many early AI projects, including SHRDLU (a natural language understanding program) and early expert systems.

Its flexibility made it ideal for research into symbolic AI — where programs manipulated human-readable rules to mimic logical thinking.

## Prolog: The Language of Logic Programming

### A Brief Origin Story

Prolog (PROgramming in LOGic) was created in the early 1970s by Alain Colmerauer and Philippe Roussel. It took a different approach to AI: instead of telling the computer *how* to do something, you told it *what* you wanted, and the system figured out the rest.

### Key Features

* **Declarative Syntax**: You define relationships and facts, not step-by-step procedures.
* **Backtracking**: The interpreter tries different possibilities until it finds a solution.
* **Unification**: It matches patterns between facts and queries.

### Simple Prolog Example

```prolog
% Facts
parent(john, mary).
parent(mary, alice).

% Rules
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

% Query
?- grandparent(john, alice).
% Prolog answers: true
```

### Prolog in Action

Prolog was the go-to language for natural language processing (NLP), expert systems, and theorem proving.

Ever heard of the Japanese Fifth Generation Computer Systems Project?

Yep, they bet big on Prolog.

## LISP vs. Prolog: Different Paths to AI

| **Feature**        | **LISP**                      | **Prolog**                  |
| ------------------ | ----------------------------- | --------------------------- |
| **Paradigm**       | Functional, symbolic AI       | Logic programming           |
| **Core Mechanism** | Recursion and list processing | Facts, rules, and inference |
| **AI Focus**       | Knowledge representation      | Rule-based reasoning        |

While LISP treated AI like solving puzzles by rearranging pieces, Prolog approached it like Sherlock Holmes solving a case by connecting facts.

## How Did These Languages Influence Modern AI?

### 1. **Symbolic AI to Machine Learning**

* Early AI was rule-based: manually written rules for decision-making.
* Modern AI (like LLMs) learns rules from massive datasets without human hand-holding.

### 2. **Natural Language Processing (NLP)**

* LISP powered SHRDLU, a natural-language understanding system in a virtual block world.
* Prolog laid the foundation for logic-based NLP techniques still used today.

### 3. **Expert Systems to Neural Networks**

* LISP enabled early expert systems like MYCIN (medical diagnosis).
* Prolog helped with rule-based reasoning, influencing modern knowledge graphs.

## My First AI Adventure with LISP

I remember typing `(+ 1 2)` into a LISP interpreter and feeling like I'd unlocked the secrets of the universe.

The code looked like alien hieroglyphs, but when I built a chatbot that could guess animals (yep, inspired by the *Animals* program), it felt like magic.

## Why LISP and Prolog Still Matter

Sure, modern AI uses Python more than LISP or Prolog.

But the principles — symbolic reasoning, pattern matching, and rule-based inference — still underpin modern machine learning models and NLP systems.

### Key Ideas

* LISP was created by John McCarthy in 1958 for symbolic AI.
* Prolog was developed in the 1970s for logic-based AI.
* Both languages influenced natural language processing, expert systems, and machine learning.
* Their principles can be seen in today’s rule-based and neural network-driven AI.

### References

1. McCarthy, J. (1960). *Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I*.
2. <https://en.wikipedia.org/wiki/LISP_programming_language>
3. <https://en.wikipedia.org/wiki/Prolog>
4. <https://builtin.com/artificial-intelligence/lisp-prolog-ai>
5. Norvig, P. (1992). *Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp*.
6. Russell, S. & Norvig, P. (2010). *Artificial Intelligence: A Modern Approach*.
