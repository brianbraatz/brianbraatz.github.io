---
title: Testing Angular and React with Behavior-Driven Development (BDD)
description: ""
slug: bdd-angular-react
date: 2019-05-10
image: post/Articles/IMAGES/cheetahtraining.png
categories:
  - Behavior Driven Design
  - Bdd
  - Testing
  - Angular
  - React
  - Software Development
  - Unit Testing
  - Javascript
  - Typescript
tags:
  - Behavior
  - Driven
  - Design
  - Bdd
  - Testing
  - Angular
  - React
  - Software
  - Development
  - Unit
  - Testing
draft: false
weight: 31
categories_ref:
  - Behavior Driven Design
  - Bdd
  - Testing
  - Angular
  - React
  - Software Development
  - Unit Testing
  - Javascript
  - Typescript
lastmod: 2025-03-14T15:45:15.367Z
---
[Animal training - Wikipedia](https://en.wikipedia.org/wiki/Animal_training)

This is the Angular and React Version of this Article

The C#- Python version is here:\
[Testing C# and Python with Behavior-Driven Development (BDD)](/post/Articles/_25new/BDD-Testing%20CSharp%20Python.md)

Also check out

* [Exploring Unit Testing with Angular and React](/post/Articles/_25new/Exploring%20Unit%20Testing%20Angular%20and%20React.md)
* [Comparing web up testing approaches](/post/Articles/_new6/Comparing%20web%20up%20testing%20approaches.md)

All Testing Articles here:\
https://brianbraatz.github.io/search/?keyword=testing

<!-- 
# Behavior Driven Design (BDD): Its History and How It Relates to Testing

## Once Upon a Time in Software Land...

You ever looked at a test case and thought, "What on Earth is this even testing?" You're not alone. Developers and testers have been scratching their heads at cryptic unit tests for years. Enter **Behavior Driven Development (BDD)**—the hero we didn't know we needed but totally deserve.

Imagine it's 2003. The internet is slow, flip phones are a thing, and unit tests are confusing as heck. Dan North, a software consultant, was tired of seeing developers struggle with writing meaningful tests. So, like any good problem solver, he invented a better way: **Behavior Driven Development**.

-->

## A Brief History of BDD

Once upon a time, in the dark ages of software development (a.k.a the early 2000s), developers and testers were at war.\
Developers wrote code that *they* thought made sense, while testers wrote test cases that developers barely understood. Chaos ensued.

Then, in 2003, **Dan North** had a revelation: "What if developers and testers spoke the same language?" And thus, **Behavior-Driven Development (BDD)** was born.

BDD took inspiration from **Test-Driven Development (TDD)** but aimed to make tests more human-readable, so that business people, testers, and developers could all be on the same page. Instead of cryptic unit test names, BDD focused on describing **behavior** in plain English using a structured format called **Gherkin** (yep, like the pickle).

The idea? **Tests should be human-readable and describe what the system actually does**. That way, even non-tech people (gasp!) could understand what was going on. So instead of writing a test that checks if `2 + 2 === 4`, you'd write:

```gherkin
Scenario: Adding two numbers
  Given I have a calculator
  When I add 2 and 2
  Then the result should be 4
```

Now the Non-Engineers and the Engineers have a common way to communicate features and how to test them .\
And theis plain language, can be used directly- or semi-directly to actually "be" the test!

## How Does BDD Compare to Other Testing Approaches?

| Feature               | Unit Testing     | BDD                    | TDD               |
| --------------------- | ---------------- | ---------------------- | ----------------- |
| Focus                 | Code correctness | System behavior        | Code design       |
| Readability           | Low              | High                   | Medium            |
| Collaboration         | Limited          | Encourages team effort | Mostly developers |
| Documentation         | Implicit         | Self-documenting       | Partial           |
| Ease of Writing Tests | Medium           | Higher (if done right) | Medium            |

### Basic Idea

* **Unit tests** check if code does what it should.
* **TDD** makes you write tests first, then code.
* **BDD** makes you write tests in plain English (or Gherkin) so everyone understands what’s happening.

## BDD in Angular

Angular loves testing, and BDD can easily be done with **Jasmine & Cucumber**. Here’s an example:

### Jasmine BDD Example

```typescript
describe('Calculator', () => {
  it('should add two numbers correctly', () => {
    const result = add(2, 2);
    expect(result).toBe(4);
  });
});
```

Simple, right? Now, if you want **Gherkin-style tests**, you can use **Cucumber.js** with Angular:

### Cucumber.js Example

```gherkin
Feature: Calculator
  Scenario: Adding two numbers
    Given I have a calculator
    When I add 2 and 2
    Then the result should be 4
```

Pair this with **Protractor**, and you’ve got yourself a full-fledged BDD setup.

## BDD in React

React plays well with **Jest & Cucumber**. Here’s a BDD-style Jest test:

### Jest BDD Example

```typescript
describe('Calculator', () => {
  test('adds two numbers correctly', () => {
    expect(add(2, 2)).toBe(4);
  });
});
```

And if you're feeling fancy, you can use **Cucumber.js** with Jest:

### Cucumber.js Example

```gherkin
Feature: Adding numbers
  Scenario: Adding two numbers
    Given I have opened the calculator
    When I enter 2 and 2
    Then I should see 4
```

This pairs beautifully with **React Testing Library** and **Cypress** for E2E testing.

## Different Ways to Do BDD in Angular and React

1. **Jasmine + Karma (Angular)** – The default, easiest way.
2. **Jest + React Testing Library (React)** – The go-to testing combo for React apps.
3. **Cucumber.js + Protractor (Angular)** – For serious Gherkin lovers.
4. **Cucumber.js + Cypress (React & Angular)** – If you want full BDD-style E2E tests.

## Conclusion

BDD is all about **writing tests that make sense**. It bridges the gap between techies and non-techies while making sure software behaves as expected. Whether you're on **Team Angular** or **Team React**, BDD can level up your testing game. So go forth and write tests that actually make sense!

***

## Key Ideas

| Concept            | Description                                             |
| ------------------ | ------------------------------------------------------- |
| **BDD**            | Behavior-driven development for better test readability |
| **History**        | Invented by Dan North in 2003 to improve unit testing   |
| **Comparison**     | BDD vs Unit Testing vs TDD                              |
| **Angular BDD**    | Using Jasmine, Karma, Cucumber.js, and Protractor       |
| **React BDD**      | Using Jest, Cucumber.js, and Cypress                    |
| **Best Practices** | Write human-readable tests that describe behavior       |

***

## References

1. Dan North’s Original BDD Post - [Link](https://dannorth.net/introducing-bdd/)
2. Jasmine Documentation - [Link](https://jasmine.github.io/)
3. Cucumber.js Documentation - [Link](https://cucumber.io/docs/)
4. Jest Documentation - [Link](https://jestjs.io/)
5. Cypress Documentation - [Link](https://www.cypress.io/)
6. Protractor Documentation - [Link](https://www.protractortest.org/)

***

Happy testing! 🚀
