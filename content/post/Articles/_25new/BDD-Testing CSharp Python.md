---
title: Testing C# and Python with Behavior-Driven Development (BDD)
description: Quick Intro to BDD
slug: bdd-cs-python
date: 2019-06-22
image: post/Articles/IMAGES/trainbengal.png
categories:
  - BDD
  - Testing
  - Software Development
  - Behavior Driven Design
  - CSharp
  - Python
  - Unit Testing
tags:
  - BDD
  - Testing
  - Software
  - Development
  - C#
  - Python
  - JavaScript
  - TypeScript
  - Tools
  - Gherkin
draft: false
weight: 129
lastmod: 2025-03-05T21:12:26.184Z
---
This is the C# and Python Version of this Article

The Angular and React Version of this Article is here:\
[Testing Angular and React with Behavior-Driven Development (BDD)](/post/Articles/_25new/BDD%20Testing%20with%20Angular%20React.md)

Also check out

* [Exploring Unit Testing with Angular and React](/post/Articles/_25new/Exploring%20Unit%20Testing%20Angular%20and%20React.md)
* [Comparing web up testing approaches](/post/Articles/_new6/Comparing%20web%20up%20testing%20approaches.md)

All Testing Articles here:\
https://brianbraatz.github.io/search/?keyword=testing

<!-- 
# Behavior-Driven Development (BDD): History, Testing, and Tools

## A Brief History of BDD

Once upon a time, in the dark ages of software development (a.k.a the early 2000s), developers and testers were at war.
Developers wrote code that *they* thought made sense, while testers wrote test cases that developers barely understood. Chaos ensued.

Then, in 2003, **Dan North** had a revelation: "What if developers and testers spoke the same language?" And thus, **Behavior-Driven Development (BDD)** was born.

BDD took inspiration from **Test-Driven Development (TDD)** but aimed to make tests more human-readable, so that business people, testers, and developers could all be on the same page. Instead of cryptic unit test names, BDD focused on describing **behavior** in plain English using a structured format called **Gherkin** (yep, like the pickle).
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

Now the Non-Engineers and the Engineers have a common way to communicate features and how to test them .

And this plain language, can be used directly- or semi-directly to actually "be" the test!

## How Does BDD Compare to Other Testing Approaches?

| Approach                | What It Focuses On                            | Pros                       | Cons                              |
| ----------------------- | --------------------------------------------- | -------------------------- | --------------------------------- |
| **TDD**                 | Writing tests before code                     | Ensures code correctness   | Harder for non-devs to understand |
| **BDD**                 | Defining behavior in human-readable scenarios | Improves collaboration     | More setup required               |
| **Unit Testing**        | Testing small, isolated pieces of code        | Fast feedback loop         | Doesn't test full behavior        |
| **Integration Testing** | Ensuring multiple components work together    | Catches system-wide issues | Slower and more complex           |

BDD is like TDD’s more extroverted cousin—it wants *everyone* involved, not just developers.

## How to Do BDD: The Gherkin Language

As introduced above, BDD scenarios are written in **Gherkin**, a simple syntax that follows a Given-When-Then structure:

```gherkin
Feature: Login System
  Scenario: Successful Login
    Given a registered user "Alice"\    When they enter valid credentials
    Then they should see the dashboard
```

Readable, right? Even your non-technical boss can understand that.

Now, let’s implement this in **C# and Python**!

***

## BDD Example in C# (Using SpecFlow)

**Step 1:** Install SpecFlow via NuGet:

```csharp
Install-Package SpecFlow
```

**Step 2:** Write a Gherkin feature file (`Login.feature`):

```gherkin
Feature: Login System
  Scenario: Successful Login
    Given a registered user "Alice"
    When they enter valid credentials
    Then they should see the dashboard
```

**Step 3:** Implement step definitions in C#:

```csharp
[Binding]
public class LoginSteps
{
    private string _username;
    private bool _isLoggedIn;

    [Given("a registered user \"(.*)\"")]
    public void GivenARegisteredUser(string username)
    {
        _username = username;
    }

    [When("they enter valid credentials")]
    public void WhenTheyEnterValidCredentials()
    {
        _isLoggedIn = (_username == "Alice");
    }

    [Then("they should see the dashboard")]
    public void ThenTheyShouldSeeTheDashboard()
    {
        Assert.IsTrue(_isLoggedIn);
    }
}
```

***

## BDD Example in Python (Using Behave)

**Step 1:** Install Behave:

```sh
pip install behave
```

**Step 2:** Write a Gherkin feature file (`login.feature`):

```gherkin
Feature: Login System
  Scenario: Successful Login
    Given a registered user "Alice"
    When they enter valid credentials
    Then they should see the dashboard
```

**Step 3:** Implement step definitions in Python (`steps/login_steps.py`):

```python
from behave import given, when, then

users = {"Alice": "password123"}

@given('a registered user "{username}"')
def step_given_registered_user(context, username):
    context.username = username

@when("they enter valid credentials")
def step_when_enter_credentials(context):
    context.is_logged_in = context.username in users

@then("they should see the dashboard")
def step_then_see_dashboard(context):
    assert context.is_logged_in
```

***

## BDD Tools by Language

| Language                  | Popular BDD Tools              |
| ------------------------- | ------------------------------ |
| **C#**                    | SpecFlow, xBehave.NET          |
| **Python**                | Behave, pytest-bdd             |
| **Java**                  | Cucumber, JBehave              |
| **JavaScript/TypeScript** | Cucumber.js, Jest with Gherkin |
| **Ruby**                  | Cucumber                       |

Yes, **you CAN test JavaScript and TypeScript with BDD**! **Cucumber.js** lets you write tests in Gherkin just like in Python or C#.

***

## Key Ideas

| Concept           | Description                                           |
| ----------------- | ----------------------------------------------------- |
| **BDD Origin**    | Created by Dan North in 2003 to improve collaboration |
| **Gherkin**       | A human-readable language for writing test scenarios  |
| **C# BDD**        | Uses SpecFlow for Gherkin-based tests                 |
| **Python BDD**    | Uses Behave to implement Gherkin tests                |
| **Tools**         | SpecFlow, Behave, Cucumber, and more                  |
| **JS/TS Support** | Yes! Cucumber.js makes it possible                    |

***

## References

1. [Dan North on Introducing BDD](https://dannorth.net/introducing-bdd/)
2. [SpecFlow Documentation](https://specflow.org/docs/)
3. [Behave Documentation](https://behave.readthedocs.io/en/latest/)
4. [Cucumber.js](https://github.com/cucumber/cucumber-js)
5. [Jest with Gherkin](https://github.com/jest-community/jest-cucumber)

***
