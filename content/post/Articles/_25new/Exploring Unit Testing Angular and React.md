---
title: Exploring Unit Testing Angular and React
description: ""
slug: unittesting-angular-react
date: 2019-06-14
image: post/Articles/IMAGES/girlstesting.png
categories:
  - Unit Testing
  - Angular
  - React
  - Testing Strategies
tags:
  - Unit
  - Testing
  - Angular
  - React
  - Jest
  - Jasmine
  - Testing
  - Strategies
  - Mocking
  - Component
  - Testing
  - Best
  - Practices
draft: false
weight: 81
categories_ref:
  - Unit Testing
  - Angular
  - React
  - Testing Strategies
lastmod: 2025-03-14T15:45:15.477Z
---
<!-- 
# Strategies for Unit Testing Angular and React (Without Losing Your Sanity)
-->

Unit testing. Just hearing those words might send a shiver down your spine.

Maybe it reminds you of a time when you confidently wrote tests, only to watch them fail spectacularly and question your entire existence.

Or maybe you just pretend unit testing doesn’t exist and hope your code never breaks (spoiler: it will).

Well, its not as inmpossible as it might feel..

Lets look at some simple setups for testing with React and Angular.

***

## 🛠️ Why Unit Testing Matters (Yes, It Really Does!)

* **Catches bugs early** – Saves you from future nightmares.
* **Improves refactoring confidence** – Make changes without fear.
* **Encourages better code** – Forces you to write modular, testable code.
* **Makes onboarding easier** – New devs can read tests to understand the codebase.

***

## ⚡ Setting Up Testing in Angular

### 1️⃣ The Testing Stack

Angular comes with **Jasmine** (for writing tests) and **Karma** (for running them). It's like Batman and Robin but for unit tests.

To get started:

```sh
ng new my-angular-app --strict
cd my-angular-app
ng test
```

Boom! You now have a fully functional testing environment.

### 2️⃣ Writing Your First Test (It’s Not That Scary!)

A simple Angular component test looks like this:

```typescript
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MyComponent } from './my.component';

describe('MyComponent', () => {
  let component: MyComponent;
  let fixture: ComponentFixture<MyComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ MyComponent ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(MyComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
```

Not too bad, right? You’re just setting up the component, creating an instance, and making sure it actually exists.

### 3️⃣ Mocking Dependencies with `TestBed`

Real services in tests? Nope. We mock those bad boys.

```typescript
import { MyService } from '../services/my.service';
import { of } from 'rxjs';

describe('MyComponent', () => {
  let myServiceStub: Partial<MyService>;

  beforeEach(() => {
    myServiceStub = {
      getData: () => of(['Test Data'])
    };

    TestBed.configureTestingModule({
      providers: [{ provide: MyService, useValue: myServiceStub }]
    });
  });
});
```

And boom! No more flaky API calls in tests.

***

## ⚡ Setting Up Testing in React

### 1️⃣ The Testing Stack

For React, the holy trinity of testing tools is:

* **Jest** – The test runner.
* **React Testing Library** – A better way to test components.
* **Mock Service Worker (MSW)** – Mocks API calls.

To set up:

```sh
npx create-react-app my-react-app --template typescript
cd my-react-app
npm install --save-dev jest @testing-library/react @testing-library/jest-dom
npm test
```

### 2️⃣ Writing Your First Test (Piece of Cake!)

```typescript
import { render, screen } from '@testing-library/react';
import MyComponent from './MyComponent';

test('renders the component', () => {
  render(<MyComponent />);
  expect(screen.getByText('Hello World')).toBeInTheDocument();
});
```

That’s it! No complex setup, no unnecessary mocks, just testing the output.

### 3️⃣ Mocking API Calls in React

For API calls, Jest and MSW are your friends.

#### Mocking Fetch with Jest

```typescript
import { render, screen } from '@testing-library/react';
import MyComponent from './MyComponent';

global.fetch = jest.fn(() =>
  Promise.resolve({ json: () => Promise.resolve(['Test Data']) })
) as jest.Mock;
```

Or, even better, use **Mock Service Worker (MSW)**:

```typescript
import { rest } from 'msw';
import { setupServer } from 'msw/node';

const server = setupServer(
  rest.get('/api/data', (req, res, ctx) => {
    return res(ctx.json(['Test Data']));
  })
);

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());
```

***

## 🏆 Best Practices for Both Angular and React

✅ **Write small, isolated tests** – Each test should focus on one thing.

✅ **Mock dependencies** – Keep tests fast and reliable.

✅ **Use meaningful test names** – `it('should render a button')` is better than `it('test1')`.

✅ **Avoid testing implementation details** – Test behavior, not internals.

✅ **Run tests automatically** – Set up CI/CD to catch bugs before deployment.

***

## 🔑 Key Ideas

| Concept                 | Details                                                     |
| ----------------------- | ----------------------------------------------------------- |
| Unit Testing Importance | Catches bugs early, improves confidence                     |
| Angular Testing Stack   | Jasmine & Karma                                             |
| React Testing Stack     | Jest & React Testing Library                                |
| Mocking in Angular      | Use `TestBed.configureTestingModule`                        |
| Mocking in React        | Use Jest or MSW                                             |
| Best Practices          | Write small tests, mock dependencies, meaningful test names |

***

## 📚 References

* [Jasmine Official Docs](https://jasmine.github.io/)
* [Jest Official Docs](https://jestjs.io/)
* [Angular Testing Guide](https://angular.io/guide/testing)
* [React Testing Library](https://testing-library.com/)
* [Mock Service Worker (MSW)](https://mswjs.io/)
