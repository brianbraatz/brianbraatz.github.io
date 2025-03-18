---
title: React In a Nutshell
description: Collected Bits of React Wisdom And Snippets
slug: react-nutshell
date: 2018-12-15
image: post/Articles/IMAGES/reactlogolong.png
categories:
  - React
  - Web Development
tags:
  - React
  - Interview
  - Questions
  - JavaScript
  - Frontend
  - Development
  - Coding
  - Examples
draft: false
weight: 9
categories_ref:
  - React
  - Web Development
slug_calculated: https://brianbraatz.github.io/p/react-nutshell
lastmod: 2025-03-14T16:40:23.532Z
---
## 1. What is React?

Ah, the classic opener.

If you don't know this, just pack up and go home.

Kidding!

But seriously, React is a JavaScript library for building user interfaces.

It's like the Lego of the web development world—snap components together to build something cool.

**Example:**

```javascript
import React from 'react';
import ReactDOM from 'react-dom';

function App() {
  return <h1>Hello, world!</h1>;
}

ReactDOM.render(<App />, document.getElementById('root'));
```

For more details, check out the [React Wikipedia page](https://en.wikipedia.org/wiki/React_\(JavaScript_library\)).

## 2. What are Components?

Components are the building blocks of a React application. Think of them as JavaScript functions that return HTML (well, JSX, but let's not split hairs).

**Example:**

```javascript
function Welcome(props) {
  return <h1>Hello, {props.name}</h1>;
}

// Usage
<Welcome name="Sara" />
```

## 3. What is JSX?

JSX stands for JavaScript XML. It's a syntax extension that allows you to write HTML directly within JavaScript. It's like mixing peanut butter and jelly—better together.

**Example:**

```javascript
const element = <h1>Hello, world!</h1>;
```

## 4. What are Props?

Props (short for "properties") are like function arguments in JavaScript. They allow you to pass data from one component to another.

**Example:**

```javascript
function Greeting(props) {
  return <h1>Hello, {props.name}</h1>;
}

// Usage
<Greeting name="John" />
```

## 5. What is State?

State is like a component's personal diary. It's data that changes over time and affects what gets rendered.

**Example:**

```javascript
import React, { useState } from 'react';

function Counter() {
  const [count, setCount] = useState(0);

  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>
        Click me
      </button>
    </div>
  );
}
```

## 6. What are Hooks?

Hooks are special functions that let you "hook into" React features. They're like power-ups for your components.

*Example:*\*

```javascript
import React, { useState, useEffect } from 'react';

unction Timer() {
 const [seconds, setSeconds] = useState(0);

 useEffect(() => {
   const interval = setInterval(() => {
     setSeconds(seconds => seconds + 1);
   }, 1000);
   return () => clearInterval(interval);
 }, []);

 return <div>Seconds: {seconds}</div>;

```

## 7. What is the Virtual DOM?

The Virtual DOM is React's way of being sneaky efficient. It's a lightweight copy of the actual DOM that React uses to see what needs updating without touching the real DOM unnecessarily.

## 8. What is Redux?

Redux is like a global state manager for your application. It helps you manage state outside of your components, making it easier to share data and debug.

**Example:**

```javascript
import { createStore } from 'redux';

// Reducer
function counter(state = 0, action) {
  switch (action.type) {
    case 'INCREMENT':
      return state + 1;
    case 'DECREMENT':
      return state - 1;
    default:
      return state;
  }
}

// Create Store
let store = createStore(counter);

// Usage
store.dispatch({ type: 'INCREMENT' });
console.log(store.getState()); // 1
```

## 9. What is Context API?

The Context API is React's way of making sure you don't have to pass props down like a hot potato through every component. It allows you to share values between components without explicitly passing props.

**Example:**

```javascript
const ThemeContext = React.createContext('light');

function App() {
  return (
    <ThemeContext.Provider value="dark">
      <Toolbar />
    </ThemeContext.Provider>
  );
}

function Toolbar() {
  return (
    <div>
      <ThemedButton />
    </div>
  );
}

function ThemedButton() {
  const theme = React.useContext(ThemeContext);
  return <button className={theme}>I am styled by theme!</button>;
}
```

## 10. What is Prop Drilling?

Prop drilling is the process of passing data through multiple layers of components. It's like playing telephone with your props—sometimes things get lost along the way.

**Example:**

```javascript
function App() {
  const user = { name: 'John' };
  return <Parent user={user} />;
}

function Parent(props) {
  return <Child user={props.user} />;
}

function Child(props) {
  return <Grandchild user={props.user} />;
}

function Grandchild(props) {
  return <h1>Hello, {props.user.name}</h1>;
}
```

<!-- 
## Key Ideas

| Concept         | Description                                                                 |
|-----------------|-----------------------------------------------------------------------------|
| React           | JavaScript library for building user interfaces.                            |
| Components      | Reusable building blocks in React applications.                             |
| JSX             | Syntax extension allowing HTML within JavaScript.                           |
| Props           | Mechanism for passing data between components.                              |
| State           | Internal data management within a component.                                |
| Hooks           | Functions that let you use state and other React features in functional components. |
| Virtual DOM     | Lightweight copy of the DOM for efficient updates.                          |
| Redux           | State management library for JavaScript applications.                       |
| Context API     | Way to share values between components without passing props.               |

-->

## 11. React Server Components

React Server Components (RSC) are one of the hottest topics in modern React.

They allow React components to be **rendered on the server** and sent to the client as HTML, reducing JavaScript bundle sizes.

**Example:**

```javascript
// This is a Server Component (runs only on the server)
export default async function ServerComponent() {
  const data = await fetchData();
  return <div>{data}</div>;
}
```

📖 Read more about it on [React's official site](https://react.dev/).

## 12. Suspense for Data Fetching

Suspense is React’s way of handling asynchronous operations like **data fetching** with built-in loading states.

**Example:**

```javascript
import React, { Suspense } from "react";

const DataComponent = React.lazy(() => import("./DataComponent"));

function App() {
  return (
    <Suspense fallback={<p>Loading...</p>}>
      <DataComponent />
    </Suspense>
  );
}
```

This makes loading states easier than ever. No more `isLoading` state variables everywhere!

## 13. Concurrent Rendering

React’s **Concurrent Mode** allows React to prepare multiple UI updates in the background without blocking the main thread.

Example scenario: **Typing in a search box while data loads**.

**Example:**

```javascript
import { useDeferredValue } from "react";

function Search({ query }) {
  const deferredQuery = useDeferredValue(query);
  return <Results search={deferredQuery} />;
}
```

React will prioritize keeping the UI responsive over rendering heavy computations.

## 14. React.memo for Performance Optimization

When a component **re-renders too much**, it might be time to use `React.memo()`. This **prevents unnecessary renders** by checking if props have changed.

**Example:**

```javascript
const ExpensiveComponent = React.memo(({ value }) => {
  console.log("Re-rendered!");
  return <p>{value}</p>;
});
```

If `value` doesn’t change, the component **won’t** re-render.

## 15. useCallback and useMemo

These hooks help with **performance optimizations** when dealing with functions or computations inside components.

**Example (useCallback):**

```javascript
import { useCallback } from "react";

function Parent() {
  const handleClick = useCallback(() => {
    console.log("Clicked!");
  }, []);
  return <Child onClick={handleClick} />;
}
```

**Example (useMemo):**

```javascript
const expensiveCalculation = useMemo(() => computeSomething(data), [data]);
```

Both help **avoid unnecessary recalculations**.

## 16. Error Boundaries

React **does not catch errors inside event handlers or asynchronous code**. But with **Error Boundaries**, you can catch UI-breaking errors before they crash the entire app.

**Example:**

```javascript
class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError(error) {
    return { hasError: true };
  }

  render() {
    if (this.state.hasError) {
      return <h1>Something went wrong.</h1>;
    }
    return this.props.children;
  }
}
```

Wrap components inside `<ErrorBoundary>` to catch errors **gracefully**.

## 17. Custom Hooks

If your components have **too much repeated logic**, create a **custom hook** instead.

**Example:**

```javascript
function useWindowSize() {
  const [size, setSize] = useState(window.innerWidth);
  useEffect(() => {
    const handleResize = () => setSize(window.innerWidth);
    window.addEventListener("resize", handleResize);
    return () => window.removeEventListener("resize", handleResize);
  }, []);
  return size;
}
```

Now, use it like this:

```javascript
const width = useWindowSize();
```

## 18. React Portal

Portals allow you to render components **outside the root DOM node**, useful for **modals, tooltips, and dropdowns**.

**Example:**

```javascript
import ReactDOM from "react-dom";

function Modal({ children }) {
  return ReactDOM.createPortal(
    <div className="modal">{children}</div>,
    document.getElementById("modal-root")
  );
}
```

No more CSS `z-index` nightmares!

## 19. Hydration in React

Hydration is when the **server renders HTML** first, and React takes over after the initial load.

**Example:**

```javascript
ReactDOM.hydrate(<App />, document.getElementById("root"));
```

It makes SSR (Server-Side Rendering) **faster** and avoids flickering during page loads.

## 20. React Fiber

React Fiber is **the core engine** behind React that allows **concurrent rendering, time slicing, and improved performance**.

While you won’t **write** Fiber directly, understanding it can help with **debugging performance issues**.

***

## 21. React Refs (useRef)

Ever wanted to interact directly with **DOM elements** in React without triggering a re-render?

Enter `useRef()`!

Refs let you reference **DOM nodes or persist values** across renders **without causing re-renders**.

**Example:**

```javascript
import { useRef, useEffect } from "react";

function FocusInput() {
  const inputRef = useRef(null);

  useEffect(() => {
    inputRef.current.focus(); // Auto-focuses the input on mount
  }, []);

  return <input ref={inputRef} type="text" />;
}
```

🔥 **Tip:** `useRef()` is also great for storing **mutable values** between renders.

***

## 22. Controlled vs Uncontrolled Components

Ever heard of **controlled components**?

React wants all inputs to be **controlled**, meaning the **state controls the value**.

**Example of a controlled component:**

```javascript
function ControlledInput() {
  const [text, setText] = useState("");

  return (
    <input
      value={text}
      onChange={(e) => setText(e.target.value)}
    />
  );
}
```

🛑 **Uncontrolled components**, on the other hand, don’t rely on state:

```javascript
function UncontrolledInput() {
  const inputRef = useRef();

  const handleSubmit = () => {
    alert(inputRef.current.value);
  };

  return (
    <div>
      <input ref={inputRef} type="text" />
      <button onClick={handleSubmit}>Submit</button>
    </div>
  );
}
```

Controlled is **React-y**. Uncontrolled is **old-school HTML form-y**. Pick your poison.

***

## 23. Forwarding Refs (`forwardRef`)

Refs are useful, but what if you need to **pass a ref to a child component**? Use `forwardRef()`!

**Example:**

```javascript
import { forwardRef } from "react";

const CustomInput = forwardRef((props, ref) => (
  <input ref={ref} {...props} />
));

function Parent() {
  const inputRef = useRef();
  return <CustomInput ref={inputRef} />;
}
```

This is **super useful** when working with **third-party UI libraries**.

***

## 24. The `useReducer` Hook

**Tired of `useState`?** When state logic gets complex, `useReducer()` comes to the rescue.

**Example:**

```javascript
function reducer(state, action) {
  switch (action.type) {
    case "increment":
      return { count: state.count + 1 };
    case "decrement":
      return { count: state.count - 1 };
    default:
      throw new Error();
  }
}

function Counter() {
  const [state, dispatch] = useReducer(reducer, { count: 0 });

  return (
    <div>
      <button onClick={() => dispatch({ type: "decrement" })}>-</button>
      <span>{state.count}</span>
      <button onClick={() => dispatch({ type: "increment" })}>+</button>
    </div>
  );
}
```

Think of `useReducer()` as **React's mini Redux**.

***

## 25. `useLayoutEffect` vs `useEffect`

Both are similar, but `useLayoutEffect()` **fires before** the browser paints the screen.

Good for **synchronizing the DOM**.

**Example:**

```javascript
useLayoutEffect(() => {
  console.log("useLayoutEffect");
});

useEffect(() => {
  console.log("useEffect");
});
```

👀 **Use `useLayoutEffect` only when necessary**, as it can block painting.

***

## 26. Context API Performance Issues

Context API is **awesome**, but **overuse it, and your app slows down**.

**Solution:** **Memoize Context Values**

```javascript
const value = useMemo(() => ({ theme, toggleTheme }), [theme]);
```

Memoizing context **prevents unnecessary re-renders**.

***

## 27. Lazy Loading Components (`React.lazy`)

Want to **load components only when needed**? Use `React.lazy()`.

**Example:**

```javascript
const LazyComponent = React.lazy(() => import("./LazyComponent"));

function App() {
  return (
    <Suspense fallback={<p>Loading...</p>}>
      <LazyComponent />
    </Suspense>
  );
}
```

🚀 This improves **performance** by reducing **initial load times**.

***

## 28. Key Props in Lists Matter!

If you **don’t use keys properly**, React **will re-render everything** instead of **just updating the changed items**.

**Good example:**

```javascript
items.map((item) => <li key={item.id}>{item.name}</li>);
```

**Bad example:**

```javascript
items.map((item, index) => <li key={index}>{item.name}</li>);
```

**Moral of the story:** Always use **stable, unique IDs** for `key` props.

***

## 29. Render Props

A technique that lets you **share logic between components** without inheritance.\
**Example:**

```javascript
 function MouseTracker({ render }) {
   const [position, setPosition] = useState({ x: 0, y: 0 });

   return <div onMouseMove={(e) => setPosition({ x: e.clientX, y: e.clientY })}>
     {render(position)}
   </div>;
 }
```

Instead of **passing JSX** as children, we **pass a function** that returns JSX.

***

## 30. Strict Mode in React

React’s **Strict Mode** helps **catch potential issues** in your code. It **doesn’t** affect the UI but highlights **side effects and warnings**.

**Example:**

```javascript
<React.StrictMode>
  <App />
</React.StrictMode>
```

🚨 **Using Strict Mode?** Don’t freak out when your component **mounts twice** in development—it’s intentional.

***

## 31. Higher-Order Components (HOCs)

If you’ve ever needed to **reuse logic across multiple components**, **HOCs** are your friends. A Higher-Order Component is **a function that takes a component and returns a new component** with enhanced functionality.

**Example:**

```javascript
function withLogging(WrappedComponent) {
  return function EnhancedComponent(props) {
    console.log("Component rendered with props:", props);
    return <WrappedComponent {...props} />;
  };
}

function Hello({ name }) {
  return <h1>Hello, {name}!</h1>;
}

const EnhancedHello = withLogging(Hello);
```

**When to use HOCs?** When you find yourself **repeating logic** across multiple components (e.g., authentication, logging, styling). However, **hooks have made HOCs less common**.

***

## 32. React’s Event Delegation Magic

Ever wondered **why React handles events differently than vanilla JavaScript**?

Instead of attaching events **directly to DOM elements**,

React uses **event delegation** by **binding all events to the root element** (`document` or `root` div).

This means:

* **Better performance** (fewer event handlers)
* **Works consistently across browsers**
* **Bubbling and capturing are still available**

**Example:**

```javascript
function Button() {
  return <button onClick={() => console.log("Clicked!")}>Click me</button>;
}
```

Even though the click event is **on the button**,

it’s **actually handled by React’s synthetic event system**.

***

## 33. React.lazy() for Component Code Splitting

We talked about `React.lazy()` before, but **did you know** you can use it for **code-splitting routes** dynamically?

**Example:**

```javascript
import { lazy, Suspense } from "react";
import { BrowserRouter as Router, Route, Routes } from "react-router-dom";

const Home = lazy(() => import("./Home"));
const About = lazy(() => import("./About"));

function App() {
  return (
    <Router>
      <Suspense fallback={<p>Loading...</p>}>
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="/about" element={<About />} />
        </Routes>
      </Suspense>
    </Router>
  );
}
```

This way, **React only loads components when needed**, reducing **initial load times**.

***

## 34. Default Props vs. Optional Chaining

Ever seen this?

```javascript
function Greeting({ name = "Stranger" }) {
  return <h1>Hello, {name}!</h1>;
}
```

That’s **default props in destructuring**. But with **optional chaining (`?.`)**, you don’t always need defaults.

**Example:**

```javascript
function Greeting(props) {
  return <h1>Hello, {props.name?.toUpperCase() ?? "Stranger"}!</h1>;
}
```

Both work, but **optional chaining (`?.`) is better when dealing with deeply nested objects**.

***

## 35. The `useImperativeHandle` Hook

**What if you want to expose certain methods** of a component but hide the rest? Use `useImperativeHandle()`.

**Example:**

```javascript
import { useRef, forwardRef, useImperativeHandle } from "react";

const CustomInput = forwardRef((props, ref) => {
const inputRef = useRef();

useImperativeHandle(ref, () => ({
    focus: () => {
    inputRef.current.focus();
    },
}));

return <input ref={inputRef} {...props} />;
});

function Parent() {
const inputRef = useRef();

return (
    <div>
    <CustomInput ref={inputRef} />
    <button onClick={() => inputRef.current.focus()}>Focus Input</button>
    </div>
);
}
```

Now, **only the `focus()` method** is exposed to the parent, **keeping everything else private**.

***

## 36. Why `key` Matters in Lists

We know `key` helps with performance, but **did you know that using array indexes as keys can break UI state?** 🤯

**Bad Example:**

```javascript
items.map((item, index) => <li key={index}>{item.name}</li>);
```

If the **array order changes**, React will **misinterpret UI updates**, causing **unexpected behavior**.

**Good Example:**

```javascript
items.map((item) => <li key={item.id}>{item.name}</li>);
```

Always use **unique IDs** for keys.

***

## 37. `dangerouslySetInnerHTML` (And Why You Should Be Scared)

Need to inject raw HTML? Use `dangerouslySetInnerHTML` (**but carefully**).

**Example:**

```javascript
function DangerousComponent({ html }) {
  return <div dangerouslySetInnerHTML={{ __html: html }} />;
}
```

🚨 **DANGER:** This makes your app **vulnerable to XSS (Cross-Site Scripting)**. Always sanitize user input!

***

## 38. React’s Hydration Concept

Hydration is when **React reuses existing server-rendered HTML** instead of re-rendering from scratch.

**Example:**

```javascript
import { hydrate } from "react-dom";

hydrate(<App />, document.getElementById("root"));
```

Why use it? **SSR (Server-Side Rendering) with Hydration** speeds up **initial page load times**.

***

## 39. The `useTransition` Hook for Smooth UI Updates

If your UI is **lagging due to expensive renders**, `useTransition()` can **delay state updates** for a smoother experience.

**Example:**

```javascript
import { useTransition, useState } from "react";

function ExpensiveComponent() {
  const [search, setSearch] = useState("");
  const [isPending, startTransition] = useTransition();

  function handleChange(event) {
    startTransition(() => {
      setSearch(event.target.value);
    });
  }
  return (
    <div>
      <input onChange={handleChange} />
      {isPending ? <p>Loading...</p> : <p>Results for {search}</p>}
    </div>
  );
}
```

This keeps the **UI responsive** while state updates in the background.

***

## 40. Why You Should Memoize Event Handlers

If you’re passing **functions as props**, **memoize them** using `useCallback()` to avoid unnecessary renders.

**Example:**

```javascript
const handleClick = useCallback(() => {
  console.log("Clicked!");
}, []);
```

***

## MANY of the Key Ideas Behind React :)

| Concept               | Description                                                                         |
| --------------------- | ----------------------------------------------------------------------------------- |
| Components            | Reusable building blocks in React applications.                                     |
| JSX                   | Syntax extension allowing HTML within JavaScript.                                   |
| Props                 | Mechanism for passing data between components.                                      |
| State                 | Internal data management within a component.                                        |
| Hooks                 | Functions that let you use state and other React features in functional components. |
| Virtual DOM           | Lightweight copy of the DOM for efficient updates.                                  |
| Redux                 | State management library for JavaScript applications.                               |
| Context API           | Way to share values between components without passing props.                       |
| Server Components     | Render on the server, reducing client-side JS                                       |
| Suspense              | Handle async data fetching elegantly                                                |
| Concurrent Rendering  | Prioritize updates, making UI smooth                                                |
| React.memo            | Prevent unnecessary renders                                                         |
| useCallback/useMemo   | Optimize functions and values                                                       |
| Error Boundaries      | Catch rendering errors gracefully                                                   |
| Custom Hooks          | Encapsulate reusable logic                                                          |
| Portals               | Render elements outside the root DOM node                                           |
| Hydration             | Improve server-rendered pages                                                       |
| React Fiber           | The core of modern React                                                            |
| `useRef`              | Persist values across renders without causing re-renders                            |
| Controlled Components | State-driven form inputs                                                            |
| `forwardRef`          | Pass refs to child components                                                       |
| `useReducer`          | Alternative to `useState` for complex logic                                         |
| `useLayoutEffect`     | Runs before browser paint                                                           |
| Context Performance   | Memoization prevents unnecessary re-renders                                         |
| Lazy Loading          | Load components only when needed                                                    |
| Key Props             | Stable keys prevent unnecessary re-renders                                          |
| Render Props          | Function-based component logic reuse                                                |
| Strict Mode           | Catches potential issues in development                                             |
| HOCs                  | Function that wraps and enhances a component                                        |
| Event Delegation      | React handles events at the root level                                              |
| Code Splitting        | Lazy-load components dynamically                                                    |
| useImperativeHandle   | Expose component methods selectively                                                |
| Hydration             | Improves SSR performance                                                            |
| useTransition         | Smooth UI updates                                                                   |

***

## References

* [React Documentation](https://react.dev/)
* [Wikipedia - React (JavaScript Library)](https://en.wikipedia.org/wiki/React_\(JavaScript_library\))
* [Server Components](https://nextjs.org/docs/app/building-your-application/rendering/server-components)
* [React Suspense](https://react.dev/reference/react/Suspense)
* [React.memo](https://react.dev/reference/react/memo)
* [Error Boundaries](https://react.dev/reference/react/Component)
