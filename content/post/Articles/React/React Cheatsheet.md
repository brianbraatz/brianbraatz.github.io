---
title: React Cheatsheet
description: 
slug: React-cheatsheet
date: 2014-05-06
image: post/Articles/IMAGES/reactlogolong.png
categories:
  - React
  - Cheatsheet
  - Web Development
tags:
  - Cheatsheet
  - React
  - Typescript
  - Javascript
  - WebDevelopment
weight: 55
draft: false
categories_ref:
  - React
  - Cheatsheet
  - Web Development
slug_calculated: https://brianbraatz.github.io/p/React-cheatsheet
lastmod: 2025-03-14T16:40:23.518Z
---
## React Cheatsheet

| **Concept**                       | **Syntax/Example**                                                            | **Description**                                         |
| --------------------------------- | ----------------------------------------------------------------------------- | ------------------------------------------------------- |
| **Component**                     | `class MyComponent extends React.Component { ... }`                           | Class-based component                                   |
|                                   | `const MyComponent = () => { ... }`                                           | Functional component                                    |
| **JSX**                           | `<div>Hello, World!</div>`                                                    | JavaScript XML syntax                                   |
| **Props**                         | `<MyComponent name="John" />`                                                 | Passing props to a component                            |
| **State**                         | `this.state = { count: 0 }; this.setState({ count: 1 });`                     | Component state in class-based components               |
|                                   | `const [count, setCount] = useState(0);`                                      | State Hook in functional components                     |
| **Lifecycle Methods**             | `componentDidMount() { ... }`                                                 | Lifecycle method in class-based components              |
|                                   | `useEffect(() => { ... }, []);`                                               | Effect Hook in functional components                    |
| **Event Handling**                | `<button onClick={this.handleClick}>Click me</button>`                        | Handling events in JSX                                  |
|                                   | `const handleClick = () => { ... };`                                          | Event handler in functional components                  |
| **Conditional Rendering**         | `{isLoggedIn ? <Dashboard /> : <Login />}`                                    | Conditional rendering with JSX                          |
| **Lists & Keys**                  | `{items.map(item => <li key={item.id}>{item.name}</li>)}`                     | Rendering lists with unique keys                        |
| **Forms**                         | `<input type="text" value={this.state.value} onChange={this.handleChange} />` | Handling form input                                     |
|                                   | `const [value, setValue] = useState("");`                                     | Handling form input with state in functional components |
| **Refs**                          | `this.myRef = React.createRef();`                                             | Creating a ref in class-based components                |
|                                   | `const myRef = useRef();`                                                     | Creating a ref in functional components                 |
| **Context**                       | `const MyContext = React.createContext();`                                    | Creating a context                                      |
|                                   | `<MyContext.Provider value={value}>...</MyContext.Provider>`                  | Providing context value                                 |
|                                   | `const value = useContext(MyContext);`                                        | Consuming context in functional components              |
| **Fragments**                     | `<React.Fragment>...</React.Fragment>`                                        | Using fragments to group elements                       |
| **React Router**                  | `<Router><Route path="/home" component={Home} /></Router>`                    | Setting up routing with React Router                    |
| **Higher-Order Components (HOC)** | `const withAuth = Component => props => { ... }`                              | Creating a HOC                                          |
| **Custom Hooks**                  | `const useCustomHook = () => { ... };`                                        | Creating custom hooks                                   |
| **Suspense & Lazy Loading**       | `const LazyComponent = React.lazy(() => import('./LazyComponent'));`          | Lazy loading components                                 |
| **Error Boundaries**              | `componentDidCatch(error, info) { ... }`                                      | Error handling in class-based components                |
