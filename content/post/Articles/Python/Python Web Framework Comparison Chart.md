---
title: Python Web Framework Comparison Chart
description: 
slug: python-webframework-compares-cheatsheet
date: 2024-05-06
image: post/Articles/IMAGES/ballpython.png
categories:
  - Python
  - Web Development
  - Python-Sanic
  - Python-Starlette
  - Python-Masonite
  - Python-FastAPI
  - Python-Responder
  - Python-Molten
  - Python-Japronto
  - Python-Klein
  - Python-Quart
  - Python-BlackSheep
  - Python-Cyclone
  - Python-Django
  - Python-Flask
  - Web Server Gateway Interface-WSGI
  - Asynchronous Server Gateway Interface-ASGI
  - Cheatsheet
tags:
  - Cheatsheet
  - WebDevelopment
weight: 5
draft: false
lastmod: 2025-02-16T19:06:57.379Z
---
## Python Web Framework Comparison Chart

| Framework  | Type         | Performance | Async Support                | Features                                              | Homepage                                              |
| ---------- | ------------ | ----------- | ---------------------------- | ----------------------------------------------------- | ----------------------------------------------------- |
| Sanic      | ASGI         | High        | Yes                          | Fast, lightweight, WebSocket support                  | [Sanic](https://sanic.dev/)                           |
| Starlette  | ASGI         | High        | Yes                          | Lightweight, GraphQL, WebSockets, Background tasks    | [Starlette](https://www.starlette.io/)                |
| Masonite   | WSGI         | Moderate    | No                           | Full-featured, MVC pattern, ORM, Mail, Queues         | [Masonite](https://docs.masoniteproject.com/)         |
| FastAPI    | ASGI         | Very High   | Yes                          | Automatic OpenAPI, Type hints, Dependency injection   | [FastAPI](https://fastapi.tiangolo.com/)              |
| Responder  | ASGI         | Moderate    | Yes                          | API-focused, WebSocket support, Jinja2 templating     | [Responder](https://responder.dev/)                   |
| Molten     | WSGI         | Moderate    | No                           | Type-safe, Dependency injection, Lightweight          | [Molten](https://moltenframework.com/)                |
| Japronto   | Custom       | Very High   | No                           | Ultra-fast, Low-level, HTTP parsing optimizations     | [Japronto](https://github.com/squeaky-pl/japronto)    |
| Klein      | WSGI/Twisted | Moderate    | Yes (via Twisted)            | Twisted-based, URL routing, Middleware                | [Klein](https://github.com/twisted/klein)             |
| Quart      | ASGI         | Moderate    | Yes                          | Flask-compatible, WebSockets, Async SQLAlchemy        | [Quart](https://quart.palletsprojects.com/)           |
| BlackSheep | ASGI         | High        | Yes                          | Fast, WebSockets, Dependency injection                | [BlackSheep](https://www.neoteroi.dev/blacksheep/)    |
| Cyclone    | WSGI/Twisted | Moderate    | Yes (via Twisted)            | Tornado-like API on Twisted, WebSockets               | [Cyclone](https://github.com/cyclone-project/cyclone) |
| **Django** | WSGI/ASGI    | Moderate    | Yes (ASGI in newer versions) | Full-featured, ORM, Admin panel, Security             | [Django](https://www.djangoproject.com/)              |
| **Flask**  | WSGI         | Moderate    | No                           | Micro-framework, Simple, Jinja2, Extensions available | [Flask](https://flask.palletsprojects.com/)           |

## Explaining WGSI vs ASGI

### **1. WSGI (Web Server Gateway Interface)**

* WSGI
  * standard interface between web servers and Python apps.
  * **synchronous** by design.
* **Used for:**
  * Traditional Python web frameworks
    * Django, Flask, and Masonite.
* **Limitation:**
  * Not good for  real-time operations
    * like WebSockets or long-lived connections.

### **2. ASGI (Asynchronous Server Gateway Interface)**

* ASGI
  * extends WSGI by adding support for **async** programming.
  * handles WebSockets,
  * long polling, etc.
* **Used for:**
  * Modern frameworks like FastAPI, Starlette, and Quart.
* **Benefit:**
  * Supports both sync and async applications.

### **3. WSGI/Twisted**

* **Twisted**
  * **event-driven networking engine**
  * predates ASGI and provides asynchronous capabilities.
* **Klein and Cyclone**
  * use Twisted for asynchronous capabilities while still being based on WSGI.
  * not fully ASGI-native but can handle some async features like WebSockets (through Twisted.)

## vs Old SKOOL CGI :)

The first web programs I  wrote ran on a CGI Gateway , and were in C and they were painful to debug, as well as CGI was very bare metal compared to today

for fun - here is a comparsion with old CGI and WSGI

### **Differences Between CGI and WSGI**

| Feature             | CGI (Common Gateway Interface)                      | WSGI (Web Server Gateway Interface)              |
| ------------------- | --------------------------------------------------- | ------------------------------------------------ |
| **Execution Model** | Starts a new process for every request              | Runs as a long-lived process                     |
| **Performance**     | Slow due to process creation overhead               | Faster due to persistent processes               |
| **Concurrency**     | Limited by process startup time                     | Efficient request handling via threads/processes |
| **Statefulness**    | Stateless (each request is isolated)                | Can maintain state across requests               |
| **Scalability**     | Poor (High overhead)                                | Better scalability                               |
| **Modern Use**      | Mostly outdated, rarely used                        | Still widely used in Python web development      |
| **Frameworks**      | Old CGI scripts (e.g., Perl, early Python web apps) | Django, Flask, Pyramid                           |
