---
title: Linux Signals in a Nutshell
description: SIGKILL, SIGTERM, SIGHUP, SIGSTOP, SIGCONT
slug: linux-signals-in-a-nutshell
date: 2017-08-14
image: post/Articles/IMAGES/linux.png
categories:
  - Linux
  - Signals
  - Process Management
tags:
  - Linux
  - Signals
  - Process management
  - Kill command
  - Interrupts
  - SIGTERM
  - SIGKILL
draft: false
weight: 323
categories_ref:
  - Linux
  - Signals
  - Process Management
slug_calculated: https://brianbraatz.github.io/p/linux-signals-in-a-nutshell
lastmod: 2025-03-14T16:40:16.060Z
---
# Linux Signals in a Nutshell

Ah, Linux signals! They are like the universe's way of telling processes, "Hey, wake up! Something important is happening!" Or in some cases, "Hey, die already!"

If you've ever been mystified by terms like `SIGKILL`, `SIGTERM`, or `SIGHUP`, then buckle up. This is your crash course in Linux signals—explained in a way that even your pet cat could understand (assuming your cat has an interest in process management).

## What Are Signals?

Signals are software interrupts sent to processes. Think of them like text messages for processes, except instead of "Hey, what's up?" it's more like "Terminate immediately!" or "Pause for a second!"

The kernel, users, or even the process itself can send signals. Each signal has a predefined number, but since humans prefer words over numbers, they also have names like `SIGTERM` (15) and `SIGKILL` (9).

## The Most Famous Signals (A.K.A The Usual Suspects)

### `SIGTERM` (15): The Polite Goodbye

This is the equivalent of saying, "Hey buddy, time to wrap things up and leave." It asks a process to terminate nicely, allowing it to clean up resources before exiting. Most well-behaved processes will comply.

### `SIGKILL` (9): The Merciless Executioner

This is the nuclear option. If `SIGTERM` is a polite tap on the shoulder, `SIGKILL` is a sledgehammer to the face. The process doesn't get a chance to clean up—it’s just gone. Use this when a process is stubbornly ignoring `SIGTERM`.

### `SIGHUP` (1): The "Hey, Wake Up!"

Originally meant for when a user logs out (or the terminal disconnects), but it's also used to tell daemons to reload their configurations. Many services like Apache or Nginx respect this signal.

### `SIGINT` (2): The Ctrl+C Effect

Ever pressed `Ctrl+C` in a terminal to stop a running process? That’s `SIGINT` in action. It tells the process, "Hey, I’m done with you!" and politely asks it to exit.

### `SIGSTOP` (19) & `SIGCONT` (18): The Pause and Play Buttons

* `SIGSTOP` freezes a process, like pausing a video.
* `SIGCONT` resumes it, like hitting play.

These are great for debugging or for when you want to stop and restart a process without killing it.

## How to Send Signals

### Using `kill`

The `kill` command is your go-to for sending signals. Despite its name, it can send any signal—not just `SIGKILL`.

```sh
kill -SIGTERM <pid>
```

Or, using numbers:

```sh
kill -15 <pid>
```

For the sledgehammer approach:

```sh
kill -9 <pid>
```

### Using `killall`

Want to send a signal to multiple processes by name? `killall` is your friend.

```sh
killall -SIGTERM firefox
```

### Using `pkill`

`pkill` is like `killall` but supports pattern matching. Want to nuke all Chrome instances?

```sh
pkill -9 chrome
```

### Using `trap`

Want to handle signals in a script? Use `trap`.

```sh
trap "echo Oh no, I got a SIGTERM!" SIGTERM
```

This lets your script react to signals instead of just dying instantly.

## Why Should You Care About Signals?

* **Graceful Shutdowns:** Servers and apps should clean up before exiting, preventing data corruption.
* **Debugging:** `SIGSTOP` and `SIGCONT` help when debugging processes.
* **Automating Tasks:** Signals help control processes in scripts.

<!-- ## The Final Signal (Conclusion)

Linux signals are like little messages flying around in your system, keeping everything in check. They might seem intimidating at first, but once you get the hang of them, they’re incredibly powerful.

Now go forth and signal responsibly. And remember—always try `SIGTERM` before resorting to `SIGKILL` (unless you really, really hate that process).

--- -->

## Key Ideas

| Concept          | Explanation                                  |
| ---------------- | -------------------------------------------- |
| **Signals**      | Software interrupts sent to processes        |
| **SIGTERM (15)** | Politely asks a process to terminate         |
| **SIGKILL (9)**  | Forcefully kills a process                   |
| **SIGHUP (1)**   | Reloads configuration or detects logout      |
| **SIGINT (2)**   | Triggered by `Ctrl+C` to interrupt a process |
| **SIGSTOP (19)** | Pauses a process                             |
| **SIGCONT (18)** | Resumes a paused process                     |
| **kill command** | Sends signals to processes                   |
| **trap command** | Handles signals in scripts                   |

***

## References

1. [Linux Man Pages - Signal](https://man7.org/linux/man-pages/man7/signal.7.html)
2. [Kill Command in Linux](https://www.cyberciti.biz/faq/linux-kill-process/)
3. [SIGHUP Explained](https://www.baeldung.com/linux/sighup-signal)
