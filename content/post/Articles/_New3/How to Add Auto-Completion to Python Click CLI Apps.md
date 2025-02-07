---
title: How to Add Auto-Completion to Python Click CLI Apps
description: Where would we be without autocompletion?
slug: auto-completion-click-cli
date: 2021-05-10
image: post/Articles/IMAGES/4.jpg
categories: []
tags:
  - Python
  - CLI
  - Click
  - Auto-Completion
  - Shell
  - Development
draft: false
weight: 480
lastmod: 2025-02-07T17:12:47.002Z
---
# How to Add Auto-Completion to Click CLI Apps

If you've ever typed a long CLI command and **immediately regretted your life choices**, this article is for you. Typing out full command names, remembering options, and scrolling through help menus is **so last century**.

**Wouldn't it be great if your CLI app could just complete commands for you?** Like magic? Well, good news: **Click supports auto-completion!**

In this guide, we’ll:

* **Enable auto-completion** in Click-powered CLI apps.
* **Set it up for Bash, Zsh, and Fish** (sorry Windows users, life is hard).
* **Make our CLI apps feel 10x smarter.**

***

## 🎯 Why Auto-Completion?

✅ Saves you from typing long commands (*because who has time for that?*)\
✅ Reduces typos (*no more `--frce` instead of `--force`*)\
✅ Makes your CLI look **polished and professional**

Click has **built-in** auto-completion support, but it needs **a little setup**. Let’s do it.

***

## 🛠 Step 1: Install Click Shell Completion

First, make sure you have **Click 8.0+**:

```sh
pip install --upgrade click
```

Now, let’s create a basic CLI app in `cli.py`:

```python
import click

@click.group()
def cli():
    """A smart CLI app with auto-completion."""
    pass

@cli.command()
def hello():
    """Say hello!"""
    click.echo("Hello, world!")

@cli.command()
def goodbye():
    """Say goodbye!"""
    click.echo("Goodbye, world!")

if __name__ == "__main__":
    cli()
```

Run `python cli.py --help`, and you’ll see a **basic CLI app with two commands**.

Now, let’s **add auto-completion**.

***

## 🔧 Step 2: Enable Auto-Completion

Click supports **shell completion**, but we need to set it up manually.

### **Generate Completion Scripts**

To enable completion, Click provides a built-in command:

```sh
python cli.py --show-completion
```

This will output a **shell completion script**. You need to **save this script** and source it in your shell.

### **For Bash Users** 🐧

Save the script to your bash completion directory:

```sh
python cli.py --show-completion bash > ~/.bash_completion
source ~/.bash_completion
```

Or add it to `.bashrc`:

```sh
echo 'source ~/.bash_completion' >> ~/.bashrc
```

### **For Zsh Users** 🦄

For Zsh, save the script to your completion functions:

```sh
mkdir -p ~/.zfunc
python cli.py --show-completion zsh > ~/.zfunc/_cli
echo 'fpath=(~/.zfunc $fpath)' >> ~/.zshrc
echo 'autoload -Uz compinit && compinit' >> ~/.zshrc
source ~/.zshrc
```

### **For Fish Users** 🐠

Fish shell users can do:

```sh
python cli.py --show-completion fish > ~/.config/fish/completions/cli.fish
```

Restart your shell, and **auto-completion should work!** 🎉

***

## 🎮 Step 3: Test It!

Try typing:

```sh
python cli.py <TAB>
```

You should see:

```
goodbye  hello
```

Now try:

```sh
python cli.py hello <TAB>
```

It should auto-complete the command! **You're now a CLI wizard.** 🧙‍♂️

***

## 🏆 Step 4: Making It Even Smarter

Want **better auto-completion**? Click lets you **suggest values** dynamically.

Let’s create a **command with auto-completing options**:

```python
@click.command()
@click.argument("color", type=click.Choice(["red", "green", "blue"], case_sensitive=False))
def choose_color(color):
    """Pick a color."""
    click.echo(f"You chose {color}!")

cli.add_command(choose_color)
```

Now, when you type:

```sh
python cli.py choose-color <TAB>
```

It will **only suggest "red", "green", or "blue"**! Smart, right?

***

## 🎉 Wrapping Up

Today, you learned how to **supercharge your Click CLI apps** with:\
✅ **Auto-completion** for commands and options\
✅ **Bash, Zsh, and Fish integration**\
✅ **Dynamic suggestions** for even better UX

Your CLI is now **faster, smarter, and less frustrating to use**. Congrats! 🎊

<!-- 
---

## 💡 Ideas for Future Articles

- **"How to Build a Full CLI App with Click and SQLite"**  
- **"Creating a Beautiful CLI Dashboard with Rich"**  
- **"Adding Logging and Debugging to Click CLI Apps"**  
-->

***

## 📚 References

1. [Click Official Docs](https://click.palletsprojects.com/)
2. [Bash Completion Guide](https://www.gnu.org/software/bash/manual/bash.html#Programmable-Completion)
3. [Zsh Auto-Completion](https://zsh.sourceforge.io/Doc/Release/Completion-System.html)
4. [Fish Shell Completions](https://fishshell.com/docs/current/completions.html)
