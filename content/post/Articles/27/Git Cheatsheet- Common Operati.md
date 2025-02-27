---
title: Git Cheatsheet- Common Operations
description: Common GIT Operations
slug: git-cheatsheet-common-ope
date: 2017-08-14
image: post/Articles/IMAGES/37.jpg
categories:
  - Git
  - Version Control
  - Cheatsheet
tags:
  - Git
  - Version
  - Control
  - Cheatsheet
draft: "False"
weight: "423"
lastmod: 2025-02-27T14:33:38.846Z
---
***

## 📌 Setting Up Git (For the Newbies)

Before diving into the fun stuff, let’s configure Git:

```sh
git config --global user.name "Your Name"
git config --global user.email "you@example.com"
```

You can check your setup with:

```sh
git config --list
```

Boom!

Git now knows who you are.

No more anonymous commits from "Some Guy on the Internet."

***

## 🎬 Starting a New Project (Or Cloning an Existing One)

**Initialize a new repo:**

```sh
git init
```

**Clone an existing repo:**

```sh
git clone https://github.com/user/repo.git
```

Now you have a Git-powered project.

It’s like giving your project superpowers (but with great power comes great responsibility).

***

## 📂 Tracking Changes

### Check the status of your repo:

```sh
git status
```

This tells you what’s changed, what’s staged, and whether you accidentally committed debug print statements.

### Add files to staging:

```sh
git add file.txt  # Add a specific file
git add .         # Add all changes
```

### Commit your changes:

```sh
git commit -m "Add a meaningful commit message"
```

Make sure your commit messages don’t look like:

```sh
git commit -m "Fixed stuff"
git commit -m "Final fix"
git commit -m "FINAL final fix"
```

Future you (and your team) will thank you.

***

## 🔄 Working with Branches

### Create a new branch:

```sh
git branch feature-branch
```

### Switch to another branch:

```sh
git checkout feature-branch
```

Or, since Git 2.23, use:

```sh
git switch feature-branch
```

### Create and switch in one go:

```sh
git checkout -b new-branch
```

### List all branches:

```sh
git branch
```

### Delete a branch (once you're done with it):

```sh
git branch -d old-branch
```

Use `-D` (uppercase) if Git refuses to delete it because it's not merged yet.

***

## 📤 Pushing and Pulling

### Push your code to remote:

```sh
git push origin branch-name
```

### Pull latest changes from remote:

```sh
git pull origin branch-name
```

***

## 🔄 Merging and Rebasing

### Merge branches:

```sh
git checkout main
git merge feature-branch
```

### Rebase (for a cleaner commit history):

```sh
git checkout feature-branch
git rebase main
```

### Abort a bad rebase:

```sh
git rebase --abort
```

Think of `merge` as adding a new chapter, while `rebase` rewrites history.

Use wisely.

***

## 🏠 Reset, Revert, and Stash (a.k.a. "Oops, Undo!")

### Undo the last commit (without losing changes):

```sh
git reset --soft HEAD~1
```

### Undo the last commit (and erase changes):

```sh
git reset --hard HEAD~1
```

### Revert a commit (create a new commit that undoes it):

```sh
git revert <commit-hash>
```

### Stash changes (hide them temporarily):

```sh
git stash
```

### Apply stashed changes:

```sh
git stash pop
```

***

## 🕵️‍♂️ Finding Stuff

### View commit history:

```sh
git log --oneline --graph --decorate --all
```

### See who changed a specific file:

```sh
git blame file.txt
```

### Search in commits:

```sh
git grep "search-term"
```

***

## 💥 Dealing with Conflicts

### When Git screams about merge conflicts:

1. Open the conflicting file.
2. Manually fix the differences (keep what you want, delete what you don’t).
3. Stage the file: `git add file.txt`
4. Commit the changes: `git commit -m "Fix merge conflict"`

Merge conflicts are like unexpected plot twists in a movie.

Sometimes painful, but necessary.

***

## 🏁 Wrapping Up

Git is an essential tool, and mastering it will save you a ton of headaches.

This cheatsheet covers the basics, but Git is deep, so don’t stop learning!

If all else fails, remember the golden rule:

```sh
git reset --hard origin/main
```

Use it with caution.

It’s the "Ctrl+Z" of Git, but it erases your local changes.

Happy coding! 🚀

***

## 🔑 Key Ideas

| Key Concept     | Summary                    |
| --------------- | -------------------------- |
| `git init`      | Start a new Git repo       |
| `git clone`     | Clone an existing repo     |
| `git add`       | Stage changes              |
| `git commit`    | Save staged changes        |
| `git branch`    | Manage branches            |
| `git merge`     | Merge branches             |
| `git rebase`    | Rebase for a clean history |
| `git reset`     | Undo changes               |
| `git stash`     | Temporarily store changes  |
| `git log`       | View commit history        |
| `git push/pull` | Sync with remote           |

***

## 🔗 References

* [Official Git Documentation](https://git-scm.com/doc)
* [GitHub Git Cheat Sheet](https://education.github.com/git-cheat-sheet-education.pdf)
* [Pro Git Book](https://git-scm.com/book/en/v2)
