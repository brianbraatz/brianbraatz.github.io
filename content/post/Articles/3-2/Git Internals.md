---
title: GIT Internals
description: How Git Works Under the Hood
slug: git-internals-explained
date: 2017-04-12
image: post/Articles/IMAGES/git.png
categories:
  - Git
  - Version Control
  - Algorithms
tags:
  - Git
  - Version Control
  - DAG
  - SHA-1
  - Intervals
draft: false
weight: 513
lastmod: 2025-03-02T23:46:47.632Z
---
<!-- 
# How Git Works Under the Hood: Intervals, Algorithms, and Code Examples -->

So, you use Git every day, pushing, pulling, and occasionally rage-quitting because of a merge conflict.

But do you actually know what’s going on under the hood?

<!-- Today, we’re going to break down Git’s inner workings in a way that actually makes sense. We’ll cover:

- How Git stores data (Hint: It’s **not** diffs!)
- The Directed Acyclic Graph (DAG) structure
- SHA-1 hashing (Git’s secret sauce)
- Intervals and how Git walks history efficiently
- How to inspect Git’s internals with code

Let’s get started! -->

***

## 1. Git’s Data Model: Snapshots, Not Diffs

Most people assume Git tracks **changes** (like traditional version control systems such as SVN or Mercurial), but nope.

Git is a **snapshot-based** system.

Each commit is a **complete snapshot** of your repository at that moment in time. The magic? If a file hasn’t changed, Git just reuses the same reference instead of storing a duplicate.

### Proof With `cat-file`

Run this in a Git repo:

```sh
git cat-file -p HEAD
```

You’ll see something like:

```
tree a1b2c3d4e5
parent 1234567890
author You <you@example.com> 1647890189 +0000
committer You <you@example.com> 1647890189 +0000

Added README
```

That `tree` hash (`a1b2c3d4e5`) represents the **snapshot** of the files at that commit.

***

## 2. Git’s Directed Acyclic Graph (DAG)

Git stores commits as a **Directed Acyclic Graph (DAG)**—a fancy way of saying commits point backward in time, forming a tree-like structure with no cycles.

Each commit has:

* A **tree** (which maps to actual file contents)
* A **parent** (or multiple parents for merges)
* Metadata (author, message, etc.)

Here’s a quick ASCII example of a Git history:

```
A ← B ← C ← D  (main branch)
        ↖
         E ← F (feature branch)
```

This structure makes history traversal super fast.

***

## 3. SHA-1 Hashing: Git’s Fingerprint System

Git identifies everything (commits, trees, blobs, etc.) using SHA-1 hashes. This ensures data integrity.

Let’s see how Git hashes things:

```sh
echo "Hello Git" | git hash-object --stdin
```

Output:

```
8ab686eafeb1f44702738c8b0f24f2567c36da6d
```

That’s a SHA-1 hash of the string **“Hello Git”**. Git does this for all files, commits, and trees.

***

## 4. Intervals and Git’s History Walks

### How `git log` Walks the Graph

When you run:

```sh
git log --graph --oneline --all
```

Git walks the commit graph using **intervals**—basically, it optimizes how it retrieves commits by skipping unnecessary paths.

Example:

If your commit history looks like:

```
A ← B ← C ← D (main)
        ↖
         E ← F (feature)
```

Git **doesn’t** scan every commit sequentially. Instead, it walks **both branches** in an interval-like pattern, minimizing redundant work.

***

## 5. Exploring Git Internals With Code

Want to see Git’s raw storage? Let’s play!

### 1. List All Git Objects

```sh
ls .git/objects
```

You’ll see folders with weird names (first 2 chars of SHA-1 hashes).

### 2. Inspect a Commit Object

Find a commit hash and run:

```sh
git cat-file -p <commit-hash>
```

It prints the commit details.

### 3. Check a Tree Object (File Snapshot)

Grab the `tree` hash from the commit and run:

```sh
git cat-file -p <tree-hash>
```

Now you see the folder structure!

### 4. Inspect a Blob (File Content)

Find a file’s blob hash and run:

```sh
git cat-file -p <blob-hash>
```

Boom! The file’s content appears.

***

## 6. Git’s Garbage Collection and Packfiles

Git compresses objects using **packfiles**. These bundle multiple objects into a single file for efficiency.

Run:

```sh
git gc
```

Git will repack objects, saving space.

To inspect packfiles:

```sh
ls .git/objects/pack
```

***

<!-- 
## Conclusion

Git is way cooler than just a version control tool—it’s a **highly optimized** content-addressable database with built-in cryptographic integrity.

Key takeaways:
- Git **stores snapshots**, not diffs.
- It organizes commits in a **DAG**.
- SHA-1 ensures data integrity.
- Git **walks history efficiently** using intervals.
- Packfiles optimize storage.

Want to go deeper? Try exploring your `.git` directory—you’ll gain a whole new appreciation for how Git really works!

---

## 🔑 Key Ideas

| Concept | Summary |
|---------|---------|
| **Snapshots, not diffs** | Git stores full snapshots of files at each commit, not changes. |
| **Directed Acyclic Graph (DAG)** | Commits form a tree-like structure with no cycles. |
| **SHA-1 Hashing** | Git fingerprints all objects to ensure integrity. |
| **Intervals in Git Log** | Git efficiently walks commit history by skipping redundant paths. |
| **Git Object Storage** | Everything (commits, trees, blobs) is stored as objects in `.git/objects`. |

---
``` -->

<!-- 

In the last article, we talked about how Git stores data, commits, and intervals for history traversal. Now, let’s go deeper and talk about **branching**—one of Git’s most powerful features.  

A Git branch isn’t what most people think it is. It’s not a separate copy of your files. It’s just a pointer.  

Today, we’ll break down:

- What a branch actually is (Spoiler: It’s a ref!)
- How `HEAD` works
- What happens under the hood when you switch branches
- How Git merges branches
- How Git deletes and recovers branches

Let’s dive in! -->

***

## 7. What Is a Git Branch? (It's Just a Pointer!)

Most people assume a branch is a separate folder or copy of files. Nope.

A **Git branch is just a file that contains a commit hash**. That’s it.

Let’s check it out!

```sh
cat .git/refs/heads/main
```

Example output:

```
9fceb02b3beecf73c4f0d7b24b3b9d09981fb17e
```

That’s just a commit hash!

When you create a branch, Git creates a new file under `.git/refs/heads/` with a different commit hash. This means branches are **cheap**—they just move a pointer.

***

## 8. How `HEAD` Works: The Active Branch

`HEAD` is a special reference that tells Git **which branch you’re currently on**.

Run:

```sh
cat .git/HEAD
```

Example output:

```
ref: refs/heads/main
```

This means `HEAD` is pointing to `main`. When you switch branches, Git **just updates this file**.

Try:

```sh
git checkout -b new-branch
cat .git/HEAD
```

Now it shows:

```
ref: refs/heads/new-branch
```

No files copied. No magic. Just a simple pointer change.

***

## 9. What Happens When You Switch Branches?

Let’s break it down:

1. **Git updates `HEAD`** to point to the new branch.
2. **Git updates the working directory** to match the new branch’s commit.
3. **Git unstages any conflicting changes** (if needed).

Try this:

```sh
git checkout -b test-branch
```

Now check:

```sh
cat .git/HEAD
```

Output:

```
ref: refs/heads/test-branch
```

Git also updates your working files to match the latest commit in `test-branch`.

***

## 10. How Merging Works Under the Hood

When you merge branches, Git looks for a **common ancestor** (usually the latest shared commit).

Example:

```
A ← B ← C (main)
     ↖
      D ← E (feature)
```

If you merge `feature` into `main`, Git finds commit **B** (the last shared commit), then combines changes from **C** and **E**.

### Merge Types

1. **Fast-forward merge**
   * If no new commits exist on `main`, Git just moves the branch pointer forward.
   * Example:

     ```sh
     git checkout main
     git merge feature
     ```

     This just updates `main` to point to `E`.

2. **Three-way merge**
   * If `main` has new commits, Git needs to create a merge commit.

     ```sh
     git merge feature
     ```

     Git creates a new commit combining changes.

***

## 11. How Git Deletes and Recovers Branches

### Deleting a Branch

A branch is just a file, so deleting it is easy:

```sh
git branch -d feature
```

This deletes `.git/refs/heads/feature`.

To force delete:

```sh
git branch -D feature
```

### Recovering a Deleted Branch

If you deleted a branch but need it back:

1. Find the last commit hash:

   ```sh
   git reflog
   ```

2. Restore the branch:

   ```sh
   git checkout -b feature <commit-hash>
   ```

Boom! The branch is back.

***

<!-- 
## Conclusion

Git’s branching system is powerful but simple at its core.  

Key takeaways:
- A **branch is just a pointer** to a commit.
- `HEAD` tells Git which branch is active.
- Switching branches just updates `HEAD` and restores files.
- Merging combines changes by finding a **common ancestor**.
- Deleting a branch removes a pointer but not the commits.
- You can recover deleted branches using `reflog`.

Now that you understand Git branching at the internal level, go try it out yourself!

---

## 🔑 Key Ideas

| Concept | Summary |
|---------|---------|
| **Branching in Git** | A branch is just a pointer to a commit, not a copy of files. |
| **HEAD Reference** | `HEAD` points to the active branch, and changing branches updates it. |
| **Fast-Forward Merge** | Moves the branch pointer if no diverging commits exist. |
| **Three-Way Merge** | Finds a common ancestor and combines changes when branches diverge. |
| **Recovering Deleted Branches** | Use `git reflog` to find lost commits and restore branches. |

---
``` 

-->

<!-- # Git Internals: More Advanced Concepts Explained

So far, we’ve covered Git’s storage model, branching, commits, and merges. But Git has even more tricks under the hood that make it incredibly powerful.

In this article, we’ll cover:

- How rebasing works internally
- What `reflog` does (and why it’s your Git safety net)
- How Git’s stash system works
- The magic of Git’s object compression (packfiles)
- How Git garbage collection cleans up unnecessary objects

Let’s jump in! -->

***

## 12. How Git Rebase Works Internally

Rebasing is one of Git’s most misunderstood features. Instead of merging branches, it **rewrites history** by moving commits.

Example scenario:

```
A ← B ← C (main)
      ↖
       D ← E (feature)
```

If we run:

```sh
git checkout feature
git rebase main
```

Git does this internally:

1. Finds the **common ancestor** (B).
2. Moves `feature` commits (`D` and `E`) onto `main`, replaying them **one by one**.
3. Updates `feature` to point to the new commit history.

The result:

```
A ← B ← C ← D' ← E' (feature)
```

Rebasing **rewrites commit hashes**, creating new commits `D'` and `E'`. This is why **you shouldn’t rebase shared branches**—it changes history!

***

## 13. The Git Reflog: Your Undo Button

Git **never actually loses commits**—even deleted ones. Every action is logged in the **reflog** (`git reflog`).

Run:

```sh
git reflog
```

Example output:

```
9fceb02 HEAD@{0}: commit: Added README
2d4f7b6 HEAD@{1}: checkout: moving from feature to main
```

This shows recent actions, like branch checkouts and commits.

If you accidentally delete a branch or reset a commit, use:

```sh
git reset --hard HEAD@{1}
```

Or restore a deleted branch:

```sh
git checkout -b feature 2d4f7b6
```

Git is **hard to break**, thanks to the reflog.

***

## 14. How Git Stash Works Internally

When you run:

```sh
git stash
```

Git **doesn’t create a branch**. Instead, it:

1. Saves your **uncommitted changes** as a **stash object**.
2. Moves `HEAD` back to a clean working directory.

Stashes are stored in:

```sh
ls .git/refs/stash
```

To view them:

```sh
git stash list
```

To restore:

```sh
git stash apply
```

Each stash is a **commit**, so you can even inspect them:

```sh
git stash show -p
```

***

## 15. Packfiles: How Git Optimizes Storage

If you check `.git/objects`, you’ll see **lots of small files**.

Over time, Git **packs** these into a single compressed file called a **packfile**.

Check packfiles:

```sh
ls .git/objects/pack
```

To manually optimize storage:

```sh
git gc
```

Git then:

* Compresses objects into fewer files.
* Eliminates duplicate objects.
* Reduces repository size.

This is why Git repos stay efficient, even with thousands of commits.

***

## 16. Git Garbage Collection: Cleaning Up Unused Objects

Git automatically **removes orphaned objects** (like old commits no longer referenced by any branch).

To see loose objects:

```sh
git fsck --unreachable
```

Run garbage collection manually:

```sh
git gc --prune=now
```

This removes:

* Orphaned commits
* Old packfiles
* Unreferenced blobs

If you accidentally delete a commit **before garbage collection runs**, you can still find it with `git reflog`.

***

<!-- ## Conclusion

Git is incredibly efficient at managing history, even when you rebase, stash, or delete things.  

Key takeaways:
- Rebasing **rewrites history** by replaying commits.
- `reflog` **tracks everything**—you can recover lost commits.
- Stashing saves uncommitted changes as **temporary commits**.
- Packfiles **optimize Git storage** by compressing objects.
- Git’s **garbage collector** removes old, unreferenced objects.

Want to go deeper? Try exploring `.git/` on your own repo!

---

## 🔑 Key Ideas

| Concept | Summary |
|---------|---------|
| **Rebase Internals** | Moves commits onto another branch, rewriting history. |
| **Reflog** | Tracks all changes to `HEAD`, allowing you to recover lost commits. |
| **Git Stash** | Temporarily saves uncommitted changes without creating a commit. |
| **Packfiles** | Git compresses multiple objects into single files for efficiency. |
| **Garbage Collection** | Removes old, unreferenced commits and objects to free space. |

---
``` -->

<!-- Here’s another follow-up article covering even more Git internals, starting at section 17.

---

```markdown
---
title: "Git Internals: Even More Hidden Magic Explained"
description: "A deep dive into Git's lesser-known internals like bisect, worktrees, bare repositories, and submodules. Learn how Git works behind the scenes."
slug: "git-hidden-internals"
date: 2019-11-10
image: "post/Articles/IMAGES/42.jpg"
categories: ["Git", "Version Control", "Internals"]
tags: ["Git", "Bisect", "Worktree", "Bare Repository", "Submodules", "Hooks"]
draft: false
weight: 512
--- -->

<!-- 
# Git Internals: Even More Hidden Magic Explained

At this point, you’ve got a solid understanding of Git’s internals—snapshots, DAGs, rebasing, reflogs, and packfiles.  

But Git has even more hidden features under the hood that can **level up your workflow**.  

In this article, we’ll cover:

- How `git bisect` finds bad commits automatically
- What worktrees are (and how they let you have multiple checkouts)
- How bare repositories work (and why they’re used in remote servers)
- How Git submodules work internally
- How Git hooks automate actions

Let’s dive in! -->

***

## 17. Git Bisect: Debugging Like a Time Traveler

Ever had a bug that **wasn’t there yesterday**? Instead of manually checking old commits, Git can **automatically** find the exact commit where the bug was introduced.

### How It Works

`git bisect` performs a **binary search** on your commit history.

1. Start bisect mode:

   ```sh
   git bisect start
   ```

2. Mark a **good commit**:

   ```sh
   git bisect good <commit-hash>
   ```

3. Mark a **bad commit**:

   ```sh
   git bisect bad HEAD
   ```

4. Git will now **checkout the midpoint commit** and ask you to test.

   * If the commit is **good**, run:
     ```sh
     git bisect good
     ```
   * If the commit is **bad**, run:
     ```sh
     git bisect bad
     ```

5. Git repeats this process until it finds the first bad commit.

6. When finished, reset:

   ```sh
   git bisect reset
   ```

This automates debugging by letting Git find **exactly where the problem started**.

***

## 18. Worktrees: Multiple Checkouts at Once

Ever wanted to **work on two branches at the same time** without stashing or committing? That’s what `git worktree` does.

### How Worktrees Work

A Git worktree is **another checkout** of the same repository but in a different folder.

#### Creating a Worktree:

```sh
git worktree add ../feature-branch feature
```

This creates a new folder `../feature-branch` where you can work on the `feature` branch **without switching** your main repo.

#### Listing Worktrees:

```sh
git worktree list
```

#### Removing a Worktree:

```sh
git worktree remove ../feature-branch
```

This is **super useful** for working on multiple branches without constantly switching.

***

## 19. Bare Repositories: What’s Inside Remote Git Repos?

When you run:

```sh
git clone git@github.com:user/repo.git
```

You’re cloning a **bare repository**.

### What’s a Bare Repository?

A **bare repo** has no working directory—just the Git data.

To create one:

```sh
git init --bare myrepo.git
```

A bare repo **only contains** the `.git` directory:

```
myrepo.git/
 ├── HEAD
 ├── refs/
 ├── objects/
 ├── hooks/
```

This is what GitHub, GitLab, and other remote services use to **store repos centrally**.

#### Why Use a Bare Repo?

* **Collaboration**: Remote repositories need to accept pushes, but a regular Git repo **can’t push to itself**.
* **Centralized Storage**: CI/CD systems often use bare repositories for automation.

***

## 20. Git Submodules: Repositories Inside Repositories

Sometimes, you need to include **another Git repo inside your own** (e.g., a shared library). That’s where **submodules** come in.

### Adding a Submodule

```sh
git submodule add https://github.com/some/library.git libs/library
```

This creates:

```
libs/library/   # A separate Git repo
.gitmodules     # Tracks submodule settings
```

### Cloning a Repo With Submodules

By default, submodules aren’t cloned. To fix this:

```sh
git clone --recurse-submodules <repo-url>
```

Or if you forgot:

```sh
git submodule update --init --recursive
```

#### How Submodules Work Internally

Submodules **aren’t stored as normal files**. Instead, Git stores a special **commit reference**:

```sh
cat .gitmodules
```

This tells Git which commit of the submodule to check out.

***

## 21. Git Hooks: Automate Everything

Git has **built-in automation** via **hooks**—scripts that run before/after Git commands.

### Where Hooks Live

Hooks are stored in `.git/hooks/`:

```sh
ls .git/hooks
```

### Common Hooks:

| Hook         | Runs When?                       |
| ------------ | -------------------------------- |
| `pre-commit` | Before a commit is created       |
| `pre-push`   | Before a `git push`              |
| `commit-msg` | When a commit message is entered |
| `post-merge` | After a successful merge         |

### Example: Preventing Bad Commit Messages

Create `.git/hooks/commit-msg`:

```sh
#!/bin/sh
if ! grep -qE "^(feat|fix|docs|chore):" "$1"; then
  echo "Commit message must start with feat:, fix:, docs:, or chore:"
  exit 1
fi
```

Make it executable:

```sh
chmod +x .git/hooks/commit-msg
```

Now Git **rejects** bad commit messages!

***

<!-- 
## Conclusion

Git has a **ton** of hidden features that make debugging, collaboration, and automation much easier.

Key takeaways:
- `git bisect` finds bad commits via **binary search**.
- `git worktree` lets you have **multiple checkouts** at once.
- Bare repositories are **remote-only Git storage** without working directories.
- `git submodule` lets you embed one repo inside another.
- Git hooks **automate actions** before/after commits, pushes, and merges.

Now that you know these internals, go experiment and level up your Git skills!

---

## 🔑 Key Ideas

| Concept | Summary |
|---------|---------|
| **Git Bisect** | Automates finding the first commit where a bug was introduced. |
| **Worktrees** | Allows multiple branches to be checked out at once. |
| **Bare Repositories** | Used for remote repositories, without a working directory. |
| **Git Submodules** | Embeds one Git repository inside another. |
| **Git Hooks** | Runs scripts before/after Git commands for automation. |

---
``` -->

***

## 22. What Is a Git Patch?

A **Git patch** is a text-based representation of a commit (or multiple commits). Instead of pushing/pulling, you can **export** a change as a `.patch` file and apply it elsewhere.

Think of it like a **portable commit**—you can send it via email, copy it to another machine, or even manually review it.

Example workflow:

1. **Generate a patch file** (`git format-patch`).
2. **Send it to someone** (email, Slack, etc.).
3. **Apply it on another repository** (`git apply`).

This is how **Linux kernel development** and many open-source projects handle contributions!

***

## 23. Creating a Git Patch

To generate a patch for the last commit:

```sh
git format-patch -1
```

This creates a `.patch` file like:

```
0001-Added-feature-X.patch
```

To generate a patch for multiple commits:

```sh
git format-patch HEAD~3
```

This creates **one `.patch` file per commit**.

To create a **single patch for all changes**:

```sh
git diff > my_changes.patch
```

This is useful when you haven’t committed changes yet.

***

## 24. Structure of a Git Patch File

A `.patch` file is just plain text! Let’s break down an example:

```
From 4f5a4e7c1342b5a1b6c6ff3a0b2f1d4a52a3d5b8 Mon Sep 17 00:00:00 2001
From: John Doe <johndoe@example.com>
Date: Mon, 1 Mar 2025 14:30:00 -0700
Subject: [PATCH] Fix typo in README

---
 README.md | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)

diff --git a/README.md b/README.md
index 3b18b12..6f7e8f1 100644
--- a/README.md
+++ b/README.md
@@ -1,4 +1,4 @@
-Hello Git Usres!
+Hello Git Users!
```

### Breakdown:

1. **Metadata (Commit Info)**
   * `From:` → Commit hash
   * `From:` → Author
   * `Date:` → Timestamp
   * `Subject:` → Commit message

2. **File Change Summary**
   * Shows the number of **insertions/deletions**.

3. **Unified Diff Format (`diff --git`)**
   * `index` → Shows blob hashes before/after the change.
   * `---` and `+++` → Indicates file modifications.
   * `@@` → Shows line numbers where changes occurred.

***

## 25. Applying a Git Patch

To apply a patch:

```sh
git apply 0001-Added-feature-X.patch
```

This **applies the change but does not commit it**.

To apply and commit:

```sh
git am 0001-Added-feature-X.patch
```

The `am` (apply mailbox) command **preserves the original commit message and author**.

***

## 26. How Git Stores and Processes Patches

Internally, a patch file is **just a diff**. When you run:

```sh
git diff > changes.patch
```

Git runs the **diff algorithm** to compute differences between the latest commit and your working directory.

When applying a patch, Git:

1. Reads the `diff` data.
2. Finds the target file(s).
3. Applies changes line by line.
4. Checks for conflicts (if needed).

### What Happens If a Patch Fails?

If a patch **doesn’t match** the current state, you get:

```
error: patch failed: README.md:1
error: README.md: patch does not apply
```

This means the file **changed since the patch was created**. You’ll need to **manually fix conflicts** before retrying.

***

## 27. Git Patches vs Cherry-Picking

Another way to move changes between branches is `git cherry-pick`:

```sh
git cherry-pick <commit-hash>
```

This applies **a commit** from one branch to another.

| Feature                         | Git Patch | Cherry-Pick |
| ------------------------------- | --------- | ----------- |
| **Requires commit history?**    | ❌ No      | ✅ Yes       |
| **Can be shared via email?**    | ✅ Yes     | ❌ No        |
| **Preserves author info?**      | ✅ Yes     | ✅ Yes       |
| **Can apply multiple commits?** | ✅ Yes     | ✅ Yes       |

Patches are **more flexible** because they work even if the repo history is different.

***

## 28. Interactive Patch Editing

Want to apply only part of a patch? Use `--reject`:

```sh
git apply --reject my_changes.patch
```

This applies **as much as possible** and creates `.rej` files for conflicts.

To manually inspect:

```sh
git apply --check my_changes.patch
```

This **dry-runs** the patch without applying it.

***

<!-- 
## Conclusion

Git patches are a **powerful, portable way to share changes** without worrying about branch history.  

Key takeaways:
- A Git patch is a **plain-text diff** that can be applied anywhere.
- `git format-patch` creates patch files from commits.
- `git apply` and `git am` apply patches with/without committing.
- Patches **work even across different repositories**.
- They’re used in **email-based workflows** like Linux kernel development.

If you’ve never used Git patches before, try creating and applying one—it’s a great tool to have in your Git toolkit!

---

## 🔑 Key Ideas

| Concept | Summary |
|---------|---------|
| **Git Patch Basics** | A `.patch` file contains commit changes in plain-text format. |
| **Creating a Patch** | Use `git format-patch` for commits, or `git diff > file.patch` for unstaged changes. |
| **Applying a Patch** | `git apply` (without committing) or `git am` (with commit metadata). |
| **Internal Storage** | Git computes patches using the `diff` algorithm. |
| **Patches vs Cherry-Pick** | Patches work across repositories, while `cherry-pick` requires a shared history. |

---
```

Let me know if you want to explore more Git internals! -->

Here's a deep dive into **Git Cherry-Picking**, starting at section 29.

***

````markdown
---
title: "Git Internals: How Cherry-Picking Works Under the Hood"
description: "A detailed look at how Git cherry-picking works, its internals, and when to use it effectively."
slug: "git-cherry-pick-internals"
date: 2017-10-22
image: "post/Articles/IMAGES/37.jpg"
categories: ["Git", "Version Control", "Internals"]
tags: ["Git", "Cherry-Pick", "Commits", "Merge", "Conflict Resolution"]
draft: false
weight: 428
---

# Git Internals: How Cherry-Picking Works Under the Hood

Sometimes, you need to **pick just one commit** from another branch without merging the entire branch. That’s where **Git cherry-picking** comes in.

Cherry-picking lets you **selectively apply commits** from anywhere in your repo’s history, without merging unwanted changes.

In this article, we’ll cover:
- What cherry-picking is and why it’s useful
- How cherry-picking works internally
- How to cherry-pick multiple commits
- Handling conflicts during cherry-picking
- Cherry-picking vs merging vs rebasing

Let’s get started!

---

## 29. What Is Git Cherry-Picking?

Git cherry-picking **applies an individual commit** from another branch to your current branch.

### Example Use Case:
You’re working on `main`, but a bugfix was added to `feature-branch`. You don’t want the **entire branch**, just the bugfix.

Instead of merging, you can **cherry-pick** the fix:

```sh
git cherry-pick <commit-hash>
````

This creates a **new commit** on your branch with the same changes as `<commit-hash>`, but **without merging the whole branch**.

***

## 30. How Cherry-Picking Works Internally

When you cherry-pick a commit, Git:

1. **Finds the commit** you specified.
2. **Applies the changes** from that commit onto your current branch.
3. **Creates a new commit** with the same changes but a **new hash**.

### Under the Hood:

A cherry-pick is equivalent to:

1. Running `git diff <commit-hash>^ <commit-hash>` to get the changes.
2. Applying those changes to the working directory.
3. Creating a new commit with those changes.

**Example:**

```
A ← B ← C ← D (main)
      ↖
       E ← F ← G (feature)
```

If you run:

```sh
git cherry-pick F
```

The result:

```
A ← B ← C ← D ← F' (main)
      ↖
       E ← F ← G (feature)
```

Even though `F` already exists in `feature`, a **new commit `F'`** is created on `main`.

***

## 31. Cherry-Picking Multiple Commits

You can cherry-pick multiple commits at once:

```sh
git cherry-pick <commit1> <commit2>
```

Or a range of commits:

```sh
git cherry-pick <start-commit>^..<end-commit>
```

For example:

```sh
git cherry-pick C^..E
```

This picks **C, D, and E**.

***

## 32. What Happens If a Cherry-Pick Fails?

If Git can’t apply a commit cleanly, it results in a **conflict**:

```
error: could not apply <commit-hash>
```

Git will stop cherry-picking and let you **resolve conflicts manually**.

### Fixing a Cherry-Pick Conflict

1. Open conflicted files and fix them.

2. Mark them as resolved:

   ```sh
   git add <fixed-file>
   ```

3. Continue the cherry-pick:

   ```sh
   git cherry-pick --continue
   ```

If you want to cancel:

```sh
git cherry-pick --abort
```

***

## 33. Cherry-Picking vs Merging vs Rebasing

Cherry-picking **isn’t the only way** to move commits between branches. Here’s how it compares to merging and rebasing:

| Feature                         | Cherry-Picking   | Merging          | Rebasing                 |
| ------------------------------- | ---------------- | ---------------- | ------------------------ |
| **Selects specific commits?**   | ✅ Yes            | ❌ No             | ❌ No                     |
| **Creates new commit hashes?**  | ✅ Yes            | ❌ No             | ✅ Yes                    |
| **Maintains original history?** | ❌ No             | ✅ Yes            | ❌ No                     |
| **Can be undone easily?**       | ✅ Yes (`revert`) | ✅ Yes (`revert`) | ⚠️ No (rewrites history) |

### When to Use Cherry-Picking:

✅ When you need **only one commit** from another branch.\
✅ When you **don’t want to merge** an entire branch.\
✅ When applying a **hotfix** from one branch to another.

***

## 34. Automating Cherry-Picking With `-x`

By default, cherry-picked commits **don’t track where they came from**. To include a reference:

```sh
git cherry-pick -x <commit-hash>
```

This adds a reference like:

```
(cherry picked from commit 4a5b3c)
```

Now it’s clear that the commit came from elsewhere!

***

## 35. Undoing a Cherry-Pick

If you made a mistake, you can undo a cherry-pick **before committing**:

```sh
git cherry-pick --abort
```

If you already committed the cherry-pick:

```sh
git revert <commit-hash>
```

This creates a **reverse commit** that undoes the cherry-picked changes.

***

<!-- 
## Conclusion

Git cherry-picking is a powerful way to **move specific changes** without merging an entire branch.

Key takeaways:
- Cherry-picking **copies** a commit’s changes into a new commit on your branch.
- It’s useful for **hotfixes** and **selectively applying changes**.
- Conflicts can happen if the commit **doesn’t apply cleanly**.
- You can cherry-pick **multiple commits at once**.
- Use `-x` to **track cherry-picked commits**.

Now go try it out and start cherry-picking like a Git pro!

---

## 🔑 Key Ideas

| Concept | Summary |
|---------|---------|
| **Cherry-Picking** | Selectively applies commits without merging a branch. |
| **Multiple Commits** | Can cherry-pick a range of commits at once. |
| **Conflict Resolution** | If conflicts occur, you must manually fix them. |
| **Undoing Cherry-Picks** | Use `--abort` before committing or `revert` after. |
| **Tracking Cherry-Picks** | `-x` adds `(cherry picked from commit XYZ)`. |

---
``` -->
