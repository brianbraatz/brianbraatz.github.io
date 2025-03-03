---
title: Git Submodules vs. Git Subtrees
description: comparison between Git Submodules and Git Subtrees
slug: git-submodules-vs-subtrees
date: 2017-12-15
image: post/Articles/IMAGES/git.png
categories:
  - GIT
  - DevOps
tags:
  - Git
  - Submodules
  - Subtrees
  - Version
  - Control
draft: false
weight: 30
lastmod: 2025-03-02T23:46:54.822Z
---
# Git Submodules vs. Git Subtrees

## Git Submodules: The Lone Rangers

**What Are They?**

Git Submodules are like those mysterious side characters in movies who have their own spin-offs.

They're repositories nested within other repositories, maintaining their own histories and identities.\
It's like having a mini-me inside your main project. ([geeksforgeeks.org](https://www.geeksforgeeks.org/git-subtree-vs-git-submodule/))

**Pros:**

* **Independent Versioning:** Each submodule can point to a specific commit, allowing for controlled updates.
* **Separation of Concerns:** Keeps codebases modular, simplifying dependency management.
* **Consistent Environment:** Developers can easily obtain the state of the project as intended by the original contributor.

**Cons:**

* **Complex Workflow:** Requires separate commands for initialization and updates.
* **Detached HEAD Issues:** Can lead to confusion if not managed properly.
* **Contributing Back Changes:** More steps involved compared to direct pushes.

**When to Use:**

* When you need to keep the embedded repository’s commit history separate from the main repository.
* When you need to reference different versions of the embedded repository.
* When you don’t need to change the embedded repository frequently. ([dev.devbf.com](https://dev.devbf.com/posts/git-submodule-vs-subtree-which-should-you-use-and-when-719ab/))

## Git Subtrees: The Team Players

**What Are They?**

Git Subtrees are like the friendly neighborhood superheroes who integrate seamlessly into your project. They allow you to nest a repository inside another as a subdirectory, merging histories directly into the parent repository. ([atlassian.com](https://www.atlassian.com/git/tutorials/git-subtree))

**Pros:**

* **Simpler Workflow:** Integrated into the main repository, no need for separate commands.
* **Unified History:** Combines commit histories, making it easier to track changes.
* **No Detached HEAD Issues:** Everything stays in sync.

**Cons:**

* **Larger Repository Size:** Includes the entire history of the subtree, which might increase the size.
* **Potential for Mixing Code:** Requires careful management to avoid mixing super and sub-project code in commits.
* **Learning Curve:** Requires learning a new merge strategy.

**When to Use:**

* When you want to share a set of files between different projects without maintaining a separate repository.
* When you need to update the embedded repository frequently.
* When you want a single, unified repository with a shared commit history. ([dev.devbf.com](https://dev.devbf.com/posts/git-submodule-vs-subtree-which-should-you-use-and-when-719ab/))

## Techniques for Git embedding another Git

### **Use a sparse checkout for the submodule**

You can configure a sparse checkout for the submodule to only check out the specific subdirectory you want.

Steps:

1. Add the submodule as usual:

   ```bash
   git submodule add <repository-url> <submodule-path>
   ```

2. Navigate to the submodule directory:

   ```bash
   cd <submodule-path>
   ```

3. Enable sparse checkout:

   ```bash
   git sparse-checkout init
   ```

4. Specify the subdirectory you want:

   ```bash
   git sparse-checkout set <subdirectory-path>
   ```

5. Update the submodule:

   ```bash
   git pull --depth=1
   ```

***

### **Use a custom branch or repository containing only the subdirectory**

If you have control over the repository being used as a submodule, you can create a separate branch or fork containing just the subdirectory. Then, add that repository as the submodule.

Steps:

6. Clone the repository containing the desired subdirectory:

   ```bash
   git clone <repository-url>
   ```

7. Remove all files except the desired subdirectory:

   ```bash
   cd <repository-name>
   git filter-repo --path <subdirectory-path> --force
   ```

8. Push the filtered repository to a new remote:

   ```bash
   git remote add origin <new-repo-url>
   git push -u origin main
   ```

9. Add this new repository as the submodule.

***

### **Use a script to pull and copy only the subdirectory**

If you don’t want the entire repository as a submodule, you can write a script to pull the repository, copy only the subdirectory, and use it in your project.

Steps:

10. Write a script to clone the repository and copy the specific directory:

    ```bash
    #!/bin/bash
    git clone --depth=1 <repository-url> temp-repo
    cp -r temp-repo/<subdirectory-path> ./desired-path
    rm -rf temp-repo
    ```

11. Execute this script when needed.

***

### **Which approach to choose?**

* If you want the submodule to stay connected to the upstream repository and be updated easily, **use sparse checkout**.
* If you want a clean repository without unrelated files, **filter the repository** or create a custom branch/repo.
* If the submodule doesn’t need to be tied to Git history, **use a script**.

## Key Ideas

| Feature                 | Git Submodules                                                                                                                                                                        | Git Subtrees                                                                                                                                                                          |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Integration**         | External repository as a subdirectory with separate history. ([geeksforgeeks.org](https://www.geeksforgeeks.org/git-subtree-vs-git-submodule/))                                       | External repository merged into the main repository with unified history. ([atlassian.com](https://www.atlassian.com/git/tutorials/git-subtree))                                      |
| **Versioning**          | Independent versioning; each submodule can point to a specific commit. ([geeksforgeeks.org](https://www.geeksforgeeks.org/git-subtree-vs-git-submodule/))                             | Unified versioning; all changes are part of the main repository. ([atlassian.com](https://www.atlassian.com/git/tutorials/git-subtree))                                               |
| **Workflow Complexity** | Requires separate commands for initialization and updates. ([geeksforgeeks.org](https://www.geeksforgeeks.org/git-subtree-vs-git-submodule/))                                         | Simpler workflow; integrated into the main repository. ([atlassian.com](https://www.atlassian.com/git/tutorials/git-subtree))                                                         |
| **Repository Size**     | Minimal impact; only references the submodule. ([gitprotect.io](https://gitprotect.io/blog/managing-git-projects-git-subtree-vs-submodule/))                                          | Potentially larger; includes the entire history of the subtree. ([gitprotect.io](https://gitprotect.io/blog/managing-git-projects-git-subtree-vs-submodule/))                         |
| **Use Cases**           | When you need to keep the embedded repository’s commit history separate. ([dev.devbf.com](https://dev.devbf.com/posts/git-submodule-vs-subtree-which-should-you-use-and-when-719ab/)) | When you want a single, unified repository with a shared commit history. ([dev.devbf.com](https://dev.devbf.com/posts/git-submodule-vs-subtree-which-should-you-use-and-when-719ab/)) |

## Related Links

* [Git Submodule vs Subtree: Which Is Right for Your Project?](https://www.graphapp.ai/blog/git-submodule-vs-subtree-which-is-right-for-your-project)
* [Git Submodule vs Subtree: Which Should You Use and When?](https://dev.devbf.com/posts/git-submodule-vs-subtree-which-should-you-use-and-when-719ab/)
* [Git Subtree: Alternative to Git Submodule | Atlassian Git Tutorial](https://www.atlassian.com/git/tutorials/git-subtree)
* [Managing Git Projects: Git Subtree vs. Submodule](https://gitprotect.io/blog/managing-git-projects-git-subtree-vs-submodule/)
* [Decoupling vs Simplicity: An In-Depth Comparison of Git Submodules vs Subtrees](https://linuxhaxor.net/code/difference-between-git-submodule-and-subtree.html)

Git doesn't support using only a specific subdirectory from a repository as a submodule directly. When you add a submodule, it clones the entire repository, not just a part of it. However, there are a few workarounds to achieve a similar result:

***
