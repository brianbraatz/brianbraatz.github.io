---
title: The History of Revision Control- From SourceSafe to Git
description: 
slug: history-revision-control
date: 2017-09-14
image: post/Articles/IMAGES/monkwriting.png
categories:
  - Version Control
  - Programming History
  - Software Development
tags:
  - Version Control
  - Programming History
  - Software Development
  - Git
  - SVN
  - Mercurial
  - CVS
  - SourceSafe
draft: "False"
weight: "348"
lastmod: 2025-03-03T02:00:07.576Z
---
# A Brief, Semi-Sarcastic History of Revision Control: From SourceSafe to Git

Once upon a time in the land of software development, people had to manage their source code manually.

It was a dark and terrible age where developers copied files into folders named `backup_final`, `backup_really_final`, and `backup_final_v2_REALLY_FINAL_THIS_TIME`.

But fear not, for version control systems (VCS) were invented to save us from this madness.

Let’s take a wildly opinionated trip down memory lane and look at some of the major version control systems that brought us to where we are today.

***

## Microsoft Visual SourceSafe (VSS): The "Oops, We Lost Your Code" Era

[Visual SourceSafe](https://en.wikipedia.org/wiki/Microsoft_Visual_SourceSafe) (VSS) was Microsoft's attempt at version control.

Introduced in the mid-'90s, it was widely used by teams who didn't know any better.

VSS had a few, uh, *quirks*:

* It stored everything in a single database file.

Guess what happened when that file got corrupted? (Hint: Developers cried.)

* It was slow.

Very slow.

* Network latency could and would destroy your repository.
* Branching?

HAHAHAHA, no.

VSS was eventually abandoned, much like the dreams of developers who used it.

***

## CVS (Concurrent Versions System): The "Hey, At Least It’s Better Than SourceSafe" Era

[CVS](https://en.wikipedia.org/wiki/Concurrent_Versions_System) was one of the first serious open-source version control systems.

It gave teams a centralized way to manage their source code and introduced key concepts like commit histories and concurrent development.

CVS was a huge improvement over VSS, but it had problems:

* It was designed in the '80s.

Need I say more?

* No atomic commits—your commit could fail *midway* and leave your repo in a broken state.
* Renaming files?

Good luck.

* Branching was theoretically possible, but nobody wanted to touch it.

CVS was eventually replaced by...

***

## SVN (Subversion): "CVS, But Slightly Less Awful"

[Subversion (SVN)](https://subversion.apache.org/) was created to fix CVS’s flaws, and it mostly succeeded.

SVN’s greatest hits included:

* Atomic commits!

Finally, a commit would either succeed or fail, but not leave the repo in limbo.

* Proper directory and file renaming support.
* Better branching and merging (well, *better* than CVS, which is a low bar).

SVN was a solid centralized VCS and ruled the land for a while.

However, it still had a major flaw: it was centralized.

This meant if the SVN server went down, so did your entire development workflow.

***

## Mercurial: "The Hipster's Distributed Version Control System"

[Mercurial](https://www.mercurial-scm.org/) (Hg) was one of the first distributed version control systems (DVCS).

It allowed developers to work offline, clone entire repositories locally, and sync changes with remote servers when needed.

Mercurial was technically great, but it had one problem: **Git existed**.

Even though Mercurial was easier to use than Git and had better Windows support, Git's overwhelming popularity and ecosystem made sure Mercurial remained a niche choice.

Today, Mercurial is mostly used by holdouts who want to be different. (Looking at you, Mozilla.

Oh wait, even they moved to Git.)

***

## Git: The "Bow Down to Your Overlord" Era

[Git](https://git-scm.com/) is the king of version control.

Created by **Linus Torvalds** (yes, the Linux guy) in 2005, Git revolutionized how developers work.

Why is Git so powerful?

* It’s **distributed**, so everyone has a full copy of the repo.
* Branching and merging are actually usable!
* Insanely fast.
* It’s free and open-source.
* Every developer uses it, so good luck arguing for something else.

Git has become *the* standard for version control.

Platforms like [GitHub](https://github.com/), [GitLab](https://about.gitlab.com/), and [Bitbucket](https://bitbucket.org/) have only cemented its dominance.

If you’re not using Git, you better have a very, *very* good reason.

***

## Conclusion: We’ve Come a Long Way

From the horrors of SourceSafe to the reign of Git, version control has evolved tremendously.

Each system improved on the mistakes of its predecessor, leading to the robust tools we use today.

So next time you run `git push --force` and break production, take a moment to appreciate how far we've come.

***

<!-- 
## Key Ideas

| Concept                     | Summary |
|-----------------------------|---------|
| **SourceSafe (VSS)**        | Microsoft's broken excuse for version control.

RIP. |
| **CVS**                     | Centralized, first step towards proper VCS, but had no atomic commits. |
| **SVN**                     | Fixed CVS issues but was still centralized. |
| **Mercurial**               | Good, but Git won. |
| **Git**                     | The undisputed king of version control. |
-->

***

## References

1. [Microsoft Visual SourceSafe](https://en.wikipedia.org/wiki/Microsoft_Visual_SourceSafe)
2. [Concurrent Versions System (CVS)](https://en.wikipedia.org/wiki/Concurrent_Versions_System)
3. [Apache Subversion (SVN)](https://subversion.apache.org/)
4. [Mercurial](https://www.mercurial-scm.org/)
5. [Git](https://git-scm.com/)
6. [GitHub](https://github.com/)
7. [GitLab](https://about.gitlab.com/)
8. [Bitbucket](https://bitbucket.org/)

***
