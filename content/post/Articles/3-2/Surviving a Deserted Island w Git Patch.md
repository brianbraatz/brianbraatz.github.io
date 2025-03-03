---
title: Surviving a Deserted Island with Git Patch
description: Its always good to be prepared...
slug: git-patching-deserted-island
date: 2018-06-15
image: post/Articles/IMAGES/castawayboulderPatch.png
categories:
  - Git
  - Version Control
  - Offline Development
  - Linux
tags:
  - Git
  - Git
  - Patching
  - Offline
  - Development
  - USB
  - Code
  - Collaboration
draft: false
weight: 7
lastmod: 2025-03-03T03:35:06.741Z
---
(\
Really **GREAT** Movie:\
[Cast Away - Wikipedia](https://en.wikipedia.org/wiki/Cast_Away)\
)

## The Situation

Alright, you’ve got to go with my scenario here.

You’re stranded on a deserted island.

Don’t ask how or why—maybe you were on a cruise, maybe you tried to be a pirate but failed spectacularly. Whatever the case, here we are.

The good news?

You have a laptop and solar panels for charging.

The bad news?

No internet. No email. No cloud storage. Nothing.

However, food isn’t an issue because we have a near-infinite supply of coconuts.

Also, for some reason, a giant shipping container full of nothing but *Vines Liquorice* washed ashore.

Why? Because it’s my story, and I *love* Vines Liquorice.

For water, we collect rainwater, and I think i saw in a movie one time that you can drink coconut water...

Also--there JUST HAPPENS to be the weirdest thing—scattered all over the beach....

...., perfectly preserved,

....thousands of AOL USB drives!!!\
<[AOL - Wikipedia](https://en.wikipedia.org/wiki/AOL)>

Apparently (in our scenario :) ) , sometime around 2002, AOL switched from mailing out CDs to mailing out USB sticks for their install media.

But since everyone was on cable or DSL by then, nobody cared.

Now  all those usb sticks somehow ended up here, undamaged by time, salt, or sun.\
(amazing no?)

And they all still work.

That’s convenient because we can only communicate with our team by tossing these USB drives into bottles and chucking them into the ocean.

(oh and there are like.. alot of bottles here too.. umm.. maybe "Fiction Author" is not the right career move for me? .. on to the software problem... )

## The Software Problem

We still have code to write.

Our team is out there, back in the real world, working on our project.

And we are stuck on this island, with empty bottles, Vines Liquoricee, AOL USB drives.. and probably a nasty sunburn....

So how do we keep our software project going in this situaiton ?

We have the latest version of the code locally...

... we need to send our changes to project team—and vice versa...

Using **only** these AOL USB sticks and ocean currents.

The catch?\
(there is a catch? I think i am getting better at this fiction thing...)

We don’t know what order the bottles with USB sticks will arrive in—for us *or* for them.

This is a totally normal problem that people run into daily, right?

So onto the solution :

## Git Patching to the Rescue

Enter **Git patching**—a method to send changes as small, self-contained patches that can be applied to the copy of the code we have on our laptop.

How to submit code to our team?

Using Git patches, we can generate changes as portable files, copy them to a AOL USB stick, and send them off in a bottle.

When our team gets the bottle (whenever that happens), they can apply the patches in a way that ensures everything stays in sync.

Here’s how it works.

***

## Step 1: Creating a Git Patch

Let’s say we make a change to our project.

First, we commit our changes:

```sh
git add .
git commit -m "Fixed the coconut sorting algorithm"
```

Now, we generate a patch:

```sh
git format-patch -1 HEAD
```

This creates a `.patch` file with the latest commit’s changes.

If we want to send multiple commits, we can do:

```sh
git format-patch -3 HEAD
```

This generates patch files for the last three commits.

***

## Step 2: Copying the Patch to an AOL USB Stick

Next, we take the `.patch` file and copy it onto an AOL USB drive:

```sh
cp 0001-Fixed-the-coconut-sorting-algorithm.patch /media/AOL_USB/
```

Now we safely eject the USB drive:

```sh
umount /media/AOL_USB/
```

And then, we place it inside a bottle, seal it tight, and toss it into the ocean.

We also include a note written on coconut leaves that says, “Apply this patch when you get it, and hope this bottle arrives before the others.”

(can you write on coconut leaves? and what are we writing WITH????)

***

## Step 3: Receiving a Patch from the Team

Let’s say, a few days later, a bottle washes ashore.\
(only a few days? that might be a plot hole... )

Inside is an AOL USB stick with a patch file from our team.

We plug it in and copy the patch to our working directory:

```sh
cp /media/AOL_USB/0001-Updated-the-palm-tree-physics.patch .
```

Then, we apply it:

```sh
git apply 0001-Updated-the-palm-tree-physics.patch
```

Boom. Changes applied.

Now our code is up to date with whatever changes they made.

***

## Step 4: Handling Out-of-Order Bottles

Because ocean currents are unreliable (thanks a lot, physics), bottles don’t always arrive in order.

What if we get a patch that depends on another patch we haven’t received yet?

That’s where **Git am** comes in.

Instead of applying patches manually, we use:

```sh
git am 0001-Updated-the-palm-tree-physics.patch
```

This ensures that patches apply in the correct order and warn us if something is missing.

If we get an error like:

```
Patch does not apply
```

That means we’re missing a previous patch.

No problem!

We put the bottle *back into the ocean* with a note:

*"This patch depends on an earlier one. If you haven't sent it yet, please do!"*

***

## Step 5: Merging and Resolving Conflicts

Sometimes, we and our team might work on the same files, leading to conflicts.

If a patch doesn’t apply cleanly, we’ll see something like this:

```
error: patch failed: coconut_sorter.py:42
error: coconut_sorter.py: patch does not apply
```

To fix this, we manually edit the file to resolve conflicts, then use:

```sh
git add coconut_sorter.py
git am --continue
```

If things get *really* messed up, we can always abort the patch with:

```sh
git am --abort
```

And start over.

***

## Conclusion

And there you have it—collaborating on a Git project while stranded on a deserted island using only AOL USB drives and bottles.

This method keeps our changes organized and ensures we and our team stay in sync even when patches arrive in random order.

So, if you ever find yourself in a similar situation (which happens *all the time*, obviously), now you know what to do.

Now, excuse me while I eat my 400th strand of Vines Liquorice for the day.

***

## Key Ideas

| Concept                           | Summary                                                                            |
| --------------------------------- | ---------------------------------------------------------------------------------- |
| **Git Patching**                  | A way to share code changes as `.patch` files.                                     |
| **format-patch**                  | Creates patch files from commits.                                                  |
| **apply vs am**                   | `git apply` applies a single patch, `git am` manages multiple patches in sequence. |
| **Handling Out-of-Order Bottles** | Use `git am` and resend bottles when dependencies are missing.                     |
| **Conflict Resolution**           | Manually fix conflicts, then use `git am --continue`.                              |
| **USB & Bottles Communication**   | A totally normal way to send code in 2025.                                         |

![](/post/Articles/3-2/Pasted%20image%2020250302161738.png)
