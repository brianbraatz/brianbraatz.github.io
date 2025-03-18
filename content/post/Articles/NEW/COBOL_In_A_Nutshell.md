---
title: COBOL In a Nutshell
description: COBOL In a Nutshell
slug: cobol-in-a-nutshell
date: 2020-10-10
image: post/Articles/IMAGES/cobol.png
categories:
  - Cobol
tags:
  - Cobol
  - Programming
  - LegacyCode
  - Y2K
  - Date
  - Handling
  - Mainframe
  - Comparison
  - Examples
draft: false
weight: 422
categories_ref:
  - Cobol
slug_calculated: https://brianbraatz.github.io/p/cobol-in-a-nutshell
lastmod: 2025-03-14T16:40:20.496Z
---
# COBOL In a Nutshell

Ah, COBOL. The language that refuses to die, much like disco and your uncle’s collection of VHS tapes.

Let's dive into the world of COBOL, the programming language that powered businesses for decades and caused mass panic around the year 2000.

## A Brief History of COBOL (a.k.a. How We Got Here)

Back in 1959, when dinosaurs (mainframes) roamed the Earth, the U.S. Department of Defense decided that all businesses needed a common programming language for data processing.

Enter COBOL (Common Business-Oriented Language), which was designed to be readable, like English—but somehow ended up making programmers cry.

COBOL was created by a committee, the CODASYL (Conference on Data Systems Languages), and one of its key contributors was **Grace Hopper**, a legendary computer scientist.

The goal? To make a language that businesses could use without needing to hire a team of rocket scientists. Spoiler alert: it worked.

More info: <https://en.wikipedia.org/wiki/COBOL>

## The Main Ideas Behind COBOL

COBOL was built for business applications, not for writing games or launching rockets. Here are some of its main features:

* **Verbose Syntax** – COBOL reads almost like a novel (if novels were full of screaming accountants).
* **Data-Oriented** – It shines in handling and processing massive amounts of data.
* **Portability** – COBOL code from the '60s can still run today, which is both impressive and terrifying.
* **Strong File Handling** – Reading, writing, and managing files is its bread and butter.
* **Fixed Formatting** – Early COBOL required code to be written in specific columns, which was great for consistency but terrible for sanity.

## The PIC Field (a.k.a. The Magic Formatting Trick)

COBOL's **PIC (Picture) Clause** is where data formatting happens.

Think of it like telling COBOL how to dress up your numbers and strings.

```cobol
01  EMPLOYEE-NAME  PIC X(30).   *> Stores a 30-character name
01  SALARY         PIC 9(7)V99. *> 7 digits before decimal, 2 after
01  EMPLOYEE-ID    PIC 9(5).    *> A 5-digit employee ID
```

* `X(n)`: Character field with `n` length.
* `9(n)`: Numeric field with `n` digits.
* `V`: Decimal point (it’s "implied", meaning it doesn’t actually exist in the data).

More info: [https://en.wikipedia.org/wiki/COBOL#Data\_types\_and\_variables](https://en.wikipedia.org/wiki/COBOL%23Data_types_and_variables)

## How COBOL Handles Dates (a.k.a. The Y2K Time Bomb)

COBOL, being old-school, often stored dates using **two-digit years** (e.g., `99` for 1999). This worked fine... until the year 2000 approached.

```cobol
01  DATE-OF-BIRTH  PIC 9(6).   *> YYMMDD format (yikes!)
```

The problem? When `99` rolled over to `00`, COBOL systems thought it was **1900 instead of 2000**, causing banking systems, payroll software, and airline reservations to freak out.

### The Y2K Crisis (a.k.a. The Time COBOL Devs Became Millionaires)

As the year 2000 approached, businesses realized their COBOL systems were in deep trouble.

Enter the **COBOL consultants**, who were suddenly in high demand, billing outrageous amounts to find and fix every instance of bad date logic.

The world *almost* ended, but thanks to thousands of COBOL devs painstakingly updating old code, disaster was averted.

More info: <https://en.wikipedia.org/wiki/Year_2000_problem>

## COBOL vs. BASIC vs. PASCAL

How does COBOL compare to other old-school languages? Let's break it down:

| Feature           | COBOL                         | BASIC                       | PASCAL                          |
| ----------------- | ----------------------------- | --------------------------- | ------------------------------- |
| **Purpose**       | Business applications         | General-purpose programming | Teaching structured programming |
| **Syntax**        | Verbose, English-like         | Simple, but inconsistent    | Clean and structured            |
| **Data Handling** | Strong for records/files      | Weak                        | Moderate                        |
| **Readability**   | Very high (some say too high) | Moderate                    | High                            |
| **Usage Today**   | Still alive in finance        | Hobbyist, legacy apps       | Mostly educational use          |

More info:

* <https://en.wikipedia.org/wiki/COBOL>
* <https://en.wikipedia.org/wiki/BASIC>
* [https://en.wikipedia.org/wiki/Pascal\_(programming\_language)](https://en.wikipedia.org/wiki/Pascal_%28programming_language%29)

## The Language in Detail (a.k.a. COBOL 101)

Let's look at some **COBOL basics**:

### Hello World

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
PROCEDURE DIVISION.
    DISPLAY "Hello, world!".
    STOP RUN.
```

Yes, COBOL insists on being wordy.

### Reading a File

```cobol
SELECT EMPLOYEE-FILE ASSIGN TO "employees.dat".

FD EMPLOYEE-FILE.
01 EMPLOYEE-RECORD.
   05 EMP-ID PIC 9(5).
   05 EMP-NAME PIC X(30).

PROCEDURE DIVISION.
    OPEN INPUT EMPLOYEE-FILE.
    READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD.
    DISPLAY EMP-NAME.
    CLOSE EMPLOYEE-FILE.
    STOP RUN.
```

### Bubble Sort

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. BubbleSort.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 ARRAYS.
    05 ARR PIC 99 OCCURS 7 TIMES VALUE 64, 34, 25, 12, 22, 11, 90.
01 COUNT PIC 9(2).
01 SWAP-FLAG PIC 9(1).
01 I PIC 9(2).
01 J PIC 9(2).
01 TEMP PIC 9(2).

PROCEDURE DIVISION.
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
        MOVE 0 TO SWAP-FLAG
        PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7 - I
            IF ARR (J) > ARR (J + 1)
                MOVE ARR (J) TO TEMP
                MOVE ARR (J + 1) TO ARR (J)
                MOVE TEMP TO ARR (J + 1)
                MOVE 1 TO SWAP-FLAG
            END-IF
        END-PERFORM
        IF SWAP-FLAG = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

DISPLAY "Sorted array is:".
PERFORM VARYING COUNT FROM 1 BY 1 UNTIL COUNT > 7
    DISPLAY ARR (COUNT)
END-PERFORM.

STOP RUN.
```

## Conclusion (a.k.a. We Got Lucky)

The Y2K bug could've ended the world, but thanks to COBOL programmers (and a lot of last-minute coding marathons), society survived.

And guess what? COBOL is still running **banking, government, and insurance systems today**.

Who knows? Maybe in 2099, COBOL developers will once again save the world.

## Key Ideas

| Topic       | Summary                                   |
| ----------- | ----------------------------------------- |
| History     | Created in 1959 for business applications |
| PIC Fields  | Used for defining data structure          |
| Y2K Bug     | Caused by two-digit year formats          |
| COBOL Today | Still used in finance & government        |

## References

* <https://en.wikipedia.org/wiki/COBOL>
* <https://en.wikipedia.org/wiki/Year_2000_problem>
* <https://en.wikipedia.org/wiki/BASIC>
* [https://en.wikipedia.org/wiki/Pascal\_(programming\_language)](https://en.wikipedia.org/wiki/Pascal_%28programming_language%29)
