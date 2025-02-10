---
title: Fortran in a Nutshell
description: "The Fortran Programming Language: History, Motivation, Relationship to Modern Languages, and Code Examples"
slug: the-fortran-programming-language:-history-motivation-relationship-to-modern-languages-and-10-code-examples
date: 2024-01-30
image: post/Articles/IMAGES/fortranpunchcard.jpg
categories:
  - Fortran
  - Languages
  - History
tags:
  - Fortran
  - Programming
  - Language
  - Scientific
  - Computing
  - History
  - Of
  - Computing
  - High-Performance
  - Computing
  - Numerical
  - Analysis
  - Modern
  - Languages
  - Syntax
  - Comparison
draft: false
weight: 327
lastmod: 2025-02-10T15:34:10.617Z
---
<!--
# The Fortran Programming Language: History, Motivation, Relationship to Modern Languages, and 10 Code Examples
-->

## Introduction

Before Python, before C, before even Lisp, there was **Fortran**.

Yes, this is **one of the oldest programming languages still in use today**. Developed in the **1950s**, Fortran (short for **Formula Translation**) was created to handle **scientific and engineering computations**—and it still does that job better than many modern languages.,

<!-- 
This article will explore:  

- The **history and motivation** behind Fortran.  
- How it influenced **modern programming languages**.  
- **10 real code examples** of Fortran in action.  
- A **table of Fortran syntax** compared to modern languages.  
-->

***

## The History of Fortran

Fortran was born at **IBM in 1957**, thanks to **John Backus and his team**. At the time, programming was done in **assembly language**, which made writing complex calculations **painfully slow and error-prone**.

### **Why Was Fortran Created?**

* Scientists and engineers needed a way to **write mathematical formulas in a human-readable way**.
* Computers were getting more powerful, but programming them was still a **nightmare**.
* **IBM wanted to make programming easier**, and Fortran was their solution.

### **Key Innovations of Fortran**

✅ **First High-Level Language** → Before Fortran, everything was hand-coded in assembly.\
✅ **First Compiler** → The original Fortran compiler was one of the first optimizing compilers ever built.\
✅ **Built for Scientific Computing** → Fortran was **designed from day one for numerical computations**.\
✅ **Still in Use Today** → Modern Fortran (Fortran 90, 95, 2003, 2018) is used in **climate modeling, physics, and engineering simulations**.

> **Further Reading:**
>
> * [Fortran Wikipedia](https://en.wikipedia.org/wiki/Fortran)
> * [The History of Fortran](https://history.computing.org/programming/fortran/)

***

## Fortran’s Influence on Modern Languages

| Feature                                       | Fortran | Modern Equivalent               |
| --------------------------------------------- | ------- | ------------------------------- |
| **Mathematical Computations**                 | ✅ Yes   | ✅ Python (NumPy), Julia, MATLAB |
| **Compiled Language**                         | ✅ Yes   | ✅ C, C++                        |
| **High Performance**                          | ✅ Yes   | ✅ Rust, C++                     |
| **Array Processing**                          | ✅ Yes   | ✅ Python (NumPy), R             |
| **Structured Programming (Introduced later)** | ✅ Yes   | ✅ C, Java, Python               |

💡 **Verdict:** Fortran is still **one of the best languages for high-performance scientific computing**!

***

## Fortran Syntax Table

| Concept                                | Fortran Code                                        | Equivalent in Python / C                                              |
| -------------------------------------- | --------------------------------------------------- | --------------------------------------------------------------------- |
| **Hello World**                        | `PRINT *, "Hello, World!"`                          | `print("Hello, World!")` / `printf("Hello, World!");`                 |
| **Variables**                          | `INTEGER :: x = 42`                                 | `x = 42` / `int x = 42;`                                              |
| **Loops**                              | `DO i = 1, 10` followed by `END DO`                 | `for i in range(1, 11):` / `for (int i=1; i<=10; i++)`                |
| **Conditionals**                       | `IF (x > 5) THEN PRINT *, "High"`                   | `if x > 5: print('High')` / `if (x > 5) { printf("High"); }`          |
| **Functions**                          | `FUNCTION square(x) RESULT(y) y = x*x END FUNCTION` | `def square(x): return x * x` / `int square(int x) { return x * x; }` |
| **Arrays**                             | `INTEGER, DIMENSION(5) :: A = (/1,2,3,4,5/)`        | `A = [1, 2, 3, 4, 5]`                                                 |
| **Modules (Introduced in Fortran 90)** | `MODULE math_functions`                             | `import math`                                                         |

***

## 10 Fortran Code Examples

### **1. Hello, World!**

```fortran
PRINT *, "Hello, World!"
```

### **2. Declaring Variables**

```fortran
INTEGER :: x = 42
```

### **3. If-Else Statement**

```fortran
IF (x > 10) THEN
    PRINT *, "X is greater than 10"
ELSE
    PRINT *, "X is 10 or less"
END IF
```

### **4. For Loop**

```fortran
DO i = 1, 5
    PRINT *, "Iteration:", i
END DO
```

### **5. Function Definition (Square Function)**

```fortran
FUNCTION square(x) RESULT(y)
    INTEGER, INTENT(IN) :: x
    INTEGER :: y
    y = x * x
END FUNCTION square
```

### **6. Arrays**

```fortran
INTEGER, DIMENSION(5) :: A = (/1, 2, 3, 4, 5/)
```

### **7. Reading User Input**

```fortran
PRINT *, "Enter a number:"
READ *, x
PRINT *, "You entered:", x
```

### **8. Recursion (Factorial Function)**

```fortran
RECURSIVE FUNCTION factorial(n) RESULT(fact)
    INTEGER, INTENT(IN) :: n
    INTEGER :: fact
    IF (n == 0) THEN
        fact = 1
    ELSE
        fact = n * factorial(n - 1)
    END IF
END FUNCTION factorial
```

### **9. Writing to a File**

```fortran
OPEN(UNIT=10, FILE="output.txt", STATUS="NEW")
WRITE(10,*) "Hello, file!"
CLOSE(10)
```

### **10. Parallel Computing with OpenMP**

```fortran
PROGRAM parallel_example
    USE OMP_LIB
    INTEGER :: i
    !$OMP PARALLEL DO
    DO i = 1, 10
        PRINT *, "Thread ", OMP_GET_THREAD_NUM(), " Processing ", i
    END DO
    !$OMP END PARALLEL DO
END PROGRAM parallel_example
```

***

## Key Takeaways

* **Fortran is the first high-level programming language and is still in use today.**
* **It remains the king of high-performance scientific computing.**
* **Modern versions (Fortran 90, 95, 2003, 2018) are still evolving.**
* **If you need to crunch numbers fast, Fortran is still one of the best options!**

***

## References

1. [Fortran Wikipedia](https://en.wikipedia.org/wiki/Fortran)
2. [The History of Fortran](https://history.computing.org/programming/fortran/)
3. [Modern Fortran Guide](https://www.fortran90.org/)
