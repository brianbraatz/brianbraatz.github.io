---
title: Bubble Sort BATTLE!
description: Bubble sort comparison -Python, C#, Javascript, Perl.. and more
slug: bubblesort-battle
date: 2024-09-22
image: bubblewarcover.png
categories: 
tags:
  - CSharp
  - CPP
  - Python
  - GoLanguage
  - Java
  - Javascript
  - ObjectiveC
  - Swift
  - Rust
  - Perl
  - EmbeddedSystems
  - 8051Assembly
  - COBOL
  - Foxpro
  - SQL
  - Forth
  - Eiffel
  - TemplateProgramming
  - Unix
  - Linux
  - Bash
  - Commodore64
  - VBSCRIPT
  - VisualBasic
  - AppleII
  - Powershell
  - TCLScripting
  - ProcessingLanguage
  - DesignPatterns
  - BubbleSort
  - Erlang
  - DesignByContract
  - WebDevelopment
weight: 1
draft: false
lastmod: 2025-02-03T00:24:16.226Z
---
## BUBBBLE BATTLE!

A Bubble Sort is a very simple sorting algorithm that repeatedly steps through the list, compares adjacent elements, and swaps them if they are in the wrong order.

And you just keep repeating until you dont find anymore swaps. Thats the algorithm.

> NOTE - you have no idea how long it will take to finish - since it keeps looping over the same elements until its done

I found a great visualization of a bubblesort here:

https://www.hackerearth.com/practice/algorithms/sorting/bubble-sort/visualize/

Pretty cool site BTW!\
Click through the arrows and you can watch the algorithm on each iteration .

![](/post/faceoff-langs/basic-article/Pasted%20image%2020250130054334.png)

SO.. on with the show:

For fun, here is basic bubble sort implemented in several languages.

Its kind of interesting, at least in this case, to compare the languages

## Python Bubble Sort

[Python](https://tinyurl.com/2kp5knsn)

```python
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        swapped = False
        for j in range(0, n-i-1):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
                swapped = True
        if not swapped:
            break
    return arr

# Example usage
arr = [64, 34, 25, 12, 22, 11, 90]
sorted_arr = bubble_sort(arr)
print("Sorted array is:", sorted_arr)
```

## C Sharp

https://en.wikipedia.org/wiki/C\_Sharp\_(programming\_language)

```c#
using System;

class Program
{
    static void BubbleSort(int[] arr)
    {
        int n = arr.Length;
        for (int i = 0; i < n; i++)
        {
            bool swapped = false;
            for (int j = 0; j < n - i - 1; j++)
            {
                if (arr[j] > arr[j + 1])
                {
                    // Swap arr[j] and arr[j + 1]
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                    swapped = true;
                }
            }
            if (!swapped) break;
        }
    }

    static void Main(string[] args)
    {
        int[] arr = { 64, 34, 25, 12, 22, 11, 90 };
        BubbleSort(arr);
        
        Console.WriteLine("Sorted array is:");
        foreach (int value in arr)
        {
            Console.Write(value + " ");
        }
    }
}
```

## Go Language

https://en.wikipedia.org/wiki/Go\_(programming\_language)

```go
package main

import (
    "fmt"
)

func bubbleSort(arr []int) {
    n := len(arr)
    for i := 0; i < n; i++ {
        swapped := false
        for j := 0; j < n-i-1; j++ {
            if arr[j] > arr[j+1] {
                arr[j], arr[j+1] = arr[j+1], arr[j]
                swapped = true
            }
        }
        if !swapped {
            break
        }
    }
}

func main() {
    arr := []int{64, 34, 25, 12, 22, 11, 90}
    bubbleSort(arr)
    fmt.Println("Sorted array is:", arr)
}
```

## Javascript

https://en.wikipedia.org/wiki/JavaScript

[Why is JavaScript called JavaScript, since it has nothing to do with Java?](https://stackoverflow.com/questions/2018731/why-is-javascript-called-javascript-since-it-has-nothing-to-do-with-java)\
"""\
From an [interview](http://www.infoworld.com/d/developer-world/javascript-creator-ponders-past-future-704) made to its creator [Brendan Eich](http://en.wikipedia.org/wiki/Brendan_Eich):

> **InfoWorld:** As I understand it, JavaScript started out as Mocha, then became LiveScript and then became JavaScript when Netscape and Sun got together. But it actually has nothing to do with Java or not much to do with it, correct?
>
> **Eich:** That’s right. It was all within six months from May till December (1995) that it was Mocha and then LiveScript. And then in early December, Netscape and **Sun** did a license agreement and it became JavaScript. And the idea was to make it a complementary scripting language to go with Java, with the compiled language.

""""

```javascript
function bubbleSort(arr) {
    let n = arr.length;
    for (let i = 0; i < n; i++) {
        let swapped = false;
        for (let j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                [arr[j], arr[j + 1]] = [arr[j + 1], arr[j]];
                swapped = true;
            }
        }
        if (!swapped) break;
    }
    return arr;
}

// Example usage
let arr = [64, 34, 25, 12, 22, 11, 90];
let sortedArr = bubbleSort(arr);
console.log("Sorted array is:", sortedArr);
```

## Java

```java
public class BubbleSort {
    static void bubbleSort(int[] arr) {
        int n = arr.length;
        boolean swapped;
        for (int i = 0; i < n; i++) {
            swapped = false;
            for (int j = 0; j < n - i - 1; j++) {
                if (arr[j] > arr[j + 1]) {
                    // Swap arr[j] and arr[j + 1]
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                    swapped = true;
                }
            }
            if (!swapped) break;
        }
    }

    public static void main(String[] args) {
        int[] arr = { 64, 34, 25, 12, 22, 11, 90 };
        bubbleSort(arr);
        System.out.println("Sorted array is:");
        for (int value : arr) {
            System.out.print(value + " ");
        }
    }
}
```

## Objective-C

https://en.wikipedia.org/wiki/Objective-C

```c++
#import <Foundation/Foundation.h>

void bubbleSort(NSMutableArray *arr) {
    NSUInteger n = [arr count];
    BOOL swapped;
    for (NSUInteger i = 0; i < n; i++) {
        swapped = NO;
        for (NSUInteger j = 0; j < n - i - 1; j++) {
            if ([arr[j] intValue] > [arr[j + 1] intValue]) {
                [arr exchangeObjectAtIndex:j withObjectAtIndex:j + 1];
                swapped = YES;
            }
        }
        if (!swapped) break;
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableArray *arr = [@[@64, @34, @25, @12, @22, @11, @90] mutableCopy];
        bubbleSort(arr);
        NSLog(@"Sorted array is: %@", arr);
    }
    return 0;
}
```

## Swift

https://en.wikipedia.org/wiki/Swift\_(programming\_language)

```swift
func bubbleSort(_ arr: inout [Int]) {
    let n = arr.count
    for i in 0..<n {
        var swapped = false
        for j in 0..<n-i-1 {
            if arr[j] > arr[j + 1] {
                arr.swapAt(j, j + 1)
                swapped = true
            }
        }
        if !swapped { break }
    }
}

var arr = [64, 34, 25, 12, 22, 11, 90]
bubbleSort(&arr)
print("Sorted array is: \(arr)")
```

## Rust

https://en.wikipedia.org/wiki/Rust\_(programming\_language)

```rust
fn bubble_sort(arr: &mut [i32]) {
    let n = arr.len();
    for i in 0..n {
        let mut swapped = false;
        for j in 0..n - i - 1 {
            if arr[j] > arr[j + 1] {
                arr.swap(j, j + 1);
                swapped = true;
            }
        }
        if !swapped {
            break;
        }
    }
}

fn main() {
    let mut arr = [64, 34, 25, 12, 22, 11, 90];
    bubble_sort(&mut arr);
    println!("Sorted array is: {:?}", arr);
}
```

## Perl

https://en.wikipedia.org/wiki/Perl

An old friend and coworker used to say to me "Perl is the only language where its much easier to just re-write some code, vs trying to READ it.. " hahaha

```perl
sub bubble_sort {
    my ($arr) = @_;
    my $n = @$arr;
    for my $i (0 .. $n-1) {
        my $swapped = 0;
        for my $j (0 .. $n-$i-2) {
            if ($arr->[$j] > $arr->[$j+1]) {
                ($arr->[$j], $arr->[$j+1]) = ($arr->[$j+1], $arr->[$j]);
                $swapped = 1;
            }
        }
        last unless $swapped;
    }
}

my @arr = (64, 34, 25, 12, 22, 11, 90);
bubble_sort(\@arr);
print "Sorted array is: @arr\n";
```

## 8051 Assembly language:

![](/post/faceoff-langs/basic-article/Pasted%20image%2020250130055411.png)

https://www.campuscomponent.com/blogs/post/getting-started-with-8051-microcontroller

### From above site

""""

## The 8051 Microcontroller Features:

* 8-bit CPU

* 4 KB ROM

* 128 bytes RAM

* 32 I/O pins

* Two 16-bit timers

* Full duplex UART for serial communication

## Understanding 8051 Microcontroller Architecture

![Getting Started with 8051 Microcontroller](https://lh7-us.googleusercontent.com/1twocL7CBTRdIFEe-48IJ9VrL08_oVkFMZARw98-eF0oBtjjpkwrJvwYYPBDxP-yJw7oJ1DAhXhIHAQVj0XT6N9cvZyeqdaDiSUS_S9T9C8srLBrg5An8Ro6W8bJLOM6N4cgl7SDBdEkGRRUrYEyBA)

""""

Back - in the 1990s I did a lot of projects with the 8051.

very very simple embedded controller with very little memory.

The assembly here is not too far off from 6502 assembly which was another popular embedded CPU.

```assembly
; 8051 Assembly Language Bubble Sort

ORG 0H

START: MOV DPTR, #ARRAY  ; Point DPTR to the start of the array
       MOV R0, #LENGTH   ; Load the length of the array
       DEC R0            ; Decrement R0 since we will use it in the loop

OUTER_LOOP: MOV R1, R0   ; R1 = R0
            MOV R2, #0   ; R2 = 0 (swapped flag)
            
INNER_LOOP: MOVX A, @DPTR  ; Load A with the current element
            MOV R3, A      ; Store the current element in R3
            INC DPTR      ; Point to the next element
            MOVX A, @DPTR ; Load A with the next element

            CJNE A, R3, NOT_EQUAL
            SJMP NO_SWAP

NOT_EQUAL: JNC NO_SWAP    ; If the current element is less than or equal to the next, skip
            MOVX @DPTR, R3 ; Otherwise swap
            DEC DPTR
            MOVX @DPTR, A
            INC DPTR
            MOV R2, #1     ; Set swapped flag

NO_SWAP:   DJNZ R1, INNER_LOOP ; Decrease R1 and continue inner loop if not zero

            JNZ R2, OUTER_LOOP  ; If no swaps happened, the array is sorted
            DEC R0              ; Decrement R0 for the next pass
            JNZ OUTER_LOOP      ; Repeat the outer loop if not zero

END

ARRAY: DB 64H, 34H, 25H, 12H, 22H, 11H, 90H ; Example array
LENGTH: EQU $ - ARRAY ; Length of the array

END START
```

* `DPTR` is used to point to the elements of the array.

* `R0` and `R1` - loop counters.

* `R2`  flag to check if any swaps occurred during a pass.

* The `MOVX` instruction is used to access external memory where the array is stored.

## COBOL

https://en.wikipedia.org/wiki/COBOL\
My farther-in-law spent his whole career writing COBOL..

its interesting..

I did a project that involved EDI with COBOL years ago..  It was interesting.. but not something I would want to do every day.

I like languages with formal datatypes.. COBOL has this weird PIC field..

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

## Foxpro

https://en.wikipedia.org/wiki/FoxPro\
OH I miss this language.... in a way..

In the early 1990s I had a job doing component level repair of 386 and 486 motherboards.. I kind of "made my own" software job, by offering to spend my weekends coding a database system to run the repair department on.

The language I did this in was foxpro!

Good times\
![](/post/faceoff-langs/basic-article/Pasted%20image%2020250130054848.png)

This was DOS btw... AND it was multi-user (!!!)

I really loved the blue color.. It just made me want to make some coffee and write some code..

Later Foxpro came out with a newer version that supported SQL queries..

At my first look of that- I determined this was dumb (my young brain did not appreciate the set theory value of queries). SQL would never take on !

Also- to be fair- the SQL query engine in that first version was very slow..

It was much more fun to write a foxpro loop and loop over the table.. and the print out the lines to the report , as you found them ..

good times..

in anycase- back to the show

below is a Foxpro bubble sort.\
It has a BASIC feel to it.

for the those not familiar, Foxpro is based on the dbase language

[Dbase](https://en.wikipedia.org/wiki/DBase)

```cobol
* Foxpro BubbleSort.prg

PUBLIC ARRAY arr(7)
arr(1) = 64
arr(2) = 34
arr(3) = 25
arr(4) = 12
arr(5) = 22
arr(6) = 11
arr(7) = 90

n = ALEN(arr)

FOR i = 1 TO n - 1
    swapped = .F.
    FOR j = 1 TO n - i
        IF arr(j) > arr(j + 1)
            temp = arr(j)
            arr(j) = arr(j + 1)
            arr(j + 1) = temp
            swapped = .T.
        ENDIF
    ENDFOR
    IF .NOT. swapped
        EXIT
    ENDIF
ENDFOR

? "Sorted array is:"
FOR k = 1 TO n
    ? arr(k)
ENDFOR
```

## SQL Bubblesort

https://en.wikipedia.org/wiki/SQL

```sql
CREATE PROCEDURE BubbleSort
AS
BEGIN
    DECLARE @i INT, @j INT, @swapped BIT, @temp INT
    DECLARE @Array TABLE (ID INT)

    -- Populate the table with example data
    INSERT INTO @Array VALUES (64), (34), (25), (12), (22), (11), (90)

    SET @i = 0
    SET @swapped = 1

    WHILE @swapped = 1
    BEGIN
        SET @swapped = 0
        SET @j = 0
        
        WHILE @j < (SELECT COUNT(*) FROM @Array) - @i - 1
        BEGIN
            DECLARE @currentValue INT, @nextValue INT
            SET @currentValue = (SELECT TOP 1 ID FROM @Array ORDER BY ID OFFSET @j ROWS FETCH NEXT 1 ROW ONLY)
            SET @nextValue = (SELECT TOP 1 ID FROM @Array ORDER BY ID OFFSET @j + 1 ROWS FETCH NEXT 1 ROW ONLY)
            
            IF @currentValue > @nextValue
            BEGIN
                -- Swap positions
                UPDATE @Array SET ID = @temp WHERE ID = @currentValue
                UPDATE @Array SET ID = @currentValue WHERE ID = @nextValue
                UPDATE @Array SET ID = @nextValue WHERE ID = @temp
                SET @swapped = 1
            END
            
            SET @j = @j + 1
        END
        
        SET @i = @i + 1
    END

    SELECT * FROM @Array
END
```

Yes- this is stupid... An sql engine natively has much better ways to sort things :)

## Forth

[Forth (programming language)](https://en.wikipedia.org/wiki/Forth_\(programming_language\))

I was kind of geeky about this when I was younger\
its a stack based language..

Later in life I met a friend who was hired to work on an Accounting system written in forth..\
the good and bad of it is this- with Forth- you are constantly making new\
"words" and then more words on top of those and so on and so on..

so the application code is "readable" to a human..

the complexity is hidden

but the problem is - all these forth programs build up all their own words for their app.. and they ar all very different..

If you hire a forth programmer to work on your system- they will have to read all the code in the system to understand it..

```forth
: SWAP ( n1 n2 -- n1 n2 )
    DUP ROT ROT
    >R SWAP R> ;

: BUBBLE-SORT ( addr u )
    0 DO
        FALSE
        0 DO
            2DUP I CELLS + @ SWAP I CELLS + @ >
            IF
                2DUP I CELLS + @ I 1+ CELLS + @ SWAP
                I CELLS + ! I 1+ CELLS + ! TRUE
            THEN
        LOOP
        2DROP
        SWAP
        1+ SWAP
        >R R> 1- DUP 0= IF LEAVE THEN
    LOOP ;

: PRINT-ARRAY ( addr u )
    0 DO
        DUP I CELLS + @ . SPACE
    LOOP DROP ;

CREATE ARRAY 7 ,
    64 , 34 , 25 , 12 , 22 , 11 , 90 ,

ARRAY 7 BUBBLE-SORT
." Sorted array is: " ARRAY 7 PRINT-ARRAY
```

## Eiffel

[Eiffel (programming language)](https://en.wikipedia.org/wiki/Eiffel_\(programming_language\)))

I dig this language

back in the 2000's I went to ALOT of conferences..

I was lucky to attend an all day workshop on the Eiffel language.\
I got to meet the very interesting Bertrand Meyer and have a lunch with him.

from the wiki\
""""""

The design of the language is closely connected with the Eiffel programming method. Both are based on a set of principles, including [design by contract](https://en.wikipedia.org/wiki/Design_by_contract "Design by contract"), [command–query separation](https://en.wikipedia.org/wiki/Command%E2%80%93query_separation "Command–query separation"), the [uniform-access principle](https://en.wikipedia.org/wiki/Uniform_access_principle "Uniform access principle"), the [single-choice principle](https://en.wikipedia.org/wiki/Single_choice_principle "Single choice principle"), the [open–closed principle](https://en.wikipedia.org/wiki/Open%E2%80%93closed_principle "Open–closed principle"), and [option–operand separation](https://en.wikipedia.org/wiki/Option%E2%80%93operand_separation "Option–operand separation").

"""""

As a younger person , I feel in love with Design by Contract as a pattern. I still dig it, but at the time it blew my mind..

Also at the time- I was working at a company where we were climbing through 1,000,000 plus lines of C++ that didnt have very good separation.. so the Eiffel language struck a chord with me

```python
class
    BUBBLE_SORT

create
    make

feature -- Initialization

    make
        local
            arr: ARRAY[INTEGER]
        do
            arr := <<64, 34, 25, 12, 22, 11, 90>>
            bubble_sort(arr)
            io.put_string("Sorted array is: ")
            across
                arr as elem
            loop
                io.put_integer(elem.item)
                io.put_string(" ")
            end
        end

feature -- Sorting

    bubble_sort(array: ARRAY[INTEGER])
        local
            swapped: BOOLEAN
            temp: INTEGER
        do
            across
                array.lower |..| array.upper + 1 as i
            loop
                swapped := False
                across
                    array.lower |..| array.upper - i.item - 1 as j
                loop
                    if array[j.item] > array[j.item + 1] then
                        temp := array[j.item]
                        array[j.item] := array[j.item + 1]
                        array[j.item + 1] := temp
                        swapped := True
                    end
                end
                if not swapped then
                    exit
                end
            end
        end
end
```

## C++ Template Metaprogramming

[C++ Template Metaprogramming ](https://en.wikipedia.org/wiki/Template_\(C%2B%2B\))

God I fell in love with this stuff.. It was my geek out thing to do on the weekends..

see [index](/post/cpp/cpp-bag/index.md)\
and [index](/post/cpp/cpp-profiler-tmp/index.md)

C++ Templates- it was discovered after the fact, are turing complete!\
It wasnt intended this way.. but C++ nerds went nuts with this

here is a god paper on C++ and Turing complete\
https://rtraba.com/wp-content/uploads/2015/05/cppturing.pdf

https://en.wikipedia.org/wiki/Turing\_completeness

This lead my interesting into functional programming languages\
https://en.wikipedia.org/wiki/Functional\_programming

```c++
#include <iostream>
#include <array>

// Swap two elements
template <std::size_t I, std::size_t J, typename T>
struct SwapHelper {
    static void swap(std::array<T, J + 1>& arr) {
        if constexpr (arr[I] > arr[J]) {
            std::swap(arr[I], arr[J]);
        }
    }
};

// Bubble Sort Implementation
template <std::size_t N, typename T>
struct BubbleSort {
    static void sort(std::array<T, N>& arr) {
        for (std::size_t i = 0; i < N - 1; ++i) {
            for (std::size_t j = 0; j < N - i - 1; ++j) {
                SwapHelper<j, j + 1, T>::swap(arr);
            }
        }
    }
};

int main() {
    std::array<int, 7> arr = { 64, 34, 25, 12, 22, 11, 90 };

    BubbleSort<7, int>::sort(arr);

    std::cout << "Sorted array is: ";
    for (const auto& value : arr) {
        std::cout << value << " ";
    }
    std::cout << std::endl;

    return 0;
}
```

check out the F# implementation.\
it has a similiar feel to the C++ template code- because they are both functional languages

## F# Language

[F# Language](https://en.wikipedia.org/wiki/F_Sharp_\(programming_language\))

```F#
let bubbleSort (arr: int[]) =
    let swap i j =
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp

    let rec sortPass n =
        if n = 1 then
            arr
        else
            for i in 0 .. n - 2 do
                if arr.[i] > arr.[i + 1] then
                    swap i (i + 1)
            sortPass (n - 1)

    sortPass (Array.length arr)

let arrayToSort = [|5; 3; 8; 4; 2|]
let sortedArray = bubbleSort arrayToSort
printfn "Sorted Array: %A" sortedArray
```

<!-- This F# implementation of bubble sort defines a `bubbleSort` function that takes an array of integers and sorts it in ascending order. Here's a breakdown of the code:

- `swap i j`**:** A helper function to swap the elements at indices `i` and `j` in the array.
    
- `sortPass n`**:** A recursive function that performs one pass of the bubble sort algorithm. It iterates through the array and swaps adjacent elements if they are in the wrong order, then recursively calls itself with `n-1` until the array is sorted.
    
- `bubbleSort (arr: int[])`**:** The main function that sorts the array by calling `sortPass` with the length of the array.
    

You can use this function to sort an array of integers as shown in the example. If you have any more questions or need further assistance, feel free to ask!
-->

## AutoHotkey

https://en.wikipedia.org/wiki/AutoHotkey

Great scripting language for Windows automation

```python
arr := [64, 34, 25, 12, 22, 11, 90]
n := arr.MaxIndex()
Loop %n%
{
    swapped := false
    Loop, % n - A_Index
    {
        if (arr[A_Index] > arr[A_Index + 1])
        {
            temp := arr[A_Index]
            arr[A_Index] := arr[A_Index + 1]
            arr[A_Index + 1] := temp
            swapped := true
        }
    }
    if (!swapped)
        break
}
MsgBox, Sorted array is: %arr%
```

## Visual Basic

[Visual Basic](https://en.wikipedia.org/wiki/Visual_Basic_\(classic\))

```vb
Module BubbleSort
    Sub Main()
        Dim arr As Integer() = {64, 34, 25, 12, 22, 11, 90}
        Dim n As Integer = arr.Length

        For i As Integer = 0 To n - 1
            Dim swapped As Boolean = False

            For j As Integer = 0 To n - i - 2
                If arr(j) > arr(j + 1) Then
                    Dim temp As Integer = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = temp
                    swapped = True
                End If
            Next

            If Not swapped Then
                Exit For
            End If
        Next

        Console.WriteLine("Sorted array is:")
        For Each value In arr
            Console.Write(value & " ")
        Next

        Console.ReadLine()
    End Sub
End Module
```

## C++

```c++
#include <iostream>
#include <vector>
using namespace std;

void bubbleSort(vector<int>& arr) {
    int n = arr.size();
    for (int i = 0; i < n; i++) {
        bool swapped = false;
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                swap(arr[j], arr[j + 1]);
                swapped = true;
            }
        }
        if (!swapped) break;
    }
}

int main() {
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90};
    bubbleSort(arr);

    cout << "Sorted array is: ";
    for (int value : arr) {
        cout << value << " ";
    }
    cout << endl;

    return 0;
}
```

## GWBASIC

https://en.wikipedia.org/wiki/GW-BASIC\
From the good old days of dos.

always number your lines in 10's .. so you can add more in the middle later haha....

```vb
10 DIM arr(7)
20 DATA 64, 34, 25, 12, 22, 11, 90

30 FOR i = 1 TO 7
40   READ arr(i)
50 NEXT i

60 FOR i = 1 TO 6
70   swapped = 0
80   FOR j = 1 TO 7 - i
90     IF arr(j) > arr(j + 1) THEN
100      temp = arr(j)
110      arr(j) = arr(j + 1)
120      arr(j + 1) = temp
130      swapped = 1
140    END IF
150   NEXT j
160   IF swapped = 0 THEN EXIT FOR
170 NEXT i

180 PRINT "Sorted array is: "
190 FOR i = 1 TO 7
200   PRINT arr(i);
210 NEXT i
220 END
```

## Apple II BASIC

https://en.wikipedia.org/wiki/Applesoft\_BASIC

```vb
10 DIM ARR(7)
20 DATA 64, 34, 25, 12, 22, 11, 90
30 FOR I = 1 TO 7
40   READ ARR(I)
50 NEXT I
60 FOR I = 1 TO 6
70   SWAPPED = 0
80   FOR J = 1 TO 7 - I
90     IF ARR(J) > ARR(J + 1) THEN
100      TEMP = ARR(J)
110      ARR(J) = ARR(J + 1)
120      ARR(J + 1) = TEMP
130      SWAPPED = 1
140    END IF
150   NEXT J
160   IF SWAPPED = 0 THEN GOTO 200
170 NEXT I
200 PRINT "SORTED ARRAY IS:"
210 FOR I = 1 TO 7
220   PRINT ARR(I)" ";
230 NEXT I
240 END
```

## Commodore 64 BASIC

[Commodore 64 BASIC](https://en.wikipedia.org/wiki/Commodore_BASIC)

NOTE: Apple and C64 look similar- hahah - probably because basic was liscenced by Microsoft

Apple's first basic was Integer Basic - Written by Woz I believe

```vb
10 DIM ARR(7)
20 DATA 64, 34, 25, 12, 22, 11, 90
30 FOR I = 1 TO 7
40   READ ARR(I)
50 NEXT I
60 FOR I = 1 TO 6
70   SWAPPED = 0
80   FOR J = 1 TO 7 - I
90     IF ARR(J) > ARR(J + 1) THEN
100      TEMP = ARR(J)
110      ARR(J) = ARR(J + 1)
120      ARR(J + 1) = TEMP
130      SWAPPED = 1
140    END IF
150   NEXT J
160   IF SWAPPED = 0 THEN GOTO 200
170 NEXT I
200 PRINT "SORTED ARRAY IS:"
210 FOR I = 1 TO 7
220   PRINT ARR(I);
230 NEXT I
240 END
```

## DOS Batch Scripting

https://en.wikipedia.org/wiki/DOS\
https://en.wikipedia.org/wiki/Batch\_file

Ahh the good old days..

who needs fancy powershell?

(haha actually BAT programming will drive you nuts- its very very hack- but it worked )

```shell
@echo off
setlocal EnableDelayedExpansion

rem Initialize the array
set arr[0]=64
set arr[1]=34
set arr[2]=25
set arr[3]=12
set arr[4]=22
set arr[5]=11
set arr[6]=90
set n=7

rem Bubble Sort
for /L %%i in (0,1,!n!-1) do (
    set swapped=0
    for /L %%j in (0,1,!n!-!i!-2) do (
        set /A next=%%j+1
        call set "a=%%arr[%%j]%%"
        call set "b=%%arr[!next!]%%"
        if !a! gtr !b! (
            call set "temp=%%arr[%%j]%%"
            call set "arr[%%j]=%%arr[!next!]%%"
            call set "arr[!next!]=%%temp%%"
            set swapped=1
        )
    )
    if !swapped! equ 0 (
        goto sorted
    )
)

:sorted
echo Sorted array is:
for /L %%i in (0,1,!n!-1) do (
    echo !arr[%%i]!
)

endlocal
```

## Powershell

https://en.wikipedia.org/wiki/PowerShell\
Compared to BAT files- Looks like a normal language eh?

```powershell
# Bubble Sort in PowerShell

$arr = @(64, 34, 25, 12, 22, 11, 90)

$n = $arr.Length
for ($i = 0; $i -lt $n; $i++) {
    $swapped = $false
    for ($j = 0; $j -lt $n - $i - 1; $j++) {
        if ($arr[$j] -gt $arr[$j + 1]) {
            $temp = $arr[$j]
            $arr[$j] = $arr[$j + 1]
            $arr[$j + 1] = $temp
            $swapped = $true
        }
    }
    if (-not $swapped) {
        break
    }
}

Write-Output "Sorted array is:"
$arr
```

## Bash

[Bash](https://en.wikipedia.org/wiki/Bash_\(Unix_shell\))

Bash is macho

```bash
#!/bin/bash

# Bubble Sort in Bash

# Initialize the array
arr=(64 34 25 12 22 11 90)

# Get the length of the array
n=${#arr[@]}

# Bubble Sort
for ((i = 0; i < n; i++)); do
    swapped=0
    for ((j = 0; j < n - i - 1; j++)); do
        if [ ${arr[j]} -gt ${arr[j + 1]} ]; then
            # Swap
            temp=${arr[j]}
            arr[j]=$[arr[j + 1]}
            arr[j + 1]=$temp
            swapped=1
        fi
    done
    if [ $swapped -eq 0 ]; then
        break
    fi
done

# Print sorted array
echo "Sorted array is: ${arr[@]}"
```

## Visual Basic Script

https://en.wikipedia.org/wiki/VBScript\
20+ years ago, most of my automation scripting was written in VB Script , with usually bat files on top of them .. .

you COULD actually spawn visual studio and debug these things..

it was good..

and then after Microsoft locked down the security of the OS- VBScript was too powerful, and so it was much less easy to just run a vbscript

a common trick was hackers would email an unsuspecting person a VB Script- and then they run that - and now the bad guys have full access to your system ..

so for your enjoyment- VB Script Bubblesort:

NOTE VB script compared to VB- has less formal types-they are compatible but not 100%

```vb
' BubbleSort.vbs

Dim arr(6)
arr(0) = 64
arr(1) = 34
arr(2) = 25
arr(3) = 12
arr(4) = 22
arr(5) = 11
arr(6) = 90

Dim n, i, j, temp, swapped
n = UBound(arr) + 1

For i = 0 To n - 1
    swapped = False

    For j = 0 To n - i - 2
        If arr(j) > arr(j + 1) Then
            temp = arr(j)
            arr(j) = arr(j + 1)
            arr(j + 1) = temp
            swapped = True
        End If
    Next

    If Not swapped Then
        Exit For
    End If
Next

WScript.Echo "Sorted array is:"
For i = 0 To n - 1
    WScript.Echo arr(i)
Next
```

## TCL (**Tool Command Language**) Scripting

https://en.wikipedia.org/wiki/Tcl\
A good friend became a TCL geek- I never got 100% into the koolaide- but I still find it interesting..\
This was an early system people would put into C and C++ programs to expose parts of the system to be scriptable in TCL.

```tcl
proc bubble_sort {arr} {
    set n [llength $arr]

    for {set i 0} {$i < $n} {incr i} {
        set swapped 0

        for {set j 0} {$j < $n - $i - 1} {incr j} {
            if {[lindex $arr $j] > [lindex $arr [expr {$j + 1}]]} {
                set temp [lindex $arr $j]
                set arr [lreplace $arr $j [expr {$j + 1}] [lindex $arr [expr {$j + 1}]] $temp]
                incr swapped
            }
        }

        if {$swapped == 0} {
            break
        }
    }

    return $arr
}

# Example usage
set arr [list 64 34 25 12 22 11 90]
set sorted_arr [bubble_sort $arr]
puts "Sorted array is: $sorted_arr"
```

<!-- 
## Processing.org Language

Gee that looks alot like javascript ! :) 

~~~javascript
int[] arr = {64, 34, 25, 12, 22, 11, 90};

void setup() {
  size(400, 200);
  bubbleSort(arr);
  println("Sorted array is:");
  for (int value : arr) {
    println(value);
  }
}

void bubbleSort(int[] arr) {
  int n = arr.length;
  for (int i = 0; i < n; i++) {
    boolean swapped = false;
    for (int j = 0; j < n - i - 1; j++) {
      if (arr[j] > arr[j + 1]) {
        int temp = arr[j];
        arr[j] = arr[j + 1];
        arr[j + 1] = temp;
        swapped = true;
      }
    }
    if (!swapped) break;
  }
}

~~~
-->

## Erlang

[Erland](https://en.wikipedia.org/wiki/Erlang_\(programming_language\))

```erlang
-module(bubble_sort).
-export([sort/1]).

sort(List) ->
    bubble_sort(List, []).

bubble_sort([], Sorted) ->
    Sorted;
bubble_sort([H|T], Sorted) ->
    bubble_sort_pass(T, H, [], Sorted).

bubble_sort_pass([], Max, L, Sorted) ->
    bubble_sort(L, [Max|Sorted]);
bubble_sort_pass([H|T], Max, L, Sorted) when H > Max ->
    bubble_sort_pass(T, H, [Max|L], Sorted);
bubble_sort_pass([H|T], Max, L, Sorted) ->
    bubble_sort_pass(T, Max, [H|L], Sorted).
```

Call this module by calling the `sort` function with a list of numbers that you want to sort. For example:

```erlang
c(bubble_sort).
{ok,bubble_sort}

bubble_sort:sort([5, 3, 8, 4, 2]).
[2,3,4,5,8]
```

Note the "c()" business is "compile" in Erlangs interactive shell.

Erlang came out of Ericsson..

Its designed to be very stable - to run phone systems

### from the wikipedia link

""""\
Erlang applications are built of very lightweight Erlang processes in the Erlang runtime system. Erlang processes can be seen as "living" objects ([object-oriented programming](https://en.wikipedia.org/wiki/Object-oriented_programming "Object-oriented programming")), with data encapsulation and [message passing](https://en.wikipedia.org/wiki/Message_passing "Message passing"), but capable of changing behavior during runtime. The Erlang runtime system provides strict [process isolation](https://en.wikipedia.org/wiki/Process_isolation "Process isolation") between Erlang processes (this includes data and garbage collection, separated individually by each Erlang process) and transparent communication between processes (see [Location transparency](https://en.wikipedia.org/wiki/Location_transparency "Location transparency")) on different Erlang nodes (on different hosts).

Joe Armstrong, co-inventor of Erlang, summarized the principles of processes in his [PhD](https://en.wikipedia.org/wiki/Doctor_of_Philosophy "Doctor of Philosophy") [thesis](https://en.wikipedia.org/wiki/Thesis "Thesis"):[\[18\]](https://en.wikipedia.org/wiki/Erlang_\(programming_language\)#cite_note-18)

* Everything is a process.
* Processes are strongly isolated.
* Process creation and destruction is a lightweight operation.
* Message passing is the only way for processes to interact.
* Processes have unique names.
* If you know the name of a process you can send it a message.
* Processes share no resources.
* Error handling is non-local.
* Processes do what they are supposed to do or fail.

Joe Armstrong remarked in an interview with Rackspace in 2013: "If [Java](https://en.wikipedia.org/wiki/Java_\(programming_language\) "Java (programming language)") is '[write once, run anywhere](https://en.wikipedia.org/wiki/Write_once,_run_anywhere "Write once, run anywhere")', then Erlang is 'write once, run forever'."[\[19\]](https://en.wikipedia.org/wiki/Erlang_\(programming_language\)#cite_note-19)

"""
