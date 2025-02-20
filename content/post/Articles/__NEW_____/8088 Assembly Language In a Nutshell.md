---
title: 8088 Assembly Language in a Nutshell
description: 
slug: 8088-assembly-nutshell
date: 2019-11-27
image: post/Articles/IMAGES/8088.png
categories:
  - Assembly
  - Embedded Systems
  - 6502 Processor
  - Retro Computing
  - Assembly Language
tags:
  - Assembly
  - Embedded
  - Systems
  - Programming
  - Microcontroller
  - Retro
  - Computing
  - Low-Level
  - Programming
  - AssemblyLanguage
draft: false
weight: 274
lastmod: 2025-02-20T12:13:33.962Z
---
# 8088 Assembly Language in a Nutshell

## The Glorious History of the 8088

Ah, the late **1970s**—a time when disco was king, and Intel decided to shake up the microprocessor world with the **8086**.

But wait!!, there's more!

In **1979**, they introduced the **8088**, a cost-effective version with an 8-bit external data bus.

This little tweak made it the perfect brain for the original **IBM PC**, and the rest, as they say, is history.

The **8088** was the bridge between 8-bit and 16-bit computing, making it a versatile choice for early personal computers.

Its architecture laid the groundwork for the x86 platform, which still dominates the PC market today.

(And modern Intel CPUS STILL support 8088 instructions!)

***

## 8088 Architecture - What’s Inside?

The **Intel 8088** is like the Swiss Army knife of microprocessors. Here's a peek under the hood:

* **Registers**:
  * **General Purpose Registers**: AX, BX, CX, DX (each can be split into high and low bytes, e.g., AH and AL).
  * **Index Registers**: SI (Source Index), DI (Destination Index).
  * **Pointer Registers**: BP (Base Pointer), SP (Stack Pointer).
  * **Instruction Pointer**: IP (points to the next instruction).
  * **Segment Registers**: CS (Code Segment), DS (Data Segment), SS (Stack Segment), ES (Extra Segment).
  * **Flags Register**: Reflects the outcome of operations (zero, carry, sign, etc.).

* **Memory Addressing**:
  * **20-bit address bus**, allowing access to a whopping **1 MB of memory**. That's right, folks, 1 megabyte!

* **Clock Speed**:
  * Typically around **4.77 MHz** in the original IBM PC. Blazing fast for its time!

***

## 8088 Instruction Set

| Mnemonic | Description                 |
| -------- | --------------------------- |
| MOV      | Move data                   |
| ADD      | Add                         |
| SUB      | Subtract                    |
| INC      | Increment                   |
| DEC      | Decrement                   |
| MUL      | Unsigned multiply           |
| DIV      | Unsigned divide             |
| AND      | Logical AND                 |
| OR       | Logical OR                  |
| XOR      | Logical exclusive OR        |
| NOT      | Logical NOT                 |
| JMP      | Unconditional jump          |
| CALL     | Call procedure              |
| RET      | Return from procedure       |
| PUSH     | Push onto stack             |
| POP      | Pop from stack              |
| NOP      | No operation (does nothing) |
| HLT      | Halt the processor          |

For all the 8088 instructions, check out the [x86 instruction listings](https://en.wikipedia.org/wiki/X86_instruction_listings)

***

# Example Code

## 1. Hello World over RS-232

```assembly
section .data
    message db 'Hello, World!', 0

section .text
    ; Initialize serial port (COM1)
    mov dx, 0x3F8       ; COM1 base port
    mov al, 0x80        ; Enable DLAB
    out dx, al
    mov dx, 0x3F9
    mov al, 0x01        ; Set baud rate divisor to 115200
    out dx, al
    mov dx, 0x3FB
    mov al, 0x03        ; 8 bits, no parity, one stop bit
    out dx, al
    mov dx, 0x3F8       ; Back to data port

    ; Send message
    mov si, message
.next_char:
    lodsb               ; Load next byte into AL
    or al, al           ; Check if end of string (null terminator)
    jz .done
.wait:
    in al, dx           ; Read Line Status Register
    test al, 0x20       ; Check if Transmitter Holding Register is empty
    jz .wait
    mov al, [si-1]      ; Get character to send
    out dx, al          ; Send character
    jmp .next_char
.done:
    hlt                 ; Halt the processor
```

## 2. Blinking an LED

```assembly
section .text
    ; Assume LED is connected to port 0x378 (parallel port)
    mov dx, 0x378       ; Parallel port address
    mov cx, 0xFFFF      ; Delay counter

blink:
    mov al, 0x01        ; Turn LED on
    out dx, al
    call delay
    mov al, 0x00        ; Turn LED off
    out dx, al
    call delay
    jmp blink

delay:
    push cx
    mov cx, 0xFFFF
.delay_loop:
    loop .delay_loop
    pop cx
    ret
```

## Bubble Sort

A Bubble sort is a simple sorting algorithm that repeatedly steps through an array, swapping adjacent elements if they are in the wrong order.

This process repeats until the array is sorted.

### 8088 Assembly Bubble Sort

```assembly
; Assume DS:SI points to the start of the array
; CX holds the number of elements in the array

bubble_sort:
    push    cx              ; Save the count of elements
    dec     cx              ; CX = number of passes (n-1)
    jz      sorted          ; If array has 1 or 0 elements, it's already sorted

outer_loop:
    mov     di, si          ; DI points to the start of the array
    mov     bx, cx          ; BX = number of comparisons per pass

inner_loop:
    mov     ax, [di]        ; Load current element into AX
    cmp     ax, [di+2]      ; Compare with the next element
    jbe     no_swap         ; If in order, no swap needed

    ; Swap the elements
    xchg    ax, [di+2]      ; Place the next element into AX
    mov     [di], ax        ; Store AX (original next element) into current position

no_swap:
    add     di, 2           ; Move to the next pair
    dec     bx              ; Decrement comparison count
    jnz     inner_loop      ; Repeat inner loop if BX != 0

    dec     cx              ; Decrement pass count
    jnz     outer_loop      ; Repeat outer loop if CX != 0

sorted:
    pop     cx              ; Restore original count
    ret
```

**Explanation**\
The routine expects the DS:SI registers to point to the start of the array.

The count of elements in the array should be in the CX register.\
**Outer Loop:**

The outer loop runs n-1 times (n is the number of elements), since a single pass ensures the largest unsorted element bubbles to its correct position.\
Inner Loop:

**Adjacent elements are compared.**

If a pair is out of order (the current element is greater than the next), they are swapped.

The xchg instruction is used for swapping values.\
Pointers and Counters:

DI serves as a pointer to the current element in the array.

BX keeps track of the number of comparisons per pass, which decreases as the largest elements settle into their final positions.

**Notes**

* This implementation assumes the array consists of 16-bit integers (WORD size). Adjustments are necessary for different data sizes.
* Ensure that the segment registers (DS) are correctly set to point to the data segment containing the array before calling this routine.
* The xchg instruction is used here , but depending on your assembler, you might need to implement the swap manually using mov instructions.

***

# References

* [8088 on Wikipedia](https://en.wikipedia.org/wiki/Intel_8088)
* [Intel 8088 Instruction Set](https://en.wikipedia.org/wiki/X86_instruction_listings)
* [IBM PC and 8088 History](https://www.pcjs.org/documents/manuals/8088/)
