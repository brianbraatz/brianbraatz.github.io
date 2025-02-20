---
title: 6502 Assembly Language in a Nutshell
description: 6502 Assembly Language in a Nutshell
slug: 6502-assembly-language-in-a-nutshell
date: 2019-11-27
image: post/Articles/IMAGES/6502.jpg
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
draft: false
weight: 274
lastmod: 2025-02-20T11:35:08.079Z
---
# 6502 Assembly Language in a Nutshell

## The Glorious History of the 6502

Ah, the **1970s**—a time of disco, bell-bottoms, and the birth of the **MOS Technology 6502**.\
Released in **1975**, this **8-bit microprocessor** powered iconic systems like the **Apple II**, **Commodore 64**, **Atari 2600**, and even the **Nintendo Entertainment System (NES)**.

The **6502 was revolutionary** because it was **cheap, simple, and fast**. Compared to its competitors like the **Motorola 6800** and **Intel 8080**, the 6502 punched above its weight. Even today, it's beloved in retro computing, classic game emulation, and embedded systems.

***

## 6502 Architecture - What’s Inside?

The **6502 is a simple but powerful processor** with the following components:

* **Registers**:
  * **Accumulator (A)**: The main register for arithmetic and logic operations.
  * **Index Registers (X and Y)**: Used for addressing and loop counters.
  * **Stack Pointer (SP)**: Keeps track of the call stack.
  * **Program Counter (PC)**: Points to the next instruction.
  * **Status Register (P)**: Holds flags that indicate operation results.

* **Memory Addressing**:
  * **16-bit address bus**, allowing access to **64 KB of memory**.

* **Clock Speed**:
  * Typically around **1 MHz**, but surprisingly efficient for its time!

***

## 6502 Instruction Set

| Mnemonic | Description                 |
| -------- | --------------------------- |
| ADC      | Add with Carry              |
| AND      | Logical AND                 |
| ASL      | Arithmetic Shift Left       |
| BCC      | Branch if Carry Clear       |
| BCS      | Branch if Carry Set         |
| BEQ      | Branch if Equal             |
| BIT      | Bit Test                    |
| BMI      | Branch if Minus             |
| BNE      | Branch if Not Equal         |
| BPL      | Branch if Positive          |
| BRK      | Force Interrupt             |
| BVC      | Branch if Overflow Clear    |
| BVS      | Branch if Overflow Set      |
| CLC      | Clear Carry Flag            |
| CLD      | Clear Decimal Mode          |
| CLI      | Clear Interrupt Disable     |
| CLV      | Clear Overflow Flag         |
| CMP      | Compare Accumulator         |
| CPX      | Compare X Register          |
| CPY      | Compare Y Register          |
| DEC      | Decrement Memory            |
| DEX      | Decrement X Register        |
| DEY      | Decrement Y Register        |
| EOR      | Exclusive OR                |
| INC      | Increment Memory            |
| INX      | Increment X Register        |
| INY      | Increment Y Register        |
| JMP      | Jump                        |
| JSR      | Jump to Subroutine          |
| LDA      | Load Accumulator            |
| LDX      | Load X Register             |
| LDY      | Load Y Register             |
| LSR      | Logical Shift Right         |
| NOP      | No Operation                |
| ORA      | Logical Inclusive OR        |
| PHA      | Push Accumulator            |
| PHP      | Push Processor Status       |
| PLA      | Pull Accumulator            |
| PLP      | Pull Processor Status       |
| ROL      | Rotate Left                 |
| ROR      | Rotate Right                |
| RTI      | Return from Interrupt       |
| RTS      | Return from Subroutine      |
| SBC      | Subtract with Carry         |
| SEC      | Set Carry Flag              |
| SED      | Set Decimal Flag            |
| SEI      | Set Interrupt Disable       |
| STA      | Store Accumulator           |
| STX      | Store X Register            |
| STY      | Store Y Register            |
| TAX      | Transfer Accumulator to X   |
| TAY      | Transfer Accumulator to Y   |
| TSX      | Transfer Stack Pointer to X |
| TXA      | Transfer X to Accumulator   |
| TXS      | Transfer X to Stack Pointer |
| TYA      | Transfer Y to Accumulator   |

***

# Example Code

## 1. Hello World over RS-232

```assembly
        LDX #$00           ; Start at the beginning of the message
        LDY #$00           ; Initialize Y register for indexing

LOOP:   LDA MESSAGE,X      ; Load the next character
        BEQ DONE           ; If zero (end of string), we're done
        JSR SEND_CHAR      ; Otherwise, send the character
        INX                ; Move to the next character
        BNE LOOP           ; Repeat until end of string

DONE:   RTS                ; Return from subroutine

SEND_CHAR:
        ; Assume the character is in A
        ; Code to send A over RS-232 goes here
        RTS                ; Return from subroutine

MESSAGE:
        .BYTE "Hello, World!", $00  ; Null-terminated string
```

<!-- 
## 2. Blinking an LED

```assembly
        LDX #$00           ; Initialize X register

BLINK:  LDA PORT           ; Load the current state of the port
        EOR #$01           ; Toggle the least significant bit
        STA PORT           ; Store it back to the port
        JSR DELAY          ; Wait for a bit
        JMP BLINK          ; Repeat forever

DELAY:
        ; Simple delay loop
        LDX #$FF
DELAY_LOOP:
        DEX
        BNE DELAY_LOOP
        RTS
SORT:   
        LDY #$00           ; Outer loop index
OUTER_LOOP:
        LDX #$00           ; Inner loop index
        LDA #$00           ; Flag to check if swapped
        STA SWAPPED

INNER_LOOP:
        LDA ARRAY,X        ; Load current element
        CMP ARRAY+1,X      ; Compare with next element
        BCC NO_SWAP        ; If already sorted, skip swap

        ; Swap elements
        LDA ARRAY,X
        PHA
        LDA ARRAY+1,X
        STA ARRAY,X
        PLA
        STA ARRAY+1,X

        LDA #$01
        STA SWAPPED        ; Mark that we swapped

NO_SWAP:
        INX
        CPX #ARRAY_LENGTH-1
        BNE INNER_LOOP

        LDA SWAPPED
        BEQ DONE_SORTING

        INY
        CPY #ARRAY_LENGTH-1
        BNE OUTER_LOOP

DONE_SORTING:
        RTS

ARRAY:
        .BYTE 5, 3, 8, 4, 2, 7
ARRAY_LENGTH = 6
SWAPPED:
        .BYTE 0
```
-->

***

## 2. Bubble Sort

```assembly
SORT:   
        LDY #$00           ; Outer loop index
OUTER_LOOP:
        LDX #$00           ; Inner loop index
        LDA #$00           ; Flag to check if swapped
        STA SWAPPED

INNER_LOOP:
        LDA ARRAY,X        ; Load current element
        CMP ARRAY+1,X      ; Compare with next element
        BCC NO_SWAP        ; If already sorted, skip swap

        ; Swap elements
        LDA ARRAY,X
        PHA
        LDA ARRAY+1,X
        STA ARRAY,X
        PLA
        STA ARRAY+1,X

        LDA #$01
        STA SWAPPED        ; Mark that we swapped

NO_SWAP:
        INX
        CPX #ARRAY_LENGTH-1
        BNE INNER_LOOP

        LDA SWAPPED
        BEQ DONE_SORTING

        INY
        CPY #ARRAY_LENGTH-1
        BNE OUTER_LOOP

DONE_SORTING:
        RTS

ARRAY:
        .BYTE 5, 3, 8, 4, 2, 7
ARRAY_LENGTH = 6
SWAPPED:
        .BYTE 0
```

# References

* [6502 on Wikipedia](https://en.wikipedia.org/wiki/MOS_Technology_6502)
* [6502 Assembly Programming Guide](http://6502.org/)
* [6502 Instruction Set Reference](http://www.6502.org/tutorials/6502opcodes.html)

**Check out this uber cool 6502 Laptop!**\
<https://www.grappendorf.net/projects/6502-home-computer/>

and good video\
The 6502 CPU Powered a Whole Generation!

<https://www.youtube.com/watch?v=acUH4lWe2NQ>
