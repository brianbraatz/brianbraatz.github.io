---
title: 8051 Assembly Language in a nutshell
description: 8051 Assembly Language in a nutshell
slug: 8051-assembly-language-in-a-nutshell
date: 2012-11-06
image: post/Articles/IMAGES/8051wide.jpg
categories:
  - Assembly Language
  - Embedded
  - 8051 Embedded Processor
  - Languages
  - History
tags:
  - Assembly
  - Microcontroller
  - RS232
  - Embedded
  - Interrupts
  - Microprocessors
  - AssemblyLanguage
  - EmbeddedSystems
  - 8051CPU
draft: false
weight: 210
categories_ref:
  - Assembly Language
  - Embedded
  - 8051 Embedded Processor
  - Languages
  - History
slug_calculated: https://brianbraatz.github.io/p/8051-assembly-language-in-a-nutshell
lastmod: 2025-03-14T16:40:19.972Z
---
# 8051 Assembly Language in a Nutshell

## The Glorious History of the 8051

Back in the 1980s, when people still thought fax machines were cool, Intel dropped the 8051 microcontroller. It wasn’t just a CPU; it was an all-in-one wonder chip—processor, RAM, ROM, timers, and I/O all bundled together.

Unlike fancy CPUs that evolved into the beasts we have today, the 8051 stayed simple and reliable.

Why? Because embedded systems don’t need a quad-core monster running at 5GHz.

They need something cheap, low-power, and predictable.

That’s why the 8051 became the go-to microcontroller for everything from washing machines to industrial robots.

Even today, variants of the 8051 are still in production. Why fix what isn’t broken?

## 8051 Architecture - What’s Inside?

The 8051 is a tiny but mighty beast. Here’s what it packs under the hood:

* **RAM:** 128 bytes (Yeah, you read that right. Bytes. Not megabytes, not gigabytes. Just bytes.)
* **Registers:** 32 general-purpose registers (organized into 4 banks)
* **ROM:** 4 KB (for program storage)
* **Timers:** 2 (Because one is never enough)
* **Serial Port:** RS-232 (No HDMI, no VGA, just old-school serial communication)
* **I/O Ports:** 4 (Each with 8-bit registers for easy control)

By modern standards, this thing is prehistoric, but back in the day, it was the Swiss Army knife of embedded computing.

## NO UI!

You want to see something on a screen? Tough luck. The 8051 doesn’t do video, audio, or fancy graphical interfaces. If you want to display something, you have to send it over an RS-232 serial port:

<https://en.wikipedia.org/wiki/RS-232>

And that’s it. You either hook it up to another computer or use an LED to blink out Morse code.

## 8051 Instruction Set

Here’s the full list of instructions supported by the 8051:

| Mnemonic | Description                    |
| -------- | ------------------------------ |
| MOV      | Move data                      |
| ADD      | Add                            |
| SUBB     | Subtract with borrow           |
| INC      | Increment                      |
| DEC      | Decrement                      |
| MUL      | Multiply                       |
| DIV      | Divide                         |
| ANL      | AND operation                  |
| ORL      | OR operation                   |
| XRL      | XOR operation                  |
| CJNE     | Compare and jump if not equal  |
| DJNZ     | Decrement and jump if not zero |
| CLR      | Clear register/bit             |
| SETB     | Set bit                        |
| CPL      | Complement bit                 |
| JMP      | Jump                           |
| CALL     | Call subroutine                |
| RET      | Return from subroutine         |
| NOP      | No operation (do nothing)      |

## Example Code: Blinking an LED Using RS-232

```assembly
MOV TMOD, #20H   ; Set timer 1 to mode 2
MOV TH1, #0FDH   ; Set baud rate to 9600
MOV SCON, #50H   ; Configure serial mode
SETB TR1         ; Start timer
AGAIN: MOV SBUF, #'H'
        JNB TI, $   ; Wait for transmit complete
        CLR TI      ; Clear TI flag
        SJMP AGAIN  ; Loop forever
```

This sends the letter 'H' endlessly over RS-232. Not quite a GUI, but hey, it works!

## Bubble Sort

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

## Clock Interrupts - Making It Do Stuff on a Schedule

Interrupts let you run code at specific intervals. Think of it as setting an alarm clock inside your microcontroller:

```assembly
ORG 000BH         ; Timer 0 interrupt vector
CLR TF0           ; Clear overflow flag
RETI             ; Return from interrupt

ORG 0030H        ; Main program start
SETB EA          ; Enable interrupts
SETB ET0         ; Enable Timer 0 interrupt
MOV TMOD, #01H   ; Timer 0 in mode 1
MOV TH0, #0FCH   ; Timer reload value
MOV TL0, #018H
SETB TR0         ; Start timer
HERE: SJMP HERE  ; Endless loop
```

This makes the 8051 do something every time the timer overflows.

## The 8051 vs. Modern Microcontrollers

Today, if you want to mess around with embedded systems, you’ll probably grab an Arduino. But in the late '90s, the 8051 was king. Here’s how it stacks up against the modern ATmega328P (used in Arduinos):

![CPU Speed & Power Chart](https://upload.wikimedia.org/wikipedia/commons/4/4e/Intel_MCS-51.jpg)

| Feature     | 8051      | ATmega328P (Arduino) |
| ----------- | --------- | -------------------- |
| Speed       | 12 MHz    | 16 MHz               |
| RAM         | 128 Bytes | 2 KB                 |
| Flash ROM   | 4 KB      | 32 KB                |
| Power Usage | 50 mW     | 15 mW                |

ITS PRETTY FUNNY - when i looked up the info for the chart above.. I was blown away at the power consumption of ATmega328P being **ONLY 15mw**!!!

There is something to be said for modern abilities to make microprocessors.

## Summary of Key Ideas

| Concept      | Explanation                                 |
| ------------ | ------------------------------------------- |
| 8051 History | Released in the 1980s, still relevant today |
| Architecture | Simple but effective, minimal RAM/ROM       |
| No UI        | Uses RS-232 for output                      |
| Instructions | Basic operations like MOV, ADD, and JMP     |
| Interrupts   | Used for scheduling tasks                   |
| Comparison   | 8051 vs Arduino - old vs. new               |

## References

* [8051 Microcontroller (Wikipedia)](https://en.wikipedia.org/wiki/Intel_MCS-51)
* [RS-232 Standard (Wikipedia)](https://en.wikipedia.org/wiki/RS-232)
* [ATmega328P Datasheet](https://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-42735-8-bit-AVR-Microcontroller-ATmega328-328P_summary.pdf)
