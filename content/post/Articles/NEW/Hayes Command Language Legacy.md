---
title: Hayes Command Set Legacy
description: Is the Hayes Command Set still used?
slug: hayes-command-set-legacy
date: 2019-06-14
image: post/Articles/IMAGES/hayes.webp
categories:
  - Modems
  - Technology
  - Networking
  - Computers
  - History
  - Nutshell
tags:
  - Modems
  - Technology
  - Networking
  - Computers
  - History
  - Hayes
  - AT
  - Commands
  - Dial-up
draft: false
weight: 453
categories_ref:
  - Modems
  - Technology
  - Networking
  - Computers
  - History
  - Nutshell
lastmod: 2025-03-14T15:45:11.563Z
---
# Inside the Hayes Command Set for Modems

Ah, modems—the magical boxes that let us scream data at each other over phone lines.

Before fiber optics, before Wi-Fi, before the world decided dial-up was slow as a turtle in molasses, there was the **Hayes AT Command Set**.

Want to make your modem dance? You spoke **Hayes**!

## A Brief History of Hayes Modems

[Hayes Microcomputer Products](https://en.wikipedia.org/wiki/Hayes_Microcomputer_Products) basically **invented** the modem command set that became the industry standard.

Founded in 1977, these folks figured out that users needed a way to control their modems without flipping switches like they were launching the Apollo program.

Thus, the **AT command set** was born.

### The Syntax of Hayes Commands

| Command     | Description                                        |
| ----------- | -------------------------------------------------- |
| `AT`        | Attention—lets the modem know a command is coming. |
| `ATD`       | Dial a number.                                     |
| `ATA`       | Answer an incoming call.                           |
| `ATH`       | Hang up.                                           |
| `ATO`       | Return online from command mode.                   |
| `ATZ`       | Reset the modem.                                   |
| `ATS0=1`    | Auto-answer after one ring.                        |
| `AT&F`      | Restore factory defaults.                          |
| `AT+MS=V34` | Force V.34 mode.                                   |
| `AT+GCI=XX` | Set country code.                                  |

And so much more!

## Things You Can Do with Hayes Commands

* **Dial a number** (`ATD555-1234`)
* **Answer a call** (`ATA`)
* **Hang up** (`ATH`)
* **Check connection speed** (`ATI`)
* **Set speaker volume** (`ATL1` to `ATL3`)
* **Enable/Disable auto-answer** (`ATS0=1` or `ATS0=0`)
* **Configure flow control** (`AT&K3`)
* **Check signal strength** (`AT+CSQ`)
* **Force a protocol (V.32, V.34, V.90, V.92, etc.)**

## Weird and Unusual Hayes Commands

Some modems let you:

* **Play DTMF tones** (`AT+VTS=3`)
* **Switch to fax mode** (`AT+FCLASS=1`)
* **Enable voice mode** (`AT+VSM`)
* **Send SMS (on GSM modems)** (`AT+CMGS`)
* **Spy on a phone line** (Not that we’d recommend it!)

## Duplex & the "Seeing Double" Issue

Ever seen double characters when typing on an old modem? That’s **half-duplex** mode for you!

* **Full-duplex** = Sends and receives simultaneously. No echoes.
* **Half-duplex** = One direction at a time. You see your own typing echoed back.

If your **cable was wrong** or your settings were borked, everything you typed **echoed back like a ghost**.

## Advanced Hayes Commands & 56K Modems

When **56K modems** became a thing (V.90, V.92), you got new commands:

* `AT+MS=V90,1` (Force V.90 mode)
* `AT+PQC=1` (Enable Quick Connect)
* `AT+PCW=1` (Enable modem-on-hold)

These **allowed faster connections** and **modem-on-hold**, which let you answer a call **without dropping your internet**.

## Other Modems That Used the Hayes Set

The **Hayes AT command set** was so good that **every modem manufacturer stole it**:

* **USRobotics** (they added proprietary tricks)
* **Zoom** (cheap but used the same commands)
* **Rockwell/Conexant chipsets** (in everything)
* **3Com** (because USR got bought by them)
* **Lucent/Agere** (yes, they had modems too)

## Did Hayes Like This?

**Absolutely not.**

They tried to sue everyone but lost. By the time they realized their **command set was being used everywhere**, it was **too late**.

Companies took their idea and ran with it.

## Hayes vs. the Competitors

| Feature          | Hayes Modems   | USRobotics     | Zoom          | Lucent        |
| ---------------- | -------------- | -------------- | ------------- | ------------- |
| Command Set      | Invented it    | Copied it      | Copied it     | Copied it     |
| Reliability      | Excellent      | Great          | Cheap         | Decent        |
| Speed            | 300 baud → 56K | 300 baud → 56K | 28.8K → 56K   | 33.6K → 56K   |
| Business Outcome | Went bankrupt  | Got acquired   | Budget choice | Telecom focus |

## Conclusion

The **Hayes Command Set** was **the modem language of the world**. It was simple, powerful, and easy to steal (oops, I mean **adopt**).

Even **modern LTE and 5G modems** still use **AT commands** today.

***

## Key Ideas Table

| Concept           | Summary                                                      |
| ----------------- | ------------------------------------------------------------ |
| Hayes Command Set | The standard modem language invented by Hayes.               |
| AT Commands       | Commands like `ATD` (Dial), `ATH` (Hang up), `ATA` (Answer). |
| Modem Competitors | USRobotics, Zoom, Rockwell, Lucent copied Hayes.             |
| Advanced Features | 56K modems introduced faster connection commands.            |
| Legal Battles     | Hayes tried to protect their commands but failed.            |
| Duplex Modes      | Full vs. Half-duplex, and why you saw **double characters**. |

***

## Reference Links

* [Hayes Microcomputer Products (Wikipedia)](https://en.wikipedia.org/wiki/Hayes_Microcomputer_Products)
* [AT Command Set (Wikipedia)](https://en.wikipedia.org/wiki/Hayes_command_set)
* [Dial-up Modems (Wikipedia)](https://en.wikipedia.org/wiki/Dial-up_Internet_access)
* [56K Modems (Wikipedia)](https://en.wikipedia.org/wiki/56k_modem)
* [AT Command List (Reference)](https://www.mikroe.com/at-command-reference)
