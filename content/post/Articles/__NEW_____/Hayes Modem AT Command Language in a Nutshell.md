---
title: Hayes Modem AT Command Language in a Nutshell
description: Its still used today in Iot, Satellites and Cell Phones
slug: inside-the-hayes-command-set
date: 2019-06-14
image: post/Articles/IMAGES/hayes.webp
categories:
  - Modems
  - Technology
  - Networking
  - Computers
  - History
  - Embedded Systems
tags:
  - Modems
  - Technology
  - Networking
  - Computers
  - History
  - Hayes
  - AT Commands
  - Dial-up
draft: false
weight: 253
lastmod: 2025-02-16T00:45:40.883Z
---
# Inside the Hayes Command Set for Modems

Ah, modems—the magical boxes that let us scream data at each other over phone lines. Before fiber optics, before Wi-Fi, before the world decided dial-up was slow as a turtle in molasses, there was the **Hayes AT Command Set**.

Want to make your modem dance? You spoke **Hayes**!

## A Brief History of Hayes Modems

[Hayes Microcomputer Products](https://en.wikipedia.org/wiki/Hayes_Microcomputer_Products) basically **invented** the modem command set that became the industry standard. Founded in 1977, these folks figured out that users needed a way to control their modems without flipping switches like they were launching the Apollo program.

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

**Absolutely not.** They tried to sue everyone but lost. By the time they realized their **command set was being used everywhere**, it was **too late**. Companies took their idea and ran with it.

## Hayes vs. the Competitors

| Feature          | Hayes Modems   | USRobotics     | Zoom          | Lucent        |
| ---------------- | -------------- | -------------- | ------------- | ------------- |
| Command Set      | Invented it    | Copied it      | Copied it     | Copied it     |
| Reliability      | Excellent      | Great          | Cheap         | Decent        |
| Speed            | 300 baud → 56K | 300 baud → 56K | 28.8K → 56K   | 33.6K → 56K   |
| Business Outcome | Went bankrupt  | Got acquired   | Budget choice | Telecom focus |

<!-- 
## Key Ideas Table

| Concept | Summary |
|---------|---------|
| Hayes Command Set | The standard modem language invented by Hayes. |
| AT Commands | Commands like `ATD` (Dial), `ATH` (Hang up), `ATA` (Answer). |
| Modem Competitors | USRobotics, Zoom, Rockwell, Lucent copied Hayes. |
| Advanced Features | 56K modems introduced faster connection commands. |
| Legal Battles | Hayes tried to protect their commands but failed. |
| Duplex Modes | Full vs. Half-duplex, and why you saw **double characters**. |

---

---
title: "Modern Uses of the Hayes Command Set"
description: "Modern Uses of the Hayes Command Set"
slug: "modern-uses-of-the-hayes-command-set"
date: "2018-12-03"
image: "post/Articles/IMAGES/31.jpg"
categories: ["Modems", "Technology", "Networking", "Computers", "History"]
tags: ["Modems", "Technology", "Networking", "Computers", "History", "Hayes", "AT Commands", "Cellular Modems"]
draft: false
weight: 312
---
-->

# Modern Uses of the Hayes Command Set

The **Hayes AT command set** is alive and well—despite dial-up modems having gone the way of the dodo.

From **cellular modems** to **IoT devices**, AT commands continue to provide a simple way to control modems, thanks to their flexibility and ease of use.

Let’s take a look at how the **Hayes command set** evolved and where it still lurks in the digital wilds today.

## The Evolution of the Hayes Command Set

Born in the 1980s, the **Hayes AT command set** standardized modem communication.

When **dial-up** died out, you’d think AT commands would have followed. Nope!

Instead, they evolved and became the **backbone** of many modern communication protocols.

### The Evolutionary Timeline:

* **1980s**: Hayes invents the AT command set for dial-up modems.
* **1990s**: Cellular modems adopt AT commands for GSM control.
* **2000s**: IoT devices and industrial hardware start integrating AT commands.
* **2020s**: Even **5G modems** still support AT commands for control functions.

## Where Are AT Commands Used Today?

### 1. **Cellular Modems & 5G/LTE Devices**

Modern cellular modems—whether 3G, 4G, or **5G**—support **AT commands** for control and diagnostics. Examples include:

* **Quectel**, **Telit**, **Sierra Wireless**, and **Huawei** modems use AT commands to control calls, SMS, and data connections.
* `AT+CGATT=1` (Attach to a cellular network)
* `AT+CMGS="+123456789"` (Send SMS)
* `AT+CSQ` (Check signal strength)

### 2. **IoT and M2M (Machine-to-Machine) Communication**

IoT devices still rely on **Hayes-style AT commands** for communication with cellular networks. Example use cases:

* Remote monitoring devices
* Industrial automation
* GPS tracking systems

Popular IoT cellular modules using AT commands:

* **SIM800** / **SIM900** GSM modules
* **ESP32 LTE shields**
* **NB-IoT (Narrowband IoT) devices**

### 3. **Satellite Modems**

Did you know that some satellite communication systems use AT commands? Yup!

* `AT+SBDI` (Iridium satellite short-burst data transmission)
* `AT+GSN` (Retrieve satellite network ID)

### 4. **Embedded Systems & Microcontrollers**

AT commands are still a lightweight way to **control modems from microcontrollers**. Devices like:

* **Arduino + GSM shields**
* **Raspberry Pi cellular modules**
* **ESP8266 Wi-Fi modules (Some AT-based variants!)**

### 5. **USB Cellular Dongles**

You can control most USB cellular modems with AT commands via serial terminals:

* Huawei E3372
* Sierra Wireless MC7455
* ZTE MF823

Try this:

```bash
screen /dev/ttyUSB0 115200
AT+CGMI   # Get manufacturer info
AT+CGMM   # Get model name
AT+CSQ    # Signal quality
```

## The Connection Between Hayes Modems & Cellular Modems

Is there a **direct lineage** between **dial-up modems** and **cellular modems**? **Absolutely!**

* **Both use AT commands.** Cellular modems evolved from dial-up standards, keeping the **Hayes-style syntax**.
* **Both use serial interfaces.** Traditional modems used COM ports, while cellular modems use USB-to-serial bridges.
* **Both rely on status responses.** A `CONNECT` response in dial-up is like a `+COPS` (Carrier Operator) response in cellular.

## Conclusion

The **Hayes AT command set** didn’t die—it **evolved**. From **dial-up modems** to **cellular** and **IoT devices**, it remains a universal way to control network connections. If you’re tinkering with cellular modems, satellite systems, or IoT gadgets, you’re still speaking the **ancient** yet **surprisingly modern** language of Hayes!

***

## Key Ideas Table

| Concept           | Summary                                                              |
| ----------------- | -------------------------------------------------------------------- |
| Hayes Command Set | Originally for dial-up modems, now used in cellular and IoT devices. |
| Cellular Modems   | Still use AT commands for network control and diagnostics.           |
| IoT & M2M Devices | Embedded systems and industrial IoT rely on AT commands.             |
| Satellite Modems  | Some satellite systems use AT commands for communication.            |
| USB Modems        | Many 4G/5G USB modems can be controlled via AT commands.             |
| Evolution         | The command set has transitioned from dial-up to mobile networks.    |

***

## Reference Links

* [Hayes AT Command Set (Wikipedia)](https://en.wikipedia.org/wiki/Hayes_command_set)
* [GSM AT Commands (Reference)](https://www.etsi.org/deliver/etsi_ts/127000_127099/127007/14.06.00_60/ts_127007v140600p.pdf)
* [Quectel AT Command Guide](https://www.quectel.com/download/quectel-ec25-at-commands-manual/)
* [Huawei USB Modem AT Commands](https://forum.huawei.com/enterprise/en/topic/at-commands-supported-by-huawei-usb-modems/thread/669104-100027)
* [Hayes Microcomputer Products (Wikipedia)](https://en.wikipedia.org/wiki/Hayes_Microcomputer_Products)
* [AT Command Set (Wikipedia)](https://en.wikipedia.org/wiki/Hayes_command_set)
* [Dial-up Modems (Wikipedia)](https://en.wikipedia.org/wiki/Dial-up_Internet_access)
* [56K Modems (Wikipedia)](https://en.wikipedia.org/wiki/56k_modem)
* [AT Command List (Reference)](https://www.mikroe.com/at-command-reference)
