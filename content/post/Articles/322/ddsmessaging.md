---
title: Data Distribution Service (DDS) in a Nutshell
description: Exploring this DOD Popular Messaging System
slug: dds-in-a-nutshell
date: 2017-05-12
image: post/Articles/IMAGES/dds.png
categories:
  - Messaging
  - Middleware
  - Distributed Systems
  - DoD
  - Cloud
  - CPP
  - C++
tags:
  - DDS
  - RTI Connext
  - Middleware
  - Messaging
  - Real-Time
draft: false
weight: 571
lastmod: 2025-03-23T00:08:01.405Z
---
# Data Distribution Service (DDS) in a Nutshell

Let’s talk about **DDS** — and no, we’re not talking about a dental degree here. We’re diving into the **Data Distribution Service**, the middleware messaging system that quietly powers a lot of the high-stakes tech you *never* hear about in the news.

If you’re building military systems, autonomous vehicles, aerospace comms, or any real-time distributed system where *latency isn’t just a number but a matter of life and death* — then DDS might already be your best friend (or soon will be).

***

## 🧠 What the Heck Is DDS?

DDS stands for **Data Distribution Service** for Real-Time Systems. It's an OMG (Object Management Group) standard for **real-time publish/subscribe communication**.

Think of it like a data delivery ninja:\
📦 Data gets published\
🎯 Subscribers get it instantly\
⚡ No polling, no REST APIs, no middlemen

It's peer-to-peer, decentralized, and built to be **ultra-fast and ultra-reliable**.

***

## 🚀 Key Features That Make DDS Shine

### 🔄 Publish/Subscribe Model

Publishers write data to a topic.\
Subscribers get updates **only if they care** about that topic.

No hand-holding or coordination between sender and receiver needed. It’s **data-centric**, not message-centric — this means you're publishing *state*, not just sending messages.

***

### ⏱️ Real-Time Communication

DDS is optimized for **hard** and **soft real-time** systems. You can define **Quality of Service (QoS)** policies like:

* Deadline (e.g. data every 10 ms)
* Latency budget
* Reliability (best-effort or guaranteed delivery)
* Lifespan (data expiration)
* Ownership (who controls a data topic)

These QoS settings allow fine-tuned performance, reliability, and determinism that’s impossible with general-purpose messaging systems like MQTT or Kafka.

***

### 🧱 Decentralized Architecture

No central broker. No single point of failure. DDS participants discover each other dynamically via **discovery protocols**.

This makes it **robust** and **scalable** — perfect for environments where connections come and go, like:

* UAV swarms
* Shipboard systems
* Mobile sensor networks

***

### 🔐 Built-in Security

DDS Security is an OMG standard extension that adds:

* Authentication
* Authorization
* Encryption
* Access control
* Logging

Because, you know, **“combat drone sends telemetry to wrong subscriber”** isn’t exactly ideal.

***

## 🛠️ Common DDS Implementations

There are multiple vendors, open-source and commercial, that provide DDS implementations:

| Vendor           | Product        | Notes                                 |
| ---------------- | -------------- | ------------------------------------- |
| RTI              | Connext DDS    | Most widely used in aerospace/defense |
| eProsima         | Fast DDS       | Open-source, used in ROS 2            |
| ADLINK           | OpenSplice DDS | Longtime player in industrial systems |
| Object Computing | OpenDDS        | Open-source, C++ focused              |

***

## 🛰️ DDS in the Wild: Where It’s Used

DDS isn’t a household name — but it **runs behind the scenes** in systems where failure isn’t an option:

* **DoD weapon systems** (e.g., Aegis Combat System, missile defense)
* **Avionics and flight control systems**
* **Medical devices**
* **Autonomous vehicles** (e.g., with ROS 2 using Fast DDS)
* **Spacecraft telemetry**
* **Industrial control and SCADA systems**

Basically, if your software can blow up, crash, or save lives, DDS might be your middleware.

***

## 🧪 How DDS Works — In Simple Terms

Let’s walk through a scenario.

### 🎓 Say Hello to Topics

Imagine a topic called `TargetLocation`.

* A radar system publishes updates to `TargetLocation`.
* A missile guidance system subscribes to `TargetLocation`.

DDS automatically:

* Finds the publisher and subscriber
* Matches QoS settings
* Starts the data flow
* Delivers updates as fast as the wire will allow

No explicit connection, no central registry, no polling.

***

### ⚙️ The DDS Stack

The DDS architecture is layered like an onion (but less likely to make you cry):

1. **DCPS** (Data-Centric Publish-Subscribe): The core API.
2. **DDS RTPS** (Real-Time Publish-Subscribe protocol): Wire protocol for interop.
3. **DDS Security**: Optional, layered on top.

It’s all about modularity and portability — and yes, you can use it across languages like C++, C, Java, Python, and even Rust (with some community help).

***

## 🧩 DDS vs. Other Messaging Systems

| Feature               | DDS            | MQTT       | Kafka      | ZeroMQ          |
| --------------------- | -------------- | ---------- | ---------- | --------------- |
| Real-Time             | ✅ Yes          | ⚠️ Limited | ❌ No       | ✅ Yes           |
| Peer-to-Peer          | ✅ Yes          | ❌ No       | ❌ No       | ✅ Yes           |
| QoS Control           | ✅ Fine-grained | ❌ Basic    | ❌ None     | ⚠️ Manual       |
| Central Broker Needed | ❌ No           | ✅ Yes      | ✅ Yes      | ❌ No            |
| Built-in Discovery    | ✅ Yes          | ❌ No       | ❌ No       | ⚠️ Partial      |
| Secure by Design      | ✅ DDS-Sec      | ⚠️ Varies  | ❌ Plug-ins | ❌ Roll-your-own |

***

## 💡 When Should You Use DDS?

### DDS is great for:

* Real-time control systems
* Safety-critical systems
* Systems requiring fine-grained QoS
* Distributed systems without a broker

### DDS might be overkill for:

* Simple IoT telemetry (use MQTT)
* Log aggregation (use Kafka)
* Internal RPC calls (use gRPC)

But if **reliability, latency, and scalability** are non-negotiable, DDS is hard to beat.

***

## 🧰 Getting Started with DDS

You can dive in with open-source:

* [eProsima Fast DDS](https://fast-dds.docs.eprosima.com/en/latest/)
* [OpenDDS](https://opendds.org/)

Or try commercial offerings with great tooling:

* [RTI Connext DDS](https://www.rti.com/)
* [ADLINK OpenSplice](https://www.adlinktech.com/)

Pro tip: DDS is **super configurable**, so expect to spend time tweaking QoS, topics, and discovery parameters. But once it’s tuned — it flies.

***

<!-- 
## 🧠 Final Thoughts

DDS is like the **silent guardian** of real-time distributed systems. It doesn’t get as much hype as Kafka or gRPC, but it’s **battle-tested, incredibly fast, and designed for mission-critical workloads**.

If you’re designing systems that **can’t afford to fail** — whether they fly, float, or roll — DDS is a rock-solid foundation.

---

## 🔑 Key Ideas

| Key Idea                      | Summary                            |
|------------------------------|------------------------------------|
| dds-in-a-nutshell            | DDS is a real-time pub/sub system used in defense and aerospace. |
| Messaging                    | DDS uses a brokerless, decentralized approach. |
| Real-Time                    | Designed for ultra-low-latency data delivery. |
| Middleware                   | DDS is a middleware standard, not just a library. |
| DDS                          | The core topic of this article.    |

--- -->

<!-- 
---
title: "Getting Started with DDS: Code Samples and Concepts"
description: "Getting Started with DDS: Code Samples and Concepts"
slug: dds-code-samples
date: 2016-08-27
image: "post/Articles/IMAGES/27.jpg"
categories: ["Messaging", "Middleware", "Distributed Systems", "DoD"]
tags: ["DDS", "Fast DDS", "OpenDDS", "Messaging", "RTI Connext", "C++"]
draft: false
weight: 439
--- -->

# Getting Started with DDS: Code Samples and Concepts

<!-- In our last DDS article, we broke down the basics of what makes **Data Distribution Service (DDS)** such a beast for real-time systems.

Now, let's roll up our sleeves, fire up a compiler, and get our hands dirty with **real DDS code**.

Whether you're using **eProsima Fast DDS**, **RTI Connext**, or **OpenDDS**, the core concepts are consistent thanks to the DDS spec. We'll focus on C++ here, which is the most widely supported language in DDS land. -->

***

## 🧰 Tools You’ll Need

We'll use **eProsima Fast DDS** for these examples because:

* It's open source
* Well-documented
* Actively maintained
* Used in ROS 2 and other modern systems

### 🛠️ Install Fast DDS (Ubuntu example)

```bash
sudo apt update
sudo apt install fastdds fastdds-tools
```

Or build from source if you like pain:\
👉 [Fast DDS GitHub](https://github.com/eProsima/Fast-DDS)

***

## 🧪 Basic Concepts in Code

DDS applications usually involve the following steps:

1. Create a **DomainParticipant**
2. Define a **Topic** with a data type
3. Create a **Publisher** or **Subscriber**
4. Use **DataWriter** (to publish) or **DataReader** (to subscribe)
5. Configure QoS as needed

***

## 🐣 Step 1: Define Your Data Type

DDS uses IDL (Interface Definition Language) to define data types.

Create a file called `HelloWorld.idl`:

```idl
module HelloWorldModule {
  struct HelloWorld {
    string message;
    long index;
  };
};
```

Then generate C++ code using Fast DDS tools:

```bash
fastrtpsgen -example x64Linux2.6gcc HelloWorld.idl
```

This gives you:

* Type support classes
* Example publisher/subscriber stubs

***

## 📤 Step 2: Create a Publisher

Here’s a minimal publisher using Fast DDS:

```cpp
#include "HelloWorldPubSubTypes.h"
#include "HelloWorld.h"
#include <fastdds/dds/domain/DomainParticipantFactory.hpp>
#include <fastdds/dds/publisher/DataWriter.hpp>
#include <fastdds/dds/topic/Topic.hpp>
#include <fastdds/dds/publisher/Publisher.hpp>
#include <iostream>

using namespace eprosima::fastdds::dds;

int main()
{
    DomainParticipantQos participant_qos;
    DomainParticipant* participant =
        DomainParticipantFactory::get_instance()->create_participant(0, participant_qos);

    TypeSupport type(new HelloWorldModule::HelloWorldPubSubType());
    type.register_type(participant);

    Topic* topic = participant->create_topic("HelloTopic", type.get_type_name(), TOPIC_QOS_DEFAULT);

    Publisher* publisher = participant->create_publisher(PUBLISHER_QOS_DEFAULT, nullptr);
    DataWriter* writer = publisher->create_datawriter(topic, DATAWRITER_QOS_DEFAULT, nullptr);

    HelloWorldModule::HelloWorld data;
    data.message("Hello DDS!");
    data.index = 1;

    std::cout << "Publishing: " << data.message() << std::endl;
    writer->write(&data);

    std::this_thread::sleep_for(std::chrono::seconds(1));
    DomainParticipantFactory::get_instance()->delete_participant(participant);
    return 0;
}
```

Compile and run, and voilà — you've got yourself a DDS publisher.

***

## 📥 Step 3: Create a Subscriber

```cpp
#include "HelloWorldPubSubTypes.h"
#include "HelloWorld.h"
#include <fastdds/dds/subscriber/DataReaderListener.hpp>
#include <fastdds/dds/subscriber/SampleInfo.hpp>
#include <iostream>

using namespace eprosima::fastdds::dds;

class HelloListener : public DataReaderListener
{
public:
    void on_data_available(DataReader* reader) override {
        HelloWorldModule::HelloWorld data;
        SampleInfo info;
        if (reader->take_next_sample(&data, &info) == ReturnCode_t::RETCODE_OK && info.valid_data) {
            std::cout << "Received: " << data.message() << " [" << data.index << "]" << std::endl;
        }
    }
};

int main()
{
    DomainParticipant* participant =
        DomainParticipantFactory::get_instance()->create_participant(0, PARTICIPANT_QOS_DEFAULT);

    TypeSupport type(new HelloWorldModule::HelloWorldPubSubType());
    type.register_type(participant);

    Topic* topic = participant->create_topic("HelloTopic", type.get_type_name(), TOPIC_QOS_DEFAULT);
    Subscriber* subscriber = participant->create_subscriber(SUBSCRIBER_QOS_DEFAULT, nullptr);

    HelloListener* listener = new HelloListener();
    DataReader* reader = subscriber->create_datareader(topic, DATAREADER_QOS_DEFAULT, listener);

    std::cout << "Waiting for data..." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(10));

    DomainParticipantFactory::get_instance()->delete_participant(participant);
    delete listener;
    return 0;
}
```

Run the subscriber first, then the publisher. Boom 💥 — you just made a distributed, real-time messaging system. No server, no broker, no tears.

***

## 🔧 Tweaking QoS

Want reliability? Add this to the QoS config:

```cpp
DataWriterQos writer_qos = DATAWRITER_QOS_DEFAULT;
writer_qos.reliability().kind = RELIABLE_RELIABILITY_QOS;

DataWriter* writer = publisher->create_datawriter(topic, writer_qos, nullptr);
```

Want data to expire after 5 seconds?

```cpp
writer_qos.lifespan().duration = eprosima::fastrtps::c_TimeInfinite;
```

You can tweak deadline, durability, history, liveliness, etc. DDS is basically QoS heaven.

***

## 🚁 Real Use Cases Revisited

These small examples can scale to control:

* Swarms of autonomous drones
* Real-time shipboard sensors
* Distributed radar systems
* Tactical data links

All using the same basic DDS pattern.

***

<!-- ## 🧠 Final Thoughts

DDS might seem intimidating at first, but once you wrap your head around the pattern — **it's magic**. Fast, reliable, brokerless communication that can power anything from robots to fighter jets.

If you're building **real-time systems that matter**, DDS gives you the power and control other protocols can only dream about. -->

***

## 🔑 Key Ideas

| Key Idea         | Summary                                   |
| ---------------- | ----------------------------------------- |
| dds-code-samples | DDS coding examples using Fast DDS in C++ |
| RTI Connext      | Commercial DDS implementation             |
| Fast DDS         | Open-source, used in ROS 2                |
| Messaging        | Pub/sub without a central broker          |
| Middleware       | DDS acts as powerful messaging middleware |
