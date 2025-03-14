---
title: Adaptive Bitrate Cloud Video Streaming- Mobile
description: Demo of Mobile App -adjusts video bitrate to match available bandwidth
slug: iphone-abv-video-demo
date: 2012-03-06 00:00:00+0000
image: post/Articles/IMAGES/iphonevideostreaming.png
categories:
  - Cloud
  - VIdeo Streaming
  - Algorithms
  - Adaptive Bitrate Video Streaming
  - Mobile
  - IOS
  - iPhone
  - Objective C
  - Mac OS
  - Apple TV
  - Android
  - Xamarin
  - CSharp
  - Java
  - Network Protocols
tags:
  - DotNet
  - CSharp
  - Cross-Platform
  - Cloud
  - ObjectiveC
  - MobileApps
  - WebDevelopment
  - iPhone
  - CPP
weight: 9
categories_ref:
  - Cloud
  - VIdeo Streaming
  - Algorithms
  - Adaptive Bitrate Video Streaming
  - Mobile
  - IOS
  - iPhone
  - Objective C
  - Mac OS
  - Apple TV
  - Android
  - Xamarin
  - CSharp
  - Java
  - Network Protocols
lastmod: 2025-03-14T15:45:30.476Z
---
# Adaptive Video Bitrate Streaming

[Adaptive Bitrate Video Streaming Wikipedia](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming)

(from Wikipedia)

**Adaptive bitrate streaming** is a technique used in [streaming multimedia](https://en.wikipedia.org/wiki/Streaming_multimedia "Streaming multimedia") over [computer networks](https://en.wikipedia.org/wiki/Computer_network "Computer network").

While in the past most video or audio streaming technologies utilized streaming protocols such as [RTP](https://en.wikipedia.org/wiki/Real-time_Transport_Protocol "Real-time Transport Protocol") with [RTSP](https://en.wikipedia.org/wiki/RTSP "RTSP"), today's adaptive streaming technologies are based almost exclusively on [HTTP](https://en.wikipedia.org/wiki/HTTP "HTTP"),[\[1\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-1) and are designed to work efficiently over large distributed HTTP networks.

Adaptive bitrate streaming works by detecting a user's [bandwidth](https://en.wikipedia.org/wiki/Bandwidth_\(computing\) "Bandwidth (computing)") and [CPU](https://en.wikipedia.org/wiki/CPU "CPU") capacity in real time, adjusting the quality of the media stream accordingly.[\[2\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-2) It requires the use of an [encoder](https://en.wikipedia.org/wiki/Encode/Decode "Encode/Decode") which encodes a single source media (video or audio) at multiple [bit rates](https://en.wikipedia.org/wiki/Bit_rate "Bit rate"). The player client[\[3\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-itec-dash-3) switches between streaming the different encodings depending on available resources.[\[4\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-mobileval-4) This results in providing very little [buffering](https://en.wikipedia.org/wiki/Data_buffer "Data buffer"), faster start times and a good experience for both high-end and low-end connections.[\[5\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-5)

More specifically, adaptive bitrate streaming is a method of video streaming over HTTP where the source content is encoded at multiple bit rates. Each of the different bit rate streams are segmented into small multi-second parts.[\[6\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-dataset-6) 

The segment size can vary depending on the particular implementation, but they are typically between two and ten seconds.[\[4\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-mobileval-4)[\[6\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-dataset-6) First, the client downloads a [manifest file](https://en.wikipedia.org/wiki/Manifest_file "Manifest file") that describes the available stream segments and their respective bit rates.

During stream start-up, the client usually requests the segments from the lowest bit rate stream. If the client finds that the network throughput is greater than the bit rate of the downloaded segment, then it will request a higher bit rate segment. Later, if the client finds that the network throughput has deteriorated, it will request a lower bit rate segment.

An adaptive bitrate (ABR) algorithm in the client performs the key function of deciding which bit rate segments to download, based on the current state of the network. Several types of ABR algorithms are in commercial use: [throughput](https://en.wikipedia.org/wiki/Throughput "Throughput")-based algorithms use the throughput achieved in recent prior downloads for decision-making (e.g., throughput rule in [dash.js](https://reference.dashif.org/dash.js)), buffer-based algorithms use only the client's current buffer level (e.g., BOLA[\[7\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-7) in [dash.js](https://reference.dashif.org/dash.js)), and hybrid algorithms combine both types of information (e.g., DYNAMIC[\[8\]](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming#cite_note-8) in [dash.js](https://reference.dashif.org/dash.js)).

## Megashares Adaptive Video Bitrate Streaming Implementation

![](/post/mobile/iphone-abv-video-demo/megashares.png)\
[Wayback machine archive of Megashares.com](https://web.archive.org/web/20100304213428/http://www.megashares.com/)\
**Above is a wayback machine of the main site**\
**We were like the MySpace of filesharing :)**

Below are some videos  of me demonstrating my iphone app which was using our Adaptive bitrate streaming implementation from the users private cloud on their Megashares account.

> Whats cool about it !!!= when the bandwidth drops and grows, the video steream automatically goes up and down in bitrate to match.

In these videos I am logged into my Megashares app on the iphone, and showing the Iphone App I wrote that would adaptively stream videos off of our Megashares servers.

Megashares was a dropbox competitor, I was Director of Engineering.

I later moved this feature into our Krashdrive mobile app, as we tried to re-juvinate the business and customer model.

We made it very easy to share huge files with your friends.

And watch your files on mobile.

But, our customers liked to share copyrighted movies , alot, so it put us into a business pickle constantly with the MPAA take down notices etc...

Aside from that , this was a fun time in my past, and I think we just bumped into alot of things all file share tech companies were running into at the time.

## IPhone Video Bitrate Megashares App Demo

### Video demo of me

**NOTE These are videos demoing me running the app and showing how to stream to an Apple TV. To actually TEST this code. I drove out into the backcountry and streamed video from our servers with spotty network coverage.. It worked Great - but I couldnt seem to find any videos of me doing that test**

{{< video "Megashares Video Streaming Apple TV Demo.mp4" >}}

### Streaming to a tv via Apple TV from the Phone

{{< video "Megashares Iphone App Streaming to a TV.mp4" >}}

There was a later feature that I added which allowed Connecting to Amazon S3 Storage and accessing your Megashares files and S3 files in the same app..

And I also built an Android Version of this app.

This code was written in Objective C and the Android was with Java and the Android SDK.

After I did this, I wrote a Xamarin version - which shared code between the two projects ..

Well a little, early Versions of Xamarin had some weird project setups - where you would make symbolic links and other tricks to share files between Android and IOS..
