---
title: Intro to Cross-Process IPC Communication in Linux
description: Just a quick overview of the options...
slug: IPC-in-linux
date: 2020-07-04
image: post/Articles/IMAGES/linuxipcwide.jpg
categories:
  - Linux
  - Inter-Process Communications
tags:
  - Linux
  - IPC
  - Pipes
  - Message
  - Queues
  - Shared
  - Sockets
  - D-Bus
  - Netlink
  - Unix
  - C
  - CPP
  - SharedMemoryIPC
draft: false
weight: 49
categories_ref:
  - Linux
  - Inter-Process Communications
slug_calculated: https://brianbraatz.github.io/p/IPC-in-linux
lastmod: 2025-03-14T16:40:26.800Z
---
## Introduction

Ah, Linux—a world where processes are like introverted teenagers, each doing their own thing, barely acknowledging each other's existence.

But sometimes, even these lone wolves need to chat, share secrets, or coordinate their actions. Enter

Inter-Process Communication (IPC), the unsung hero that makes these interactions possible.

## The Early Days: Pipes and FIFOs

In the beginning, there were **pipes**.

Not the kind you smoke, but the kind that let one process whisper sweet nothings into another's ear.

Think of pipes as the tin-can telephones of the Linux world—simple, direct, and only working if both parties are paying attention.

```c
#include <unistd.h>
#include <stdio.h>

int main() {
    int fd[2];
    pipe(fd);
    if (fork() == 0) {
        // Child process
        close(fd[0]);
        write(fd[1], "Hello, parent!", 14);
        close(fd[1]);
    } else {
        // Parent process
        char buffer[15];
        close(fd[1]);
        read(fd[0], buffer, 14);
        buffer[14] = '\0';
        printf("%s\n", buffer);
        close(fd[0]);
    }
    return 0;
}
```

But pipes had their limitations—they were anonymous and only worked between related processes.

Enter **FIFOs** (named pipes), the extroverted cousins of pipes.

FIFOs had names and didn't care about parent-child relationships, allowing any process to join the conversation.

```c
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>

int main() {
    mkfifo("/tmp/myfifo", 0666);
    if (fork() == 0) {
        // Child process
        int fd = open("/tmp/myfifo", O_WRONLY);
        write(fd, "Hello, FIFO!", 12);
        close(fd);
    } else {
        // Parent process
        char buffer[13];
        int fd = open("/tmp/myfifo", O_RDONLY);
        read(fd, buffer, 12);
        buffer[12] = '\0';
        printf("%s\n", buffer);
        close(fd);
    }
    unlink("/tmp/myfifo");
    return 0;
}
```

## Message Queues: The Post Office of IPC

For those who thought pipes were too primitive, Linux introduced **message queues**, which work like the postal service for processes.

```c
#include <sys/msg.h>
#include <stdio.h>
#include <string.h>

struct msg_buffer {
    long msg_type;
    char msg_text[100];
};

int main() {
    key_t key = ftok("progfile", 65);
    int msgid = msgget(key, 0666 | IPC_CREAT);
    struct msg_buffer message;
    message.msg_type = 1;
    strcpy(message.msg_text, "Hello, Message Queue!");
    msgsnd(msgid, &message, sizeof(message), 0);
    msgrcv(msgid, &message, sizeof(message), 1, 0);
    printf("%s\n", message.msg_text);
    msgctl(msgid, IPC_RMID, NULL);
    return 0;
}
```

## Shared Memory: The Communal Bulletin Board

For processes that needed to share large amounts of data quickly, **shared memory** became the go-to method.

```c
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>
#include <string.h>

int main() {
    key_t key = ftok("shmfile", 65);
    int shmid = shmget(key, 1024, 0666 | IPC_CREAT);
    char *str = (char*) shmat(shmid, (void*)0, 0);
    strcpy(str, "Hello, Shared Memory!");
    printf("%s\n", str);
    shmdt(str);
    shmctl(shmid, IPC_RMID, NULL);
    return 0;
}
```

## Sockets: The Network Socialites

For processes that needed to communicate over networks, **sockets** came into play.

```c
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>

int main() {
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in servaddr;
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = INADDR_ANY;
    servaddr.sin_port = htons(8080);
    bind(sockfd, (struct sockaddr*)&servaddr, sizeof(servaddr));
    listen(sockfd, 5);
    int connfd = accept(sockfd, (struct sockaddr*)NULL, NULL);
    char buffer[1024] = {0};
    read(connfd, buffer, 1024);
    printf("%s\n", buffer);
    close(connfd);
    close(sockfd);
    return 0;
}
```

## D-Bus: The Butler of Desktop IPC

As desktop environments became more complex, there was a need for a more organized communication system. Enter **D-Bus**, the polite butler that ensures messages are delivered to the right recipients in desktop applications.

more on wikipedia:

https://en.wikipedia.org/wiki/D-Bus\
"\
**D-Bus** (short for "**Desktop Bus**"[\[4\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-4)) is a [message-oriented middleware](https://en.wikipedia.org/wiki/Message-oriented_middleware "Message-oriented middleware") mechanism that allows communication between multiple [processes](https://en.wikipedia.org/wiki/Process_\(computing\) "Process (computing)") running concurrently on the same machine.[\[5\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-intro_dbus-5)[\[6\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-Cocagne_2012-6) D-Bus was developed as part of the [freedesktop.org](https://en.wikipedia.org/wiki/Freedesktop.org "Freedesktop.org") project, initiated by [GNOME](https://en.wikipedia.org/wiki/GNOME "GNOME") developer [Havoc Pennington](https://en.wikipedia.org/wiki/Havoc_Pennington "Havoc Pennington") to standardize services provided by [Linux](https://en.wikipedia.org/wiki/Linux "Linux") [desktop environments](https://en.wikipedia.org/wiki/Desktop_environment "Desktop environment") such as [GNOME](https://en.wikipedia.org/wiki/GNOME "GNOME") and [KDE](https://en.wikipedia.org/wiki/KDE "KDE").[\[7\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-intro_dbus_q1-7)[\[8\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-Palmieri_2005-8)\[*[dead link](https://en.wikipedia.org/wiki/Wikipedia:Link_rot "Wikipedia:Link rot")*]

D-Bus is an [inter-process communication](https://en.wikipedia.org/wiki/Inter-process_communication "Inter-process communication") (IPC) mechanism initially designed to replace the [software component](https://en.wikipedia.org/wiki/Software_component "Software component") communications systems used by the [GNOME](https://en.wikipedia.org/wiki/GNOME "GNOME") and [KDE](https://en.wikipedia.org/wiki/KDE "KDE") Linux [desktop environments](https://en.wikipedia.org/wiki/Desktop_environment "Desktop environment") ([CORBA](https://en.wikipedia.org/wiki/CORBA "CORBA") and [DCOP](https://en.wikipedia.org/wiki/Desktop_communication_protocol "Desktop communication protocol") respectively).[\[13\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-dbus_tut_q1-13)[\[14\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-intro_dbus_q2-14) The components of these desktop environments are normally distributed in many processes, each one providing only a few—usually one—*services*. These services may be used by regular client [applications](https://en.wikipedia.org/wiki/Application_software "Application software") or by other components of the desktop environment to perform their tasks.[\[15\]](https://en.wikipedia.org/wiki/D-Bus#cite_note-15)

"

<!-- 
## Conclusion

From pipes to sockets, Linux has a wealth of IPC mechanisms, each with its quirks and specialties. Whether you’re passing notes through pipes, mailing letters via message queues, or shouting across the network with sockets, Linux has you covered. So the next time you get frustrated debugging an IPC issue, just remember—at least you're not writing assembly!
-->

## References

* [Linux IPC Overview](https://man7.org/linux/man-pages/man7/ipc.7.html)
* [Introduction to Linux IPC](https://tldp.org/LDP/lpg/node7.html)
* [Message Queues and Shared Memory](https://man7.org/linux/man-pages/man2/msgget.2.html)
* [Sockets Programming](https://beej.us/guide/bgipc/)
