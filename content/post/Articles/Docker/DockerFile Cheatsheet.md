---
title: DockerFile Cheatsheet
description: 
slug: docker-file-cheatsheet
date: 2023-02-06
image: post/Articles/IMAGES/docker1.jpg
categories: 
tags:
  - Cheatsheet
  - Docker
  - DockerFile
  - WebDevelopment
weight: 10
draft: false
lastmod: 2025-01-31T00:22:01.738Z
---
### Dockerfile Cheatsheet

| **Instruction** | **Syntax/Example**                                          | **Description**                                                                                     |
| --------------- | ----------------------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| `FROM`          | `FROM ubuntu:20.04`                                         | Specifies the base image                                                                            |
| `RUN`           | `RUN apt-get update && apt-get install -y python3`          | Executes a command in the shell during the build process                                            |
| `COPY`          | `COPY . /app`                                               | Copies files from the host to the container                                                         |
| `ADD`           | `ADD https://example.com/file.tar.gz /app/`                 | Similar to `COPY` but can also retrieve remote files and unpack archives                            |
| `WORKDIR`       | `WORKDIR /app`                                              | Sets the working directory for the `RUN`, `CMD`, `ENTRYPOINT`, `COPY` and `ADD` instructions        |
| `CMD`           | `CMD ["python3", "app.py"]`                                 | Provides the default command to run the container                                                   |
| `ENTRYPOINT`    | `ENTRYPOINT ["python3", "app.py"]`                          | Configures the container to run as an executable                                                    |
| `ENV`           | `ENV APP_ENV=production`                                    | Sets environment variables                                                                          |
| `EXPOSE`        | `EXPOSE 80`                                                 | Informs Docker that the container listens on the specified network ports at runtime                 |
| `VOLUME`        | `VOLUME /data`                                              | Creates a mount point with the specified path and marks it as holding externally mounted volumes    |
| `USER`          | `USER appuser`                                              | Sets the user name or UID to use when running the image                                             |
| `LABEL`         | `LABEL version="1.0"`                                       | Adds metadata to the image                                                                          |
| `ARG`           | `ARG VERSION=1.0`                                           | Defines a variable that users can pass at build-time to the builder with the `docker build` command |
| `ONBUILD`       | `ONBUILD RUN apt-get update`                                | Adds a trigger instruction to be executed when the image is used as a base for another build        |
| `STOPSIGNAL`    | `STOPSIGNAL SIGKILL`                                        | Sets the system call signal that will be sent to the container to exit                              |
| `HEALTHCHECK`   | HEALTHCHECK --interval=30s CMD curl --fail http://localhost | Tells Docker how to test that the container is still working                                        |
