---
title: Apache Tomcat Code Snippets
description: 
slug: apache-tomcat-code-snip
date: 2017-08-15
image: post/Articles/IMAGES/apacheatomcat.png
categories:
  - Tomcat
  - Java
  - DevOps
  - Configuration
  - Cloud
tags:
  - Tomcat
  - Java
  - DevOps
  - Configuration
draft: "False"
weight: "642"
lastmod: 2025-03-03T14:10:37.785Z
---
<!-- 
## 10 Apache Tomcat Code Snippets You Need in Your Life

Ah, Apache Tomcat.

That wonderful beast that powers many of our Java web applications while simultaneously making us question our life choices when it refuses to start.

If you've ever had to wrestle with `server.xml` or wondered why your logs look like something straight out of the Matrix, this article is for you.

Let's dive into 10 essential Apache Tomcat code snippets to make your life easier (or at least marginally less painful).

--- -->

### 1.

**Starting Tomcat from the Terminal**

Because clicking on icons is for the weak.

```sh
cd /path/to/tomcat/bin
./startup.sh  # Linux/Mac
startup.bat  # Windows
```

If this doesn’t work, check your Java installation… or just start praying.

***

### 2.

**Stopping Tomcat Properly**

Because `kill -9` should not be your default debugging tool.

```sh
cd /path/to/tomcat/bin
./shutdown.sh  # Linux/Mac
shutdown.bat  # Windows
```

If Tomcat refuses to shut down, *then* you may proceed with extreme prejudice using `kill -9`.

***

### 3.

**Changing the Default HTTP Port**

Tired of port 8080?

Want to make your co-workers hate you by switching it to something else?

Edit `conf/server.xml`:

```xml
<Connector port="9090" protocol="HTTP/1.1"
           connectionTimeout="20000"
           redirectPort="8443" />
```

Congratulations, you have now successfully confused everyone who uses your server.

***

### 4.

**Enabling HTTPS on Tomcat**

Because security is not optional (even if configuring it is a pain).

```xml
<Connector port="8443" protocol="org.apache.coyote.http11.Http11NioProtocol"
           maxThreads="200"
           SSLEnabled="true">
    <SSLHostConfig>
        <Certificate certificateFile="conf/your-cert.pem"
                     certificateKeyFile="conf/your-key.pem"
                     type="RSA" />
    </SSLHostConfig>
</Connector>
```

Don't forget to restart Tomcat and sacrifice a small offering to the SSL gods.

***

### 5.

**Deploying a WAR File Manually**

Because automation is overrated.

```sh
cp myapp.war /path/to/tomcat/webapps/
```

Or, if you're fancy, use Tomcat's built-in manager (but who has time for GUIs?).

***

### 6.

**Checking Active Tomcat Processes**

For when you have no idea why Tomcat isn't responding.

```sh
ps aux | grep tomcat  # Linux/Mac
tasklist | findstr tomcat  # Windows
```

If you see multiple instances, well… good luck.

***

### 7.

**Setting Up a Basic DataSource for MySQL**

Because hardcoding database credentials is for amateurs.

```xml
<Resource name="jdbc/MyDB" auth="Container"
          type="javax.sql.DataSource" driverClassName="com.mysql.cj.jdbc.Driver"
          url="jdbc:mysql://localhost:3306/mydb"
          username="root" password="password"
          maxTotal="20" maxIdle="10" maxWaitMillis="10000" />
```

Put this in `conf/context.xml`, restart Tomcat, and hope for the best.

***

### 8.

**Rotating Logs Like a Pro**

If your `catalina.out` is bigger than your hopes and dreams, it’s time for log rotation.

Create a new logrotate config:

```sh
vi /etc/logrotate.d/tomcat
```

Add this magic:

```sh
/path/to/tomcat/logs/catalina.out {
    copytruncate
    daily
    rotate 7
    compress
    missingok
}
```

Now your logs won’t take over your hard drive (hopefully).

***

### 9.

**Setting JVM Options for Better Performance**

Tomcat loves memory, so give it what it wants.

Edit `setenv.sh` or `setenv.bat`:

```sh
export CATALINA_OPTS="-Xms512m -Xmx2g -XX:+UseG1GC"
```

Because a well-fed Tomcat is a happy Tomcat.

***

### 10.

**Reloading Context Without Restarting Tomcat**

Because sometimes, you just need a quick refresh.

```sh
touch /path/to/tomcat/webapps/myapp/META-INF/context.xml
```

This will trigger a reload of the context without bringing the whole server down.

Magic!

✨

***

## Key Ideas

| Slug                           | Summary                                       |
| ------------------------------ | --------------------------------------------- |
| 10-apache-tomcat-code-snippets | Useful Tomcat code snippets for devs & admins |
| tomcat-https                   | How to enable HTTPS on Tomcat                 |
| tomcat-memory-config           | Setting JVM memory options for performance    |
| tomcat-log-rotation            | Keeping Tomcat logs under control             |
| tomcat-deployment              | Deploying WAR files manually                  |

***

## References

* [Official Apache Tomcat Documentation](https://tomcat.apache.org/)
* [Tomcat Configuration Guide](https://tomcat.apache.org/tomcat-9.0-doc/config/)
* [Log Rotation Best Practices](https://www.baeldung.com/linux/logrotate)
* [Tomcat Memory Tuning](https://www.javacodegeeks.com/2013/03/tuning-jvm-tomcat-memory-settings.html)

***
