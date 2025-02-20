---
title: Java JNI in a Nutshell
description: Hook your Java Code  up to other languages!
slug: java-jni
date: 2021-11-17
image: post/Articles/IMAGES/javajni.png
categories:
  - Java
  - CPP
  - Assembly Language
tags:
  - Java
  - Jni
  - Jni
  - Java
  - Native
  - Code
  - C
  - Programming
  - Interoperability
  - Modern
  - Languages
  - Syntax
  - Comparison
  - CPP
draft: false
weight: 490
lastmod: 2025-02-20T14:10:35.390Z
---
# Java JNI in Detail: History, Motivation, Relationship to Modern Languages, and 10 Code Examples

## Introduction

Ever found yourself writing Java and thinking, *"Man, I wish I could just call this C function directly!"*?

Well, say hello to **Java Native Interface (JNI)**! JNI is Java's **built-in mechanism for interacting with native code written in C, C++, or even assembly**. It's **a bridge between Java and the raw power of lower-level languages**.

<!--
In this article, we'll cover:  

- The **history and motivation** behind JNI.  
- How it compares to **modern interoperability approaches**.  
- **10 real JNI code examples**.  
- A **table comparing JNI syntax to other language interoperability mechanisms**.  
-->

***

## The History of JNI

JNI was introduced with **Java 1.1 (1997)** as a way to allow Java programs to **call native C/C++ functions**.

### **Why Was JNI Created?**

* Java runs in a **managed environment (JVM)**, which means it can't directly access **low-level system resources**.
* Many **existing libraries** were written in **C and C++**, and developers wanted a way to **reuse that code**.
* Performance-critical applications (like **graphics, networking, and cryptography**) often **needed native speed**.

### **Key Innovations of JNI**

✅ **Seamless Interoperability** → Call **C and C++** functions from Java.\
✅ **Platform Independence** → Java code remains **portable**, while **native code is compiled separately**.\
✅ **Access to System APIs** → Allows Java applications to **interact with OS-specific features**.\
✅ **High-Performance Computing** → Useful for **speed-critical** applications like **game engines and image processing**.

> **Further Reading:**
>
> * [JNI Wikipedia](https://en.wikipedia.org/wiki/Java_Native_Interface)
> * [Official Java JNI Documentation](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/)

***

## JNI vs. Modern Interoperability Techniques

| Feature                       | JNI       | Modern Equivalent         |
| ----------------------------- | --------- | ------------------------- |
| **Native Code Execution**     | ✅ Yes     | ✅ Rust FFI, Python ctypes |
| **Works with C & C++**        | ✅ Yes     | ✅ Rust, C#, Python        |
| **Garbage Collection Safety** | ❌ No      | ✅ JNA, SWIG               |
| **Ease of Use**               | ❌ Complex | ✅ JNA, Python ctypes      |
| **Performance**               | ✅ Fast    | ✅ Rust FFI, C Extensions  |

💡 **Verdict:** JNI is **powerful but clunky**, best for **performance-critical** Java applications.

***

## JNI Syntax Table

| Concept                       | JNI Code (Java + C)                                                          | Equivalent in C# PInvoke                      |
| ----------------------------- | ---------------------------------------------------------------------------- | --------------------------------------------- |
| **Calling a Native Function** | `System.loadLibrary("native");`                                              | `[DllImport("native.dll")]`                   |
| **Passing Parameters**        | `JNIEXPORT void JNICALL Java_Class_method(JNIEnv *env, jobject obj, jint x)` | `public static extern void Method(int x);`    |
| **Returning Values**          | `return (*env)->NewStringUTF(env, "Hello, JNI!");`                           | `return "Hello, PInvoke!";`                   |
| **Working with Arrays**       | `(*env)->GetIntArrayElements(env, arr, NULL);`                               | `IntPtr arrPtr = Marshal.AllocHGlobal(size);` |
| **String Conversion**         | `(*env)->GetStringUTFChars(env, jstr, NULL);`                                | `Marshal.PtrToStringAnsi(strPtr);`            |

***

## 10 JNI Code Examples

### **1. Loading a Native Library in Java**

```java
class Main {
    static {
        System.loadLibrary("native");
    }
    public native void sayHello();
}
```

### **2. C Implementation of `sayHello`**

```c
#include <jni.h>
#include <stdio.h>
#include "Main.h"

JNIEXPORT void JNICALL Java_Main_sayHello(JNIEnv *env, jobject obj) {
    printf("Hello from C!\n");
}
```

### **3. Passing an Integer from Java to C**

```java
public native int square(int x);
```

```c
JNIEXPORT jint JNICALL Java_Main_square(JNIEnv *env, jobject obj, jint x) {
    return x * x;
}
```

### **4. Returning a String from C to Java**

```c
JNIEXPORT jstring JNICALL Java_Main_getMessage(JNIEnv *env, jobject obj) {
    return (*env)->NewStringUTF(env, "Hello from JNI!");
}
```

### **5. Working with Arrays**

```c
JNIEXPORT void JNICALL Java_Main_processArray(JNIEnv *env, jobject obj, jintArray arr) {
    jint *elements = (*env)->GetIntArrayElements(env, arr, NULL);
    elements[0] += 10;
    (*env)->ReleaseIntArrayElements(env, arr, elements, 0);
}
```

### **6. Calling a Java Method from C**

```c
JNIEXPORT void JNICALL Java_Main_callJavaMethod(JNIEnv *env, jobject obj) {
    jclass cls = (*env)->GetObjectClass(env, obj);
    jmethodID mid = (*env)->GetMethodID(env, cls, "javaMethod", "()V");
    (*env)->CallVoidMethod(env, obj, mid);
}
```

### **7. Accessing a Java Field from C**

```c
JNIEXPORT void JNICALL Java_Main_modifyField(JNIEnv *env, jobject obj) {
    jclass cls = (*env)->GetObjectClass(env, obj);
    jfieldID fid = (*env)->GetFieldID(env, cls, "num", "I");
    jint num = (*env)->GetIntField(env, obj, fid);
    (*env)->SetIntField(env, obj, fid, num + 1);
}
```

### **8. Handling Exceptions in JNI**

```c
JNIEXPORT void JNICALL Java_Main_causeError(JNIEnv *env, jobject obj) {
    jclass exClass = (*env)->FindClass(env, "java/lang/Exception");
    (*env)->ThrowNew(env, exClass, "Custom JNI Exception");
}
```

### **9. Using a C Struct in Java**

```c
typedef struct {
    int x;
    int y;
} Point;
```

### **10. Calling Native Windows APIs from Java**

```c
#include <windows.h>

JNIEXPORT void JNICALL Java_Main_showMessage(JNIEnv *env, jobject obj) {
    MessageBox(NULL, "Hello from JNI!", "JNI Message", MB_OK);
}
```

***

## Key Takeaways

* **JNI is a powerful but complex way to call native C/C++ code from Java.**
* **It’s useful for performance-critical applications, but alternatives like JNA exist.**
* **Still widely used in Java projects that require system-level access.**

***

## References

1. [JNI Wikipedia](https://en.wikipedia.org/wiki/Java_Native_Interface)
2. [Official Java JNI Documentation](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/)
3. [JNI Tutorial](https://www.ibm.com/developerworks/java/tutorials/j-jni/)
