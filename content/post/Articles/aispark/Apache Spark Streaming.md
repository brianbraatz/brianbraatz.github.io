---
title: Apache Spark-Real Time Streaming-PHI-2-Llama.cpp Setup Guide
description: Walk through of setting up Llama.cpp for real-time AI-powered document processing
slug: apache-spark-pyspark-Llamacpp-stream
date: 2018-01-15
image: post/Articles/IMAGES/apachespark.png
categories:
  - Big Data
  - Python
  - Data Engineering
  - Cloud
  - AI
  - Machine Learning
  - Hadoop
  - Apache Spark
  - PySpark
  - PHI-2
  - Llama.cpp
tags:
  - Apache Spark
  - PySpark
  - Big Data
  - MLlib
  - PHI-2
  - Llama.cpp
  - Spark Streaming
draft: false
weight: 910
lastmod: 2025-03-19T22:11:34.205Z
---
<!-- # **Apache Spark and PySpark: A Beginner's Guide (Part 11)**

Welcome back to our **Apache Spark and PySpark series**! In **Part 10**, we built an **AI-powered document processing pipeline** that combined **Spark MLlib, PHI-2, and Llama.cpp**. Now, we take it to the next level by enabling **real-time processing with Spark Streaming**. This allows us to process incoming document feeds **on the fly**, analyze them, and generate AI-powered insights dynamically. -->

## **Why Use Spark Streaming with Llama.cpp?**

<!-- 
With traditional batch processing, we process a dataset **after** it is fully collected. But what if we want to: ✅ **Analyze news articles in real-time?**\
✅ **Process customer feedback as it arrives?**
✅ **Run AI-powered insights on live document feeds?** -->

Spark Streaming allows us to process **continuous streams of data** and apply AI models like **Llama.cpp for real-time analysis**.

***

## **Step 1: Setting Up Spark Streaming**

### **1.1 Install Required Packages**

```sh
pip install pyspark llama-cpp-python transformers torch
```

### **1.2 Initialize Spark Streaming**

```python
from pyspark.sql import SparkSession
from pyspark.streaming import StreamingContext

# Initialize Spark session
spark = SparkSession.builder \
    .appName("AI-Streaming-Processing") \
    .getOrCreate()

# Create Streaming Context (batch interval: 5 seconds)
ssc = StreamingContext(spark.sparkContext, batchDuration=5)
```

***

## **Step 2: Creating a Streaming Data Source**

For this example, we assume **new documents** are arriving in a folder (`~/streaming_documents/`).

### **2.1 Monitor Incoming Files**

```python
# Monitor new text files in the directory
document_stream = ssc.textFileStream("file:///home/user/streaming_documents")
```

Each time a **new document** is saved in this folder, Spark Streaming **automatically picks it up** and processes it.

***

## **Step 3: Applying AI-Powered Processing**

### **3.1 Summarization with PHI-2**

```python
from transformers import AutoTokenizer, AutoModelForSeq2SeqLM

tokenizer = AutoTokenizer.from_pretrained("microsoft/phi-2")
model = AutoModelForSeq2SeqLM.from_pretrained("microsoft/phi-2")

def summarize_text(text):
    inputs = tokenizer(text, return_tensors="pt", max_length=512, truncation=True)
    summary_ids = model.generate(inputs["input_ids"], max_length=100)
    return tokenizer.decode(summary_ids[0], skip_special_tokens=True)
```

```python
document_summaries = document_stream.map(summarize_text)
document_summaries.pprint()
```

***

### **3.2 AI Insights with Llama.cpp**

```python
from llama_cpp import Llama

llm = Llama(model_path="llama-2-7b.Q4_K.gguf")

def generate_ai_insight(text):
    response = llm(f"{text}\n\nQuestion: What are the key takeaways?\nAnswer:")
    return response["choices"][0]["text"].strip()

ai_insights = document_summaries.map(generate_ai_insight)
ai_insights.pprint()
```

This takes the **real-time streaming documents**, summarizes them with **PHI-2**, and then applies **Llama.cpp** to extract **key insights**.

***

## **Step 4: Running the Streaming Pipeline**

### **4.1 Start Streaming**

```python
ssc.start()  # Start the streaming computation
ssc.awaitTermination()  # Wait for the streaming to terminate
```

Now, whenever **new text files** appear in `~/streaming_documents/`, they are **processed in real-time**, summarized, and analyzed by AI.

***

## **Example Inputs and Outputs**

### **Incoming Document (Saved in **\`\`**):**

```txt
Title: The Future of AI
Artificial intelligence is transforming industries, from healthcare to finance. With the rise of large language models and improved compute resources, AI is expected to automate more tasks and enable new innovations.
```

### **Real-Time AI Processing Output:**

**Summarized Text:**

```txt
AI is revolutionizing industries. Large models and improved compute are driving automation and innovation.
```

**Llama.cpp AI Insights:**

```txt
Key Takeaways: AI is expected to automate more tasks across industries. Innovations in compute power will further accelerate adoption.
```

***

## **Final Thoughts: Why This Matters**

🚀 **Real-Time AI-Powered Processing:** No need to wait for batch jobs—analyze data **as it arrives**.\
⚡ **Dynamic NLP Pipelines:** PHI-2 condenses large text, and Llama.cpp extracts key takeaways—**automatically!**\
💡 **Scalable and Cost-Effective:** Runs on **local machines** with **no need for expensive cloud APIs**.

***
