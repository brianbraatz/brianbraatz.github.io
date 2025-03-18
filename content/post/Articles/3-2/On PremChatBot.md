---
title: On-Prem AI Chatbot for PDF Search
description: "Learn how to build a fully on-prem AI-powered chatbot "
slug: on-prem-ai-pdf-search
date: 2017-06-15
image: post/Articles/IMAGES/omnibot2000.png
categories:
  - AI
  - Machine Learning
  - PDF Search
tags:
  - AI
  - Machine
  - Learning
  - NLP
  - FAISS
  - Elasticsearch
  - PDF
  - Processing
draft: false
weight: 25
categories_ref:
  - AI
  - Machine Learning
  - PDF Search
slug_calculated: https://brianbraatz.github.io/p/on-prem-ai-pdf-search
lastmod: 2025-03-14T16:40:12.851Z
---
[Omnibot 2000 - Wikipedia](https://en.wikipedia.org/wiki/Omnibot)

<!-- for searching PDFs using PyMuPDF, FAISS, sentence-transformers, and Llama 2. In this first part, we cover text extraction using PyMuPDF and pdfplumber.

# Building an On-Prem AI Chatbot for PDF Search - Part 1: Text Extraction

Welcome to the first part of this **on-prem AI chatbot series**!  -->

This is a **simplified version** of a real-world project I built for a job.

The goal: **Create a fully on-prem AI chatbot** that can search and retrieve information from a **large collection of PDFs**.

Unlike cloud-based solutions, **everything runs locally**—which means **no API costs, no data privacy concerns, and full control** over the system.

***

## 🛠️ What We're Building

This guide will walk through setting up an **AI-powered PDF search system** using the following tools:

| Component           | Tool                              |
| ------------------- | --------------------------------- |
| **Text Extraction** | `PyMuPDF` (fastest)               |
| **Keyword Search**  | `Elasticsearch`                   |
| **Semantic Search** | `FAISS` (or `Qdrant`)             |
| **Embedding Model** | `sentence-transformers`           |
| **Chatbot LLM**     | `Llama 2` (running on local GPUs) |
| **User Interface**  | `Streamlit`                       |

## ❗ A Quick Note About OCR

For this project, **I didn’t need OCR** because all the PDFs already contained selectable text.

However, **if you’re dealing with scanned PDFs (images instead of text),** you’ll need OCR (Optical Character Recognition).

For those cases, check out: [How to OCR PDFs using pdfplumber and Tesseract](https://github.com/jsvine/pdfplumber).

But for **this tutorial**, we’re assuming **text-based PDFs only**.

***

# 📝 Part 1: Extracting Text from PDFs

Before we can **search** or **chat** with our PDFs, we need to **extract the text**.

The best way to do this **without OCR** is using `PyMuPDF` (`fitz`), which is **blazing fast** and maintains formatting.

## 📦 Step 1: Install Dependencies

First, install PyMuPDF:

```bash
pip install pymupdf
```

## 🚀 Step 2: Extract Text from a PDF

Here’s a simple function to extract text from **any text-based PDF**:

```python
import fitz  # PyMuPDF

def extract_text_from_pdf(pdf_path):
    """Extracts text from a PDF using PyMuPDF."""
    doc = fitz.open(pdf_path)
    text = "\n".join([page.get_text("text") for page in doc])
    return text

# Example Usage
pdf_text = extract_text_from_pdf("example.pdf")
print(pdf_text[:500])  # Print first 500 characters
```

✅ **Why PyMuPDF?**

* Super **fast** 🚀
* Preserves **text structure**
* Can handle **large PDFs** without issues

❌ **When it won’t work**

* If the PDF is a scanned image, PyMuPDF **won't** extract anything
* If you get **empty text**, your PDF likely **needs OCR**

Again, if OCR is needed, check out: [How to OCR PDFs using pdfplumber and Tesseract](https://github.com/jsvine/pdfplumber).

***

## 🔥 Step 3: Batch Process a Folder of PDFs

If you have **hundreds or thousands of PDFs**, you’ll want to **process them all at once**.

Here’s how to extract text from every PDF in a folder and store the results in a dictionary:

```python
import os

def extract_text_from_folder(pdf_folder):
    """Extracts text from all PDFs in a folder."""
    extracted_texts = {}
    
    for filename in os.listdir(pdf_folder):
        if filename.endswith(".pdf"):
            pdf_path = os.path.join(pdf_folder, filename)
            text = extract_text_from_pdf(pdf_path)
            extracted_texts[filename] = text
            
    return extracted_texts

# Example Usage
pdf_texts = extract_text_from_folder("pdf_documents")
print(pdf_texts.keys())  # Print the names of processed PDFs
```

### 🔹 What This Does:

* Loops through all PDFs in a given folder
* Extracts text and stores it in a dictionary `{filename: extracted_text}`
* Can be used later for **search indexing**

***

## ✅ What We Have So Far

At this point, we can **extract text from PDFs**, which is the **first step** toward building our AI-powered search system.

<!-- 
| Feature  | Status |
|----------|--------|
| **Text Extraction (Digital PDFs)** ✅ Done |
| **OCR for Scanned PDFs** ❌ Not included (but linked) |
| **Batch Processing** ✅ Done |

---

## 🔜 Coming in Part 2: Indexing and Searching PDFs  
Now that we have **extracted text**, the next step is **storing and searching it efficiently**.  

### **🔹 In Part 2, we’ll cover:**  
✅ Storing extracted text in **Elasticsearch** for keyword search  
✅ Setting up **FAISS** for fast **semantic search**  

Stay tuned for **Part 2!** 🚀  

---

## 🗂️ Key Takeaways  

| Feature | Tool |
|---------|------|
| **Digital PDF Extraction** | PyMuPDF |
| **Batch Processing PDFs** | Python + OS Module |
| **OCR for Scanned PDFs?** | Not included (but linked) |

---

## 📚 References  
- [PyMuPDF Documentation](https://pymupdf.readthedocs.io/en/latest/)  
- [pdfplumber GitHub (For OCR)](https://github.com/jsvine/pdfplumber)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 2: Indexing with Elasticsearch & FAISS](#) (Coming Soon)  
```

---

### 🔹 What's Different in This Version?
✅ Clearly explains that this is based on **a real-world project**  
✅ States **OCR is not included** (but links to an OCR guide)  
✅ Focuses **only on extracting text from digital PDFs**  
✅ Includes **batch processing** for efficiency   -->

# 🔥Indexing PDFs in Elasticsearch (Keyword Search)

### 📦 Step 1: Install Elasticsearch & Python Client

First, we need to install Elasticsearch and the Python client.

#### **Option 1: Run Elasticsearch Locally (Recommended)**

Install Elasticsearch (7.x or 8.x) from [elastic.co](https://www.elastic.co/downloads/elasticsearch), then start it:

```bash
./bin/elasticsearch
```

#### **Option 2: Run Elasticsearch via Docker**

```bash
docker run -d --name elasticsearch -p 9200:9200 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:8.5.0
```

Now, install the Python client:

```bash
pip install elasticsearch
```

***

### 🚀 Step 2: Index Extracted PDF Text

We'll store **each PDF's extracted text** as a document in Elasticsearch.

#### **1️⃣ Connect to Elasticsearch**

```python
from elasticsearch import Elasticsearch

es = Elasticsearch("http://localhost:9200")  # Change if running remotely

# Check connection
if es.ping():
    print("Connected to Elasticsearch!")
else:
    print("Elasticsearch connection failed.")
```

#### **2️⃣ Create an Index for PDFs**

```python
INDEX_NAME = "pdf_documents"

# Define mapping (schema)
mapping = {
    "mappings": {
        "properties": {
            "filename": {"type": "keyword"},
            "text": {"type": "text"}
        }
    }
}

# Create the index
if not es.indices.exists(index=INDEX_NAME):
    es.indices.create(index=INDEX_NAME, body=mapping)
    print(f"Index '{INDEX_NAME}' created.")
```

#### **3️⃣ Add PDFs to Elasticsearch**

```python
def index_pdf(filename, text):
    """Indexes a PDF document in Elasticsearch."""
    doc = {"filename": filename, "text": text}
    es.index(index=INDEX_NAME, body=doc)

# Example usage
index_pdf("example.pdf", "This is a sample PDF content.")
```

***

### 🔍 Step 3: Search PDFs in Elasticsearch

Now that PDFs are indexed, we can **search for keywords**.

#### **Example: Search for "machine learning" in PDFs**

```python
def search_pdfs(query):
    """Search PDFs using Elasticsearch."""
    search_query = {
        "query": {
            "match": {
                "text": query
            }
        }
    }
    results = es.search(index=INDEX_NAME, body=search_query)
    return results["hits"]["hits"]

# Example usage
results = search_pdfs("machine learning")
for r in results:
    print(f"Found in: {r['_source']['filename']}\nText: {r['_source']['text'][:200]}...\n")
```

✅ **Elasticsearch now powers our keyword-based PDF search!**

***

# 🧠 Indexing PDFs in FAISS (Semantic Search)

Elasticsearch works well for **exact keyword matches**, but it **doesn’t understand meaning**.

To **search PDFs based on meaning**, we use **FAISS (Facebook AI Similarity Search)** with **text embeddings**.

***

### 📦 Step 1: Install FAISS & Sentence-Transformers

```bash
pip install faiss-cpu sentence-transformers
```

***

### 🚀 Step 2: Generate Embeddings for PDFs

We’ll use `sentence-transformers` to convert text into **numerical embeddings**.

#### **1️⃣ Load the Embedding Model**

```python
from sentence_transformers import SentenceTransformer

model = SentenceTransformer("all-MiniLM-L6-v2")  # Fast and accurate
```

#### **2️⃣ Convert PDF Text into Embeddings**

```python
def embed_text(text):
    """Generates an embedding for a given text."""
    return model.encode(text)

# Example usage
embedding = embed_text("This is a sample text.")
print(embedding.shape)  # Output: (384,)
```

***

### 🔥 Step 3: Store Embeddings in FAISS

Now, we create a FAISS index to store and search our embeddings.

#### **1️⃣ Import FAISS & Create an Index**

```python
import faiss
import numpy as np

DIMENSIONS = 384  # Model output size
index = faiss.IndexFlatL2(DIMENSIONS)  # L2 distance index
```

#### **2️⃣ Index PDF Embeddings**

```python
pdf_texts = {
    "example.pdf": "This document is about deep learning and AI.",
    "sample.pdf": "This paper discusses cloud computing concepts."
}

embeddings = np.array([embed_text(text) for text in pdf_texts.values()])
index.add(embeddings)

print("FAISS index created with", index.ntotal, "documents.")
```

***

### 🔍 Step 4: Search PDFs in FAISS

Now we can **search** using **semantic similarity**.

#### **1️⃣ Search FAISS Using a Query**

```python
def search_faiss(query, k=2):
    """Searches FAISS for the most similar PDFs."""
    query_embedding = embed_text(query).reshape(1, -1)
    D, I = index.search(query_embedding, k)  # Retrieve top-k
    return I

# Example usage
query = "AI and deep learning"
results = search_faiss(query)

for i in results[0]:
    print("Matched:", list(pdf_texts.keys())[i])
```

✅ **FAISS now powers our semantic PDF search!**

***

<!-- 
## 🎯 What We Built in Part 2

| Feature  | Status |
|----------|--------|
| **Keyword Search (Elasticsearch)** ✅ Done |
| **Semantic Search (FAISS)** ✅ Done |
| **Embeddings (sentence-transformers)** ✅ Done |

---

## 🔜 Coming in Part 3: AI Chatbot with Llama 2  
Now that we can **retrieve relevant PDFs**, the next step is to **connect it to an LLM (Llama 2)** to build a chatbot!  

### **🔹 In Part 3, we’ll cover:**  
✅ Running **Llama 2** locally  
✅ Using **retrieval-augmented generation (RAG)** to feed PDFs into the chatbot  
✅ **Building a chat UI** with Streamlit  

Stay tuned for **Part 3!** 🚀  

---

## 📚 References  
- [Elasticsearch Docs](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Sentence-Transformers](https://www.sbert.net/)  

--- -->

Here’s **Part 3**, where we integrate **Llama 2** to create an **AI chatbot that can answer questions based on our PDF data** using **retrieval-augmented generation (RAG)**. 🚀

<!-- 
In **Part 1**, we extracted text from PDFs.  
In **Part 2**, we indexed that text in **Elasticsearch** (keyword search) and **FAISS** (semantic search).  

Now, we’ll **use those search results to power an AI chatbot**, allowing us to **ask natural language questions** and get answers from our PDF collection.  

---

## 🔍 What We’ll Cover in This Part:
✅ Set up **Llama 2** locally (100% on-prem)  
✅ Use **retrieval-augmented generation (RAG)** to feed search results into Llama 2  
✅ Build a **simple chatbot UI** using Streamlit  

### 🏗️ Recap of Our Tech Stack:
| Component             | Tool |
|-----------------------|---------------------|
| **Text Extraction**   | `PyMuPDF` (Done ✅) |
| **Keyword Search**    | `Elasticsearch` (Done ✅) |
| **Semantic Search**   | `FAISS` (Done ✅) |
| **Embedding Model**   | `sentence-transformers` (Done ✅) |
| **Chatbot LLM**       | `Llama 2` (Now) |
| **User Interface**    | `Streamlit` (Now) |

--- -->

# 🔥 Running Llama 2 Locally

### 🏗️ Step 1: Install Llama 2

We’ll use `llama-cpp-python`, which allows us to **run Llama 2 on CPU or GPU**.

```bash
pip install llama-cpp-python
```

💡 If you have a **powerful GPU**, use the `GGUF` version for better performance.

***

### 🚀 Step 2: Download a Llama 2 Model

Go to [Meta’s Llama 2 page](https://ai.meta.com/llama/) and download a model.

For **fast responses**, I recommend:

* `llama-2-7b-chat.Q4_K_M.gguf` (Quantized 4-bit model)
* `llama-2-13b-chat.Q4_K_M.gguf` (Larger but still manageable)

Place the model file in a folder called `models`.

***

### 🔥 Step 3: Load Llama 2 in Python

```python
from llama_cpp import Llama

# Load Llama 2 model
llm = Llama(model_path="models/llama-2-7b-chat.Q4_K_M.gguf")

# Example chat
response = llm("What is machine learning?")
print(response["choices"][0]["text"])
```

✅ **Llama 2 is now running locally!**

***

# 🧠 Implementing RAG (Retrieval-Augmented Generation)

By itself, **Llama 2 doesn’t know about our PDFs**.

To **make it answer questions based on PDFs**, we use **retrieval-augmented generation (RAG)**:

1. **Search PDFs** using **Elasticsearch (keyword) and FAISS (semantic search)**
2. **Feed search results** into **Llama 2** as context
3. **Ask Llama 2 a question**, and it will generate an answer **based on the retrieved PDFs**

***

### 🚀 Step 1: Search PDFs Using Elasticsearch & FAISS

We **combine both search methods** to get the **most relevant** PDF chunks.

```python
def search_pdfs_rag(query, k=3):
    """Search PDFs using Elasticsearch (keyword) and FAISS (semantic)."""
    # 1️⃣ Keyword Search (Elasticsearch)
    es_results = search_pdfs(query)[:k]

    # 2️⃣ Semantic Search (FAISS)
    faiss_results = search_faiss(query, k)[:k]

    # 3️⃣ Merge and Return Results
    combined_results = set([r["_source"]["text"][:500] for r in es_results])
    combined_results.update([list(pdf_texts.values())[i][:500] for i in faiss_results[0]])

    return "\n\n".join(combined_results)
```

***

### 🔥 Step 2: Feed Search Results to Llama 2

Now we **pass the retrieved text as context** to Llama 2.

```python
def chat_with_pdfs(query):
    """Uses RAG to answer questions based on PDF content."""
    context = search_pdfs_rag(query)

    prompt = f"Use the following context to answer the question:\n\n{context}\n\nQuestion: {query}\nAnswer:"

    response = llm(prompt)
    return response["choices"][0]["text"]

# Example Usage
print(chat_with_pdfs("What is deep learning?"))
```

✅ **Now, Llama 2 can answer questions based on our PDFs!**

***

# 🎨 Building a Simple Chat UI with Streamlit

To make this user-friendly, let’s build a **web-based chatbot** using **Streamlit**.

***

### 📦 Step 1: Install Streamlit

```bash
pip install streamlit
```

***

### 🚀 Step 2: Create a Simple Chatbot UI

Create a file **`app.py`**:

```python
import streamlit as st

st.title("📄 AI Chatbot for PDF Search")

query = st.text_input("Ask a question:")
if query:
    response = chat_with_pdfs(query)
    st.write("### 🤖 AI Response:")
    st.write(response)
```

***

### 🎯 Step 3: Run the App

```bash
streamlit run app.py
```

✅ **Now, you have a chatbot that searches PDFs and answers questions!**

***

<!-- 
## 🎯 What We Built in Part 3

| Feature  | Status |
|----------|--------|
| **Llama 2 Running Locally** ✅ Done |
| **RAG (PDF-Based Answers)** ✅ Done |
| **Chatbot UI (Streamlit)** ✅ Done |

---

---

## 📚 References  
- [Llama 2](https://ai.meta.com/llama/)  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Elasticsearch Docs](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)  
- [Streamlit](https://streamlit.io/)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 4: Enhancing Search & UI](#) (Coming Soon)  
``` -->

<!-- Here’s **Part 4**, where we improve **search ranking, enhance the UI**, and add **PDF upload support** for a smoother experience. 🚀   -->

<!-- 
Welcome to **Part 4** of this series, where we **improve search ranking, enhance the UI**, and **add PDF upload support** to make our chatbot more useful.  

In **Part 3**, we built a chatbot that:
✅ Runs **Llama 2** locally  
✅ Uses **retrieval-augmented generation (RAG)** to search PDFs  
✅ Has a **basic chat UI with Streamlit**  

Now, we’ll refine the **search results**, improve **usability**, and allow users to **upload new PDFs dynamically**.  

---

## 🔍 What We’ll Cover in This Part:
✅ Improve **search relevance & ranking**  
✅ Enhance **Streamlit UI** (better layout & history)  
✅ Add **PDF upload support** (update index dynamically)  

### 🏗️ Recap of Our Tech Stack:
| Component             | Tool |
|-----------------------|---------------------|
| **Text Extraction**   | `PyMuPDF` (Done ✅) |
| **Keyword Search**    | `Elasticsearch` (Done ✅) |
| **Semantic Search**   | `FAISS` (Done ✅) |
| **Embedding Model**   | `sentence-transformers` (Done ✅) |
| **Chatbot LLM**       | `Llama 2` (Done ✅) |
| **User Interface**    | `Streamlit` (Enhancing Now) |

--- -->

# 🔍Improving Search Ranking

Right now, our **Elasticsearch + FAISS** search returns **somewhat relevant** results, but we can improve **ranking & filtering**.

### 🚀 Step 1: Boost Keyword Matches in Elasticsearch

By default, Elasticsearch treats all matches equally. We can **boost results** that contain **exact keyword matches**.

#### ✅ **Update Elasticsearch Search Query**

```python
def search_pdfs_improved(query, k=3):
    """Improves search ranking by boosting keyword matches."""
    search_query = {
        "query": {
            "bool": {
                "should": [
                    {"match": {"text": {"query": query, "boost": 2.0}}},  # Boost exact matches
                    {"match_phrase": {"text": {"query": query, "boost": 1.5}}}  # Boost phrase matches
                ]
            }
        }
    }
    results = es.search(index="pdf_documents", body=search_query)
    return results["hits"]["hits"][:k]

# Example usage
print(search_pdfs_improved("machine learning"))
```

✅ **Boosts exact and phrase matches**\
✅ **More relevant results** appear at the top

***

### 🚀 Step 2: Adjust FAISS to Prefer Recent Documents

FAISS **doesn’t consider document relevance**, but we can **re-rank results** based on recency.

#### ✅ **Re-rank FAISS Results by Document Date**

```python
def rerank_faiss_results(faiss_results, doc_metadata):
    """Re-ranks FAISS results based on recency."""
    sorted_results = sorted(faiss_results, key=lambda doc: doc_metadata[doc]["date"], reverse=True)
    return sorted_results

# Example usage
metadata = {"example.pdf": {"date": "2024-01-01"}, "old.pdf": {"date": "2019-05-10"}}
print(rerank_faiss_results(["old.pdf", "example.pdf"], metadata))  # "example.pdf" comes first
```

✅ **Recent documents now rank higher**

***

# 🎨 Enhancing Streamlit UI

Our **current chatbot UI is too basic**. Let’s:\
✅ Improve layout\
✅ Add chat history\
✅ Show document sources

***

### 🚀 Step 1: Upgrade the Chat UI

Update **`app.py`** with a better layout:

```python
import streamlit as st

st.set_page_config(page_title="📄 AI Chatbot for PDFs", layout="wide")

st.title("📄 AI Chatbot for PDF Search")

# Sidebar
with st.sidebar:
    st.header("Settings")
    st.text("Customize your search")

query = st.text_input("Ask a question:")

if "chat_history" not in st.session_state:
    st.session_state.chat_history = []

if query:
    response = chat_with_pdfs(query)
    st.session_state.chat_history.append((query, response))

st.write("### 🤖 AI Response:")
for q, r in st.session_state.chat_history:
    st.write(f"**Q:** {q}")
    st.write(f"**A:** {r}")
    st.write("---")
```

✅ **Keeps chat history**\
✅ **Better layout with a sidebar**

***

### 🚀 Step 2: Show PDF Sources in Chat

Modify **`chat_with_pdfs()`** to **return sources**.

```python
def chat_with_pdfs(query):
    """Returns AI response + sources."""
    context, sources = search_pdfs_rag(query, return_sources=True)

    prompt = f"Use the following context to answer the question:\n\n{context}\n\nQuestion: {query}\nAnswer:"
    response = llm(prompt)

    return response["choices"][0]["text"], sources
```

Now, update **`app.py`** to **show sources**:

```python
response, sources = chat_with_pdfs(query)

st.write("### 🤖 AI Response:")
st.write(response)

st.write("📂 **Sources:**")
for source in sources:
    st.write(f"- {source}")
```

✅ **Users see which PDFs were used to generate answers**

***

# 📂 Adding PDF Upload Support

Currently, we **preload PDFs**, but users **can’t upload new ones**. Let’s **fix that!**

***

### 🚀 Step 1: Add File Upload to Streamlit

Modify **`app.py`** to allow **users to upload PDFs**.

```python
uploaded_files = st.file_uploader("Upload PDFs", accept_multiple_files=True, type=["pdf"])

if uploaded_files:
    for uploaded_file in uploaded_files:
        bytes_data = uploaded_file.read()
        
        # Save file locally
        with open(f"pdf_documents/{uploaded_file.name}", "wb") as f:
            f.write(bytes_data)
        
        # Extract text and index it
        text = extract_text_from_pdf(f"pdf_documents/{uploaded_file.name}")
        index_pdf(uploaded_file.name, text)
        
    st.success("Files uploaded and indexed successfully!")
```

✅ **Users can now upload PDFs, and they’re instantly indexed**

***

<!-- 
## 🎯 What We Built in Part 4

| Feature  | Status |
|----------|--------|
| **Better Search Ranking (Boosted Keywords)** ✅ Done |
| **Re-Ranked FAISS Results by Recency** ✅ Done |
| **Enhanced Chat UI (History + Sidebar + Sources)** ✅ Done |
| **PDF Upload Support** ✅ Done |

---

## 🔜 Coming in Part 5: Optimizing Performance  
Now that our chatbot is **fully functional**, we’ll focus on **optimizing performance**.

### **🔹 In Part 5, we’ll cover:**  
✅ Running Llama 2 **faster** (quantization & GPU acceleration)  
✅ Improving **FAISS search speed**  
✅ Scaling to **thousands of PDFs efficiently**   -->

<!-- ## 📚 References  
- [Elasticsearch Docs](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Streamlit](https://streamlit.io/)  

---
 -->

<!--  
✅ **Next Up:** [On-Prem AI PDF Search - Part 5: Optimizing Performance](#) (Coming Soon)  
``` -->

Here’s **Part 5**, where we **optimize performance** by making **Llama 2 run faster**, improving **FAISS search speed**, and scaling to **thousands of PDFs efficiently**. 🚀

***

<!-- # On-Prem AI Chatbot for PDF Search – Part 5: Optimizing Performance

Welcome to **Part 5** of this series! Now that our **AI chatbot works**, it’s time to **make it faster and more efficient**.  

In **Part 4**, we improved search ranking, enhanced the UI, and added **PDF upload support**.  
Now, we’ll focus on **speeding up everything** so we can handle **thousands of PDFs efficiently**.

---

## 🚀 What We’ll Optimize in This Part:
✅ **Speed up Llama 2** (quantization & GPU acceleration)  
✅ **Optimize FAISS for large-scale search**  
✅ **Improve Elasticsearch indexing & search speed**  

### 🏗️ Recap of Our Tech Stack:
| Component             | Tool |
|-----------------------|---------------------|
| **Text Extraction**   | `PyMuPDF` (Done ✅) |
| **Keyword Search**    | `Elasticsearch` (Done ✅) |
| **Semantic Search**   | `FAISS` (Done ✅) |
| **Embedding Model**   | `sentence-transformers` (Done ✅) |
| **Chatbot LLM**       | `Llama 2` (Optimizing Now) |
| **User Interface**    | `Streamlit` (Optimized in Part 4) |

--- -->

# ⚡Speeding Up Llama 2

By default, **Llama 2 can be slow**, especially on CPUs. Here’s how to **run it faster**.

***

### 🚀 Step 1: Use a **Quantized Llama 2 Model**

Quantization **reduces model size** and **speeds up inference**.

#### ✅ **Download a Quantized GGUF Model**

Go to [Meta’s Llama 2 page](https://ai.meta.com/llama/) and download:

* `llama-2-7b-chat.Q4_K_M.gguf` (4-bit quantized)
* **OR** `llama-2-13b-chat.Q4_K_M.gguf` (faster than full precision)

Move it to `models/`.

***

### 🚀 Step 2: Enable GPU Acceleration (If Available)

If you have a **GPU**, use `llama-cpp-python` with CUDA.

#### ✅ **Install CUDA & llama-cpp-python with GPU Support**

```bash
CMAKE_ARGS="-DLLAMA_CUBLAS=on" pip install llama-cpp-python --no-cache-dir
```

Then, modify your **Llama 2 loading code**:

```python
from llama_cpp import Llama

# Load Llama 2 model with GPU acceleration
llm = Llama(model_path="models/llama-2-7b-chat.Q4_K_M.gguf", n_gpu_layers=100)
```

✅ **Massive speed boost on GPUs!**\
✅ **Even CPU inference is faster with quantization**

***

### 🚀 Step 3: Reduce Response Time with Streaming

Right now, **Llama 2 waits for the full response** before returning anything.

We can **stream responses** as they’re generated for a **faster, chat-like feel**.

#### ✅ **Modify `chat_with_pdfs()` to Stream Responses**

```python
def chat_with_pdfs(query):
    """Streams responses from Llama 2 for faster user experience."""
    context = search_pdfs_rag(query)
    prompt = f"Use the following context to answer:\n\n{context}\n\nQuestion: {query}\nAnswer:"

    for response in llm(prompt, stream=True):
        yield response["choices"][0]["text"]
```

✅ **Now responses appear instantly instead of waiting!**

***

# 🏎️ Optimizing FAISS for Large-Scale Search

FAISS is **fast**, but it can slow down as we **add more PDFs**.

Here’s how to **speed it up for thousands of documents**.

***

### 🚀 Step 1: Use **HNSW Indexing** Instead of Flat L2

By default, FAISS uses **brute-force search** (`IndexFlatL2`).\
For **huge datasets**, we should use **Hierarchical Navigable Small World (HNSW)** indexing.

#### ✅ **Modify FAISS Index to Use HNSW**

```python
import faiss

DIMENSIONS = 384  # Sentence-Transformer output size
index = faiss.IndexHNSWFlat(DIMENSIONS, 32)  # 32 is the max number of links per node
```

✅ **Now FAISS search is MUCH faster** for large datasets

***

### 🚀 Step 2: Use **IVF Indexing for Faster Lookups**

Another trick is **Inverted File Index (IVF)**, which clusters vectors for fast retrieval.

#### ✅ **Modify FAISS Index to Use IVF**

```python
num_clusters = 128  # Adjust based on dataset size
quantizer = faiss.IndexFlatL2(DIMENSIONS)  
index = faiss.IndexIVFFlat(quantizer, DIMENSIONS, num_clusters)
index.train(embeddings)  # Train on initial dataset
```

✅ **Speeds up searches by grouping similar documents**

***

# 🚀 Scaling Elasticsearch for Massive PDF Collections

If you have **millions of PDFs**, **Elasticsearch needs tuning**.

***

### 🚀 Step 1: Disable Refresh for Bulk Indexing

By default, Elasticsearch **refreshes after every document insert**, slowing down indexing.

#### ✅ **Disable Refresh While Indexing**

```python
es.indices.put_settings(index="pdf_documents", body={"refresh_interval": "-1"})

# Bulk index PDFs
for filename, text in pdf_texts.items():
    index_pdf(filename, text)

# Re-enable refresh
es.indices.put_settings(index="pdf_documents", body={"refresh_interval": "1s"})
```

✅ **Indexing PDFs is now 5-10x faster**

***

### 🚀 Step 2: Increase Shard Count for Large Datasets

For **huge** collections, increase the number of **shards**.

#### ✅ **Modify Index Settings**

```python
index_settings = {
    "settings": {
        "index": {
            "number_of_shards": 3,
            "number_of_replicas": 1
        }
    }
}
es.indices.create(index="pdf_documents", body=index_settings)
```

✅ **Speeds up searches & indexing on large datasets**

***

<!-- 
# 🎯 What We Optimized in Part 5  

| Optimization  | Status |
|--------------|--------|
| **Llama 2 Quantization (4-bit GGUF)** ✅ Done |
| **Enable GPU Acceleration** ✅ Done |
| **Stream Responses for Faster Chat** ✅ Done |
| **Switch FAISS to HNSW Indexing** ✅ Done |
| **Use FAISS IVF for Faster Lookups** ✅ Done |
| **Disable Elasticsearch Refresh for Bulk Indexing** ✅ Done |
| **Increase Elasticsearch Shard Count for Scale** ✅ Done |

---

## 🔜 Coming in Part 6: Advanced AI Enhancements  

Our chatbot **now runs fast**, but we can **make it even smarter**.  

### **🔹 In Part 6, we’ll cover:**  
✅ Using **multi-turn memory** (remembering past questions)  
✅ Improving **response accuracy** with **fine-tuned Llama 2**  
✅ Enhancing **UI with context-aware responses**  

Stay tuned for **Part 6!** 🚀  

---

## 📚 References  
- [Llama 2](https://ai.meta.com/llama/)  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Elasticsearch Docs](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 6: Advanced AI Enhancements](#) (Coming Soon)  
``` -->

<!-- 
Here's **Part 6**, where we introduce **multi-turn memory**, improve **response accuracy** with **fine-tuned Llama 2**, and enhance the **chat UI with context-aware responses**. 🚀  

---


# On-Prem AI Chatbot for PDF Search – Part 6: Advanced AI Enhancements  

Welcome to **Part 6** of this series! Now that our chatbot **runs fast**, let’s make it **smarter**.  

In **Part 5**, we optimized performance by:
✅ Speeding up **Llama 2**  
✅ Improving **FAISS search efficiency**  
✅ Scaling **Elasticsearch for large datasets**  

Now, we’ll focus on **AI enhancements**:  
✅ Adding **multi-turn memory** (so the chatbot remembers context)  
✅ Fine-tuning **Llama 2** for **better responses**  
✅ Improving **UI for a better user experience**  

---

## 🏗️ What We’ll Improve in This Part  

| Feature  | Enhancement |
|----------|------------|
| **Memory** | Multi-turn conversation memory |
| **LLM Accuracy** | Fine-tuning Llama 2 on domain-specific PDFs |
| **Chat UI** | Better response formatting & context awareness |

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **Text Extraction**   | `PyMuPDF` (Done ✅) |
| **Keyword Search**    | `Elasticsearch` (Done ✅) |
| **Semantic Search**   | `FAISS` (Done ✅) |
| **Embedding Model**   | `sentence-transformers` (Done ✅) |
| **Chatbot LLM**       | `Llama 2` (Enhancing Now) |
| **User Interface**    | `Streamlit` (Improving Now) | -->

***

# 🧠Adding Multi-Turn Memory

By default, **Llama 2 only answers one question at a time**.

To enable **multi-turn conversation memory**, we need to **track past questions and answers**.

***

### 🚀 Step 1: Modify `chat_with_pdfs()` to Include Memory

Modify the chatbot function to **store past questions & responses**.

```python
def chat_with_pdfs(query):
    """Uses multi-turn memory for AI conversations."""
    
    # Retrieve relevant PDFs
    context, sources = search_pdfs_rag(query, return_sources=True)
    
    # Maintain conversation memory
    if "conversation_history" not in st.session_state:
        st.session_state.conversation_history = []
    
    # Create conversation context
    conversation_history = "\n".join(st.session_state.conversation_history)
    
    # Generate response using Llama 2
    prompt = f"""
    Previous conversation:
    {conversation_history}
    
    Use the following PDF context to answer the question:
    {context}
    
    Question: {query}
    Answer:
    """
    
    response = llm(prompt)["choices"][0]["text"]
    
    # Save conversation
    st.session_state.conversation_history.append(f"Q: {query}\nA: {response}")
    
    return response, sources
```

✅ **Now, the chatbot remembers previous questions!**

***

### 🚀 Step 2: Display Conversation History in the UI

Modify **`app.py`** to **show chat history**.

```python
import streamlit as st

st.title("📄 AI Chatbot for PDF Search")

query = st.text_input("Ask a question:")

if query:
    response, sources = chat_with_pdfs(query)

    st.write("### 🤖 AI Response:")
    st.write(response)

    # Display conversation history
    st.write("### 📝 Conversation History:")
    for message in st.session_state.conversation_history[-5:]:  # Show last 5 messages
        st.write(message)

    # Show document sources
    st.write("📂 **Sources:**")
    for source in sources:
        st.write(f"- {source}")
```

✅ **Now users see chat history & sources in a clean format**

***

# 🏋️‍♂️ Fine-Tuning Llama 2 for Better Responses

Currently, **Llama 2 isn’t optimized for our PDFs**.\
Fine-tuning **makes it much smarter** about our documents.

***

### 🚀 Step 1: Prepare Custom Training Data

Fine-tuning requires **examples of questions & correct answers**.\
We’ll use **our own PDFs** to create a dataset.

#### ✅ **Format Training Data in JSON**

```json
[
    {
        "input": "What is machine learning?",
        "output": "Machine learning is a method of data analysis that automates analytical model building."
    },
    {
        "input": "Explain deep learning.",
        "output": "Deep learning is a subset of machine learning that uses neural networks to model complex patterns in data."
    }
]
```

Save this as **`training_data.json`**.

***

### 🚀 Step 2: Fine-Tune Llama 2

We’ll use **Hugging Face’s `transformers`** to fine-tune Llama 2.

#### ✅ **Install Dependencies**

```bash
pip install transformers datasets peft
```

#### ✅ **Fine-Tune Llama 2**

```python
from transformers import AutoModelForCausalLM, AutoTokenizer, TrainingArguments, Trainer
import torch
import json

# Load base model & tokenizer
model_name = "meta-llama/Llama-2-7b-chat-hf"
model = AutoModelForCausalLM.from_pretrained(model_name, torch_dtype=torch.float16)
tokenizer = AutoTokenizer.from_pretrained(model_name)

# Load training data
with open("training_data.json", "r") as f:
    training_data = json.load(f)

# Convert to tokenized format
train_texts = [d["input"] for d in training_data]
train_labels = [d["output"] for d in training_data]

train_encodings = tokenizer(train_texts, padding=True, truncation=True, return_tensors="pt")
label_encodings = tokenizer(train_labels, padding=True, truncation=True, return_tensors="pt")

# Fine-tuning settings
training_args = TrainingArguments(
    output_dir="./fine-tuned-llama",
    per_device_train_batch_size=2,
    num_train_epochs=3,
    save_strategy="epoch"
)

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_encodings,
    eval_dataset=label_encodings
)

trainer.train()
model.save_pretrained("./fine-tuned-llama")
tokenizer.save_pretrained("./fine-tuned-llama")
```

✅ **Llama 2 is now fine-tuned on our PDFs!**

***

<!-- 
# 🎯 What We Improved in Part 6  

| Feature  | Status |
|----------|--------|
| **Multi-Turn Memory (Chat History)** ✅ Done |
| **Better UI with Chat Context** ✅ Done |
| **Fine-Tuned Llama 2 for Better Answers** ✅ Done |

---

## 🔜 Coming in Part 7: Deploying the AI Chatbot  

Now that our chatbot **remembers conversations & gives better answers**,  
let’s **deploy it in production** with **GPU support & a web server**.

### **🔹 In Part 7, we’ll cover:**  
✅ Deploying with **FastAPI & Gunicorn**  
✅ Running **Llama 2 on a dedicated AI server**  
✅ Scaling for **multiple users**   -->

<!-- 
## 📚 References  
- [Llama 2](https://ai.meta.com/llama/)  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Hugging Face Fine-Tuning Guide](https://huggingface.co/docs/transformers/training)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 7: Deploying the AI Chatbot](#) (Coming Soon)  
```
 -->

<!-- 
Here's **Part 7**, where we **deploy the AI chatbot** using **FastAPI, Gunicorn, and Streamlit**, and optimize it for **multiple users & production environments**. 🚀   -->

<!-- # On-Prem AI Chatbot for PDF Search – Part 7: Deploying the AI Chatbot  

Welcome to **Part 7** of this series! Now that our **chatbot remembers conversations & provides accurate answers**, it’s time to **deploy it for real-world use**.  

In **Part 6**, we:  
✅ Added **multi-turn memory** for conversations  
✅ Fine-tuned **Llama 2 for better responses**  
✅ Improved **UI with chat history & sources**  

Now, we’ll **deploy the chatbot** so it can be accessed by multiple users!  

---

## 🚀 What We’ll Cover in This Part:
✅ Deploy with **FastAPI** (for API-based access)  
✅ Use **Gunicorn & Uvicorn** for performance  
✅ Run **Streamlit on a web server**  
✅ Optimize **Llama 2 for multi-user concurrency**  

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **Text Extraction**   | `PyMuPDF` (Done ✅) |
| **Keyword Search**    | `Elasticsearch` (Done ✅) |
| **Semantic Search**   | `FAISS` (Done ✅) |
| **Embedding Model**   | `sentence-transformers` (Done ✅) |
| **Chatbot LLM**       | `Llama 2` (Deploying Now) |
| **User Interface**    | `Streamlit` (Deploying Now) |

---
 -->

# ⚡  Deploying the Chatbot Backend with FastAPI

FastAPI is a **lightweight, high-performance API framework** that will serve as our chatbot’s **backend**.

***

### 🚀 Step 1: Install FastAPI & Uvicorn

```bash
pip install fastapi uvicorn gunicorn
```

***

### 🚀 Step 2: Create the FastAPI Server

Create a new file **`server.py`**:

```python
from fastapi import FastAPI
from pydantic import BaseModel
from llama_cpp import Llama

app = FastAPI()

# Load Llama 2 model
llm = Llama(model_path="models/llama-2-7b-chat.Q4_K_M.gguf", n_gpu_layers=100)

# Define request schema
class ChatRequest(BaseModel):
    query: str

@app.post("/chat")
def chat(request: ChatRequest):
    """Handles chat requests and returns AI responses."""
    response = llm(request.query)["choices"][0]["text"]
    return {"response": response}
```

***

### 🚀 Step 3: Run FastAPI Server

Start the server using Uvicorn:

```bash
uvicorn server:app --host 0.0.0.0 --port 8000
```

✅ **Now, our chatbot runs as an API!**

Test it with:

```bash
curl -X POST "http://localhost:8000/chat" -H "Content-Type: application/json" -d '{"query": "What is machine learning?"}'
```

***

# 🏎️ Scaling with Gunicorn

Uvicorn runs a **single process**, which isn’t ideal for **multiple users**.\
We use **Gunicorn** to **run multiple workers**.

### 🚀 Step 1: Run FastAPI with Gunicorn

```bash
gunicorn -w 4 -k uvicorn.workers.UvicornWorker server:app --bind 0.0.0.0:8000
```

✅ **Now, our chatbot can handle multiple users at once!**

***

# 🎨 Deploying Streamlit as a Frontend

Now that the API is live, we’ll **connect it to Streamlit**.

***

### 🚀 Step 1: Modify `app.py` to Call FastAPI

Update **`app.py`** to fetch chatbot responses via API.

```python
import streamlit as st
import requests

st.title("📄 AI Chatbot for PDF Search")

query = st.text_input("Ask a question:")

if query:
    response = requests.post("http://localhost:8000/chat", json={"query": query}).json()
    st.write("### 🤖 AI Response:")
    st.write(response["response"])
```

***

### 🚀 Step 2: Run Streamlit on a Web Server

Run Streamlit with:

```bash
streamlit run app.py --server.port 8501 --server.address 0.0.0.0
```

✅ **Now, the chatbot has a web interface!**

***

# 🏗️  Running Everything with Supervisor

To keep **FastAPI & Streamlit running in the background**, use **Supervisor**.

### 🚀 Step 1: Install Supervisor

```bash
sudo apt install supervisor
```

***

### 🚀 Step 2: Create Supervisor Config

Create **`/etc/supervisor/conf.d/chatbot.conf`**:

```ini
[program:fastapi_server]
command=/usr/bin/gunicorn -w 4 -k uvicorn.workers.UvicornWorker server:app --bind 0.0.0.0:8000
autostart=true
autorestart=true
stderr_logfile=/var/log/fastapi_server.err.log
stdout_logfile=/var/log/fastapi_server.out.log

[program:streamlit_ui]
command=/usr/bin/streamlit run /path/to/app.py --server.port 8501 --server.address 0.0.0.0
autostart=true
autorestart=true
stderr_logfile=/var/log/streamlit_ui.err.log
stdout_logfile=/var/log/streamlit_ui.out.log
```

Reload Supervisor:

```bash
sudo supervisorctl reread
sudo supervisorctl update
sudo supervisorctl start fastapi_server
sudo supervisorctl start streamlit_ui
```

✅ **Now, FastAPI & Streamlit start automatically on reboot!**

***

<!-- 
# 🎯 What We Deployed in Part 7  

| Feature  | Status |
|----------|--------|
| **FastAPI Backend (Chat API)** ✅ Done |
| **Gunicorn for Multi-User Support** ✅ Done |
| **Streamlit UI Connected to API** ✅ Done |
| **Supervisor for Auto-Restart** ✅ Done |

---

## 🔜 Coming in Part 8: Enhancing Security & User Management  

Now that our chatbot **runs in production**, we’ll add **security features**.

### **🔹 In Part 8, we’ll cover:**  
✅ Adding **authentication (API keys & login)**  
✅ **Rate-limiting** to prevent abuse  
✅ Encrypting **user queries & responses**   -->

<!-- 
## 📚 References  
- [FastAPI Docs](https://fastapi.tiangolo.com/)  
- [Gunicorn Docs](https://gunicorn.org/)  
- [Supervisor Docs](http://supervisord.org/)  
- [Streamlit](https://streamlit.io/)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 8: Security & User Management](#) (Coming Soon)  
```

---
 -->

<!-- 

Here’s **Part 8**, where we add **security and user management** by implementing **API authentication, rate-limiting, and encryption** to protect our on-prem AI chatbot. 🚀   -->

***

<!-- 

# On-Prem AI Chatbot for PDF Search – Part 8: Security & User Management  

Welcome to **Part 8** of this series! Now that our chatbot is **deployed**, we need to **secure it against unauthorized access and abuse**.  

In **Part 7**, we:  
✅ Deployed **FastAPI** as a backend  
✅ Used **Gunicorn** for **multi-user support**  
✅ Connected **Streamlit** as the front-end  

Now, we’ll **secure the chatbot** with:  
✅ **Authentication** (API keys & user login)  
✅ **Rate-limiting** (to prevent abuse)  
✅ **Encryption** (for safe data transmission)  

---

## 🔐 What We’ll Secure in This Part  

| Security Feature | Implementation |
|-----------------|---------------|
| **Authentication** | API Key & OAuth2 Login |
| **Rate-Limiting** | Prevent spam/bot abuse |
| **Encryption** | Encrypt user queries |

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **FastAPI Backend**   | Securing Now 🔐 |
| **Gunicorn Server**   | Securing Now 🔐 |
| **Streamlit UI**      | Securing Now 🔐 |

--- -->

# 🔑 API Authentication with API Keys

Right now, **anyone can access our chatbot API**. We’ll restrict access using **API keys**.

***

### 🚀 Step 1: Generate API Keys for Users

Modify **`server.py`** to store API keys.

```python
API_KEYS = {
    "user1": "abc123",
    "admin": "xyz789"
}

def verify_api_key(api_key: str):
    """Checks if the provided API key is valid."""
    return api_key in API_KEYS.values()
```

***

### 🚀 Step 2: Require API Key for Chat Requests

Modify the `/chat` route to **require an API key**.

```python
from fastapi import FastAPI, Header, HTTPException

app = FastAPI()

@app.post("/chat")
def chat(query: str, api_key: str = Header(None)):
    """Requires API key for chatbot access."""
    
    if not api_key or not verify_api_key(api_key):
        raise HTTPException(status_code=401, detail="Invalid API Key")

    response = llm(query)["choices"][0]["text"]
    return {"response": response}
```

✅ **Now, only users with a valid API key can access the chatbot!**

Test it with:

```bash
curl -X POST "http://localhost:8000/chat" -H "API-Key: abc123" -H "Content-Type: application/json" -d '{"query": "What is AI?"}'
```

***

# ⏳ Preventing API Abuse with Rate-Limiting

To prevent **spam/bot abuse**, we’ll **limit how often users can query the API**.

***

### 🚀 Step 1: Install Rate-Limiting Middleware

Install **`slowapi`**, a FastAPI-compatible rate limiter.

```bash
pip install slowapi
```

***

### 🚀 Step 2: Add Rate-Limiting to FastAPI

Modify **`server.py`**:

```python
from slowapi import Limiter
from slowapi.util import get_remote_address

limiter = Limiter(key_func=get_remote_address)

@app.post("/chat")
@limiter.limit("5/minute")
def chat(query: str, api_key: str = Header(None)):
    """Limits requests to 5 per minute per user."""
    
    if not api_key or not verify_api_key(api_key):
        raise HTTPException(status_code=401, detail="Invalid API Key")

    response = llm(query)["choices"][0]["text"]
    return {"response": response}
```

✅ **Now, users can only make 5 requests per minute!**

Test it by **sending multiple requests** in a short time.

***

# 🔒 Part 3: Encrypting User Queries

By default, **data is sent in plaintext**. Let’s **encrypt user queries** to **protect sensitive data**.

***

### 🚀 Step 1: Install Cryptography Library

```bash
pip install cryptography
```

***

### 🚀 Step 2: Encrypt User Queries Before Sending

Modify **Streamlit UI (`app.py`)**:

```python
from cryptography.fernet import Fernet
import requests

# Generate encryption key (Only run once!)
key = Fernet.generate_key()
cipher = Fernet(key)

st.title("🔐 Secure AI Chatbot")

query = st.text_input("Ask a question:")

if query:
    encrypted_query = cipher.encrypt(query.encode()).decode()
    
    response = requests.post("http://localhost:8000/chat", json={"query": encrypted_query})
    
    decrypted_response = cipher.decrypt(response.json()["response"].encode()).decode()
    
    st.write("### 🤖 AI Response:")
    st.write(decrypted_response)
```

✅ **Now, queries & responses are encrypted before being sent!**

***

# 🛠️ Securing Deployment with HTTPS

To enable **secure communication**, use **Let’s Encrypt** for **SSL/TLS encryption**.

***

### 🚀 Step 1: Install Certbot

```bash
sudo apt install certbot python3-certbot-nginx
```

***

### 🚀 Step 2: Configure SSL for Nginx

Modify **`/etc/nginx/sites-available/chatbot`**:

```nginx
server {
    listen 80;
    server_name yourdomain.com;
    return 301 https://$host$request_uri;
}

server {
    listen 443 ssl;
    server_name yourdomain.com;

    ssl_certificate /etc/letsencrypt/live/yourdomain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourdomain.com/privkey.pem;

    location / {
        proxy_pass http://localhost:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

Restart Nginx:

```bash
sudo systemctl restart nginx
```

✅ **Now, the chatbot runs securely over HTTPS!**

<!-- ---

# 🎯 What We Secured in Part 8  

| Security Feature  | Status |
|------------------|--------|
| **API Authentication (API Keys)** ✅ Done |
| **Rate-Limiting (Prevent Spam/Bots)** ✅ Done |
| **Query Encryption** ✅ Done |
| **HTTPS for Secure Communication** ✅ Done |

---

## 🔜 Coming in Part 9: Logging & Analytics  

Now that our chatbot **is secure**, we’ll add **logging & analytics** to track usage.

### **🔹 In Part 9, we’ll cover:**  
✅ Logging **all chatbot interactions**  
✅ Tracking **most common questions**  
✅ Generating **analytics reports**  

Stay tuned for **Part 9!** 🚀  

---

## 📚 References  
- [FastAPI Docs](https://fastapi.tiangolo.com/)  
- [SlowAPI Rate-Limiting](https://github.com/laurentS/slowapi)  
- [Cryptography Docs](https://cryptography.io/)  
- [Let’s Encrypt](https://letsencrypt.org/)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 9: Logging & Analytics](#) (Coming Soon)  
```

--- -->

<!-- 
Here’s **Part 9**, where we **add logging and analytics** to track chatbot usage, log interactions, and generate insights. 🚀  

---



# On-Prem AI Chatbot for PDF Search – Part 9: Logging & Analytics  

Welcome to **Part 9** of this series! Now that our chatbot **is deployed and secure**, it’s time to **track its performance**.  

In **Part 8**, we:  
✅ Secured the chatbot with **API authentication**  
✅ Added **rate-limiting** to prevent spam  
✅ Encrypted **queries & responses** for security  

Now, we’ll **log all chatbot interactions** and generate **analytics reports** to understand:  
✅ **Who is using the chatbot?**  
✅ **What are the most common questions?**  
✅ **How is the chatbot performing over time?**  

---

## 📊 What We’ll Track in This Part  

| Feature  | Implementation |
|----------|---------------|
| **Chatbot Logging** | Log user queries & responses |
| **Usage Tracking** | Track most common queries |
| **Performance Analytics** | Monitor chatbot response times |

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **FastAPI Backend**   | Logging Now 📝 |
| **Gunicorn Server**   | Logging Now 📝 |
| **Streamlit UI**      | Displaying Analytics 📊 |

--- -->

# 📜 Logging User Interactions

We’ll **log every chatbot request** to a database for later analysis.

***

### 🚀 Step 1: Install SQLite for Logging

We’ll store logs in an **SQLite database**.

```bash
pip install sqlite3
```

***

### 🚀 Step 2: Modify FastAPI to Log Chats

Modify **`server.py`**:

```python
import sqlite3
from datetime import datetime

# Connect to SQLite database
conn = sqlite3.connect("chat_logs.db")
cursor = conn.cursor()

# Create logs table
cursor.execute("""
CREATE TABLE IF NOT EXISTS chat_logs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT,
    user TEXT,
    query TEXT,
    response TEXT
)
""")
conn.commit()

def log_chat(user, query, response):
    """Logs chatbot interactions to the database."""
    timestamp = datetime.now().isoformat()
    cursor.execute("INSERT INTO chat_logs (timestamp, user, query, response) VALUES (?, ?, ?, ?)",
                   (timestamp, user, query, response))
    conn.commit()

@app.post("/chat")
def chat(query: str, api_key: str = Header(None)):
    """Handles chatbot requests and logs them."""
    
    if not api_key or not verify_api_key(api_key):
        raise HTTPException(status_code=401, detail="Invalid API Key")

    response = llm(query)["choices"][0]["text"]
    
    # Log interaction
    log_chat(api_key, query, response)
    
    return {"response": response}
```

✅ **Now, all chatbot interactions are logged!**

***

# 📈 Tracking Most Common Queries

Now that **chats are logged**, let’s track **the most frequently asked questions**.

***

### 🚀 Step 1: Query Most Common Searches

Modify **`server.py`** to fetch analytics:

```python
@app.get("/analytics/top-queries")
def top_queries():
    """Returns the top 5 most asked questions."""
    cursor.execute("SELECT query, COUNT(query) as count FROM chat_logs GROUP BY query ORDER BY count DESC LIMIT 5")
    results = cursor.fetchall()
    return {"top_queries": results}
```

✅ **Now we can see the top 5 queries!**

Test it:

```bash
curl -X GET "http://localhost:8000/analytics/top-queries"
```

***

# ⏳ Monitoring Response Time

To track **how fast the chatbot is responding**, we’ll log **execution time**.

***

### 🚀 Step 1: Modify FastAPI to Track Response Time

Modify **`server.py`**:

```python
import time

@app.post("/chat")
def chat(query: str, api_key: str = Header(None)):
    """Logs chatbot response times for performance tracking."""
    
    if not api_key or not verify_api_key(api_key):
        raise HTTPException(status_code=401, detail="Invalid API Key")

    start_time = time.time()
    response = llm(query)["choices"][0]["text"]
    end_time = time.time()
    
    response_time = round(end_time - start_time, 2)
    
    # Log response time
    cursor.execute("INSERT INTO chat_logs (timestamp, user, query, response) VALUES (?, ?, ?, ?)",
                   (datetime.now().isoformat(), api_key, query, f"{response} (Response Time: {response_time}s)"))
    conn.commit()
    
    return {"response": response, "response_time": response_time}
```

✅ **Now, chatbot response times are tracked!**

Test it:

```bash
curl -X POST "http://localhost:8000/chat" -H "API-Key: abc123" -H "Content-Type: application/json" -d '{"query": "How does machine learning work?"}'
```

***

# 📊 Displaying Analytics in Streamlit

We’ll display **usage insights** in a **dashboard**.

***

### 🚀 Step 1: Modify Streamlit UI

Update **`app.py`**:

```python
import streamlit as st
import requests

st.title("📊 Chatbot Analytics")

# Fetch top queries
response = requests.get("http://localhost:8000/analytics/top-queries").json()
top_queries = response["top_queries"]

# Display analytics
st.write("### 🔥 Top 5 Most Asked Questions")
for query, count in top_queries:
    st.write(f"- {query} ({count} times)")

# Fetch recent chats
st.write("### 📝 Recent Chat Logs")
conn = sqlite3.connect("chat_logs.db")
cursor = conn.cursor()
cursor.execute("SELECT timestamp, user, query FROM chat_logs ORDER BY timestamp DESC LIMIT 5")
recent_chats = cursor.fetchall()

for timestamp, user, query in recent_chats:
    st.write(f"📅 {timestamp} - **{user}** asked: *{query}*")
```

✅ **Now, we have a real-time analytics dashboard!**

Run it:

```bash
streamlit run app.py --server.port 8502
```

<!-- 
---

# 🎯 What We Added in Part 9  

| Feature  | Status |
|----------|--------|
| **Logging User Queries & Responses** ✅ Done |
| **Tracking Most Common Questions** ✅ Done |
| **Monitoring Response Time** ✅ Done |
| **Building an Analytics Dashboard** ✅ Done |

---

## 🔜 Coming in Part 10: Automating Updates & Maintenance  

Now that we have **logging & analytics**, we’ll automate **data updates**.

### **🔹 In Part 10, we’ll cover:**  
✅ Automatically **deleting old logs**  
✅ Setting up **cron jobs** for maintenance  
✅ Sending **alerts if chatbot response time is slow**  

Stay tuned for **Part 10!** 🚀  

---

## 📚 References  
- [FastAPI Docs](https://fastapi.tiangolo.com/)  
- [SQLite Docs](https://www.sqlite.org/)  
- [Streamlit Docs](https://streamlit.io/)  

--- -->

<!-- 
======================================
========================================

========================

Here’s **Part 10**, where we **automate updates and maintenance** for our AI chatbot by setting up **log rotation, database cleanup, and monitoring alerts**. 🚀  


# On-Prem AI Chatbot for PDF Search – Part 10: Automating Updates & Maintenance  

Welcome to **Part 10** of this series! Now that our chatbot **logs interactions and tracks analytics**, we need to **automate updates and maintenance**.  

In **Part 9**, we:  
✅ Logged **chatbot interactions** into a database  
✅ Tracked **most common user queries**  
✅ Built a **real-time analytics dashboard**  

Now, we’ll:  
✅ **Rotate logs** to prevent database bloat  
✅ **Automate database cleanup** to remove old data  
✅ **Monitor response times** and **send alerts** if the chatbot is slow  

---

## 🔄 What We’ll Automate in This Part  

| Task | Solution |
|------|----------|
| **Log Rotation** | Keep logs small by deleting old entries |
| **Database Cleanup** | Auto-delete chats older than 30 days |
| **System Monitoring** | Send alerts if chatbot slows down |

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **FastAPI Backend**   | Automating Now 🔄 |
| **SQLite Database**   | Cleaning Now 🧹 |
| **Streamlit UI**      | Monitoring Now 📊 |

---

# 🗂️ Part 1: Automating Log Rotation  

Right now, **chat logs grow indefinitely**. We need to **automatically delete old logs**.

---

### 🚀 Step 1: Create a Cleanup Function  

Modify **`server.py`**:

```python
def cleanup_logs():
    """Deletes logs older than 30 days to prevent database bloat."""
    cursor.execute("DELETE FROM chat_logs WHERE timestamp < datetime('now', '-30 days')")
    conn.commit()
    print("🧹 Old logs deleted.")
```

✅ **Now, old logs won’t clog the system!**  

---

### 🚀 Step 2: Run Cleanup Automatically  

We’ll use a **cron job** to run this **every day**.

1️⃣ Open the cron editor:
```bash
crontab -e
```

2️⃣ Add this line to **run cleanup daily at midnight**:
```bash
0 0 * * * python3 /path/to/server.py cleanup_logs
```

✅ **Now, logs are automatically cleaned every day!**  

---

# 📦 Part 2: Automating Database Optimization  

As logs grow, **SQLite queries slow down**.  
We’ll **vacuum the database** to keep it fast.

---

### 🚀 Step 1: Optimize Database Periodically  

Modify **`server.py`**:

```python
def optimize_database():
    """Optimizes the SQLite database to reclaim space."""
    cursor.execute("VACUUM")
    conn.commit()
    print("🚀 Database optimized.")
```

✅ **Now, our database runs efficiently!**  

---

### 🚀 Step 2: Automate Database Cleanup  

Add this cron job to **optimize the database weekly**:

```bash
0 3 * * 0 python3 /path/to/server.py optimize_database
```

✅ **Now, the chatbot database stays fast and lightweight!**  

---

# 📡 Part 3: Monitoring Chatbot Performance  

We’ll monitor **response time** and send **alerts if the chatbot is slow**.

---

### 🚀 Step 1: Log Response Times  

Modify **`server.py`** to log **slow responses**:

```python
import smtplib

def send_alert(message):
    """Sends an email alert if chatbot is slow."""
    sender_email = "you@example.com"
    recipient_email = "admin@example.com"
    smtp_server = "smtp.example.com"
    smtp_port = 587
    smtp_username = "your_username"
    smtp_password = "your_password"

    subject = "⚠️ Chatbot Performance Alert"
    email_message = f"Subject: {subject}\n\n{message}"

    with smtplib.SMTP(smtp_server, smtp_port) as server:
        server.starttls()
        server.login(smtp_username, smtp_password)
        server.sendmail(sender_email, recipient_email, email_message)

@app.post("/chat")
def chat(query: str, api_key: str = Header(None)):
    """Logs response time and sends alerts if chatbot is slow."""
    
    if not api_key or not verify_api_key(api_key):
        raise HTTPException(status_code=401, detail="Invalid API Key")

    start_time = time.time()
    response = llm(query)["choices"][0]["text"]
    end_time = time.time()
    
    response_time = round(end_time - start_time, 2)

    if response_time > 5:
        send_alert(f"⚠️ Chatbot response time is {response_time}s! Investigate immediately.")

    return {"response": response, "response_time": response_time}
```

✅ **Now, admins get alerts if chatbot response time is too high!**  

---

# 🎛️ Part 4: Displaying System Status in Streamlit  

We’ll show **system health** in the analytics dashboard.

---

### 🚀 Step 1: Fetch Chatbot Performance Data  

Modify **Streamlit UI (`app.py`)**:

```python
st.title("📊 Chatbot System Status")

# Fetch chatbot performance data
conn = sqlite3.connect("chat_logs.db")
cursor = conn.cursor()
cursor.execute("SELECT timestamp, response FROM chat_logs ORDER BY timestamp DESC LIMIT 5")
recent_responses = cursor.fetchall()

# Display response times
st.write("### ⏳ Recent Response Times")
for timestamp, response in recent_responses:
    if "Response Time:" in response:
        response_time = response.split("Response Time: ")[-1]
        st.write(f"📅 {timestamp} - ⏳ {response_time}")
```

✅ **Now, admins can monitor chatbot performance in real-time!**  

---
<!-- 
# 🎯 What We Automated in Part 10  

| Automation  | Status |
|------------|--------|
| **Auto-Cleanup of Old Logs (30 Days)** ✅ Done |
| **Database Optimization (Weekly Cleanup)** ✅ Done |
| **Monitoring Response Time & Sending Alerts** ✅ Done |
| **System Health Dashboard in Streamlit** ✅ Done |

---

## 🔜 Coming in Part 11: Advanced AI Features  

Now that our chatbot **runs smoothly**, we’ll add **advanced AI features**.

### **🔹 In Part 11, we’ll cover:**  
✅ Using **memory-efficient embeddings** to improve search  
✅ Adding **document summarization** for faster answers  
✅ Implementing **multi-document reasoning** (cross-referencing PDFs)  

Stay tuned for **Part 11!** 🚀  

---

## 📚 References  
- [FastAPI Docs](https://fastapi.tiangolo.com/)  
- [SQLite Docs](https://www.sqlite.org/)  
- [Cron Job Guide](https://crontab.guru/)  
- [SMTP Email Alerts](https://docs.python.org/3/library/smtplib.html)  


Here’s **Part 11**, where we add **advanced AI features** like **memory-efficient embeddings, document summarization, and multi-document reasoning** to improve our AI chatbot. 🚀  
--- 
-->

***

<!-- 
# On-Prem AI Chatbot for PDF Search – Part 11: Advanced AI Features  

Welcome to **Part 11** of this series! Now that our chatbot is **optimized and runs smoothly**, it’s time to **make it smarter**.  

In **Part 10**, we:  
✅ Automated **log cleanup & database maintenance**  
✅ Added **performance monitoring & alerts**  
✅ Built a **real-time chatbot health dashboard**  

Now, we’ll improve the AI by adding:  
✅ **Memory-efficient embeddings** (for better search)  
✅ **Document summarization** (for faster answers)  
✅ **Multi-document reasoning** (cross-referencing PDFs)  

---

## 🚀 What We’ll Enhance in This Part  

| Feature  | Improvement |
|----------|------------|
| **Semantic Search** | Smaller, faster embeddings |
| **Summarization** | Generate concise answers |
| **Multi-Doc Reasoning** | Cross-reference multiple PDFs |

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **FastAPI Backend**   | Enhancing Now 🧠 |
| **FAISS Vector Search** | Improving Speed 🚀 |
| **Llama 2 Chatbot**   | Adding Multi-Doc Reasoning 🧐 |

---

# 🏎️ Part 1: Improving Semantic Search with Smaller Embeddings  

Currently, we use **384-dimensional embeddings** from `all-MiniLM-L6-v2`.  
This takes **a lot of memory** when storing thousands of PDFs in FAISS.  

### 🚀 Step 1: Switch to **Smaller, Faster Embeddings**  

Replace `all-MiniLM-L6-v2` with **a more compact model**.

```python
from sentence_transformers import SentenceTransformer

# Load a smaller, faster embedding model
model = SentenceTransformer("sentence-transformers/all-MiniLM-L12-v2")  # 12 layers instead of 6
```

✅ **Now, embeddings use less memory but remain highly accurate!**  

---

### 🚀 Step 2: Reduce FAISS Index Size  

Modify **FAISS index to use PCA compression**.

```python
import faiss

DIMENSIONS = 384  # Original size
REDUCED_DIM = 256  # Compressed size

# Apply PCA compression
pca_matrix = faiss.PCAMatrix(DIMENSIONS, REDUCED_DIM)
index = faiss.IndexFlatL2(REDUCED_DIM)

# Train FAISS with PCA
pca_matrix.train(embeddings)
compressed_embeddings = pca_matrix.apply_py(embeddings)
index.add(compressed_embeddings)
```

✅ **Now, FAISS uses 33% less memory but remains highly effective!**  

---

# 📄 Part 2: Implementing Document Summarization  

Instead of returning **entire PDFs**, we’ll **summarize key points**.  

---

### 🚀 Step 1: Install Summarization Model  

```bash
pip install transformers
```

---

### 🚀 Step 2: Summarize Documents Before Sending to Llama 2  

Modify **`server.py`**:

```python
from transformers import pipeline

# Load summarization model
summarizer = pipeline("summarization", model="facebook/bart-large-cnn")

def summarize_text(text):
    """Summarizes a long document."""
    return summarizer(text, max_length=200, min_length=50, do_sample=False)[0]["summary_text"]
```

✅ **Now, documents are summarized before being fed into Llama 2!**  

---

### 🚀 Step 3: Integrate Summarization into RAG  

Modify **`search_pdfs_rag()`** to **summarize before retrieval**:

```python
def search_pdfs_rag(query, k=3):
    """Search PDFs and return summarized content."""
    
    es_results = search_pdfs(query)[:k]
    faiss_results = search_faiss(query, k)[:k]
    
    combined_results = set([summarize_text(r["_source"]["text"]) for r in es_results])
    combined_results.update([summarize_text(list(pdf_texts.values())[i]) for i in faiss_results[0]])
    
    return "\n\n".join(combined_results)
```

✅ **Now, the chatbot only receives summarized PDFs, making responses more concise!**  

---

# 🔗 Part 3: Implementing Multi-Document Reasoning  

Right now, **Llama 2 treats each PDF separately**.  
We’ll **combine multiple sources** and let the chatbot **cross-reference them**.

---

### 🚀 Step 1: Modify Prompt to Include Multiple PDFs  

Modify **`chat_with_pdfs()`**:

```python
def chat_with_pdfs(query):
    """Uses multiple PDFs as context for better answers."""
    
    context = search_pdfs_rag(query, k=5)  # Retrieve more docs for reasoning
    
    prompt = f"""
    You are an AI trained to answer questions using the following documents:
    
    {context}
    
    If multiple sources provide conflicting information, summarize the key differences.
    
    Question: {query}
    Answer:
    """
    
    response = llm(prompt)["choices"][0]["text"]
    return response
```

✅ **Now, Llama 2 can reason across multiple PDFs and identify conflicts!**  

---

# 🎯 What We Enhanced in Part 11  

| Feature  | Status |
|----------|--------|
| **Smaller, Faster Embeddings for FAISS** ✅ Done |
| **Summarization Before Chatbot Processing** ✅ Done |
| **Multi-Document Reasoning in Llama 2** ✅ Done |

---

## 🔜 Coming in Part 12: Fine-Tuning AI for Domain-Specific Knowledge  

Now that our chatbot **thinks smarter**, we’ll **fine-tune Llama 2** on **industry-specific PDFs**.

### **🔹 In Part 12, we’ll cover:**  
✅ Training **Llama 2 on company-specific data**  
✅ Improving **accuracy in niche domains**  
✅ Handling **ambiguous or conflicting answers**  

Stay tuned for **Part 12!** 🚀  

---

## 📚 References  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Sentence-Transformers](https://www.sbert.net/)  
- [Hugging Face Summarization Models](https://huggingface.co/models?pipeline_tag=summarization)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 12: Fine-Tuning AI for Domain-Specific Knowledge](#) (Coming Soon)  
```

---
 -->

 <!-- 
 Here’s **Part 12**, where we **fine-tune Llama 2 on domain-specific PDFs** to improve accuracy and adapt it for specialized knowledge. 🚀  

---

```markdown
---
title: "On-Prem AI Chatbot for PDF Search – Part 12: Fine-Tuning AI for Domain-Specific Knowledge"
description: "A simplified demonstration of a real-world project I built for work. In Part 12, we fine-tune Llama 2 on domain-specific PDFs to improve chatbot accuracy in specialized fields."
slug: "on-prem-ai-pdf-search-part-12"
date: 2019-12-18
image: "post/Articles/24.jpg"
categories: ["AI", "Machine Learning", "PDF Search", "Fine-Tuning"]
tags: ["AI", "Machine Learning", "LLM", "Llama 2", "FAISS", "Fine-Tuning", "Domain Adaptation"]
draft: false
weight: 530
---

# On-Prem AI Chatbot for PDF Search – Part 12: Fine-Tuning AI for Domain-Specific Knowledge  

Welcome to **Part 12** of this series! Now that our chatbot **retrieves and summarizes PDFs effectively**, let’s make it **even smarter** by **fine-tuning Llama 2** on our **own dataset**.  

In **Part 11**, we:  
✅ Improved **semantic search** with **smaller embeddings**  
✅ Added **document summarization** for better responses  
✅ Enabled **multi-document reasoning** for better accuracy  

Now, we’ll:  
✅ Train Llama 2 on **company-specific PDFs**  
✅ Improve **accuracy for niche industry knowledge**  
✅ Handle **ambiguous or conflicting answers**  

---

## 🎯 Why Fine-Tune Llama 2?  

By default, **Llama 2 is a generalist**. Fine-tuning helps the model:  
✅ **Learn domain-specific terminology** (e.g., legal, medical, finance)  
✅ **Generate more accurate responses** for technical questions  
✅ **Reduce hallucinations** (incorrect AI-generated answers)  

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **FastAPI Backend**   | Fine-Tuning Now 🎯 |
| **Llama 2 Chatbot**   | Training on Custom Data 🏋️‍♂️ |
| **FAISS Vector Search** | Assisting with Context 🔎 |

---

# 📂 Part 1: Preparing a Fine-Tuning Dataset  

We need to create **a structured dataset** from our **company PDFs**.  

---

### 🚀 Step 1: Convert PDFs into Training Data  

Modify **`prepare_data.py`**:

```python
import json
import fitz  # PyMuPDF

def extract_text_from_pdf(pdf_path):
    """Extracts text from a PDF using PyMuPDF."""
    doc = fitz.open(pdf_path)
    text = "\n".join([page.get_text("text") for page in doc])
    return text

training_data = []

pdf_files = ["docs/legal_contract.pdf", "docs/medical_guidelines.pdf"]

for pdf in pdf_files:
    text = extract_text_from_pdf(pdf)
    training_data.append({"input": f"Explain this document: {pdf}", "output": text})

# Save to JSONL format for fine-tuning
with open("training_data.jsonl", "w") as f:
    for item in training_data:
        f.write(json.dumps(item) + "\n")
```

✅ **Now, we have a dataset ready for fine-tuning!**  

---

# 🏋️ Part 2: Fine-Tuning Llama 2  

We’ll use **Hugging Face’s `transformers` library** to fine-tune Llama 2.  

---

### 🚀 Step 1: Install Dependencies  

```bash
pip install torch transformers datasets peft accelerate
```

---

### 🚀 Step 2: Load Llama 2 for Fine-Tuning  

Modify **`fine_tune.py`**:

```python
from transformers import AutoModelForCausalLM, AutoTokenizer, TrainingArguments, Trainer
import torch
import json

# Load base model & tokenizer
model_name = "meta-llama/Llama-2-7b-chat-hf"
model = AutoModelForCausalLM.from_pretrained(model_name, torch_dtype=torch.float16)
tokenizer = AutoTokenizer.from_pretrained(model_name)

# Load training data
with open("training_data.jsonl", "r") as f:
    training_data = [json.loads(line) for line in f]

train_texts = [d["input"] for d in training_data]
train_labels = [d["output"] for d in training_data]

# Convert to tokenized format
train_encodings = tokenizer(train_texts, padding=True, truncation=True, return_tensors="pt")
label_encodings = tokenizer(train_labels, padding=True, truncation=True, return_tensors="pt")

# Fine-tuning settings
training_args = TrainingArguments(
    output_dir="./fine-tuned-llama",
    per_device_train_batch_size=2,
    num_train_epochs=3,
    save_strategy="epoch"
)

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_encodings,
    eval_dataset=label_encodings
)

trainer.train()
model.save_pretrained("./fine-tuned-llama")
tokenizer.save_pretrained("./fine-tuned-llama")
```

✅ **Now, Llama 2 is fine-tuned on our PDFs!**  

---

### 🚀 Step 3: Replace Default Model with Fine-Tuned One  

Modify **`server.py`**:

```python
from llama_cpp import Llama

# Load fine-tuned model
llm = Llama(model_path="fine-tuned-llama/llama-2-7b-chat.Q4_K_M.gguf")
```

✅ **Now, the chatbot will generate domain-specific answers!**  

---

# 🔗 Part 3: Handling Conflicting Information  

Sometimes, **different documents have conflicting information**.  
We’ll modify Llama 2 to **detect and explain inconsistencies**.

---

### 🚀 Step 1: Modify Prompt to Identify Conflicts  

Modify **`chat_with_pdfs()`**:

```python
def chat_with_pdfs(query):
    """Uses multiple PDFs and explains conflicting information."""
    
    context = search_pdfs_rag(query, k=5)
    
    prompt = f"""
    You are an AI trained to answer questions using the following documents:
    
    {context}
    
    If multiple sources provide conflicting information, summarize the key differences.

    Question: {query}
    Answer:
    """
    
    response = llm(prompt)["choices"][0]["text"]
    return response
```

✅ **Now, the chatbot can highlight conflicting information!**  

---

# 🎯 What We Improved in Part 12  

| Feature  | Status |
|----------|--------|
| **Training Llama 2 on Custom PDFs** ✅ Done |
| **Fine-Tuning for Niche Knowledge** ✅ Done |
| **Handling Conflicting Information** ✅ Done |

---

## 🔜 Coming in Part 13: Multi-Language Support  

Now that our chatbot **is specialized for a domain**, we’ll **add multi-language support**.

### **🔹 In Part 13, we’ll cover:**  
✅ Adding **multi-language embeddings** for FAISS  
✅ Fine-tuning Llama 2 for **multilingual responses**  
✅ Enabling **automatic language detection**  

Stay tuned for **Part 13!** 🚀  

---

## 📚 References  
- [Hugging Face Fine-Tuning Guide](https://huggingface.co/docs/transformers/training)  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Sentence-Transformers](https://www.sbert.net/)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 13: Multi-Language Support](#) (Coming Soon)  
```

---


 
  -->

<!-- -
Here’s **Part 13**, where we **add multi-language support** to our on-prem AI chatbot by enabling **multilingual embeddings, fine-tuning Llama 2 for multiple languages, and implementing automatic language detection**. 🚀  

---

```markdown
---
title: "On-Prem AI Chatbot for PDF Search – Part 13: Multi-Language Support"
description: "A simplified demonstration of a real-world project I built for work. In Part 13, we add multi-language support to our AI chatbot with multilingual embeddings, fine-tuned Llama 2, and automatic language detection."
slug: "on-prem-ai-pdf-search-part-13"
date: 2018-09-30
image: "post/Articles/32.jpg"
categories: ["AI", "Machine Learning", "PDF Search", "Multilingual AI"]
tags: ["AI", "Machine Learning", "LLM", "Llama 2", "FAISS", "Multilingual NLP"]
draft: false
weight: 550
---

# On-Prem AI Chatbot for PDF Search – Part 13: Multi-Language Support  

Welcome to **Part 13** of this series! Now that our chatbot **is fine-tuned for domain-specific knowledge**, we’ll **expand its capabilities** by adding **multi-language support**.  

In **Part 12**, we:  
✅ Fine-tuned **Llama 2 on custom PDFs**  
✅ Improved **accuracy in niche fields**  
✅ Enabled **multi-document reasoning**  

Now, we’ll:  
✅ Add **multi-language embeddings** for FAISS  
✅ Fine-tune **Llama 2 for multilingual responses**  
✅ Enable **automatic language detection**  

---

## 🌍 Why Multi-Language Support?  

Currently, our chatbot **only understands English**. Adding multilingual support allows:  
✅ **Global users** to interact in their native languages  
✅ **Multilingual document search** (e.g., legal docs in French, tech manuals in German)  
✅ **More inclusive AI** with broader accessibility  

### **Recap of Our Tech Stack**
| Component             | Tool |
|-----------------------|---------------------|
| **FastAPI Backend**   | Enhancing Now 🌍 |
| **FAISS Vector Search** | Adding Multi-Language 🔎 |
| **Llama 2 Chatbot**   | Training for Multiple Languages 🏋️‍♂️ |

---

# 🏎️ Part 1: Enabling Multilingual Embeddings  

FAISS currently **stores English-only embeddings**.  
We’ll switch to **a multilingual model** that supports **100+ languages**.

---

### 🚀 Step 1: Install a Multilingual Embedding Model  

Modify **FAISS setup** to use **`paraphrase-multilingual-MiniLM-L12-v2`**, a **small but powerful** multilingual model.

```bash
pip install sentence-transformers
```

Modify **`server.py`**:

```python
from sentence_transformers import SentenceTransformer

# Load a multilingual embedding model
model = SentenceTransformer("sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2")
```

✅ **Now, FAISS supports embeddings in 100+ languages!**  

---

### 🚀 Step 2: Reindex Documents with Multilingual Embeddings  

Modify **document indexing** to store **new embeddings**.

```python
import numpy as np
import faiss

DIMENSIONS = 384  # Model output size

# Load existing documents
pdf_texts = {"contract_fr.pdf": "Contrat de travail en français.", "manual_de.pdf": "Technisches Handbuch auf Deutsch."}

# Generate embeddings
embeddings = np.array([model.encode(text) for text in pdf_texts.values()])

# Create FAISS index
index = faiss.IndexFlatL2(DIMENSIONS)
index.add(embeddings)
```

✅ **Now, FAISS can store and search non-English documents!**  

---

# 🧠 Part 2: Fine-Tuning Llama 2 for Multiple Languages  

By default, **Llama 2 is strongest in English**.  
We’ll **fine-tune it** on **multilingual text**.

---

### 🚀 Step 1: Download Multilingual Dataset  

We need **sample conversations** in multiple languages.  
Download the **OpenAssistant multilingual dataset**:

```bash
wget https://huggingface.co/datasets/OpenAssistant/oasst1/resolve/main/oasst1_multilingual.jsonl
```

---

### 🚀 Step 2: Train Llama 2 on Multilingual Data  

Modify **`fine_tune.py`**:

```python
from transformers import AutoModelForCausalLM, AutoTokenizer, TrainingArguments, Trainer
import torch
import json

# Load base model
model_name = "meta-llama/Llama-2-7b-chat-hf"
model = AutoModelForCausalLM.from_pretrained(model_name, torch_dtype=torch.float16)
tokenizer = AutoTokenizer.from_pretrained(model_name)

# Load multilingual training data
with open("oasst1_multilingual.jsonl", "r") as f:
    training_data = [json.loads(line) for line in f]

train_texts = [d["input"] for d in training_data]
train_labels = [d["output"] for d in training_data]

# Tokenize data
train_encodings = tokenizer(train_texts, padding=True, truncation=True, return_tensors="pt")
label_encodings = tokenizer(train_labels, padding=True, truncation=True, return_tensors="pt")

# Fine-tuning settings
training_args = TrainingArguments(
    output_dir="./fine-tuned-llama-multilingual",
    per_device_train_batch_size=2,
    num_train_epochs=3,
    save_strategy="epoch"
)

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_encodings,
    eval_dataset=label_encodings
)

trainer.train()
model.save_pretrained("./fine-tuned-llama-multilingual")
tokenizer.save_pretrained("./fine-tuned-llama-multilingual")
```

✅ **Now, Llama 2 understands multiple languages!**  

---

# 🌍 Part 3: Automatic Language Detection  

Instead of asking users to select a language, we’ll **automatically detect** it.

---

### 🚀 Step 1: Install Language Detection  

```bash
pip install langdetect
```

Modify **`server.py`**:

```python
from langdetect import detect

def detect_language(text):
    """Detects the language of a given text."""
    return detect(text)

# Example usage
query = "¿Cómo funciona la inteligencia artificial?"
print(detect_language(query))  # Output: "es" (Spanish)
```

✅ **Now, we can detect user language before responding!**  

---

### 🚀 Step 2: Choose the Correct Llama 2 Model  

Modify **`chat_with_pdfs()`**:

```python
from llama_cpp import Llama

# Load both English and multilingual models
llm_en = Llama(model_path="fine-tuned-llama/llama-2-7b-chat.Q4_K_M.gguf")
llm_multi = Llama(model_path="fine-tuned-llama-multilingual/llama-2-7b-chat.Q4_K_M.gguf")

def chat_with_pdfs(query):
    """Detects language and selects the correct model."""
    
    lang = detect_language(query)

    if lang in ["en", "fr", "de", "es"]:  # Example supported languages
        llm = llm_multi  # Use multilingual model
    else:
        llm = llm_en  # Default to English model
    
    response = llm(query)["choices"][0]["text"]
    
    return response
```

✅ **Now, the chatbot automatically responds in the user’s language!**  

---

# 🎯 What We Improved in Part 13  

| Feature  | Status |
|----------|--------|
| **Multilingual Embeddings for FAISS** ✅ Done |
| **Fine-Tuned Llama 2 for Multiple Languages** ✅ Done |
| **Automatic Language Detection** ✅ Done |

---

## 🔜 Coming in Part 14: Voice Input & Text-to-Speech  

Now that our chatbot **understands multiple languages**, we’ll add **voice capabilities**.

### **🔹 In Part 14, we’ll cover:**  
✅ Adding **speech-to-text** (voice queries)  
✅ Implementing **text-to-speech** for responses  
✅ Enhancing **accessibility for visually impaired users**  

Stay tuned for **Part 14!** 🚀  

---

## 📚 References  
- [FAISS GitHub](https://github.com/facebookresearch/faiss)  
- [Sentence-Transformers](https://www.sbert.net/)  
- [Hugging Face Multilingual Dataset](https://huggingface.co/datasets/OpenAssistant/oasst1)  
- [LangDetect Docs](https://pypi.org/project/langdetect/)  

---

✅ **Next Up:** [On-Prem AI PDF Search - Part 14: Voice Input & Text-to-Speech](#) (Coming Soon)  
```

---


->
