---
title: OpenAPi-Create PDFs of your OpenApi- Generate Latex
description: Transform your OpenAPI (Swagger) JSON into a professional PDF document using Python and LaTeX.
slug: swagger-to-latex-pdf
date: 2018-06-11
image: post/Articles/IMAGES/openapi.png
categories:
  - API Documentation
  - Python
  - LaTeX
  - PDF
  - Document Imaging
  - OpenAPI
  - CI/CD
  - CI\CD
  - DevOps
  - Cloud
tags:
  - Swagger
  - OpenAPI
  - Python
  - LaTeX
  - PDF
  - Generation
draft: false
weight: 104
categories_ref:
  - API Documentation
  - Python
  - LaTeX
  - PDF
  - Document Imaging
  - OpenAPI
  - CI/CD
  - CI\CD
  - DevOps
  - Cloud
slug_calculated: https://brianbraatz.github.io/p/swagger-to-latex-pdf
lastmod: 2025-03-14T16:40:10.258Z
---
Swagger UI is **great** for interactive API docs, but sometimes you need a **professional PDF document**.

This can be for legal contract reasons\
(which happened to me :) - which is why i did this in first place )

Or maybe you are delivering a product with an API, and you want to deliver a PDF of the API as a techincal artifact for the end customer.

<!--

Maybe your boss wants **printed documentation**.  
Maybe you need a **formal API reference** for external clients.  
Maybe you just love **beautiful LaTeX formatting**.  
-->

Whatever the reason, let’s take a `swagger.json` file and turn it into **a  LaTeX-generated PDF** using Python. 🚀

***

## 🔧 Step 1: Install Dependencies

We'll use the following Python libraries:

* `json` – To parse the `swagger.json`
* `jinja2` – To generate LaTeX templates dynamically
* `subprocess` – To compile LaTeX into a PDF

### Install Required Packages:

```sh
pip install jinja2
sudo apt install texlive texlive-latex-extra pdflatex
```

(For Windows, install **MiKTeX** or **TeX Live** manually.)

***

## 📜 Step 2: Create the LaTeX Template

First, create a **LaTeX template** (`swagger_template.tex`) that will be dynamically filled using Jinja2.

```tex
\documentclass{article}
\usepackage{longtable}
\usepackage{hyperref}
\usepackage{geometry}
\geometry{a4paper, margin=1in}

\title{API Documentation}
\author{Generated from OpenAPI}
\date{\today}

\begin{document}

\maketitle

\section*{API Overview}
This document provides a detailed reference for the API endpoints.

\section*{Endpoints}

{% for path, methods in paths.items() %}
\subsection*{{Path: \texttt{{path}}}}

{% for method, details in methods.items() %}
\textbf{Method:} \texttt{{ method.upper() }} \\  
\textbf{Summary:} {{ details.summary | default("No summary provided") }} \\  
\textbf{Description:} {{ details.description | default("No description provided") }} \\  

\textbf{Parameters:}
\begin{longtable}{|p{3cm}|p{3cm}|p{8cm}|}
\hline
\textbf{Name} & \textbf{Type} & \textbf{Description} \\
\hline
{% for param in details.parameters %}
{{ param.name }} & {{ param.schema.type | default("Unknown") }} & {{ param.description | default("No description") }} \\
\hline
{% endfor %}
\end{longtable}

\textbf{Responses:}
\begin{longtable}{|p{2cm}|p{3cm}|p{9cm}|}
\hline
\textbf{Code} & \textbf{Type} & \textbf{Description} \\
\hline
{% for code, response in details.responses.items() %}
{{ code }} & {{ response.content["application/json"].schema.type | default("Unknown") }} & {{ response.description | default("No description") }} \\
\hline
{% endfor %}
\end{longtable}

{% endfor %}
{% endfor %}

\end{document}
```

***

## 📝 Step 3: Write a Python Script to Convert `swagger.json` to LaTeX

Now, create a Python script (`swagger_to_latex.py`) to read the Swagger JSON file, render it into the LaTeX template, and compile it into a **PDF**.

```python
import json
import os
import subprocess
from jinja2 import Template

# File paths
SWAGGER_JSON_FILE = "swagger.json"
LATEX_TEMPLATE_FILE = "swagger_template.tex"
GENERATED_LATEX_FILE = "api_documentation.tex"
OUTPUT_PDF = "api_documentation.pdf"

# Load Swagger JSON
with open(SWAGGER_JSON_FILE, "r") as f:
    swagger_data = json.load(f)

# Load LaTeX template
with open(LATEX_TEMPLATE_FILE, "r") as f:
    template_content = f.read()

# Create Jinja2 template
template = Template(template_content)

# Render LaTeX file with OpenAPI data
rendered_latex = template.render(paths=swagger_data.get("paths", {}))

# Write LaTeX content to file
with open(GENERATED_LATEX_FILE, "w") as f:
    f.write(rendered_latex)

print(f"✅ LaTeX file '{GENERATED_LATEX_FILE}' generated successfully.")

# Compile LaTeX to PDF
try:
    subprocess.run(["pdflatex", GENERATED_LATEX_FILE], check=True)
    print(f"✅ PDF '{OUTPUT_PDF}' generated successfully.")
except subprocess.CalledProcessError:
    print("❌ Error compiling LaTeX to PDF.")
```

***

## 🏗 Step 4: Run the Script

1️⃣ Place your **`swagger.json`** in the same directory as the script.\
2️⃣ Run the script:

```sh
python swagger_to_latex.py
```

3️⃣ If successful, your **API documentation PDF** will be generated as `api_documentation.pdf`.

***

<!-- 
## 🎨 Step 5: Customize the LaTeX Style  

Want a **fancier** document? Modify the LaTeX template:  
✅ **Change the document class**: Use `\documentclass{report}` for a multi-section doc.  
✅ **Add a cover page**: Use `\titlepage`.  
✅ **Style tables**: Use `\renewcommand{\arraystretch}{1.5}` for better table spacing.  

---
-->

## 🏗 Latex Output

```tex
\documentclass{report}
\usepackage{longtable}
\usepackage{hyperref}
\usepackage{geometry}
\usepackage{fancyhdr}

\geometry{a4paper, margin=1in}
\setcounter{secnumdepth}{0}
\pagestyle{fancy}
\fancyhf{}
\rhead{API Documentation}
\lhead{Generated from OpenAPI}
\rfoot{\thepage}

\title{API Documentation}
\author{Generated from OpenAPI Specification}
\date{\today}

\begin{document}

\maketitle
\tableofcontents

\chapter{Introduction}
This document provides a comprehensive reference for the API, including available endpoints, request parameters, response structures, and authentication methods.

\chapter{Endpoints}

\section{GET /users}
\textbf{Description:} Retrieve a list of users.

\textbf{Request Parameters:}
\begin{longtable}{|p{3cm}|p{3cm}|p{8cm}|}
\hline
\textbf{Name} & \textbf{Type} & \textbf{Description} \\
\hline
limit & integer & Number of users to retrieve (optional) \\
\hline
offset & integer & Pagination offset (optional) \\
\hline
\end{longtable}

\textbf{Response:}
\begin{longtable}{|p{2cm}|p{3cm}|p{9cm}|}
\hline
\textbf{Code} & \textbf{Type} & \textbf{Description} \\
\hline
200 & array (User) & List of user objects \\
\hline
400 & string & Invalid request parameters \\
\hline
\end{longtable}

\section{POST /users}
\textbf{Description:} Create a new user.

\textbf{Request Body:}
\begin{longtable}{|p{3cm}|p{3cm}|p{8cm}|}
\hline
\textbf{Field} & \textbf{Type} & \textbf{Description} \\
\hline
name & string & The name of the user (required) \\
\hline
email & string & The email of the user (required) \\
\hline
\end{longtable}

\textbf{Response:}
\begin{longtable}{|p{2cm}|p{3cm}|p{9cm}|}
\hline
\textbf{Code} & \textbf{Type} & \textbf{Description} \\
\hline
201 & object (User) & The created user object \\
\hline
400 & string & Invalid request body \\
\hline
\end{longtable}

\section{GET /users/{id}}
\textbf{Description:} Retrieve a user by ID.

\textbf{Path Parameters:}
\begin{longtable}{|p{3cm}|p{3cm}|p{8cm}|}
\hline
\textbf{Name} & \textbf{Type} & \textbf{Description} \\
\hline
id & integer & The unique ID of the user (required) \\
\hline
\end{longtable}

\textbf{Response:}
\begin{longtable}{|p{2cm}|p{3cm}|p{9cm}|}
\hline
\textbf{Code} & \textbf{Type} & \textbf{Description} \\
\hline
200 & object (User) & The user object \\
\hline
404 & string & User not found \\
\hline
\end{longtable}

\chapter{Authentication}
All API requests require authentication using a Bearer token in the HTTP Authorization header.

\begin{verbatim}
Authorization: Bearer <your-token-here>
\end{verbatim}

\end{document}
```

## Pdf Output

and here is what the Latex Looks like when converted into PDF\
![](/post/Articles/26/openappdfsample.png)

<!-- 
## 🔥 Conclusion  

Now, you can **automatically convert Swagger JSON into a beautiful LaTeX-generated PDF**. 🚀  

✅ **Transforms API docs into a polished, printable format**  
✅ **Automates documentation workflows**  
✅ **Great for clients, partners, and formal API specs**  

With this, your API documentation isn’t just functional—it’s **professional and elegant**.  

Now go forth and **LaTeXify your APIs like a pro!** 📜  

---

## 🔑 Key Takeaways  

| Summary        | Details |
|---------------|---------|
| **Why use LaTeX?** | Creates professional, high-quality PDF documentation. |
| **What does the Python script do?** | Converts `swagger.json` into LaTeX and compiles it to PDF. |
| **Which tools are needed?** | `jinja2` for templates, `pdflatex` for PDF compilation. |
| **How to run it?** | `python swagger_to_latex.py` |
| **How to customize output?** | Modify the LaTeX template for branding & styles. |

```

-->

### **How to Create a PDF from LaTeX (Using pdflatex)**

Once you have your LaTeX document (`api_documentation.tex`), follow these steps to generate a **PDF**.

***

## **🚀 Method 1: Using pdflatex (Recommended)**

If you have **TeX Live** (Linux/macOS) or **MiKTeX** (Windows) installed, you can compile the `.tex` file into a **PDF** using the `pdflatex` command.

### **1️⃣ Install LaTeX Compiler**

* **Linux (Ubuntu/Debian)**
  ```sh
  sudo apt install texlive texlive-latex-extra
  ```
* **macOS**
  ```sh
  brew install mactex
  ```
* **Windows**
  * Install **MiKTeX** from <https://miktex.org/download>.

***

### **2️⃣ Run pdflatex**

Navigate to the directory where your `.tex` file is located and compile it:

```sh
pdflatex api_documentation.tex
```

If your document uses **tables or references**, run `pdflatex` **twice** to ensure everything compiles correctly.

```sh
pdflatex api_documentation.tex
pdflatex api_documentation.tex
```

***

### **3️⃣ Check the Output**

After running `pdflatex`, you should see a new file:

```
api_documentation.pdf
```

Open it in any **PDF viewer** to check the formatting.

***

## **📜 Method 2: Using Overleaf (Online)**

If you don't want to install anything, use [Overleaf](https://www.overleaf.com/)—a **free online LaTeX editor**.

### **Steps**

1. Create a **new Overleaf project**.
2. Upload your **`api_documentation.tex`** file.
3. Click **Compile** to generate the PDF.
4. Click **Download PDF**.

***

## **🖥️ Method 3: Using a GUI LaTeX Editor**

If you prefer a graphical interface, install **TeXworks** (comes with MiKTeX) or **TeXmaker**.

1. Open `api_documentation.tex` in the editor.
2. Click **Compile** (Make sure **pdflatex** is selected).
3. Save and download the PDF.

***

## **🔥 Bonus: Automate with Python**

If you want to generate the PDF **automatically** using Python, add this to your script:

```python
import subprocess

latex_file = "api_documentation.tex"

# Compile the LaTeX document into a PDF
subprocess.run(["pdflatex", latex_file], check=True)
subprocess.run(["pdflatex", latex_file], check=True)  # Second pass for proper references

print("✅ PDF generated successfully!")
```

Now, every time you update `swagger.json`, the script **regenerates the PDF automatically**. 🚀

***

<!-- 

## **🎯 Conclusion**
✅ **For command-line users:** `pdflatex api_documentation.tex`  
✅ **For online users:** Use [Overleaf](https://www.overleaf.com/)  
✅ **For GUI users:** Try TeXworks or TeXmaker  
✅ **For automation:** Use Python + `pdflatex`  

Now go forth and **create beautiful API PDFs like a pro!** 📜🔥


---
title: "How to Automate PDF API Documentation Generation with Jenkins"
description: "Learn how to integrate LaTeX compilation in Jenkins to automatically generate API documentation PDFs from Swagger JSON."
slug: "automate-pdf-api-docs-jenkins"
date: 2019-02-21
image: "post/Articles/IMAGES/45.jpg"
categories: ["API Documentation", "CI/CD", "Jenkins", "LaTeX"]
tags: ["Jenkins", "Automation", "Swagger", "OpenAPI", "LaTeX", "PDF Generation"]
draft: false
weight: 621

---

So, we’ve built a **Swagger-to-LaTeX** pipeline to create **beautiful API documentation PDFs**.  
But wouldn’t it be **better if Jenkins automated this**?  

Imagine:  
✅ Every time `swagger.json` changes, Jenkins generates a new **PDF**  
✅ The documentation team always has **the latest API reference**  
✅ No one has to manually compile LaTeX ever again 🎉  

Let’s make it happen. 🚀  
-->

# How to Automate PDF API Documentation Generation with Jenkins

***

## 🔧 Step 1: Install Dependencies on Jenkins Server

### **1️⃣ Install Required Packages**

Jenkins needs:

* **Git** (to pull changes)
* **Python** (to process Swagger JSON)
* **LaTeX (pdflatex)** (to generate PDFs)

#### **For Linux (Ubuntu/Debian)**

```sh
sudo apt update
sudo apt install git python3 python3-pip texlive texlive-latex-extra -y
pip install jinja2
```

#### **For macOS**

```sh
brew install texlive python
pip3 install jinja2
```

#### **For Windows**

1. Install **MiKTeX** from [miktex.org](https://miktex.org/download).
2. Install Python from [python.org](https://www.python.org/downloads/).
3. Run:
   ```sh
   pip install jinja2
   ```

***

## 📜 Step 2: Add PDF Generation Script to Jenkins

Place this Python script in your **documentation repository** (`generate_pdf.py`).

```python
import json
import os
import subprocess
from jinja2 import Template

# File paths
SWAGGER_JSON_FILE = "swagger.json"
LATEX_TEMPLATE_FILE = "swagger_template.tex"
GENERATED_LATEX_FILE = "api_documentation.tex"
OUTPUT_PDF = "api_documentation.pdf"

# Load Swagger JSON
with open(SWAGGER_JSON_FILE, "r") as f:
    swagger_data = json.load(f)

# Load LaTeX template
with open(LATEX_TEMPLATE_FILE, "r") as f:
    template_content = f.read()

# Create Jinja2 template
template = Template(template_content)

# Render LaTeX file with OpenAPI data
rendered_latex = template.render(paths=swagger_data.get("paths", {}))

# Write LaTeX content to file
with open(GENERATED_LATEX_FILE, "w") as f:
    f.write(rendered_latex)

print(f"✅ LaTeX file '{GENERATED_LATEX_FILE}' generated successfully.")

# Compile LaTeX to PDF
try:
    subprocess.run(["pdflatex", GENERATED_LATEX_FILE], check=True)
    subprocess.run(["pdflatex", GENERATED_LATEX_FILE], check=True)  # Second pass for proper references
    print(f"✅ PDF '{OUTPUT_PDF}' generated successfully.")
except subprocess.CalledProcessError:
    print("❌ Error compiling LaTeX to PDF.")
```

***

## 🔄 Step 3: Set Up Jenkins Pipeline to Generate PDF

### **1️⃣ Create a New Jenkins Job**

1. Open **Jenkins Dashboard** → **New Item**
2. Select **Freestyle Project**
3. Name it **Generate-API-Docs-PDF**
4. Under **Source Code Management**, select **Git** and add your documentation repo URL.
5. Under **Build Triggers**, enable **Poll SCM** and set a schedule:
   ```
   H/10 * * * *
   ```
   *(Runs every 10 minutes if there’s a change)*

***

### **2️⃣ Add Build Steps**

Under **Build Steps**, click **"Add build step" → "Execute Shell"** and enter:

```sh
#!/bin/bash

DOCS_REPO="/var/lib/jenkins/workspace/Generate-API-Docs-PDF"

# Navigate to repo
cd "$DOCS_REPO"

# Pull latest changes
git pull origin main

# Run the Python script to generate the PDF
python3 generate_pdf.py

# Check if PDF was generated
if [ -f "api_documentation.pdf" ]; then
    echo "✅ PDF successfully generated."
else
    echo "❌ PDF generation failed!" >&2
    exit 1
fi
```

***

## 📩 Step 4: Automatically Upload the PDF

Once the PDF is generated, we can:\
✅ **Archive it in Jenkins**\
✅ **Push it to another repository**\
✅ **Email it to the documentation team**

### **1️⃣ Archive PDF in Jenkins**

Under **Post-Build Actions**, click **"Archive the artifacts"** and enter:

```
api_documentation.pdf
```

Now, Jenkins will **store the PDF**, and it can be **downloaded anytime**.

***

### **2️⃣ Push PDF to Another Repo**

To store the PDF in a central documentation repository, add this to the **Build Steps**:

```sh
#!/bin/bash

DOCS_REPO="/var/lib/jenkins/workspace/Generate-API-Docs-PDF"
PDF_STORAGE_REPO="/var/lib/jenkins/workspace/API-PDF-Storage"

# Clone storage repo if it doesn’t exist
if [ ! -d "$PDF_STORAGE_REPO" ]; then
    git clone git@github.com:your-org/api-pdf-repo.git "$PDF_STORAGE_REPO"
fi

# Copy PDF to storage repo
cp "$DOCS_REPO/api_documentation.pdf" "$PDF_STORAGE_REPO/"

# Commit and push
cd "$PDF_STORAGE_REPO"
git add api_documentation.pdf
git commit -m "Updated API PDF - $(date)"
git push origin main

echo "✅ PDF pushed to storage repository."
```

***

### **3️⃣ Send the PDF via Email**

1. **Install Jenkins Mailer Plugin** (Manage Jenkins → Plugins → Install "Mailer")
2. Under **Post-Build Actions**, click **"E-mail Notification"**
3. Enter recipients:
   ```
   docs-team@example.com
   ```
4. In **Advanced Settings**, check:\
   ✅ Attach PDF (`api_documentation.pdf`)

Now, Jenkins will **email the latest PDF** whenever it’s generated. 📩

***

## 🚨 Step 5: Fail Build if PDF Generation Fails

To prevent outdated PDFs, **fail the build if PDF generation breaks**.

Modify the **Build Steps** script:

```sh
if [ ! -f "api_documentation.pdf" ]; then
    echo "❌ PDF generation failed!" >&2
    exit 1
fi
```

If the PDF isn’t generated, Jenkins **fails the build** and alerts the team. 🚨

***

<!-- 
## 🎯 Conclusion: Fully Automated API Docs PDF

✅ **Jenkins detects Swagger changes**  
✅ **Runs a Python script to generate LaTeX**  
✅ **Compiles a beautiful API PDF**  
✅ **Stores the PDF in Jenkins**  
✅ **Uploads it to a central repo**  
✅ **Emails the docs team with the latest version**  

With this setup, your API documentation **always stays up to date**—**without manual work**. 🚀  

Now go forth and **automate like a pro!** 📜🔥  

---

## 🔑 Key Takeaways

| Summary        | Details |
|---------------|---------|
| **Why automate PDF generation?** | Keeps API documentation up to date without manual effort. |
| **What tools are needed?** | Jenkins, Python, LaTeX, Git. |
| **How does Jenkins trigger updates?** | Polls for changes in `swagger.json`. |
| **Where is the PDF stored?** | Jenkins archives, Git repo, and emailed to the team. |
| **What happens if PDF generation fails?** | Jenkins build fails, preventing outdated docs. |

```
-->
