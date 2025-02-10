---
title: Handling TIFF Images with Python, Flask and AJAX
description: Python Version of the Blazor Article
slug: tiff-images-python-and-ajax
date: 2024-10-05
image: post/Articles/IMAGES/pollen.jpg
categories:
  - CSharp
  - Document Imaging
  - SignalR
  - DotNet
  - TIFF Images
  - Medical Images
  - Microscope Images
  - Python
  - Python-Flask
tags:
  - Tiff
  - Jpg
  - Python
  - Flask
  - Ajax
  - WebDevelopment
  - MedicalImaging
draft: false
weight: 30
lastmod: 2025-02-09T22:19:31.793Z
---
See C# version here for explanation\
[Handling TIFF Images in Blazor with SignalR](/post/Articles/NEW/Handling%20TIFF%20Images%20in%20Blazor%20with%20SignalR.md)

***

## **Install Your Tools**

```sh
pip install flask pillow
```

***

## **How the Python Server Works**

The **Flask server** handles both file uploads and the **TIFF-to-PNG conversion**.

1. **File Upload (`/upload` route)**:
   * The user selects a TIFF file in the browser and uploads it to the server.
   * Flask saves the uploaded file in the `uploads/` folder.
   * The server responds with a success message and the file path.

2. **TIFF-to-PNG Conversion (`/convert` route)**:
   * The client sends the TIFF file path to this route.
   * The **Pillow (PIL)** library loads the TIFF file.
   * The image is converted and saved as a PNG file.
   * The PNG file is then sent back to the client.

### **`server.py`**

```python
from flask import Flask, request, jsonify, send_file
from PIL import Image
import os

app = Flask(__name__)
UPLOAD_FOLDER = "uploads"
os.makedirs(UPLOAD_FOLDER, exist_ok=True)

@app.route("/upload", methods=["POST"])
def upload_file():
    if 'file' not in request.files:
        return jsonify({"error": "No file part"}), 400
    file = request.files['file']
    if file.filename == '':
        return jsonify({"error": "No selected file"}), 400
    
    file_path = os.path.join(UPLOAD_FOLDER, file.filename)
    file.save(file_path)
    return jsonify({"message": "File uploaded successfully", "file_path": file_path})

@app.route("/convert", methods=["POST"])
def convert_tiff_to_png():
    data = request.json
    file_path = data.get("file_path")
    if not file_path or not os.path.exists(file_path):
        return jsonify({"error": "File not found"}), 400
    
    img = Image.open(file_path)
    png_path = file_path.rsplit(".", 1)[0] + ".png"
    img.save(png_path, "PNG")
    
    return send_file(png_path, mimetype='image/png')

if __name__ == "__main__":
    app.run(debug=True)
```

***

## **Step 3: How the HTML and JavaScript Work**

The **frontend** consists of an HTML page and JavaScript using AJAX to communicate with the server.

1. **User Selects a File**:
   * The `<input>` element allows users to pick a TIFF file.

2. **Upload the File to Flask**:
   * JavaScript grabs the selected file and sends it via an AJAX `POST` request.

3. **Request PNG Conversion**:
   * After upload, the client sends another AJAX request to convert the TIFF to PNG.
   * The server responds with the converted PNG file.
   * JavaScript updates the `<img>` element with the PNG file URL.

### **`index.html`**

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>TIFF to PNG Converter</title>
</head>
<body>
    <h3>TIFF to PNG Image Display</h3>
    <input type="file" id="fileInput">
    <button onclick="uploadFile()">Upload & Convert</button>
    <br>
    <p id="statusMessage"></p>
    <img id="imageDisplay" style="border: 1px solid #000; max-width: 500px;" />

    <script>
        async function uploadFile() {
            let fileInput = document.getElementById("fileInput");
            let file = fileInput.files[0];
            if (!file) {
                alert("Please select a TIFF file");
                return;
            }
            
            let formData = new FormData();
            formData.append("file", file);
            let response = await fetch("/upload", { method: "POST", body: formData });
            let result = await response.json();
            if (result.error) {
                alert(result.error);
                return;
            }
            
            let pngResponse = await fetch("/convert", { method: "POST", body: JSON.stringify({ file_path: result.file_path }), headers: { "Content-Type": "application/json" }});
            if (pngResponse.ok) {
                let blob = await pngResponse.blob();
                document.getElementById("imageDisplay").src = URL.createObjectURL(blob);
                document.getElementById("statusMessage").textContent = "Image converted successfully";
            } else {
                alert("Error converting image");
            }
        }
    </script>
</body>
</html>
```

***

<!--

## **And There You Have It**
You’ve now successfully wrestled TIFF images into submission and made them web-friendly with Python and AJAX. 🏆 The **Flask backend** takes care of file uploads and TIFF-to-PNG conversion, while the **frontend** handles user interactions and image display.

Now go forth and dazzle your users with crisp, high-quality images, all while laughing at how browsers still can’t handle TIFFs. Cheers! 🍻

-->

<!-- 
---
title: "How to Wrestle TIFF Images into a Web App with Python and AJAX (and Win)"
description: "How to Wrestle TIFF Images into a Web App with Python and AJAX (and Win)"
slug: "how-to-wrestle-tiff-images-into-a-web-app-with-python-and-ajax-and-win"
date: "2024-12-10"
image: "post/Articles/IMAGES/35.jpg"
categories: []
tags: ["Tiff", "Jpg", "Python", "Flask", "Ajax", "Web Development", "Image Processing"]
draft: false
weight: 30
---

# **How to Wrestle TIFF Images into a Web App with Python and AJAX (and Win)**

## **Welcome to the TIFF Cage Match**

Alright, let’s talk about **TIFF** files. You know, those beefy, high-quality image files that refuse to be compressed like their JPEG cousins? The ones that scientists, doctors, and other people with fancy lab coats love to use? Yeah, those.

If you’ve ever tried to display a TIFF in a web browser, you’ve probably been met with the digital equivalent of a confused shrug. Turns out, browsers don’t support TIFFs because they’re too high-maintenance. But fear not, my fellow devs, because we have a **cunning plan**: convert that diva TIFF into a PNG and send it to the client using **Python (Flask) and AJAX**.

In this article, we’re going to:
1. **Convert TIFFs to PNGs on the server**
2. **Send them to the browser using AJAX**
3. **Let the browser display the image like it’s no big deal**
4. **Let users upload TIFFs dynamically**

Ready? Let’s do this. 🚀

---

## **Step 1: Install Your Tools**
Before we start, slap these packages into your Python project:
```sh
pip install flask pillow
```

---

## **Step 2: How the Python Server Works**

The **Flask server** handles both file uploads and the **TIFF-to-PNG conversion**.

1. **File Upload (`/upload` route)**:
   - The user selects a TIFF file in the browser and uploads it to the server.
   - Flask saves the uploaded file in the `uploads/` folder.
   - The server responds with a success message and the file path.

2. **TIFF-to-PNG Conversion (`/convert` route)**:
   - The client sends the TIFF file path to this route.
   - The **Pillow (PIL)** library loads the TIFF file.
   - The image is converted and saved as a PNG file.
   - The PNG file is then sent back to the client.

### **`server.py`**
```python
from flask import Flask, request, jsonify, send_file
from PIL import Image
import os

app = Flask(__name__)
UPLOAD_FOLDER = "uploads"
os.makedirs(UPLOAD_FOLDER, exist_ok=True)

@app.route("/upload", methods=["POST"])
def upload_file():
    if 'file' not in request.files:
        return jsonify({"error": "No file part"}), 400
    file = request.files['file']
    if file.filename == '':
        return jsonify({"error": "No selected file"}), 400
    
    file_path = os.path.join(UPLOAD_FOLDER, file.filename)
    file.save(file_path)
    return jsonify({"message": "File uploaded successfully", "file_path": file_path})

@app.route("/convert", methods=["POST"])
def convert_tiff_to_png():
    data = request.json
    file_path = data.get("file_path")
    if not file_path or not os.path.exists(file_path):
        return jsonify({"error": "File not found"}), 400
    
    img = Image.open(file_path)
    png_path = file_path.rsplit(".", 1)[0] + ".png"
    img.save(png_path, "PNG")
    
    return send_file(png_path, mimetype='image/png')

if __name__ == "__main__":
    app.run(debug=True)
```

---

## **Step 3: How the HTML and JavaScript Work**

The **frontend** consists of an HTML page and JavaScript using AJAX to communicate with the server.

1. **User Selects a File**:
   - The `<input>` element allows users to pick a TIFF file.

2. **Upload the File to Flask**:
   - JavaScript grabs the selected file and sends it via an AJAX `POST` request.

3. **Request PNG Conversion**:
   - After upload, the client sends another AJAX request to convert the TIFF to PNG.
   - The server responds with the converted PNG file.
   - JavaScript updates the `<img>` element with the PNG file URL.

### **`index.html`**
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>TIFF to PNG Converter</title>
</head>
<body>
    <h3>TIFF to PNG Image Display</h3>
    <input type="file" id="fileInput">
    <button onclick="uploadFile()">Upload & Convert</button>
    <br>
    <p id="statusMessage"></p>
    <img id="imageDisplay" style="border: 1px solid #000; max-width: 500px;" />

    <script>
        async function uploadFile() {
            let fileInput = document.getElementById("fileInput");
            let file = fileInput.files[0];
            if (!file) {
                alert("Please select a TIFF file");
                return;
            }
            
            let formData = new FormData();
            formData.append("file", file);
            let response = await fetch("/upload", { method: "POST", body: formData });
            let result = await response.json();
            if (result.error) {
                alert(result.error);
                return;
            }
            
            let pngResponse = await fetch("/convert", { method: "POST", body: JSON.stringify({ file_path: result.file_path }), headers: { "Content-Type": "application/json" }});
            if (pngResponse.ok) {
                let blob = await pngResponse.blob();
                document.getElementById("imageDisplay").src = URL.createObjectURL(blob);
                document.getElementById("statusMessage").textContent = "Image converted successfully";
            } else {
                alert("Error converting image");
            }
        }
    </script>
</body>
</html>
```

---

## **And There You Have It**
You’ve now successfully wrestled TIFF images into submission and made them web-friendly with Python and AJAX. 🏆 The **Flask backend** takes care of file uploads and TIFF-to-PNG conversion, while the **frontend** handles user interactions and image display.

Now go forth and dazzle your users with crisp, high-quality images, all while laughing at how browsers still can’t handle TIFFs. Cheers! 🍻


-->
