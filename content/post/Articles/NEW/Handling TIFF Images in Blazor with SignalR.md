---
title: Handling TIFF Images in Blazor with SignalR
description: Dealing with Medial, Document and Microscope images in Blazor Web
slug: images-blazor-signalr-tiff
date: 2024-10-05
image: post/Articles/IMAGES/SEMMisc_pollen.jpg
categories:
  - CSharp
  - Document Imaging
  - SignalR
  - DotNet
  - TIFF Images
  - Medical Images
  - Microscope Images
tags:
  - Tiff
  - Jpg
  - Blazor
  - Signalr
  - CSharp
  - MedicalImaging
  - DocumentScanning
  - WebDevelopment
draft: false
weight: 312
categories_ref:
  - CSharp
  - Document Imaging
  - SignalR
  - DotNet
  - TIFF Images
  - Medical Images
  - Microscope Images
slug_calculated: https://brianbraatz.github.io/p/images-blazor-signalr-tiff
lastmod: 2025-03-14T16:40:21.263Z
---
![](/post/Articles/IMAGES/SEMMisc_pollen.jpg)

By Dartmouth College Electron Microscope Facility - Source and public domain notice at Dartmouth College Electron Microscope Facility (\[1], \[2]), Public Domain, https://commons.wikimedia.org/w/index.php?curid=24407

<https://en.wikipedia.org/wiki/Scanning_electron_microscope>

<!-- 
# **How to Wrestle TIFF Images into a Blazor App with SignalR (and Win)**

## **Welcome to the TIFF Cage Match**
-->

**See the PYTHON Version of the Article here:**\
[Handling TIFF Images with Python Flask and AJAX](/post/Articles/NEW/Handling%20TIFF%20Images%20with%20Python%20Flask%20and%20AJAX.md)

## \*\* TIFF?\*\*

Alright, let’s talk about **TIFF** files. You know, those beefy, high-quality image files that refuse to be compressed like their JPEG cousins?

The ones that scientists, doctors, and other people with fancy lab coats love to use? Yeah, those.

If you’ve ever tried to display a TIFF in a web browser, you’ve probably been met with the digital equivalent of a confused shrug.

Turns out, browsers don’t support TIFFs because they’re too high-maintenance.\
The are lossless, which can make them quite large

see\
[JPEG Compression The Good, the Bad, and the Pixelated](/post/Articles/NEW/JPEG%20Compression%20The%20Good,%20the%20Bad,%20and%20the%20Pixelated.md)

<!-- 
But fear not, my fellow devs, because we have a **cunning plan**: convert that diva TIFF into a PNG and send it to the client using **SignalR**.

In this article, we’re going to:
1. **Convert TIFFs to PNGs on the server**
2. **Send them to the browser via SignalR**
3. **Let the browser display the image like it’s no big deal**
4. **Let users upload TIFFs dynamically**

Ready? Let’s do this. 🚀

## **TIFF vs. JPEG: A Quick and (Very) Biased Comparison**
-->

### **TIFF: The Overachiever**

TIFF (Tagged Image File Format) is like that one friend who insists on bringing a 4K projector to movie night.

It’s high quality, lossless, and supports multiple pages, transparency, and all sorts of extras. It’s used in:

* **Medical imaging** (X-rays, MRIs, fancy scans)
* **Scientific imaging** (Electron microscopes, astrophotography, things that make you go “whoa”)
* **Document scanning** (Lawyers and accountants love this stuff)
* **Printing and publishing** (Because blurry images are for amateurs)

### **PNG: The Chill One**

It’s lightweight, compressed, and supported by literally everything.

It’s perfect for your vacation photos but totally unsuitable if you need pixel-perfect precision.

***

## **The Browser’s TIFF Support**

Here’s the deal: modern web browsers simply **refuse to display TIFFs** natively.

<!-- 
It’s like trying to teach a cat to fetch—you can try, but it won’t end well. 
-->

Since we can’t make browsers like TIFFs, we need to **convert them into PNGs** before sending them over.

<!-- And that’s where **SignalR** swoops in like a hero in a cape.
-->

***

## **Test - Example**

What's the code below do? It gives you a Blazor ui allowing you to up load a TIFF file to the sever.

The TIFF is then converted, **in memory**, and sent back to the Browser as a base64 encoded PNG.

In a real situation the TIFF file likely will come from server file location or a cloud provider .

The point of the code is to demonstrate the TIFF conversion on the fly.

So your mileage may vary...

<!-- 
# 🚀 Blazor + SignalR: Upload TIFF, Convert to PNG, and Display in Client

This guide shows how to:
1. **Upload a TIFF file from Blazor.**
2. **Convert TIFF to PNG on the server.**
3. **Send the PNG (Base64) to clients using SignalR.**
4. **Display the PNG in the Blazor UI.**

---

-->

What the Sample Does:

1. **User can Upload a TIFF file from Blazor.**
2. **Converts the TIFF to PNG on the server.**
3. **Server Send the PNG (Base64) to clients using SignalR.**
4. **Client Displays the PNG in the Blazor UI.**

## 🖥️ Server-Side (ASP.NET Core + SignalR)

### ✅ 1. SignalR Hub (Server)

This hub handles sending Base64 PNG images to connected clients.

```csharp
using Microsoft.AspNetCore.SignalR;
using System.Threading.Tasks;

public class ImageHub : Hub
{
    public async Task SendPngImage(string base64Png)
    {
        await Clients.All.SendAsync("ReceiveImage", base64Png);
    }
}
```

***

### ✅ 2. Server-Side Controller to Handle File Upload

This API controller:

* Accepts TIFF file uploads.
* Converts the TIFF image to PNG.
* Encodes PNG as Base64.
* Sends the PNG to clients via SignalR.

```csharp
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.SignalR;
using System.IO;
using System.Threading.Tasks;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.Formats.Png;

[Route("api/[controller]")]
[ApiController]
public class UploadController : ControllerBase
{
    private readonly IHubContext<ImageHub> _hubContext;

    public UploadController(IHubContext<ImageHub> hubContext)
    {
        _hubContext = hubContext;
    }

    [HttpPost("upload")]
    public async Task<IActionResult> UploadTiff(IFormFile file)
    {
        if (file == null || file.Length == 0)
        {
            return BadRequest("No file uploaded.");
        }

        using (MemoryStream inputStream = new MemoryStream())
        {
            await file.CopyToAsync(inputStream);
            byte[] tiffBytes = inputStream.ToArray();
            string base64Png = ConvertTiffToPngBase64(tiffBytes);

            // Send PNG as Base64 via SignalR
            await _hubContext.Clients.All.SendAsync("ReceiveImage", base64Png);

            return Ok(new { message = "Image uploaded and converted successfully" });
        }
    }

    private string ConvertTiffToPngBase64(byte[] tiffBytes)
    {
        using (MemoryStream inputStream = new MemoryStream(tiffBytes))
        using (Image image = Image.Load(inputStream)) // Load TIFF
        using (MemoryStream outputStream = new MemoryStream())
        {
            image.Save(outputStream, new PngEncoder()); // Convert to PNG
            byte[] pngBytes = outputStream.ToArray();
            return Convert.ToBase64String(pngBytes);
        }
    }
}
```

***

## 🌍 Client-Side (Blazor)

### ✅ 3. Blazor Page (`ImageUploader.razor`)

This component:

* Lets users **upload a TIFF file**.
* Sends the file to the **server for conversion**.
* Listens for **SignalR messages** to receive **Base64 PNG**.
* Displays the **converted PNG**.

```razor
@page "/upload"
@inject NavigationManager Navigation
@inject Microsoft.AspNetCore.SignalR.Client.HubConnection HubConnection
@inject HttpClient Http

<h3>Upload a TIFF Image</h3>

<InputFile OnChange="UploadFile" />

@if (string.IsNullOrEmpty(base64Image))
{
    <p>Waiting for image...</p>
}
else
{
    <h3>Converted PNG:</h3>
    <img src="@base64Image" alt="Converted PNG" style="max-width: 100%; border: 1px solid #ccc;" />
}

@code {
    private string base64Image;

    protected override async Task OnInitializedAsync()
    {
        HubConnection = new Microsoft.AspNetCore.SignalR.Client.HubConnectionBuilder()
            .WithUrl(Navigation.ToAbsoluteUri("/imagehub")) // Ensure it matches your server hub
            .Build();

        // Listen for the "ReceiveImage" event from SignalR
        HubConnection.On<string>("ReceiveImage", (base64) =>
        {
            base64Image = $"data:image/png;base64,{base64}"; // Format Base64 for <img> tag
            StateHasChanged(); // Refresh UI
        });

        await HubConnection.StartAsync();
    }

    private async Task UploadFile(InputFileChangeEventArgs e)
    {
        var file = e.File;
        if (file == null) return;

        var content = new MultipartFormDataContent();
        var fileContent = new StreamContent(file.OpenReadStream(file.Size));
        content.Add(fileContent, "file", file.Name);

        var response = await Http.PostAsync("api/upload/upload", content);
        if (response.IsSuccessStatusCode)
        {
            Console.WriteLine("Image uploaded successfully");
        }
    }
}
```

***

## ✅ 4. Register SignalR in `Program.cs`

Make sure **SignalR and the upload controller** are registered in `Program.cs`:

```csharp
var builder = WebApplication.CreateBuilder(args);

builder.Services.AddRazorPages();
builder.Services.AddServerSideBlazor();
builder.Services.AddSignalR();
builder.Services.AddControllers(); // Register API controllers
builder.Services.AddHttpClient(); // Needed for HTTP file uploads

var app = builder.Build();

app.MapControllers(); // Map API routes
app.MapHub<ImageHub>("/imagehub");
app.MapBlazorHub();
app.MapFallbackToPage("/_Host");

app.Run();
```

***

<!--
## 🔥 How This Works
### 🖼️ User Flow
1. **User selects a TIFF file** from Blazor UI.
2. **File is uploaded to ASP.NET Core API (`UploadController`).**
3. **Server converts TIFF → PNG** and **encodes it as Base64**.
4. **SignalR sends the Base64 PNG to all clients.**
5. **Blazor client receives the PNG** and updates the UI dynamically.

---

## 🎯 Features
✅ **Blazor UI lets users upload TIFF files**  
✅ **Server converts TIFF → PNG in memory (no disk usage)**  
✅ **SignalR sends the Base64 PNG to all connected clients**  
✅ **Blazor dynamically updates the `<img>` tag to display PNG**  

Would you like me to add **logging, error handling, or caching** for performance? 🚀🔥
-->

<!-- 
---

## **Nuget Packages Needed**

```sh
dotnet add package System.Drawing.Common
dotnet add package Microsoft.AspNetCore.SignalR
```

---

## **The SignalR Hub**

### **`Hubs/ImageHub.cs`**

```csharp
using Microsoft.AspNetCore.SignalR;
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Threading.Tasks;

public class ImageHub : Hub
{
    public async Task SendTiffAsPng(string filePath)
    {
        if (!File.Exists(filePath))
        {
            await Clients.Caller.SendAsync("ReceiveImage", "data:image/png;base64,", "File not found. Sad times.");
            return;
        }

        try
        {
            using (var tiffImage = Image.FromFile(filePath))
            using (var memoryStream = new MemoryStream())
            {
                tiffImage.Save(memoryStream, ImageFormat.Png);
                string base64Png = Convert.ToBase64String(memoryStream.ToArray());
                string imageDataUrl = $"data:image/png;base64,{base64Png}";
                
                await Clients.All.SendAsync("ReceiveImage", imageDataUrl, "Here’s your glorious PNG.");
            }
        }
        catch (Exception ex)
        {
            await Clients.Caller.SendAsync("ReceiveImage", "data:image/png;base64,", $"Oops: {ex.Message}");
        }
    }
}
```

---

## **Step 3: File Upload Service**

### **`Services/FileUploadService.cs`**

```csharp
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using System;
using System.IO;
using System.Threading.Tasks;

public class FileUploadService
{
    private readonly IWebHostEnvironment _env;
    
    public FileUploadService(IWebHostEnvironment env)
    {
        _env = env;
    }

    public async Task<string> SaveFileAsync(IFormFile file)
    {
        if (file == null || file.Length == 0)
            throw new ArgumentException("That’s not a real file, buddy.");

        var uploadPath = Path.Combine(_env.WebRootPath, "uploads");
        Directory.CreateDirectory(uploadPath);
        
        var filePath = Path.Combine(uploadPath, file.FileName);
        
        await using var stream = new FileStream(filePath, FileMode.Create);
        await file.CopyToAsync(stream);

        return filePath;
    }
}
```
---

## **Blazor Page For Upload and Display converted TIFF**
### **`Pages/Index.razor`**
```razor
@page "/"
@inject NavigationManager Navigation
@inject FileUploadService FileUploadService
@implements IAsyncDisposable

<h3>TIFF to PNG Image Display</h3>

<input type="file" @onchange="OnFileSelected" />
<button @onclick="RequestImage" disabled="@string.IsNullOrEmpty(UploadedFilePath)">Convert & Show Image</button>

@if (!string.IsNullOrEmpty(ImageData))
{
    <p>@StatusMessage</p>
    <img src="@ImageData" alt="Your glorious PNG" style="border: 1px solid #000; max-width: 500px;" />
}

@code {
    private HubConnection? hubConnection;
    private string ImageData = "";
    private string StatusMessage = "Waiting for image...";
    private string UploadedFilePath = "";

    protected override async Task OnInitializedAsync()
    {
        hubConnection = new HubConnectionBuilder()
            .WithUrl(Navigation.ToAbsoluteUri("/imageHub"))
            .WithAutomaticReconnect()
            .Build();

        hubConnection.On<string, string>("ReceiveImage", (imageDataUrl, status) =>
        {
            ImageData = imageDataUrl;
            StatusMessage = status;
            StateHasChanged();
        });

        await hubConnection.StartAsync();
    }

    private async Task OnFileSelected(ChangeEventArgs e)
    {
        var file = (e.Value as Microsoft.AspNetCore.Components.Forms.IBrowserFile);
        if (file != null)
        {
            var filePath = await FileUploadService.SaveFileAsync(file);
            UploadedFilePath = filePath;
        }
    }

    private async Task RequestImage()
    {
        if (hubConnection != null && !string.IsNullOrEmpty(UploadedFilePath))
        {
            await hubConnection.SendAsync("SendTiffAsPng", UploadedFilePath);
        }
    }
}
```

## How It Works

### User Uploads a TIFF File:
- The file is saved in `/wwwroot/uploads/` using `FileUploadService`.

### User Clicks "Convert & Show Image":
- The Blazor app calls SignalR to request the conversion.

### SignalR Hub Converts the TIFF to PNG:
- The image is read, converted, and sent as a Base64 string.

### Client Receives and Displays the PNG:
- The image is shown directly in the browser.
-->

<!-- 
## Bonus: Add Client-Side Logging
You can add logging inside Blazor for debugging:

```razor
@code {
    private async Task DebugLog(string message)
    {
        await JS.InvokeVoidAsync("console.log", message);
    }
}
```
Then call `DebugLog("Message Here")` inside any method.
-->

## Final Notes and Thoughts

✅ **Blazor Server** fully supports SignalR, making this approach efficient.\
✅ **Blazor WASM** can also work but requires a backend API for image conversion.\
✅ **Static File Serving** allows direct access to uploaded images in `/wwwroot/uploads/`, so you could just convert the file and serve it with static file sharing
