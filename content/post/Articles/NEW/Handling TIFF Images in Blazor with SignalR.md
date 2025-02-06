---
title: Handling TIFF Images in Blazor with SignalR
description: Dealing with Medial, Document and Microscope images in Blazor Web
slug: images-blazor-signalr-tiff
date: 2024-10-05
image: post/Articles/IMAGES/SEMMisc_pollen.jpg
categories: 
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
lastmod: 2025-02-06T19:13:37.894Z
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

***

## **Nuget Packages Needed**

```sh
dotnet add package System.Drawing.Common
dotnet add package Microsoft.AspNetCore.SignalR
```

***

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

***

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

***

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

* The file is saved in `/wwwroot/uploads/` using `FileUploadService`.

### User Clicks "Convert & Show Image":

* The Blazor app calls SignalR to request the conversion.

### SignalR Hub Converts the TIFF to PNG:

* The image is read, converted, and sent as a Base64 string.

### Client Receives and Displays the PNG:

* The image is shown directly in the browser.

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

## Final Notes

✅ **Blazor Server** fully supports SignalR, making this approach efficient.\
✅ **Blazor WASM** can also work but requires a backend API for image conversion.\
✅ **Static File Serving** allows direct access to uploaded images in `/wwwroot/uploads/`.
