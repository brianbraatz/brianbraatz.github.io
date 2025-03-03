---
title: WPF Flow Documents Created in C# Directly
description: How to make Flow Documents programmatically in C# code
slug: wpf-flow-documents-in-code
date: 2016-11-15
image: post/Articles/IMAGES/FlowDocument-floater.png
categories:
  - WPF Flow Documents
  - PDF
  - Postscript
  - Document Imaging
tags:
  - WPF
  - XAML
  - Maui
  - Xamarin
  - FlowDocuments
  - PDF
  - PostScript
  - DotNet
draft: false
weight: 30
lastmod: 2025-03-03T00:12:11.129Z
---
![](/post/Articles/IMAGES/FlowDocument-floater.png)\
From\
<https://wpf.2000things.com/tag/flowdocument/>

Great article here:

<https://wpf-tutorial.com/rich-text-controls/creating-flowdocument-from-code-behind/>

basically

With this window

```xml
<Window x:Class="WpfTutorialSamples.Rich_text_controls.CodeBehindFlowDocumentSample"
	xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
	xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml" Title="CodeBehindFlowDocumentSample" Height="200" Width="300">
	<Grid>
		<FlowDocumentScrollViewer Name="fdViewer" />
	</Grid>
</Window>
```

and this code, you can assemble a Flow Document programmatically....

```c#
using System;
using System.Windows;
using System.Windows.Documents;
using System.Windows.Media;

namespace WpfTutorialSamples.Rich_text_controls {
  public partial class CodeBehindFlowDocumentSample: Window {
    public CodeBehindFlowDocumentSample() {
      InitializeComponent();

      FlowDocument doc = new FlowDocument();

      Paragraph p = new Paragraph(new Run("Hello, world!"));
      p.FontSize = 36;
      doc.Blocks.Add(p);

      p = new Paragraph(new Run("The ultimate programming greeting!"));
      p.FontSize = 14;
      p.FontStyle = FontStyles.Italic;
      p.TextAlignment = TextAlignment.Left;
      p.Foreground = Brushes.Gray;
      doc.Blocks.Add(p);

      fdViewer.Document = doc;
    }
  }
}
```

![](/post/Articles/_new5/Pasted%20image%2020250209070814.png)

see here for write up on Flowdocuments vs Postscript

[WPF Flow Documents Compared to PDF-Postscript](/post/Articles/_new5/WPF%20Flow%20Documents%20Compared%20to%20PDF-Postscript.md)

and if you are interested in documents:

[Generating Postscript-PDF from Old Broken HTML with Python](/post/Articles/_New4/Generating%20Postscript-PDF%20from%20Old%20Broken%20HTML%20with%20Python.md)

[Postscript in a Nutshell](/post/Articles/_New4/Postscript%20in%20a%20Nutshell.md)

<!-- 
reminds me of this book  
<http://www.charlespetzold.com/wpf/>

Read that years ago.. all the examples are in 100% C# , no Xaml..

![](post/Articles/_new5/Pasted%20image%2020250209071013.png)




-->
