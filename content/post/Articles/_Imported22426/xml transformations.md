---
title: XSLT - Advanced XML Transformations
description: XSLT - Advanced XML Transformations
slug: xslt-advanced-xml-transformations
date: 2018-01-26
image: post/Articles/IMAGES/pictureframes3.png
categories:
  - XML
  - Transformation
  - XSLT
  - Data Conversion
tags:
  - XML
  - Transformation
  - XSLT
  - Data Conversion
  - HTML
  - Schema
  - Formatting
  - Text Conversion
draft: false
weight: 238
lastmod: 2025-02-27T18:11:30.257Z
---
# XSLT - Advanced XML Transformations

If you've ever looked at an XML file and thought, "Wow, this is some serious data spaghetti," then congratulations! You have officially encountered the chaos that is raw XML.

But fear not! XSLT (Extensible Stylesheet Language Transformations) is here to save the day. It takes that XML mess and turns it into something useful—like HTML, CSV, or even a completely different XML format.

In this article, we’ll explore the power of XSLT with ten awesome examples.

Grab some coffee, because things are about to get transformative!

***

## What is XSLT?

XSLT is like a translator for XML. It reads one XML format, applies a set of rules (stylesheets), and outputs a totally new format.

Most people know it for converting XML into HTML for web pages, but XSLT can do so much more.

It can:

* Convert XML to another XML schema
* Transform XML into JSON (with some creativity)
* Generate plain text files, like CSV
* Format data into strange and beautiful things (yes, really)

***

## XSLT Basics

XSLT uses **XPath** to navigate XML and match elements.

A basic XSLT stylesheet looks something like this:

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <html>
            <body>
                <h1>Transformed XML Data</h1>
                <xsl:apply-templates/>
            </body>
        </html>
    </xsl:template>
</xsl:stylesheet>
```

This simple XSLT file takes XML and wraps it in HTML.

Now, let’s get into the fun stuff.

***

# 10 Practical XSLT Examples

### Example 1: XML to HTML Table

#### **Input XML**

```xml
<employees>
    <employee>
        <name>John Doe</name>
        <role>Developer</role>
    </employee>
    <employee>
        <name>Jane Smith</name>
        <role>Designer</role>
    </employee>
</employees>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <html>
            <body>
                <table border="1">
                    <tr><th>Name</th><th>Role</th></tr>
                    <xsl:for-each select="employees/employee">
                        <tr>
                            <td><xsl:value-of select="name"/></td>
                            <td><xsl:value-of select="role"/></td>
                        </tr>
                    </xsl:for-each>
                </table>
            </body>
        </html>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output HTML**

```html
<html>
    <body>
        <table border="1">
            <tr><th>Name</th><th>Role</th></tr>
            <tr><td>John Doe</td><td>Developer</td></tr>
            <tr><td>Jane Smith</td><td>Designer</td></tr>
        </table>
    </body>
</html>
```

***

### Example 2: Converting XML to CSV

#### **Input XML**

```xml
<students>
    <student>
        <name>Alice</name>
        <age>22</age>
    </student>
    <student>
        <name>Bob</name>
        <age>24</age>
    </student>
</students>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="text"/>
    <xsl:template match="/">
        <xsl:text>Name,Age&#10;</xsl:text>
        <xsl:for-each select="students/student">
            <xsl:value-of select="name"/>,<xsl:value-of select="age"/>
            <xsl:text>&#10;</xsl:text>
        </xsl:for-each>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output CSV**

```csv
Name,Age
Alice,22
Bob,24
```

***

### Example 3: Transforming XML into Another XML Schema

Sometimes, XML formats don’t match. XSLT can fix that.

#### **Input XML**

```xml
<oldFormat>
    <item>
        <title>Book 1</title>
        <price>19.99</price>
    </item>
</oldFormat>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <newFormat>
            <xsl:for-each select="oldFormat/item">
                <product>
                    <name><xsl:value-of select="title"/></name>
                    <cost><xsl:value-of select="price"/></cost>
                </product>
            </xsl:for-each>
        </newFormat>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output XML**

```xml
<newFormat>
    <product>
        <name>Book 1</name>
        <cost>19.99</cost>
    </product>
</newFormat>
```

***

(Additional 7 examples omitted for brevity, but would include XML to JSON, filtering elements, conditional transformations, grouping, sorting, etc.)

***

## Example 4: XML to JSON (Yes, It's Possible!)

#### **Input XML**

```xml
<library>
    <book>
        <title>The Hobbit</title>
        <author>J.R.R. Tolkien</author>
    </book>
    <book>
        <title>1984</title>
        <author>George Orwell</author>
    </book>
</library>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="text"/>
    <xsl:template match="/">
        <xsl:text>{ "books": [</xsl:text>
        <xsl:for-each select="library/book">
            <xsl:text>{"title": "</xsl:text><xsl:value-of select="title"/><xsl:text>",</xsl:text>
            <xsl:text> "author": "</xsl:text><xsl:value-of select="author"/><xsl:text>"}</xsl:text>
            <xsl:if test="position() != last()">
                <xsl:text>,</xsl:text>
            </xsl:if>
        </xsl:for-each>
        <xsl:text>] }</xsl:text>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output JSON**

```json
{ "books": [
    {"title": "The Hobbit", "author": "J.R.R. Tolkien"},
    {"title": "1984", "author": "George Orwell"}
] }
```

***

## Example 5: Filtering XML Elements

Let’s filter only books published after the year 2000.

#### **Input XML**

```xml
<books>
    <book year="1998">
        <title>Old Book</title>
    </book>
    <book year="2005">
        <title>New Book</title>
    </book>
</books>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <filteredBooks>
            <xsl:for-each select="books/book[@year &gt; 2000]">
                <xsl:copy-of select="."/>
            </xsl:for-each>
        </filteredBooks>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output XML**

```xml
<filteredBooks>
    <book year="2005">
        <title>New Book</title>
    </book>
</filteredBooks>
```

***

## Example 6: Sorting Elements

#### **Input XML**

```xml
<students>
    <student>
        <name>Charlie</name>
        <grade>85</grade>
    </student>
    <student>
        <name>Alice</name>
        <grade>90</grade>
    </student>
</students>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <sortedStudents>
            <xsl:for-each select="students/student">
                <xsl:sort select="name"/>
                <xsl:copy-of select="."/>
            </xsl:for-each>
        </sortedStudents>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output XML**

```xml
<sortedStudents>
    <student>
        <name>Alice</name>
        <grade>90</grade>
    </student>
    <student>
        <name>Charlie</name>
        <grade>85</grade>
    </student>
</sortedStudents>
```

***

## Example 7: Grouping Elements

#### **Input XML**

```xml
<orders>
    <order>
        <customer>John</customer>
        <item>Phone</item>
    </order>
    <order>
        <customer>John</customer>
        <item>Laptop</item>
    </order>
</orders>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:key name="customerKey" match="order" use="customer"/>
    <xsl:template match="/">
        <groupedOrders>
            <xsl:for-each select="orders/order[generate-id() = generate-id(key('customerKey', customer)[1])]">
                <customer name="{customer}">
                    <xsl:for-each select="key('customerKey', customer)">
                        <item><xsl:value-of select="item"/></item>
                    </xsl:for-each>
                </customer>
            </xsl:for-each>
        </groupedOrders>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output XML**

```xml
<groupedOrders>
    <customer name="John">
        <item>Phone</item>
        <item>Laptop</item>
    </customer>
</groupedOrders>
```

***

## Example 8: Formatting Text

#### **Input XML**

```xml
<message>Hello World</message>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <formattedMessage>
            <xsl:value-of select="translate(message, ' ', '_')"/>
        </formattedMessage>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output XML**

```xml
<formattedMessage>Hello_World</formattedMessage>
```

***

## Example 9: Adding Default Values

#### **Input XML**

```xml
<user>
    <name>Bob</name>
</user>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <user>
            <xsl:copy-of select="name"/>
            <age><xsl:value-of select="'Unknown'"/></age>
        </user>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output XML**

```xml
<user>
    <name>Bob</name>
    <age>Unknown</age>
</user>
```

***

## Example 10: Removing Empty Elements

#### **Input XML**

```xml
<data>
    <value>42</value>
    <value></value>
</data>
```

#### **XSLT Transformation**

```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="node()|@*">
        <xsl:if test="string(.) != ''">
            <xsl:copy>
                <xsl:apply-templates select="@*|node()"/>
            </xsl:copy>
        </xsl:if>
    </xsl:template>
</xsl:stylesheet>
```

#### **Output XML**

```xml
<data>
    <value>42</value>
</data>
```

***

# References

1. [W3C XSLT Specification](https://www.w3.org/TR/xslt/)
2. [MDN Guide to XSLT](https://developer.mozilla.org/en-US/docs/Web/XSLT)
3. [XPath Syntax Guide](https://www.w3schools.com/xml/xpath_syntax.asp)
