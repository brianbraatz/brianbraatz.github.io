---
title: Python Ray Tracing Explored...
description: Amazing Ray Tracing Imaging with Python
slug: python-ray-tracing
date: 2017-12-05
image: post/Articles/IMAGES/python-raytracing.png
categories:
  - Python
  - Document Imaging
tags:
  - Python
draft: false
weight: 312
lastmod: 2025-03-03T00:10:06.877Z
---
![](/post/Articles/_____new9999/PythonRayTracing/animation.gif)

**VERY COOL** Ray Tracing Project in Python:

[GitHub - rafael-fuente/Python-Raytracer: A basic Ray Tracer that exploits numpy arrays and functions to work reasonably fast.](https://github.com/rafael-fuente/Python-Raytracer)

(from the site:)

# Python-Raytracer

A basic Ray Tracer that exploits numpy arrays and functions to work reasonably fast compared with a pure Python implementation.\
The code is written keeping as much readability as possible.

* Refraction
* Thin film interference
* Textures
* Monte Carlo Ray Tracing with importance sampling

## Installation

Just clone or download this repo. You'll need to install two packages.

1. Pillow is a fork of the PIL package.  It provides the Image module for this application.\
   to install it run the following.

```
pip install pillow
```

2. Numpy is a scientific package that helps with mathematical functions.

```
pip install numpy
```

## Examples

See the examples to see how to render the following images:

```
python example1.py
```

![](/post/Articles/_____new9999/PythonRayTracing/EXAMPLE1.png)

```
python example2.py
```

![](/post/Articles/_____new9999/PythonRayTracing/EXAMPLE2.png)

```
python example3.py
```

![](/post/Articles/_____new9999/PythonRayTracing/EXAMPLE3.png)

```
python example4.py
```

![](/post/Articles/_____new9999/PythonRayTracing/EXAMPLE4.png)

```
python example_cornell_box.py
```

![](/post/Articles/_____new9999/PythonRayTracing/cornell_box.png)\
Some animations:\
https://www.youtube.com/watch?v=vt9vAcZQT4A

{{< youtube "vt9vAcZQT4A" >}}

A basic version of this raytracer can be found here:\
https://www.excamera.com/sphinx/article-ray.html

A C++ version of this raytracer can be found here:\
https://github.com/rafael-fuente/sightpy-weekend-raytracer
