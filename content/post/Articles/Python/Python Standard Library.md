---
title: Python Standard Library Cheatsheet
description: Python Standard Library Cheatsheet
slug: python-stdlib-cheatsheet
date: 2014-05-06
image: post/Articles/IMAGES/10.jpg
categories: 
tags:
  - Cheatsheet
  - Python-StandardLibrary
weight: 30
draft: false
lastmod: 2025-01-30T15:35:38.341Z
---
## Python Standard Library Overview

```python
Python Standard Library
 ├── Built-in Functions
 │    └── Functions like print(), len(), range(), etc.
 ├── os
 │    ├── os.path
 │    ├── os.system
 │    └── os.environ
 ├── sys
 │    ├── sys.argv
 │    ├── sys.path
 │    └── sys.modules
 ├── datetime
 │    ├── datetime.datetime
 │    ├── datetime.date
 │    ├── datetime.time
 │    └── datetime.timedelta
 ├── json
 │    ├── json.load
 │    ├── json.dump
 │    ├── json.loads
 │    └── json.dumps
 ├── re
 │    ├── re.compile
 │    ├── re.search
 │    └── re.match
 ├── math
 │    ├── math.sqrt
 │    ├── math.sin
 │    ├── math.cos
 │    └── math.pi
 ├── collections
 │    ├── collections.Counter
 │    ├── collections.defaultdict
 │    └── collections.namedtuple
 ├── itertools
 │    ├── itertools.chain
 │    ├── itertools.cycle
 │    └── itertools.permutations
 └── urllib
      ├── urllib.request
      ├── urllib.parse
      └── urllib.error
```

<!--

### Key Packages and Modules:

- **Built-in Functions:** Core functions like `print()`, `len()`, `range()`, etc.
    
- **os:** Operating system interfaces like `os.path`, `os.system`, `os.environ`.
    
- **sys:** System-specific parameters and functions like `sys.argv`, `sys.path`, `sys.modules`.
    
- **datetime:** Date and time manipulation like `datetime.datetime`, `datetime.date`, `datetime.time`, `datetime.timedelta`.
    
- **json:** JSON serialization and deserialization like `json.load`, `json.dump`, `json.loads`, `json.dumps`.
    
- **re:** Regular expression operations like `re.compile`, `re.search`, `re.match`.
    
- **math:** Mathematical functions like `math.sqrt`, `math.sin`, `math.cos`, `math.pi`.
    
- **collections:** Container datatypes like `collections.Counter`, `collections.defaultdict`, `collections.namedtuple`.
    
- **itertools:** Functions creating iterators for efficient looping like `itertools.chain`, `itertools.cycle`, `itertools.permutations`.
    
- **urllib:** URL handling modules like `urllib.request`, `urllib.parse`, `urllib.error`.
    

This textual representation should help you understand the structure of some commonly used standard packages in Python. If you need more specific information or have any other requests, let me know!

make a cheatsheet , in a markdown table of the Python Standard Library

Here's a concise cheatsheet for some commonly used modules in the Python Standard Library in a markdown table:
-->

### Python Standard Library Cheatsheet

| **Module**     | **Common Functions/Classes**                                                                     | **Description**                          |
| -------------- | ------------------------------------------------------------------------------------------------ | ---------------------------------------- |
| `os`           | `os.path`, `os.system()`, `os.environ`, `os.mkdir()`                                             | Operating system interfaces              |
| `sys`          | `sys.argv`, `sys.path`, `sys.modules`, `sys.exit()`                                              | System-specific parameters and functions |
| `datetime`     | `datetime.datetime`, `datetime.date`, `datetime.time`, `datetime.timedelta`                      | Date and time manipulation               |
| `json`         | `json.load()`, `json.dump()`, `json.loads()`, `json.dumps()`                                     | JSON serialization and deserialization   |
| `re`           | `re.compile()`, `re.search()`, `re.match()`, `re.findall()`, `re.sub()`                          | Regular expression operations            |
| `math`         | `math.sqrt()`, `math.sin()`, `math.cos()`, `math.pi`, `math.log()`                               | Mathematical functions                   |
| `collections`  | `collections.Counter`, `collections.defaultdict`, `collections.namedtuple`                       | Container datatypes                      |
| `itertools`    | `itertools.chain()`, `itertools.cycle()`, `itertools.permutations()`, `itertools.combinations()` | Functions creating iterators             |
| `random`       | `random.random()`, `random.randint()`, `random.choice()`, `random.shuffle()`                     | Generating random numbers                |
| `string`       | `string.ascii_letters`, `string.digits`, `string.punctuation`                                    | Common string operations and constants   |
| `urllib`       | `urllib.request`, `urllib.parse`, `urllib.error`                                                 | URL handling modules                     |
| `subprocess`   | `subprocess.run()`, `subprocess.Popen()`, `subprocess.call()`                                    | Subprocess management                    |
| `shutil`       | `shutil.copy()`, `shutil.move()`, `shutil.rmtree()`, `shutil.disk_usage()`                       | High-level file operations               |
| `logging`      | `logging.basicConfig()`, `logging.debug()`, `logging.info()`, `logging.error()`                  | Logging facility for Python              |
| `configparser` | `configparser.ConfigParser()`, `configparser.read()`, `configparser.get()`                       | Configuration file parser                |
| `socket`       | `socket.socket()`, `socket.bind()`, `socket.listen()`, `socket.accept()`                         | Low-level networking interface           |
| `threading`    | `threading.Thread()`, `threading.Lock()`, `threading.Event()`, `threading.Timer()`               | Thread-based parallelism                 |
