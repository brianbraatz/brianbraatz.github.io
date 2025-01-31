---
title: 
description: 
slug: 
date: 2023-04-06
image: post/Articles/IMAGES/21.jpg
categories: 
tags:
  - Cheatsheet
  - SQL
  - PostgresSql
  - MySql
  - SqlLite
weight: 30
draft: true
lastmod: 2025-01-31T14:42:43.464Z
---
https://chatgpt.com/c/679cbef8-27b8-8008-8a67-e8dd3f55f895

Chat history

ChatGPT\
ChatGPT

Explore GPTs\
Today\
Yesterday\
Previous 7 Days\
Upgrade plan\
More access to the best models

ChatGPT

Share

User\
You said:\
Take my last request to make a factory based database context and re-write the code to have the same behavior only now using the  Python  language .\
ChatGPT said:\
ChatGPT\
Memory updated\
Here's a Python implementation of your database context factory using SQLAlchemy, which supports SQLite, MySQL, and PostgreSQL.

1. Install Dependencies\
   Ensure you have SQLAlchemy and the necessary database drivers installed:

pip install sqlalchemy psycopg2 mysqlclient sqlite3\
2\. Define Database Contexts\
Each context corresponds to a different database type.

from sqlalchemy import create\_engine\
from sqlalchemy.orm import sessionmaker, declarative\_base

Base = declarative\_base()

class SqliteDbContext:\
def **init**(self, connection\_string):\
self.engine = create\_engine(connection\_string, echo=True)\
self.SessionLocal = sessionmaker(bind=self.engine)\
Base.metadata.create\_all(self.engine)

```
def get_session(self):
    return self.SessionLocal()
```

class MySqlDbContext:\
def **init**(self, connection\_string):\
self.engine = create\_engine(connection\_string, echo=True)\
self.SessionLocal = sessionmaker(bind=self.engine)\
Base.metadata.create\_all(self.engine)

```
def get_session(self):
    return self.SessionLocal()
```

class PostgresDbContext:\
def **init**(self, connection\_string):\
self.engine = create\_engine(connection\_string, echo=True)\
self.SessionLocal = sessionmaker(bind=self.engine)\
Base.metadata.create\_all(self.engine)

```
def get_session(self):
    return self.SessionLocal()
```

3\. Define Configuration Loader\
Reads the database settings from a JSON configuration file (config.json).

import json

class ConfigLoader:\
@staticmethod\
def load\_config(file\_path="config.json"):\
with open(file\_path, "r") as file:\
return json.load(file)\
Example config.json:

{\
"ActiveFactory": "sqlite",\
"factories": \[\
{ "FactoryName": "sqlite", "FactoryConfigString": "sqlite:///my\_database.db" },\
{ "FactoryName": "mysql", "FactoryConfigString": "mysql://user:password@localhost/mydb" },\
{ "FactoryName": "postgres", "FactoryConfigString": "postgresql://user:password@localhost/mydb" }\
]\
}\
4\. Implement Factory Interface\
This factory reads the config and returns the correct database context.

class IDBContextFactory:\
def create\_context(self, factory\_name=None):\
raise NotImplementedError("Subclasses must implement create\_context.")\
5\. Implement the Database Context Factory\
class DBContextFactory(IDBContextFactory):\
def **init**(self):\
self.config = ConfigLoader.load\_config()\
self.factories = {f\["FactoryName"]: f\["FactoryConfigString"] for f in self.config\["factories"]}\
self.active\_factory = self.config\["ActiveFactory"]

```
def create_context(self, factory_name=None):
    factory_name = factory_name or self.active_factory
    connection_string = self.factories.get(factory_name)

    if not connection_string:
        raise ValueError(f"Unknown factory: {factory_name}")

    if factory_name == "sqlite":
        return SqliteDbContext(connection_string)
    elif factory_name == "mysql":
        return MySqlDbContext(connection_string)
    elif factory_name == "postgres":
        return PostgresDbContext(connection_string)
    else:
        raise ValueError(f"Unsupported factory: {factory_name}")
```

6\. Usage Example\
if **name** == "**main**":\
factory = DBContextFactory()\
db\_context = factory.create\_context()

```
session = db_context.get_session()

# Example Query: session.execute("SELECT 1") for testing
result = session.execute("SELECT 1").fetchall()
print(result)

session.close()
```

Key Features\
Uses SQLAlchemy for ORM.\
Loads database settings from config.json.\
Factory pattern dynamically creates the correct database context.\
Supports SQLite, MySQL, and PostgreSQL.\
Encapsulates session creation for easy querying.\
Would you like any enhancements, such as adding a repository pattern?

You’ve hit the Free plan limit for GPT-4o.\
Responses will use another model until your limit resets after 7:35 AM.

Get Plus

ChatGPT can make mistakes. Check important info.\
?
