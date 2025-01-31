---
title: bad sql
description: 
slug: bad-sql
date: 2023-04-06
image: post/Articles/IMAGES/23.jpg
categories: 
tags:
  - Cheatsheet
  - SQL
  - PostgresSql
  - MySql
  - SqlLite
weight: 30
draft: true
lastmod: 2025-01-31T17:11:47.645Z
---
Memory updated

Here's a TypeScript-based command-line `DBContextFactory` that supports SQLite, MySQL, and PostgreSQL. This implementation follows the same factory pattern as in the C#, Python, and Go versions.

### Steps:

1. Install dependencies:

   sh

   CopyEdit

   `npm init -y npm install mysql2 pg sqlite3 dotenv yargs`

2. Create the following files:

***

### `db/IDBContextFactory.ts`

typescript

CopyEdit

`export interface IDBContextFactory {     createContext(): Promise<any>; }`

***

```c#
export interface IDBContextFactory {
    createContext(): Promise<any>;
}

```

### `db/SqliteDbContext.ts`

typescript

CopyEdit

`import { IDBContextFactory } from "./IDBContextFactory"; import sqlite3 from "sqlite3"; import { open, Database } from "sqlite";  export class SqliteDbContext implements IDBContextFactory {     private dbFilePath: string;      constructor(dbFilePath: string) {         this.dbFilePath = dbFilePath;     }      async createContext(): Promise<Database> {         return open({             filename: this.dbFilePath,             driver: sqlite3.Database         });     } }`

***

### `db/MySqlDbContext.ts`

typescript

CopyEdit

`import { IDBContextFactory } from "./IDBContextFactory"; import mysql, { Connection } from "mysql2/promise";  export class MySqlDbContext implements IDBContextFactory {     private config: any;      constructor(config: any) {         this.config = config;     }      async createContext(): Promise<Connection> {         return mysql.createConnection(this.config);     } }`

***

### `db/PostgresDbContext.ts`

typescript

CopyEdit

`import { IDBContextFactory } from "./IDBContextFactory"; import { Pool, PoolClient } from "pg";  export class PostgresDbContext implements IDBContextFactory {     private pool: Pool;      constructor(config: any) {         this.pool = new Pool(config);     }      async createContext(): Promise<PoolClient> {         return this.pool.connect();     } }`

***

### `db/DBContextFactory.ts`

typescript

CopyEdit

``import { IDBContextFactory } from "./IDBContextFactory"; import { SqliteDbContext } from "./SqliteDbContext"; import { MySqlDbContext } from "./MySqlDbContext"; import { PostgresDbContext } from "./PostgresDbContext"; import fs from "fs";  interface FactorySetting {     factoryName: string;     factoryConfig: any; }  export class DBContextFactory {     private activeFactory: string;     private factories: FactorySetting[];      constructor(configFilePath: string) {         const configData = JSON.parse(fs.readFileSync(configFilePath, "utf8"));         this.activeFactory = configData.activeFactory;         this.factories = configData.factories;     }      createContext(): Promise<any> {         const factorySetting = this.factories.find(f => f.factoryName === this.activeFactory);         if (!factorySetting) {             throw new Error(`Factory not found: ${this.activeFactory}`);         }          switch (factorySetting.factoryName) {             case "sqlite":                 return new SqliteDbContext(factorySetting.factoryConfig).createContext();             case "mysql":                 return new MySqlDbContext(factorySetting.factoryConfig).createContext();             case "postgres":                 return new PostgresDbContext(factorySetting.factoryConfig).createContext();             default:                 throw new Error(`Unsupported factory: ${factorySetting.factoryName}`);         }     } }``

***

### `config.json`

json

CopyEdit

`{     "activeFactory": "sqlite",     "factories": [         {             "factoryName": "sqlite",             "factoryConfig": "database.sqlite"         },         {             "factoryName": "mysql",             "factoryConfig": {                 "host": "localhost",                 "user": "root",                 "password": "password",                 "database": "testdb"             }         },         {             "factoryName": "postgres",             "factoryConfig": {                 "host": "localhost",                 "user": "postgres",                 "password": "password",                 "database": "testdb",                 "port": 5432             }         }     ] }`

***

### `index.ts`

typescript

CopyEdit

`import { DBContextFactory } from "./db/DBContextFactory"; import yargs from "yargs"; import { hideBin } from "yargs/helpers";  const argv = yargs(hideBin(process.argv))     .option("config", {         alias: "c",         type: "string",         description: "Path to config file",         default: "config.json"     })     .help()     .argv;  async function main() {     try {         const factory = new DBContextFactory(argv.config);         const dbContext = await factory.createContext();          console.log("Database context created successfully.");         console.log(dbContext);          // Close the database connection         if (dbContext.end) {             dbContext.end();         } else if (dbContext.close) {             dbContext.close();         }     } catch (error) {         console.error("Error creating database context:", error);     } }  main();`

***

### Running the Program

sh

CopyEdit

`ts-node index.ts --config=config.json`

This script will read `config.json`, determine which database context to create, and initialize it accordingly. Let me know if you need any modifications!
