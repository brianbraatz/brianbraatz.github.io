---
Description: DBContextFactory CLI Typescript
Notes: A conversational AI system that listens, learns, and challenges
author: 
Url: https://chatgpt.com/c/679cafb2-eb60-8008-a7f7-34131af6a916
Created: 2025-01-31  07:46:02
Modified: 
Tags: 
title: DBContextFactory CLI Typescript-webclip
date: 2025-01-31T15:46:14.902Z
lastmod: 2025-01-31T15:46:16.088Z
---
# DBContextFactory CLI Typescript

source: https://chatgpt.com/c/679cafb2-eb60-8008-a7f7-34131af6a916

> ## Excerpt
>
> A conversational AI system that listens, learns, and challenges

***

Here's a TypeScript-based command-line `DBContextFactory` that supports SQLite, MySQL, and PostgreSQL. This implementation follows the same factory pattern as in the C#, Python, and Go versions.

### Steps:

1. Install dependencies:

   ```
   <div><p>sh</p><p><code id="code-lang-sh">npm init -y
   npm install mysql2 pg sqlite3 dotenv yargs
   </code></p></div>
   ```

2. Create the following files:

***

### `db/IDBContextFactory.ts`

```
<div><p>typescript</p><p><code id="code-lang-typescript"><span>export</span> <span>interface</span> <span>IDBContextFactory</span> {
    <span>createContext</span>(): <span>Promise</span>&lt;<span>any</span>&gt;;
}
</code></p></div>
```

***

### `db/SqliteDbContext.ts`

```
<div><p>typescript</p><p><code id="code-lang-typescript"><span>import</span> { <span>IDBContextFactory</span> } <span>from</span> <span>"./IDBContextFactory"</span>;
<span>import</span> sqlite3 <span>from</span> <span>"sqlite3"</span>;
<span>import</span> { open, <span>Database</span> } <span>from</span> <span>"sqlite"</span>;

<span>export</span> <span>class</span> <span>SqliteDbContext</span> <span>implements</span> <span>IDBContextFactory</span> {
    <span>private</span> <span>dbFilePath</span>: <span>string</span>;

    <span>constructor</span>(<span>dbFilePath: <span>string</span></span>) {
        <span>this</span>.<span>dbFilePath</span> = dbFilePath;
    }

    <span>async</span> <span>createContext</span>(): <span>Promise</span>&lt;<span>Database</span>&gt; {
        <span>return</span> <span>open</span>({
            <span>filename</span>: <span>this</span>.<span>dbFilePath</span>,
            <span>driver</span>: sqlite3.<span>Database</span>
        });
    }
}
</code></p></div>
```

***

### `db/MySqlDbContext.ts`

```
<div><p>typescript</p><p><code id="code-lang-typescript"><span>import</span> { <span>IDBContextFactory</span> } <span>from</span> <span>"./IDBContextFactory"</span>;
<span>import</span> mysql, { <span>Connection</span> } <span>from</span> <span>"mysql2/promise"</span>;

<span>export</span> <span>class</span> <span>MySqlDbContext</span> <span>implements</span> <span>IDBContextFactory</span> {
    <span>private</span> <span>config</span>: <span>any</span>;

    <span>constructor</span>(<span>config: <span>any</span></span>) {
        <span>this</span>.<span>config</span> = config;
    }

    <span>async</span> <span>createContext</span>(): <span>Promise</span>&lt;<span>Connection</span>&gt; {
        <span>return</span> mysql.<span>createConnection</span>(<span>this</span>.<span>config</span>);
    }
}
</code></p></div>
```

***

### `db/PostgresDbContext.ts`

```
<div><p>typescript</p><p><code id="code-lang-typescript"><span>import</span> { <span>IDBContextFactory</span> } <span>from</span> <span>"./IDBContextFactory"</span>;
<span>import</span> { <span>Pool</span>, <span>PoolClient</span> } <span>from</span> <span>"pg"</span>;

<span>export</span> <span>class</span> <span>PostgresDbContext</span> <span>implements</span> <span>IDBContextFactory</span> {
    <span>private</span> <span>pool</span>: <span>Pool</span>;

    <span>constructor</span>(<span>config: <span>any</span></span>) {
        <span>this</span>.<span>pool</span> = <span>new</span> <span>Pool</span>(config);
    }

    <span>async</span> <span>createContext</span>(): <span>Promise</span>&lt;<span>PoolClient</span>&gt; {
        <span>return</span> <span>this</span>.<span>pool</span>.<span>connect</span>();
    }
}
</code></p></div>
```

***

### `db/DBContextFactory.ts`

```
<div><p>typescript</p><p><code id="code-lang-typescript"><span>import</span> { <span>IDBContextFactory</span> } <span>from</span> <span>"./IDBContextFactory"</span>;
<span>import</span> { <span>SqliteDbContext</span> } <span>from</span> <span>"./SqliteDbContext"</span>;
<span>import</span> { <span>MySqlDbContext</span> } <span>from</span> <span>"./MySqlDbContext"</span>;
<span>import</span> { <span>PostgresDbContext</span> } <span>from</span> <span>"./PostgresDbContext"</span>;
<span>import</span> fs <span>from</span> <span>"fs"</span>;

<span>interface</span> <span>FactorySetting</span> {
    <span>factoryName</span>: <span>string</span>;
    <span>factoryConfig</span>: <span>any</span>;
}

<span>export</span> <span>class</span> <span>DBContextFactory</span> {
    <span>private</span> <span>activeFactory</span>: <span>string</span>;
    <span>private</span> <span>factories</span>: <span>FactorySetting</span>[];

    <span>constructor</span>(<span>configFilePath: <span>string</span></span>) {
        <span>const</span> configData = <span>JSON</span>.<span>parse</span>(fs.<span>readFileSync</span>(configFilePath, <span>"utf8"</span>));
        <span>this</span>.<span>activeFactory</span> = configData.<span>activeFactory</span>;
        <span>this</span>.<span>factories</span> = configData.<span>factories</span>;
    }

    <span>createContext</span>(): <span>Promise</span>&lt;<span>any</span>&gt; {
        <span>const</span> factorySetting = <span>this</span>.<span>factories</span>.<span>find</span>(<span><span>f</span> =&gt;</span> f.<span>factoryName</span> === <span>this</span>.<span>activeFactory</span>);
        <span>if</span> (!factorySetting) {
            <span>throw</span> <span>new</span> <span>Error</span>(<span>`Factory not found: <span>${<span>this</span>.activeFactory}</span>`</span>);
        }

        <span>switch</span> (factorySetting.<span>factoryName</span>) {
            <span>case</span> <span>"sqlite"</span>:
                <span>return</span> <span>new</span> <span>SqliteDbContext</span>(factorySetting.<span>factoryConfig</span>).<span>createContext</span>();
            <span>case</span> <span>"mysql"</span>:
                <span>return</span> <span>new</span> <span>MySqlDbContext</span>(factorySetting.<span>factoryConfig</span>).<span>createContext</span>();
            <span>case</span> <span>"postgres"</span>:
                <span>return</span> <span>new</span> <span>PostgresDbContext</span>(factorySetting.<span>factoryConfig</span>).<span>createContext</span>();
            <span>default</span>:
                <span>throw</span> <span>new</span> <span>Error</span>(<span>`Unsupported factory: <span>${factorySetting.factoryName}</span>`</span>);
        }
    }
}
</code></p></div>
```

***

### `config.json`

```
<div><p>json</p><p><code id="code-lang-json"><span>{</span>
    <span>"activeFactory"</span><span>:</span> <span>"sqlite"</span><span>,</span>
    <span>"factories"</span><span>:</span> <span>[</span>
        <span>{</span>
            <span>"factoryName"</span><span>:</span> <span>"sqlite"</span><span>,</span>
            <span>"factoryConfig"</span><span>:</span> <span>"database.sqlite"</span>
        <span>}</span><span>,</span>
        <span>{</span>
            <span>"factoryName"</span><span>:</span> <span>"mysql"</span><span>,</span>
            <span>"factoryConfig"</span><span>:</span> <span>{</span>
                <span>"host"</span><span>:</span> <span>"localhost"</span><span>,</span>
                <span>"user"</span><span>:</span> <span>"root"</span><span>,</span>
                <span>"password"</span><span>:</span> <span>"password"</span><span>,</span>
                <span>"database"</span><span>:</span> <span>"testdb"</span>
            <span>}</span>
        <span>}</span><span>,</span>
        <span>{</span>
            <span>"factoryName"</span><span>:</span> <span>"postgres"</span><span>,</span>
            <span>"factoryConfig"</span><span>:</span> <span>{</span>
                <span>"host"</span><span>:</span> <span>"localhost"</span><span>,</span>
                <span>"user"</span><span>:</span> <span>"postgres"</span><span>,</span>
                <span>"password"</span><span>:</span> <span>"password"</span><span>,</span>
                <span>"database"</span><span>:</span> <span>"testdb"</span><span>,</span>
                <span>"port"</span><span>:</span> <span>5432</span>
            <span>}</span>
        <span>}</span>
    <span>]</span>
<span>}</span>
</code></p></div>
```

***

### `index.ts`

```
<div><p>typescript</p><p><code id="code-lang-typescript"><span>import</span> { <span>DBContextFactory</span> } <span>from</span> <span>"./db/DBContextFactory"</span>;
<span>import</span> yargs <span>from</span> <span>"yargs"</span>;
<span>import</span> { hideBin } <span>from</span> <span>"yargs/helpers"</span>;

<span>const</span> argv = <span>yargs</span>(<span>hideBin</span>(process.<span>argv</span>))
    .<span>option</span>(<span>"config"</span>, {
        <span>alias</span>: <span>"c"</span>,
        <span>type</span>: <span>"string"</span>,
        <span>description</span>: <span>"Path to config file"</span>,
        <span>default</span>: <span>"config.json"</span>
    })
    .<span>help</span>()
    .<span>argv</span>;

<span>async</span> <span>function</span> <span>main</span>(<span></span>) {
    <span>try</span> {
        <span>const</span> factory = <span>new</span> <span>DBContextFactory</span>(argv.<span>config</span>);
        <span>const</span> dbContext = <span>await</span> factory.<span>createContext</span>();

        <span>console</span>.<span>log</span>(<span>"Database context created successfully."</span>);
        <span>console</span>.<span>log</span>(dbContext);

        <span>// Close the database connection</span>
        <span>if</span> (dbContext.<span>end</span>) {
            dbContext.<span>end</span>();
        } <span>else</span> <span>if</span> (dbContext.<span>close</span>) {
            dbContext.<span>close</span>();
        }
    } <span>catch</span> (error) {
        <span>console</span>.<span>error</span>(<span>"Error creating database context:"</span>, error);
    }
}

<span>main</span>();
</code></p></div>
```

***

### Running the Program

```
<div><p>sh</p><p><code id="code-lang-sh">ts-node index.ts --config=config.json
</code></p></div>
```

This script will read `config.json`, determine which database context to create, and initialize it accordingly. Let me know if you need any modifications!
