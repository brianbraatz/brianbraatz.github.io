---
title: Building a Serverless ToDo App-React, Next.js, NextAuth, AWS Lambda, and DynamoDB
description: With a little vercel and tailwnd to Spice up and Deploy
slug: todo-app-react-nextauth-aws
date: 2021-08-04
image: post/Articles/IMAGES/todo.png
categories:
  - Cloud
  - Next.js
  - Nextjs
  - NextAuth
  - AWS
  - AWS Lambda
  - DynamoDB
  - Authentication
  - React
tags:
  - Next.js
  - React
  - NextAuth
  - OAuth
  - Serverless
draft: false
weight: 631
lastmod: 2025-03-21T04:15:30.639Z
---
# Building a Serverless ToDo App – Part 1: Next.js, React & NextAuth Setup

Welcome to ur quest to build the most over-engineered, enterprise-grade ToDo list known to humankind — because simple local storage just isn’t fun enough, right?

We’re going to build a fully serverless ToDo app using **Next.js**, **React**, **NextAuth.js**, **AWS Lambda**, and **DynamoDB**.

Yes, it's just a list of todos.

Yes, we’re using enough cloud tech to run NASA.

***

## 🧱 Step 1: Spin up the Next.js app

First things first, let’s create the Next.js app like all proper developers do — by copying from the docs and pretending we wrote it ourselves.

```bash
npx create-next-app@latest todo-app --typescript
cd todo-app
```

If you don’t want TypeScript, remove the `--typescript` flag, but I promise it’s worth the tiny bit of pain.

Now install **NextAuth.js**, our trusty authentication wizard:

```bash
npm install next-auth
```

***

## 🛂 Step 2: Set up NextAuth with GitHub login

We’re gonna let users log in using GitHub, because password forms are sooo 2009.

Create this file:

```ts
// pages/api/auth/[...nextauth].ts
```

And drop in this magical incantation:

```ts
import NextAuth from "next-auth"
import GitHubProvider from "next-auth/providers/github"

export default NextAuth({
  providers: [
    GitHubProvider({
      clientId: process.env.GITHUB_ID!,
      clientSecret: process.env.GITHUB_SECRET!,
    }),
  ],
  secret: process.env.NEXTAUTH_SECRET,
})
```

Now go set up a GitHub OAuth app here: <https://github.com/settings/developers>

Use `http://localhost:3000` as your homepage and callback URL (don’t forget to click Save, or you’ll wonder why nothing works for 20 minutes).

Then, create a `.env.local` file and add:

```env
GITHUB_ID=your-client-id-here
GITHUB_SECRET=your-secret-here
NEXTAUTH_SECRET=make-up-a-secret-here
NEXTAUTH_URL=http://localhost:3000
```

***

## 🪄 Step 3: Wrap your app in a SessionProvider

To give your whole app access to authentication data, we wrap everything in the magical `<SessionProvider>`.

Open up:

```tsx
// pages/_app.tsx
```

And wrap it like a burrito:

```tsx
import { SessionProvider } from "next-auth/react"
import type { AppProps } from "next/app"

export default function App({ Component, pageProps: { session, ...pageProps } }: AppProps) {
  return (
    <SessionProvider session={session}>
      <Component {...pageProps} />
    </SessionProvider>
  )
}
```

Boom — now you’ve got access to the session from anywhere. Kinda like global state, but legal.

***

## 😎 Step 4: Add a login/logout button

Now let’s see this baby in action.

Open up `pages/index.tsx` and paste this in:

```tsx
import { useSession, signIn, signOut } from "next-auth/react"

export default function Home() {
  const { data: session } = useSession()

  if (session) {
    return (
      <div>
        <p>Hey {session.user?.name}, you're logged in 🎉</p>
        <button onClick={() => signOut()}>Logout</button>
      </div>
    )
  }

  return (
    <div>
      <p>You’re not logged in yet 😢</p>
      <button onClick={() => signIn("github")}>Login with GitHub</button>
    </div>
  )
}
```

Now run your dev server:

```bash
npm run dev
```

Go to <http://localhost:3000>, and boom 💥

You now have a login button that opens GitHub, and a logout button that kicks the user out. We’re cooking now.

***

## 🧪 Sanity Check: Is it working?

If you can:

* Login with GitHub
* See your name show up
* Logout and go back to sad mode

Then congrats — you’ve got auth working!

If not, double-check your `.env.local` values. Or scream into the void — either helps.

***

<!-- ## 🔮 What’s Next?

In we’re gonna set up **AWS Lambda** using the **Serverless Framework**, so we can write our backend functions like fancy cloud people.

We’ll also prepare a couple of sample endpoints (`getTodos`, `addTodo`) that we’ll hook into later.

If you’ve never deployed a Lambda function before, don’t worry — it’s easier than finding the right charger for your old Android phone.

---

See you in Part 2, where we summon the cloud beasts. 🐉☁️ -->

## 🗝️ Key Ideas

| Key Idea                      | Summary                           |
| ----------------------------- | --------------------------------- |
| NextAuth.js                   | Handles all our auth flow         |
| GitHub OAuth                  | Lets users log in securely        |
| SessionProvider               | Shares auth session across app    |
| Protected session-aware pages | React components use `useSession` |
| Next up: Lambda + API Gateway | Backend magic time                |

***

# Part 2: AWS Lambda + Serverless Framework Setup

In **Part 1**, we got authentication up and running with Next.js and NextAuth.js — like responsible devs.

Now it's time to call upon the power of the cloud and deploy some backend code using **AWS Lambda** via the **Serverless Framework**.

We're gonna write our own API using Lambdas like the absolute legends we are.

***

## 📦 Step 1: Set up the backend folder

Let’s keep things tidy.

Inside the root of your project, make a sibling folder for the backend:

```bash
mkdir backend
cd backend
npm init -y
```

We’re going to install all the things we need to conjure cloud functions:

```bash
npm install serverless serverless-offline typescript @types/aws-lambda esbuild --save-dev
```

We'll use:

* `serverless` – deploys code to AWS Lambda
* `serverless-offline` – lets us test locally
* `typescript` – because we love ourselves
* `esbuild` – because life’s too short for slow builds

***

## ⚙️ Step 2: Configure Serverless Framework

Create a file called `serverless.yml` in the `backend/` folder.

Here’s the basic config to start:

```yaml
service: todo-backend

frameworkVersion: '3'

provider:
  name: aws
  runtime: nodejs18.x
  region: us-east-1

plugins:
  - serverless-offline

functions:
  getTodos:
    handler: functions/getTodos.handler
    events:
      - http:
          path: todos
          method: get

  addTodo:
    handler: functions/addTodo.handler
    events:
      - http:
          path: todos
          method: post
```

We just declared two functions: `getTodos` and `addTodo`.

They’ll be accessible over HTTP via API Gateway. Free REST API, baby.

***

## 🧠 Step 3: Add your function handlers

Let’s make a folder for your Lambdas:

```bash
mkdir -p functions
```

Create `getTodos.ts`:

```ts
// functions/getTodos.ts

import { APIGatewayProxyHandler } from "aws-lambda"

export const handler: APIGatewayProxyHandler = async () => {
  return {
    statusCode: 200,
    body: JSON.stringify([
      { id: 1, text: "Learn Lambda", done: false },
      { id: 2, text: "Build serverless ToDo app", done: true },
    ]),
  }
}
```

And `addTodo.ts`:

```ts
// functions/addTodo.ts

import { APIGatewayProxyHandler } from "aws-lambda"

export const handler: APIGatewayProxyHandler = async (event) => {
  const body = JSON.parse(event.body || "{}")
  const newTodo = {
    id: Date.now(),
    text: body.text,
    done: false,
  }

  return {
    statusCode: 201,
    body: JSON.stringify(newTodo),
  }
}
```

They’re fake for now — no database yet — but we’ll fix that in Part 4. Pinky swear.

***

## 🧪 Step 4: Test locally with serverless-offline

Before you deploy your code to the skies, let’s test locally.

Update your `package.json` scripts:

```json
"scripts": {
  "dev": "serverless offline start"
}
```

Then run:

```bash
npm run dev
```

Now you should see:

```
http://localhost:3000/dev/todos (GET)
http://localhost:3000/dev/todos (POST)
```

Test the GET route in your browser.

For the POST route, you can use [Postman](https://www.postman.com/) or cURL:

```bash
curl -X POST http://localhost:3000/dev/todos \
  -H "Content-Type: application/json" \
  -d '{"text": "Buy a pineapple"}'
```

You should get back your new todo. 🍍

***

## ☁️ Step 5: Deploy to AWS

Make sure you’ve configured your AWS credentials (with `aws configure` or environment variables).

Then deploy with:

```bash
npx serverless deploy
```

If all goes well, you'll get public URLs for your endpoints like:

```
GET - https://abc123.execute-api.us-east-1.amazonaws.com/dev/todos
POST - https://abc123.execute-api.us-east-1.amazonaws.com/dev/todos
```

Now your API is officially on the cloud.

Cue dramatic orchestral music. 🎻✨

***

<!-- ## 🧵 What’s Next?

In **Part 3**, we’ll protect these Lambda routes by passing in the **NextAuth JWT** from our frontend and verifying it inside the Lambda functions.

That way, nobody can spam your ToDo list with pineapple memes unless they’re logged in.

We’re getting real secure, real fast. 🔐

--- -->

## 🗝️ Key Ideas

| Key Idea                          | Summary                                   |
| --------------------------------- | ----------------------------------------- |
| Serverless Framework              | Simplifies Lambda + API Gateway setup     |
| Local dev with serverless-offline | Lets you test functions before deploying  |
| HTTP Functions                    | Create RESTful endpoints with YAML config |
| AWS Lambda                        | Our serverless backend brain              |
| API Gateway                       | Exposes our Lambdas over the web          |
| Next up: Auth protection          | Because not everyone deserves your todos  |

<!-- ---
title: "Building a Serverless ToDo App – Part 3: Protecting AWS Lambda Routes with NextAuth JWTs"
description: "Building a Serverless ToDo App – Part 3: Protecting AWS Lambda Routes with NextAuth JWTs"
slug: "todo-app-part-3-auth-lambda"
date: 2018-03-14
image: "post/Articles/IMAGES/40.jpg"
categories: ["Next.js", "Authentication", "AWS", "Serverless"]
tags: ["JWT", "NextAuth", "AWS Lambda", "API Gateway", "Auth"]
draft: false
weight: 504
--- -->

# Part 3: Protecting AWS Lambda Routes with NextAuth JWTs

Alright, welcome to **Part 3** of our cloud-powered ToDo adventure!

So far, we’ve got:

* ✅ A frontend with **Next.js**, **React**, and **NextAuth**
* ✅ A backend with **AWS Lambda** and **API Gateway**
* 🚨 But absolutely zero protection on our Lambda routes

If someone finds your API endpoint right now, they could post **"Rickroll"** todos all day long.

Not on our watch. Let’s lock this thing down using **NextAuth JWTs**.

***

## 🧠 Quick Overview: What’s the plan?

1. Configure NextAuth to use **JWT sessions**
2. Send the JWT in the **Authorization header** when calling Lambda
3. In the Lambda function, **verify** the JWT using the secret
4. Only allow access if the token is valid

Sounds fancy. It is. But also not too hard.

Let’s do it. 🛡️

***

## 🔐 Step 1: Use JWTs in NextAuth

By default, NextAuth supports JWTs. But we’re going to be explicit, because we’re classy like that.

Update your `[...nextauth].ts`:

```ts
// pages/api/auth/[...nextauth].ts

import NextAuth from "next-auth"
import GitHubProvider from "next-auth/providers/github"
import { JWT } from "next-auth/jwt"

export default NextAuth({
  providers: [
    GitHubProvider({
      clientId: process.env.GITHUB_ID!,
      clientSecret: process.env.GITHUB_SECRET!,
    }),
  ],
  secret: process.env.NEXTAUTH_SECRET,
  session: {
    strategy: "jwt",
  },
  callbacks: {
    async jwt({ token, user }) {
      // Attach user ID to token
      if (user) token.id = user.id
      return token
    },
    async session({ session, token }) {
      // Expose user ID in session
      if (token) session.user.id = token.id
      return session
    },
  },
})
```

***

## 🧾 Step 2: Send the JWT to your Lambda

Inside your frontend, get the token using `getToken` from NextAuth.

Install the helper:

```bash
npm install next-auth
```

Then in your component or utility:

```ts
// utils/api.ts

import { getToken } from "next-auth/jwt"

export async function fetchTodos() {
  const token = await getToken({ req: { headers: {} }, secret: process.env.NEXTAUTH_SECRET })

  const res = await fetch("https://your-api-id.execute-api.us-east-1.amazonaws.com/dev/todos", {
    headers: {
      Authorization: `Bearer ${token}`,
    },
  })

  return res.json()
}
```

Pro tip: If you're using `getServerSideProps`, you can call `getToken({ req })` directly from the context.

***

## 🔍 Step 3: Verify JWT in Lambda

Now let's head over to the backend.

Install the JWT package in your `backend/`:

```bash
npm install jsonwebtoken
npm install @types/jsonwebtoken --save-dev
```

Then update your function to verify the token:

```ts
// functions/getTodos.ts

import { APIGatewayProxyHandler } from "aws-lambda"
import jwt from "jsonwebtoken"

const SECRET = process.env.NEXTAUTH_SECRET || "shhh-very-secret"

export const handler: APIGatewayProxyHandler = async (event) => {
  const authHeader = event.headers.Authorization || ""

  if (!authHeader.startsWith("Bearer ")) {
    return { statusCode: 401, body: "Missing or invalid token" }
  }

  const token = authHeader.slice(7)

  try {
    const decoded = jwt.verify(token, SECRET)
    console.log("Authenticated user:", decoded)

    return {
      statusCode: 200,
      body: JSON.stringify([
        { id: 1, text: "Secure ToDo 1", done: false },
        { id: 2, text: "Secure ToDo 2", done: true },
      ]),
    }
  } catch (err) {
    return {
      statusCode: 403,
      body: "Invalid or expired token",
    }
  }
}
```

Now your API is locked tighter than grandma’s cookie jar. 🍪🔒

***

## 🧪 Test It All Together

1. Log in with GitHub in your frontend
2. Call the secure Lambda route with your JWT in the header
3. See your sweet, sweet todos coming back (or errors if you mess up — which is part of the fun)

If you get a 401 or 403, double-check:

* The token is being sent
* You’re using the same `NEXTAUTH_SECRET` in both frontend and backend
* You didn’t copy-paste something from Stack Overflow without reading 😬

***

## 🧮 Bonus: DRY your auth logic

Create a `verifyJwt.ts` helper:

```ts
// utils/verifyJwt.ts

import jwt from "jsonwebtoken"

const SECRET = process.env.NEXTAUTH_SECRET || "shhh-very-secret"

export function verifyToken(authHeader?: string) {
  if (!authHeader?.startsWith("Bearer ")) {
    throw new Error("Missing token")
  }

  const token = authHeader.slice(7)
  return jwt.verify(token, SECRET)
}
```

Use it in any function. Your code just got 37% cleaner. ✨

***

<!-- 
## 🔮 What’s Next?

In **Part 4**, we’ll hook up a real **DynamoDB** database, so we’re not just making up todos in memory.

We’ll store todos per user (based on their token) and actually save + retrieve data.

Because this is a *real* app now.

Get ready to battle IAM policies and DynamoDB quirks. It’s gonna be wild. 🦓 -->

***

## 🗝️ Key Ideas

| Key Idea                      | Summary                               |
| ----------------------------- | ------------------------------------- |
| JWT session strategy          | Stores session data in a signed token |
| Token sent via Authorization  | Protects your Lambda endpoints        |
| Token verified inside Lambda  | Keeps your backend secure             |
| Secret shared across services | MUST be the same for both ends        |
| Next: store todos in DynamoDB | It's DB time, baby 🗃️                |

<!-- 
---
title: "Building a Serverless ToDo App – Part 4: Storing Todos in DynamoDB"
description: "Building a Serverless ToDo App – Part 4: Storing Todos in DynamoDB"
slug: "todo-app-part-4-dynamodb"
date: 2017-07-29
image: "post/Articles/IMAGES/38.jpg"
categories: ["Next.js", "AWS", "DynamoDB", "Backend"]
tags: ["DynamoDB", "AWS Lambda", "NextAuth", "ToDo", "Serverless"]
draft: false
weight: 712
--- -->

# Part 4: Storing Todos in DynamoDB

<!-- 
Welcome back to our saga of building the world’s most *absurdly overengineered* ToDo app.  -->

So far we’ve built:

* ✅ A slick frontend with Next.js and NextAuth
* ✅ A backend using AWS Lambda
* ✅ Authentication with secure JWT-based access

But right now, our todos are living in memory.

Which means the moment you refresh, *poof!* 💨 They vanish into the ether like your dreams of becoming a DJ.

Time to give these todos a home — a safe, warm place where they can live forever (or until you delete them).

Let’s bring in **DynamoDB**, the serverless NoSQL database from AWS.

***

## 🧱 Step 1: Create a DynamoDB Table

Go to the AWS Console, search for **DynamoDB**, and click **"Create table"**.

Use the following settings:

* **Table name**: `todos`
* **Partition key**: `userId` (type: String)
* **Sort key**: `id` (type: String)

Keep the rest of the settings as defaults, and hit **Create**.

Boom. You’ve now got a place to dump todos like a digital hoarder. 🧻

***

## 🧪 Step 2: Add AWS SDK to your backend

Inside the `backend/` folder, install the AWS SDK v3:

```bash
npm install @aws-sdk/client-dynamodb @aws-sdk/lib-dynamodb
```

***

## 📦 Step 3: Create a reusable DB client

Let’s set up a DynamoDB DocumentClient that doesn’t make you cry every time you use it.

```ts
// backend/utils/dynamo.ts

import { DynamoDBClient } from "@aws-sdk/client-dynamodb"
import { DynamoDBDocumentClient } from "@aws-sdk/lib-dynamodb"

const client = new DynamoDBClient({ region: "us-east-1" })

export const ddb = DynamoDBDocumentClient.from(client)
```

Simple. Clean. Fancy.

***

## 🧾 Step 4: Store todos in `addTodo`

Update `addTodo.ts` to store the todo in DynamoDB:

```ts
// backend/functions/addTodo.ts

import { APIGatewayProxyHandler } from "aws-lambda"
import { PutCommand } from "@aws-sdk/lib-dynamodb"
import { ddb } from "../utils/dynamo"
import jwt from "jsonwebtoken"

const SECRET = process.env.NEXTAUTH_SECRET || "super-secret"

export const handler: APIGatewayProxyHandler = async (event) => {
  try {
    const auth = event.headers.Authorization || ""
    const token = jwt.verify(auth.slice(7), SECRET) as any
    const userId = token.sub

    const body = JSON.parse(event.body || "{}")

    const newTodo = {
      userId,
      id: Date.now().toString(),
      text: body.text,
      done: false,
    }

    await ddb.send(
      new PutCommand({
        TableName: "todos",
        Item: newTodo,
      })
    )

    return {
      statusCode: 201,
      body: JSON.stringify(newTodo),
    }
  } catch (err) {
    return {
      statusCode: 500,
      body: JSON.stringify({ error: "Failed to add todo" }),
    }
  }
}
```

We extract the `sub` field from the JWT (which is usually the user ID), then store the todo under their ID.

***

## 📬 Step 5: Read todos in `getTodos`

Now update your `getTodos.ts` to pull the user’s todos from DynamoDB:

```ts
// backend/functions/getTodos.ts

import { APIGatewayProxyHandler } from "aws-lambda"
import { QueryCommand } from "@aws-sdk/lib-dynamodb"
import { ddb } from "../utils/dynamo"
import jwt from "jsonwebtoken"

const SECRET = process.env.NEXTAUTH_SECRET || "super-secret"

export const handler: APIGatewayProxyHandler = async (event) => {
  try {
    const auth = event.headers.Authorization || ""
    const token = jwt.verify(auth.slice(7), SECRET) as any
    const userId = token.sub

    const result = await ddb.send(
      new QueryCommand({
        TableName: "todos",
        KeyConditionExpression: "userId = :uid",
        ExpressionAttributeValues: {
          ":uid": userId,
        },
      })
    )

    return {
      statusCode: 200,
      body: JSON.stringify(result.Items),
    }
  } catch (err) {
    return {
      statusCode: 500,
      body: JSON.stringify({ error: "Failed to fetch todos" }),
    }
  }
}
```

You’re now officially storing and fetching todos per user like a total pro. 😎

***

## 🛑 Step 6: Don’t forget permissions!

Your Lambda functions need permission to read and write from DynamoDB.

Update your `serverless.yml`:

```yaml
provider:
  name: aws
  runtime: nodejs18.x
  region: us-east-1
  iamRoleStatements:
    - Effect: "Allow"
      Action:
        - "dynamodb:PutItem"
        - "dynamodb:Query"
      Resource: "arn:aws:dynamodb:us-east-1:*:table/todos"
```

Now redeploy:

```bash
npx serverless deploy
```

***

## ✅ Test it out

1. Log in with GitHub
2. Use your frontend to add a todo (we’ll hook this up in Part 5!)
3. Check DynamoDB — your new todo should be sitting there, feeling proud of itself

***

<!-- ## 🧠 What’s Next?

In **Part 5**, we’ll wire up the UI — the form to add todos, and the list to display them.

We’ll call our Lambda APIs using `fetch`, send along the JWT, and turn our app into a real productivity machine (or a procrastination tracker, depending on how you use it).

We’re *this* close to MVP greatness. 💪 -->

***

## 🗝️ Key Ideas

| Key Idea                  | Summary                                    |
| ------------------------- | ------------------------------------------ |
| DynamoDB Table            | Stores todos with userId as partition key  |
| AWS SDK v3                | Used to interact with DynamoDB             |
| Token-based auth          | Securely associates todos with users       |
| PutCommand / QueryCommand | Store and retrieve user-specific items     |
| Next up: UI integration   | Time to make it pretty and interactive! 🎨 |

<!-- ---
title: "Building a Serverless ToDo App – Part 5: Wiring Up the UI with AWS Lambda"
description: "Building a Serverless ToDo App – Part 5: Wiring Up the UI with AWS Lambda"
slug: "todo-app-part-5-ui"
date: 2015-11-12
image: "post/Articles/IMAGES/41.jpg"
categories: ["Next.js", "React", "Serverless", "Frontend"]
tags: ["UI", "Next.js", "React", "AWS Lambda", "JWT", "ToDo"]
draft: false
weight: 589
--- -->

# Part 5: Wiring Up the UI with AWS Lambda

Alrighty, welcome to **Part 5**, the part where we *finally* connect the dots and bring everything to life.

We’ve got:

* ✅ Auth
* ✅ Lambda functions
* ✅ Secure access with JWTs
* ✅ A database (DynamoDB!) that’s more persistent than a clingy ex

Now it's time to **hook up the UI** — meaning:

* Display todos pulled from the Lambda API
* Let the user add a todo
* Show loading/error states like pros
* And of course, keep it ✨ cute ✨

<!-- Let’s do this! -->

***

## 🧠 Step 1: Fetch todos from the API

We’re gonna hit the `/todos` endpoint and include the JWT.

Let’s make a helper.

### 🛠️ `utils/api.ts`

```ts
import { getToken } from "next-auth/jwt"

const API_URL = process.env.NEXT_PUBLIC_API_URL // e.g., https://abc123.execute-api.us-east-1.amazonaws.com/dev

export async function fetchTodos(token: string) {
  const res = await fetch(`${API_URL}/todos`, {
    headers: {
      Authorization: `Bearer ${token}`,
    },
  })

  if (!res.ok) throw new Error("Failed to fetch todos")

  return res.json()
}

export async function addTodo(token: string, text: string) {
  const res = await fetch(`${API_URL}/todos`, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${token}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ text }),
  })

  if (!res.ok) throw new Error("Failed to add todo")

  return res.json()
}
```

Set the URL in your `.env.local`:

```env
NEXT_PUBLIC_API_URL=https://your-api-id.execute-api.us-east-1.amazonaws.com/dev
```

***

## 💻 Step 2: Build the UI

Let’s build a cute little dashboard that shows todos and lets you add more.

### 🧑‍🎨 `pages/index.tsx`

```tsx
import { useSession, getSession } from "next-auth/react"
import { useEffect, useState } from "react"
import { fetchTodos, addTodo } from "../utils/api"

export default function Home() {
  const { data: session, status } = useSession()
  const [todos, setTodos] = useState([])
  const [newTodo, setNewTodo] = useState("")
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState("")

  useEffect(() => {
    const loadTodos = async () => {
      if (!session) return
      try {
        setLoading(true)
        const token = await fetch("/api/auth/session").then(res => res.json()).then(s => s.token)
        const data = await fetchTodos(token)
        setTodos(data)
      } catch (err) {
        setError("Failed to load todos")
      } finally {
        setLoading(false)
      }
    }

    loadTodos()
  }, [session])

  const handleAdd = async () => {
    if (!newTodo.trim()) return
    try {
      const token = await fetch("/api/auth/session").then(res => res.json()).then(s => s.token)
      const todo = await addTodo(token, newTodo)
      setTodos(prev => [...prev, todo])
      setNewTodo("")
    } catch (err) {
      setError("Failed to add todo")
    }
  }

  if (status === "loading") return <p>Loading...</p>
  if (!session) return <p>Please log in to view your todos.</p>

  return (
    <div style={{ maxWidth: 600, margin: "2rem auto", padding: "1rem" }}>
      <h1>📝 Your ToDo List</h1>

      {loading && <p>Loading todos...</p>}
      {error && <p style={{ color: "red" }}>{error}</p>}

      <ul>
        {todos.map((todo: any) => (
          <li key={todo.id}>
            {todo.text} {todo.done ? "✅" : ""}
          </li>
        ))}
      </ul>

      <input
        type="text"
        value={newTodo}
        onChange={(e) => setNewTodo(e.target.value)}
        placeholder="Enter a new todo"
      />
      <button onClick={handleAdd}>Add Todo</button>
    </div>
  )
}
```

This is a bit basic, but hey, it works!

You:

* See todos
* Add todos
* Use serverless APIs like a boss

***

## 🪄 Step 3: Make your session include the token (optional)

NextAuth doesn’t expose the token in the session by default on the client. If you want to include it in the session for easy access, you can update the session callback like this:

### In `[...nextauth].ts`:

```ts
callbacks: {
  async jwt({ token, user }) {
    if (user) token.id = user.id
    return token
  },
  async session({ session, token }) {
    if (token) {
      session.user.id = token.id
      session.token = token // <- add this
    }
    return session
  }
}
```

Now you can get `session.token` directly on the frontend.

***

## ✅ That’s a Wrap!

You’ve now got a real, live, working ToDo app that:

* Authenticates users via GitHub
* Stores data in DynamoDB
* Uses AWS Lambda for all backend logic
* Is serverless and extremely scalable

Also, you did all of this with basically zero infrastructure to maintain. Just some code, a little YAML, and pure cloud power. ☁️💪

***

## 🧠 What’s Next?

If you want to keep going, you can:

* Add a **delete** button
* Toggle `done` status with a click
* Add filtering (All, Active, Done)
* Make it pretty with Tailwind or shadcn/ui
* Deploy the frontend to **Vercel**

Or you can just take a victory nap. You earned it. 😴

***

## 🗝️ Key Ideas

| Key Idea                      | Summary                                      |
| ----------------------------- | -------------------------------------------- |
| fetchTodos / addTodo          | Custom fetch helpers using JWTs              |
| Session-aware frontend        | `useSession` gives access to logged-in state |
| Secure API interaction        | Token sent via headers to protected Lambda   |
| Real-time UI                  | React state updates after API calls          |
| Serverless frontend + backend | Fully cloud-native productivity stack        |

<!-- 
---
title: "Building a Serverless ToDo App – Part 6: Polishing, Deploying, and Final Touches"
description: "Building a Serverless ToDo App – Part 6: Polishing, Deploying, and Final Touches"
slug: "todo-app-part-6-polish-deploy"
date: 2019-08-19
image: "post/Articles/IMAGES/43.jpg"
categories: ["Next.js", "Deployment", "UI", "AWS"]
tags: ["Deployment", "Vercel", "Tailwind CSS", "CI/CD", "Serverless"]
draft: false
weight: 793
--- -->

# Part 6: Polishing, Deploying, and Final Touches

Congratulations, you magnificent cloud creature! 🥳

You've made it to the **grand finale** of our totally over-the-top ToDo app adventure.

At this point, you've got:

* ✅ Auth via NextAuth.js
* ✅ Backend API in AWS Lambda
* ✅ Todos in DynamoDB
* ✅ A shiny React-based frontend calling secure endpoints

But we’re not done yet.

This part is all about **polishing the app**, **deploying it to the cloud**, and **adding those final sprinkles of awesomeness** to make it feel like a real product, not just a weekend hack.

Let’s make it sparkle. ✨

***

## 🚀 Step 1: Deploy the Frontend to Vercel

First, push your `frontend/` code to a GitHub repo.

Then go to <https://vercel.com>, sign in, and click **"New Project"**.

Connect your repo and follow the setup prompts.

Make sure to add these **Environment Variables** in the Vercel dashboard:

```env
GITHUB_ID=your-client-id
GITHUB_SECRET=your-client-secret
NEXTAUTH_SECRET=your-nextauth-secret
NEXTAUTH_URL=https://your-vercel-url.vercel.app
NEXT_PUBLIC_API_URL=https://your-api-id.execute-api.region.amazonaws.com/dev
```

Then hit deploy.

Boom. Your app is live on the internet.

You’re officially in production, baby. 🕺

***

## 💅 Step 2: Add Tailwind for Styling

Install Tailwind CSS in your Next.js project:

```bash
npm install -D tailwindcss postcss autoprefixer
npx tailwindcss init -p
```

Update `tailwind.config.js`:

```js
/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./pages/**/*.{ts,tsx}", "./components/**/*.{ts,tsx}"],
  theme: {
    extend: {},
  },
  plugins: [],
}
```

Update `globals.css`:

```css
@tailwind base;
@tailwind components;
@tailwind utilities;
```

Now start sprinkling Tailwind classes all over your UI like:

```tsx
<h1 className="text-2xl font-bold mb-4">📝 Your ToDo List</h1>
<button className="bg-blue-500 text-white px-4 py-2 rounded">Add Todo</button>
```

It’s like instant pretty. 💅

***

## ❌ Step 3: Add Delete Functionality

Your users should be able to delete todos — it’s 2025, after all.

### Add a DELETE route in your Lambda config:

```yaml
deleteTodo:
  handler: functions/deleteTodo.handler
  events:
    - http:
        path: todos/{id}
        method: delete
```

### Then the function:

```ts
// functions/deleteTodo.ts

import { APIGatewayProxyHandler } from "aws-lambda"
import { DeleteCommand } from "@aws-sdk/lib-dynamodb"
import { ddb } from "../utils/dynamo"
import jwt from "jsonwebtoken"

const SECRET = process.env.NEXTAUTH_SECRET || "super-secret"

export const handler: APIGatewayProxyHandler = async (event) => {
  try {
    const auth = event.headers.Authorization || ""
    const token = jwt.verify(auth.slice(7), SECRET) as any
    const userId = token.sub
    const todoId = event.pathParameters?.id

    await ddb.send(
      new DeleteCommand({
        TableName: "todos",
        Key: {
          userId,
          id: todoId,
        },
      })
    )

    return { statusCode: 204, body: "" }
  } catch (err) {
    return {
      statusCode: 500,
      body: JSON.stringify({ error: "Delete failed" }),
    }
  }
}
```

### Then in the frontend:

```tsx
const handleDelete = async (id: string) => {
  try {
    const token = await fetch("/api/auth/session").then(res => res.json()).then(s => s.token)
    await fetch(`${process.env.NEXT_PUBLIC_API_URL}/todos/${id}`, {
      method: "DELETE",
      headers: {
        Authorization: `Bearer ${token}`,
      },
    })
    setTodos(todos.filter(todo => todo.id !== id))
  } catch (err) {
    setError("Failed to delete todo")
  }
}
```

Now add a 🗑️ button in your UI and you’re good to go.

***

## ⚙️ Step 4: Automate Deployments (Optional)

Wanna go full pro mode?

* Use **GitHub Actions** or **Vercel’s Git Integration** to auto-deploy on push
* Add a CI workflow that lints your code
* Create a `serverless deploy` GitHub Action for backend updates

Now your app updates itself while you eat nachos. 🤌

***

## 🌑 Bonus: Dark Mode? Animations? AI?

Feeling spicy?

* Add **dark mode** using Tailwind’s `dark` class
* Use **Framer Motion** to animate todo entries
* Hook up **ChatGPT** to auto-write todos for you (because why not?)

Sky’s the limit — or rather, **cloud’s the limit** ☁️

***

## 🎉 You Made It!

You’ve officially built a full-stack, serverless, authenticated ToDo app using:

* **Next.js** for frontend
* **React** for UI
* **NextAuth.js** for secure login
* **AWS Lambda + API Gateway** for backend
* **DynamoDB** for storage
* **Vercel** for deployment

You should feel proud. Like, real proud. Take a selfie with your app. Post it. Brag a little. You earned it. 😎

***

## 🗝️ Key Ideas

| Key Idea          | Summary                                  |
| ----------------- | ---------------------------------------- |
| Vercel Deployment | One-click frontend hosting with env vars |
| Tailwind CSS      | Instant pretty with utility classes      |
| Delete Function   | Securely remove todos from DynamoDB      |
| CI/CD             | Automate deployment and backend updates  |
| Project complete! | Ship it, share it, flex it 💪            |

***

<!-- 
And that’s a wrap on our 6-part series!

If you made it this far, you're officially a **serverless superhero**.

Now go forth and build something even more ridiculous.

Or just... cross "Build ToDo App" off your todo list. ✅
```

<button class="copy-button">Copy Markdown</button>
``` -->
