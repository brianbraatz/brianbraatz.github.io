---
title: Next JS Snippets Collection
description: Collected Bits of Next JS Wisdom
slug: next-js-snippets
date: 2023-12-15
image: post/Articles/IMAGES/nextjs.png
categories:
  - NextJs
  - Web Development
  - Testing
  - Javascript
  - Typescript
  - Unit Testing
tags:
  - React
  - Interview
  - Questions
  - JavaScript
  - Frontend
  - Development
  - Coding
  - Examples
draft: false
weight: 370
categories_ref:
  - NextJs
  - Web Development
  - Testing
  - Javascript
  - Typescript
  - Unit Testing
slug_calculated: https://brianbraatz.github.io/p/next-js-snippets
lastmod: 2025-03-14T16:40:23.509Z
---
Next.js: the framework that turns React into a **supercharged**, **server-rendered**, **SEO-friendly** powerhouse.

If React is a Swiss Army knife, Next.js is that version with a built-in flamethrower and bottle opener.

:)\
(How's that for a visual? )

## A Brief History of Next.js (Or: How We Got Here)

Next.js was born in 2016, crafted by the fine folks at [Vercel](https://vercel.com/) (then called ZEIT). Their mission? To make React even better by simplifying server-side rendering (SSR) and static site generation (SSG).

Before Next.js, doing SSR with React felt like assembling IKEA furniture without a manual—painful, confusing, and somehow always missing one key piece.

Here’s how Next.js evolved over the years:

| Version | Release Date | Notable Features                              |
| ------- | ------------ | --------------------------------------------- |
| 1.0     | 2016-10-25   | Server-side rendering (SSR), simple routing   |
| 2.0     | 2017-03-27   | Static exports, custom server support         |
| 3.0     | 2017-08-14   | Prefetching, dynamic imports                  |
| 4.0     | 2017-12-07   | Improved SSR, better error handling           |
| 5.0     | 2018-03-09   | Webpack 4, multi-zone support                 |
| 6.0     | 2018-06-27   | Automatic static optimization                 |
| 7.0     | 2018-11-21   | Faster builds, improved prefetching           |
| 8.0     | 2019-02-27   | Incremental Static Regeneration (ISR)         |
| 9.0     | 2019-07-17   | API routes, automatic static generation       |
| 10.0    | 2020-10-27   | Internationalized routing, image optimization |
| 11.0    | 2021-06-15   | Webpack 5, faster refresh                     |
| 12.0    | 2021-10-26   | Middleware, React 18 support                  |
| 13.0    | 2022-10-25   | App directory, RSC (React Server Components)  |
| 14.0    | 2023-10-26   | TurboPack, huge performance boost             |

<!--
## 1. Server-Side Rendering (SSR) Without Tears

With Next.js, SSR is as easy as adding `getServerSideProps`. No need for weird workarounds or sacrificing your sanity.

## 2. Static Site Generation (SSG) for the Speed Demons

Want **blazing-fast** websites? Next.js can pre-build pages at build time. Just use `getStaticProps`, and boom—your site loads faster than your WiFi drops during a Zoom call.

## 3. API Routes: No More Backend Hassle

Next.js lets you write backend endpoints right inside your project. No need to spin up a separate Express server. Just create a file under `/pages/api`, and you’ve got a fully functional API. Magic!

## 4. Automatic Image Optimization (Because Nobody Likes Slow Images)

Next.js has built-in image optimization. That means **no more** slow-loading, unoptimized images. Just use the `<Image />` component, and Next.js does all the hard work for you.

## 5. Middleware for Ultimate Control

Middleware lets you **intercept requests** and modify them before they hit your pages. Perfect for authentication, logging, or just messing with users for fun (please don’t).

## 6. Internationalized Routing (Your Website, But Multilingual)

Need a site in **English, Spanish, and Klingon**? No problem. Next.js has built-in internationalization, so you can easily create multi-language sites without any headaches.

## 7. Hot Reloading (The Good Kind of Hot)

Next.js has **fast refresh**, which means you see updates instantly. No need to constantly refresh your browser like it’s 1999.

## 8. Edge Functions: The Future is Now

With **Edge Functions**, your code runs closer to the user, reducing latency and making your app snappier than a fresh bag of Doritos.

## 9. Hybrid Rendering (Because Why Choose?)

Next.js lets you mix SSR, SSG, and ISR (Incremental Static Regeneration). This means you can **pre-render some pages, generate others on the fly, and update content dynamically**—all in the same app.

## 10. Works Seamlessly with Vercel (Because They Made It)

Deploying Next.js on Vercel is **literally one click**. Since Vercel created Next.js, they made sure it runs like a dream on their platform.

---

## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **SSR** | Server-side rendering for SEO-friendly, dynamic pages |
| **SSG** | Prebuilt static pages for super-fast performance |
| **API Routes** | Create backend APIs without needing a separate server |
| **Image Optimization** | Automatically optimizes images for faster loading |
| **Middleware** | Intercept and modify requests before they reach the page |
| **Internationalization** | Easily create multilingual websites |
| **Hot Reloading** | See updates instantly while coding |
| **Edge Functions** | Run code close to users for lower latency |
| **Hybrid Rendering** | Mix SSR, SSG, and ISR for flexibility |
| **Vercel Deployment** | Seamlessly deploy on Vercel with one click |

## References

- [Next.js Official Docs](https://nextjs.org/docs)
- [Vercel](https://vercel.com/)
- [React.js](https://reactjs.org/)
- [Incremental Static Regeneration (ISR)](https://nextjs.org/docs/basic-features/data-fetching/incremental-static-regeneration)
- [Next.js GitHub Repo](https://github.com/vercel/next.js)

---

🚀 **That’s it!** Now go build something amazing with Next.js (and don’t forget to deploy it on Vercel for ultimate bragging rights). Happy coding! 🎉

-->

## 1. Creating a Simple Page in Next.js

Every file inside the `/pages` directory is a route automatically. Here’s the simplest page you can create:

```javascript
// pages/index.js
export default function Home() {
  return <h1>Welcome to Next.js!</h1>;
}
```

## 2. Dynamic Routing with URL Parameters

Need dynamic routes? No problem.

```javascript
// pages/post/[id].js
import { useRouter } from 'next/router';

export default function Post() {
  const router = useRouter();
  const { id } = router.query;
  return <h1>Post ID: {id}</h1>;
}
```

## 3. Server-Side Rendering (SSR)

Need to fetch data **on each request**? Use `getServerSideProps`.

```javascript
// pages/ssr-example.js
export async function getServerSideProps() {
  const res = await fetch('https://jsonplaceholder.typicode.com/posts/1');
  const data = await res.json();
  return { props: { post: data } };
}

export default function SSRPage({ post }) {
  return <h1>{post.title}</h1>;
}
```

## 4. Static Site Generation (SSG)

For super-fast performance, pre-render pages at build time.

```javascript
// pages/ssg-example.js
export async function getStaticProps() {
  const res = await fetch('https://jsonplaceholder.typicode.com/posts/1');
  const data = await res.json();
  return { props: { post: data } };
}

export default function SSGPage({ post }) {
  return <h1>{post.title}</h1>;
}
```

## 5. API Routes (Your Built-in Backend)

Want to create an API **inside** your Next.js project? Here you go:

```javascript
// pages/api/hello.js
export default function handler(req, res) {
  res.status(200).json({ message: 'Hello, API!' });
}
```

## 6. Using Middleware for Custom Logic

Middleware can intercept requests **before** they hit your pages.

```javascript
// middleware.js
import { NextResponse } from 'next/server';

export function middleware(req) {
  if (!req.cookies.authToken) {
    return NextResponse.redirect('/login');
  }
}
```

## 7. Optimized Images with Next.js

Next.js has an **amazing** `<Image />` component that auto-optimizes images.

```javascript
import Image from 'next/image';

export default function OptimizedImage() {
  return <Image src="/example.jpg" width={500} height={500} alt="Example" />;
}
```

## 8. Using Edge Functions (Next.js 12+)

Run server-side code **closer** to the user!

```javascript
// pages/api/edge-example.js
export default async function handler(req) {
  return new Response('Hello from the edge!', {
    status: 200,
  });
}
```

## 9. Incremental Static Regeneration (ISR)

Regenerate static pages **dynamically** without a full rebuild.

```javascript
// pages/isr-example.js
export async function getStaticProps() {
  const res = await fetch('https://jsonplaceholder.typicode.com/posts/1');
  const data = await res.json();
  return { props: { post: data }, revalidate: 10 };
}

export default function ISRPage({ post }) {
  return <h1>{post.title}</h1>;
}
```

## 10. Internationalized Routing (i18n)

Easily handle multiple languages **without a headache**.

```javascript
// next.config.js
module.exports = {
  i18n: {
    locales: ['en', 'es', 'fr'],
    defaultLocale: 'en',
  },
};
```

<!-- 
---

## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **Simple Pages** | Every file in `/pages` is automatically a route |
| **Dynamic Routing** | Use `[param].js` to create dynamic routes |
| **SSR** | Fetch data on every request with `getServerSideProps` |
| **SSG** | Pre-build pages with `getStaticProps` for speed |
| **API Routes** | Built-in backend API functionality inside `/pages/api/` |
| **Middleware** | Intercept requests and redirect users if needed |
| **Image Optimization** | Use the `<Image />` component for auto-optimized images |
| **Edge Functions** | Run server code closer to users for lower latency |
| **ISR** | Regenerate static pages dynamically with `revalidate` |
| **i18n** | Internationalized routing built directly into Next.js |

## References

- [Next.js Official Docs](https://nextjs.org/docs)
- [Next.js Routing](https://nextjs.org/docs/routing/introduction)
- [API Routes](https://nextjs.org/docs/api-routes/introduction)
- [Image Optimization](https://nextjs.org/docs/basic-features/image-optimization)
- [Internationalized Routing](https://nextjs.org/docs/advanced-features/i18n-routing)

---

🔥 **That’s it!** Now you’ve got all the Next.js code examples you need to build some epic web apps. Time to **code** and **deploy**. 🚀

-->

## 11. Custom `_app.js` for Global State Management

Want to persist global state across pages? Modify `_app.js`.

```javascript
// pages/_app.js
import { useState } from 'react';
import '../styles/globals.css';

function MyApp({ Component, pageProps }) {
  const [user, setUser] = useState(null);
  return <Component {...pageProps} user={user} setUser={setUser} />;
}
export default MyApp;
```

## 12. Custom `_document.js` for Modifying `<html>` and `<body>`

Use `_document.js` to modify the document structure.

```javascript
// pages/_document.js
import { Html, Head, Main, NextScript } from 'next/document';

export default function Document() {
  return (
    <Html lang="en">
      <Head>
        <link rel="stylesheet" href="/custom.css" />
      </Head>
      <body>
        <Main />
        <NextScript />
      </body>
    </Html>
  );
}
```

## 13. Fetch Data on the Client Side with `useEffect`

If you don’t need SSR, fetch data client-side instead.

```javascript
import { useEffect, useState } from 'react';

export default function ClientFetch() {
  const [data, setData] = useState(null);

  useEffect(() => {
    fetch('/api/example')
      .then((res) => res.json())
      .then((data) => setData(data));
  }, []);

  return <div>{data ? JSON.stringify(data) : 'Loading...'}</div>;
}
```

## 14. Creating Protected Routes (Authentication)

Protect pages by checking authentication status.

```javascript
// pages/protected.js
import { useRouter } from 'next/router';
import { useEffect } from 'react';

export default function ProtectedPage() {
  const router = useRouter();

  useEffect(() => {
    const token = localStorage.getItem('authToken');
    if (!token) router.push('/login');
  }, []);

  return <h1>Protected Content</h1>;
}
```

## 15. Custom Express Server with Next.js

Want more control? Use a custom server.

```javascript
// server.js
const express = require('express');
const next = require('next');
const app = next({ dev: process.env.NODE_ENV !== 'production' });
const handle = app.getRequestHandler();

app.prepare().then(() => {
  const server = express();

  server.get('*', (req, res) => {
    return handle(req, res);
  });

  server.listen(3000, () => console.log('Server running on port 3000'));
});
```

## 16. Adding SEO Meta Tags Dynamically

Boost SEO with dynamic `<meta>` tags.

```javascript
import Head from 'next/head';

export default function SEOPage() {
  return (
    <>
      <Head>
        <title>SEO in Next.js</title>
        <meta name="description" content="Learn SEO in Next.js" />
      </Head>
      <h1>SEO Page</h1>
    </>
  );
}
```

## 17. Styling with Tailwind CSS

Next.js works great with Tailwind.

```javascript
// Install Tailwind: npm install tailwindcss postcss autoprefixer
// Add Tailwind to globals.css
import 'tailwindcss/tailwind.css';

export default function TailwindExample() {
  return <h1 className="text-4xl font-bold text-blue-500">Hello Tailwind!</h1>;
}
```

## 18. Using `next/script` for Third-Party Scripts

Load third-party scripts efficiently.

```javascript
import Script from 'next/script';

export default function ExternalScript() {
  return (
    <>
      <h1>Using Next.js Script</h1>
      <Script src="https://example.com/script.js" strategy="lazyOnload" />
    </>
  );
}
```

## 19. Prefetching Links for Performance

Improve navigation speed with prefetching.

```javascript
import Link from 'next/link';

export default function PrefetchExample() {
  return (
    <Link href="/about" prefetch>
      Go to About Page
    </Link>
  );
}
```

## 20. Handling 404 Pages

Customize your 404 error page.

```javascript
// pages/404.js
export default function Custom404() {
  return <h1>Oops! Page Not Found 😢</h1>;
}
```

<!-- 
---

## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **Custom `_app.js`** | Manage global state and styles |
| **Custom `_document.js`** | Modify the document structure |
| **Client-Side Fetching** | Use `useEffect` to fetch data on the client |
| **Protected Routes** | Redirect users if not authenticated |
| **Custom Server** | Use Express with Next.js for more control |
| **SEO Optimization** | Use `<Head>` to add meta tags |
| **Tailwind CSS** | Style Next.js apps with Tailwind |
| **Third-Party Scripts** | Use `next/script` for optimized script loading |
| **Prefetching** | Improve navigation speed with link prefetching |
| **Custom 404 Page** | Create a unique 404 error page |

## References

- [Next.js Official Docs](https://nextjs.org/docs)
- [Next.js SEO](https://nextjs.org/docs/advanced-features/seo)
- [Tailwind CSS](https://tailwindcss.com/)
- [Next.js Custom Server](https://nextjs.org/docs/advanced-features/custom-server)
- [Error Handling](https://nextjs.org/docs/advanced-features/custom-error-page)

---

🚀 **And that’s 10 more!** Now you’re **unstoppable** with Next.js. Go build something **amazing**! 🎉
-->

## 21. Using Middleware for Custom Headers

Want to add security headers or modify responses dynamically? Middleware is your best friend.

```javascript
// middleware.js
import { NextResponse } from 'next/server';

export function middleware(req) {
  const res = NextResponse.next();
  res.headers.set('X-Custom-Header', 'MyHeaderValue');
  return res;
}
```

## 22. Prefetching API Requests with SWR

For blazing-fast UI updates, use **SWR** for API fetching with caching and revalidation.

```javascript
import useSWR from 'swr';

const fetcher = (url) => fetch(url).then((res) => res.json());

export default function Dashboard() {
  const { data, error } = useSWR('/api/data', fetcher);

  if (error) return <div>Failed to load</div>;
  if (!data) return <div>Loading...</div>;

  return <h1>Welcome, {data.user}</h1>;
}
```

## 23. Generating Dynamic Sitemaps

Make sure search engines love your Next.js app with a **dynamic sitemap**.

```javascript
// pages/sitemap.xml.js
import { getServerSideProps } from 'next';

export default function Sitemap() {}

export async function getServerSideProps({ res }) {
  const sitemap = `<?xml version="1.0" encoding="UTF-8"?>
    <urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
      <url><loc>https://example.com/</loc></url>
    </urlset>`;
  res.setHeader('Content-Type', 'text/xml');
  res.write(sitemap);
  res.end();
  return { props: {} };
}
```

## 24. Optimizing Fonts with `next/font`

Improve performance by loading Google Fonts **the Next.js way**.

```javascript
import { Inter } from 'next/font/google';

const inter = Inter({ subsets: ['latin'] });

export default function Home() {
  return <h1 className={inter.className}>Optimized Fonts!</h1>;
}
```

## 25. Caching API Responses with `revalidate`

Want data that refreshes every 10 seconds? **Incremental Static Regeneration (ISR)** has you covered.

```javascript
export async function getStaticProps() {
  const res = await fetch('https://jsonplaceholder.typicode.com/posts/1');
  const data = await res.json();

  return {
    props: { post: data },
    revalidate: 10,
  };
}
```

## 26. Creating a Custom 500 Error Page

Make sure your **error pages** are just as polished as the rest of your app.

```javascript
// pages/500.js
export default function Custom500() {
  return <h1>Something went wrong! 🚨</h1>;
}
```

## 27. Enabling React Strict Mode

For better debugging and catching potential issues, enable **strict mode** in `next.config.js`.

```javascript
// next.config.js
module.exports = {
  reactStrictMode: true,
};
```

## 28. Lazy Loading Components for Performance Boosts

Reduce initial load times by **dynamically importing** components.

```javascript
import dynamic from 'next/dynamic';

const HeavyComponent = dynamic(() => import('../components/HeavyComponent'), {
  ssr: false,
});

export default function Page() {
  return <HeavyComponent />;
}
```

## 29. Using WebSockets in Next.js API Routes

Need **real-time updates**? WebSockets work inside API routes!

```javascript
// pages/api/socket.js
import { Server } from 'socket.io';

export default function handler(req, res) {
  if (!res.socket.server.io) {
    const io = new Server(res.socket.server);
    res.socket.server.io = io;
  }
  res.end();
}
```

## 30. Customizing the Build Output Directory

Change the default `.next` folder to something custom in `next.config.js`.

```javascript
// next.config.js
module.exports = {
  distDir: 'build',
};
```

***

<!-- 
## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **Middleware** | Modify requests and responses dynamically |
| **SWR** | Fetch data with caching and automatic revalidation |
| **Dynamic Sitemaps** | Generate sitemaps at runtime for SEO |
| **Optimized Fonts** | Load Google Fonts efficiently using `next/font` |
| **ISR Caching** | Refresh static data without rebuilding the whole site |
| **Custom 500 Page** | Create a better experience for server errors |
| **Strict Mode** | Catch potential issues in development |
| **Lazy Loading** | Dynamically load components to speed up initial load |
| **WebSockets** | Implement real-time updates in API routes |
| **Custom Build Directory** | Change the default output folder |

## References

- [Next.js Official Docs](https://nextjs.org/docs)
- [SWR - Stale While Revalidate](https://swr.vercel.app/)
- [WebSockets in Next.js](https://socket.io/)
- [Custom Next.js Middleware](https://nextjs.org/docs/advanced-features/middleware)
- [Incremental Static Regeneration (ISR)](https://nextjs.org/docs/basic-features/data-fetching/incremental-static-regeneration)

---

🚀 **And there you have it!** Ten more advanced Next.js tricks to keep your app **fast, efficient, and feature-packed**. What’s next? Go build something epic! 🎉
-->

## 31. Advanced Dynamic Routing with Catch-All Routes

Need a route that captures multiple segments? **Catch-all routes** got you covered.

```javascript
// pages/blog/[...slug].js
import { useRouter } from 'next/router';

export default function BlogPost() {
  const router = useRouter();
  const { slug } = router.query;

  return <h1>Blog Post: {slug?.join(' / ')}</h1>;
}
```

## 32. Using Edge Functions for Lightning-Fast Requests

Deploy **serverless functions at the edge** for near-instant responses.

```javascript
// pages/api/edge.js
export const config = { runtime: 'edge' };

export default async function handler(req) {
  return new Response(JSON.stringify({ message: 'Hello from the edge!' }), {
    headers: { 'Content-Type': 'application/json' },
  });
}
```

## 33. Optimizing API Responses with `Cache-Control`

Use **cache headers** to boost performance and reduce unnecessary requests.

```javascript
// pages/api/cached-data.js
export default function handler(req, res) {
  res.setHeader('Cache-Control', 's-maxage=60, stale-while-revalidate');
  res.json({ message: 'Cached response' });
}
```

## 34. Implementing Role-Based Access Control (RBAC)

Secure pages based on user roles dynamically.

```javascript
// components/ProtectedRoute.js
import { useRouter } from 'next/router';
import { useEffect } from 'react';

export default function ProtectedRoute({ user, allowedRoles, children }) {
  const router = useRouter();
  useEffect(() => {
    if (!user || !allowedRoles.includes(user.role)) {
      router.push('/unauthorized');
    }
  }, [user, allowedRoles, router]);

  return <>{children}</>;
}
```

## 35. Custom Webpack Configuration

Extend Next.js with a custom **Webpack config**.

```javascript
// next.config.js
module.exports = {
  webpack: (config) => {
    config.module.rules.push({
      test: /\.md$/, use: 'raw-loader',
    });
    return config;
  },
};
```

## 36. Prefetching API Data for Better UX

Preload API calls before users even navigate to a page.

```javascript
// pages/index.js
import Link from 'next/link';
import { useEffect } from 'react';

export default function Home() {
  useEffect(() => {
    fetch('/api/data'); // Prefetch API data
  }, []);

  return <Link href="/dashboard">Go to Dashboard</Link>;
}
```

## 37. Custom ESLint Rules in Next.js

Force best practices with **custom ESLint rules**.

```javascript
// .eslintrc.js
module.exports = {
  rules: {
    'no-console': 'warn',
    'react/no-unescaped-entities': 'off',
  },
};
```

## 38. Testing Next.js Apps with Jest and React Testing Library

Unit test your Next.js components like a pro.

```javascript
// __tests__/index.test.js
import { render, screen } from '@testing-library/react';
import Home from '../pages/index';

test('renders welcome message', () => {
  render(<Home />);
  expect(screen.getByText(/Welcome to Next.js!/i)).toBeInTheDocument();
});
```

## 39. Implementing Webhooks for Real-Time Events

Trigger actions based on external events.

```javascript
// pages/api/webhook.js
export default async function handler(req, res) {
  if (req.method === 'POST') {
    console.log('Webhook received:', req.body);
    res.status(200).send('OK');
  } else {
    res.status(405).send('Method Not Allowed');
  }
}
```

## 40. Handling Background Jobs with Serverless Functions

Process long-running tasks asynchronously.

```javascript
// pages/api/background-task.js
export default async function handler(req, res) {
  setTimeout(() => console.log('Background job done!'), 5000);
  res.status(200).json({ status: 'Job started' });
}
```

***

<!-- 
## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **Catch-All Routes** | Dynamic paths that capture multiple URL segments |
| **Edge Functions** | Deploy serverless functions closer to users for speed |
| **Cache-Control** | Optimize API responses with caching strategies |
| **RBAC (Role-Based Access)** | Restrict access based on user roles |
| **Custom Webpack Config** | Extend Next.js with additional Webpack rules |
| **API Prefetching** | Load API data before navigation for better UX |
| **Custom ESLint Rules** | Enforce best practices in your Next.js project |
| **Jest & Testing Library** | Unit test Next.js components effectively |
| **Webhooks** | React to external events dynamically |
| **Background Jobs** | Run async tasks with serverless functions |

## References

- [Next.js Official Docs](https://nextjs.org/docs)
- [Next.js Middleware](https://nextjs.org/docs/advanced-features/middleware)
- [Testing Next.js Apps](https://nextjs.org/docs/testing)
- [Serverless Background Jobs](https://vercel.com/docs/functions/serverless-functions)
- [Edge Functions in Next.js](https://vercel.com/docs/functions/edge-functions)

---

🎯 
-->
