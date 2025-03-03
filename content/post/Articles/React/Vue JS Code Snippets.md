---
title: Vue.JS  Code Snippet Collection
description: Collected Bits of Vue JS Wisdom
slug: vue-code-snippets
date: 2020-12-15
image: post/Articles/IMAGES/vuejs.png
categories:
  - VueJs
  - Web Development
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
weight: 700
lastmod: 2025-03-03T00:15:14.256Z
---
## A Brief History of Vue.js (How It All Began)

Vue.js was created by **Evan You** in 2014.

He wanted a framework that combined **React’s reactivity** with **Angular’s templating**—but without the complexity.

Vue.js started as a **personal project** but quickly became one of the most loved frontend frameworks.

Here’s how Vue.js has evolved over time:

| Version | Release Date | Notable Features                           |
| ------- | ------------ | ------------------------------------------ |
| 0.6     | 2014-02-01   | Initial release, basic reactivity          |
| 1.0     | 2015-10-27   | Directives, Components, Vue Router support |
| 2.0     | 2016-09-30   | Virtual DOM, Improved Reactivity, Vuex     |
| 2.5     | 2017-10-13   | TypeScript support, Better SSR             |
| 3.0     | 2020-09-18   | Composition API, Fragments, Teleport       |
| 3.2     | 2021-08-05   | Performance improvements, `script setup`   |

***

## 1. The Composition API (Vue 3)

The **Composition API** makes it easier to organize logic in Vue components.

```javascript
import { ref } from 'vue';

export default {
  setup() {
    const count = ref(0);
    return { count };
  },
};
```

## 2. Vue Directives (`v-bind`, `v-if`, `v-for`)

Directives make Vue **declarative and expressive**.

```html
<p v-if="user">Hello, {{ user.name }}!</p>
<ul>
  <li v-for="item in items" :key="item.id">{{ item.name }}</li>
</ul>
```

## 3. Two-Way Data Binding (`v-model`)

No need for complicated state management—Vue does it natively.

```html
<input v-model="message" />
<p>{{ message }}</p>
```

## 4. Vue Components (Reusable UI Blocks)

Vue encourages breaking UIs into small, **reusable** components.

```javascript
// Button.vue
<template>
  <button @click="onClick">Click me</button>
</template>

<script>
export default {
  methods: {
    onClick() {
      alert('Button clicked!');
    }
  }
};
</script>
```

## 5. Vue Router (Navigation Made Easy)

Vue Router enables **SPA navigation** without full-page reloads.

```javascript
// router.js
import { createRouter, createWebHistory } from 'vue-router';
import Home from './pages/Home.vue';
import About from './pages/About.vue';

const routes = [
  { path: '/', component: Home },
  { path: '/about', component: About },
];

export default createRouter({
  history: createWebHistory(),
  routes,
});
```

## 6. Vuex (State Management)

For complex applications, Vuex keeps shared state **organized**.

```javascript
import { createStore } from 'vuex';

export default createStore({
  state: { count: 0 },
  mutations: {
    increment(state) {
      state.count++;
    },
  },
});
```

## 7. Lifecycle Hooks (`mounted`, `updated`, `beforeUnmount`)

Execute code at different stages of a component’s life.

```javascript
export default {
  mounted() {
    console.log('Component is mounted!');
  }
};
```

## 8. Vue’s Teleport API (Move Elements Anywhere)

Teleport lets you render UI **outside of the root app container**.

```html
<teleport to="body">
  <div class="modal">Hello from teleport!</div>
</teleport>
```

## 9. Scoped Styles (Avoid CSS Conflicts)

Keep styles isolated to a specific component.

```vue
<style scoped>
h1 {
  color: red;
}
</style>
```

## 10. Using `watch` to React to Data Changes

Run logic when a reactive value **changes**.

```javascript
import { ref, watch } from 'vue';

const count = ref(0);
watch(count, (newVal, oldVal) => {
  console.log(`Count changed from ${oldVal} to ${newVal}`);
});
```

***

<!-- 

## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **Composition API** | Organize logic better in Vue 3 |
| **Directives** | Enhance templates (`v-if`, `v-for`, `v-bind`) |
| **Two-Way Binding** | Synchronize data using `v-model` |
| **Components** | Reusable, modular UI elements |
| **Vue Router** | Client-side navigation made simple |
| **Vuex** | Centralized state management |
| **Lifecycle Hooks** | Execute code at different stages of a component’s lifecycle |
| **Teleport API** | Render components outside of the Vue root |
| **Scoped Styles** | Keep styles limited to components |
| **Watchers** | React to state changes dynamically |

## References

- [Vue.js Official Docs](https://vuejs.org/guide/introduction.html)
- [Vue Router](https://router.vuejs.org/)
- [Vuex](https://vuex.vuejs.org/)
- [Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
- [Teleport API](https://vuejs.org/guide/built-ins/teleport.html)

---

-->

## 11. Using Computed Properties for Performance Optimization

Computed properties **cache results** and recalculate only when dependencies change.

```javascript
export default {
  data() {
    return { firstName: 'John', lastName: 'Doe' };
  },
  computed: {
    fullName() {
      return `${this.firstName} ${this.lastName}`;
    }
  }
};
```

## 12. Using Watchers to React to Data Changes

Watchers allow you to run logic **whenever a data property changes**.

```javascript
export default {
  data() {
    return { count: 0 };
  },
  watch: {
    count(newValue, oldValue) {
      console.log(`Count changed from ${oldValue} to ${newValue}`);
    }
  }
};
```

## 13. Conditional Rendering with `v-show`

Use `v-show` instead of `v-if` when **toggling visibility without removing elements from the DOM**.

```html
<p v-show="isVisible">I am visible!</p>
<button @click="isVisible = !isVisible">Toggle</button>
```

## 14. Using Slots for Reusable Components

Slots allow you to pass **custom content** into components.

```vue
<!-- Parent.vue -->
<Child>
  <template v-slot:header>
    <h1>Custom Header</h1>
  </template>
</Child>

<!-- Child.vue -->
<template>
  <slot name="header"></slot>
  <p>Default content</p>
</template>
```

## 15. Emitting Custom Events

Child components can **emit events** to their parent.

```vue
<!-- Child.vue -->
<template>
  <button @click="$emit('customEvent', 'Hello from Child!')">Click me</button>
</template>

<!-- Parent.vue -->
<Child @customEvent="handleEvent" />

<script>
export default {
  methods: {
    handleEvent(msg) {
      console.log(msg);
    }
  }
};
</script>
```

## 16. Using Key Modifiers in Event Handling

Key modifiers simplify keyboard event handling.

```html
<input @keyup.enter="submitForm" placeholder="Press Enter to submit" />
```

## 17. Transition Effects with Vue

Vue makes **animations and transitions** easy.

```vue
<template>
  <transition name="fade">
    <p v-if="isVisible">Hello, Vue!</p>
  </transition>
</template>

<style>
.fade-enter-active, .fade-leave-active {
  transition: opacity 0.5s;
}
.fade-enter, .fade-leave-to {
  opacity: 0;
}
</style>
```

## 18. Using Filters for Data Formatting (Vue 2 Only)

Filters allow **quick formatting** of text.

```vue
<template>
  <p>{{ price | currency }}</p>
</template>

<script>
export default {
  filters: {
    currency(value) {
      return `$${value.toFixed(2)}`;
    }
  }
};
</script>
```

## 19. Fetching API Data with `fetch` and Vue’s Lifecycle Hooks

Make API requests **on component mount**.

```javascript
export default {
  data() {
    return { posts: [] };
  },
  async mounted() {
    const res = await fetch('https://jsonplaceholder.typicode.com/posts');
    this.posts = await res.json();
  }
};
```

## 20. Creating Custom Directives

Custom directives let you **extend Vue’s functionality**.

```javascript
// Register a global directive
Vue.directive('focus', {
  inserted(el) {
    el.focus();
  }
});

<!-- Usage -->
<input v-focus />
```

***

<!-- 
## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **Computed Properties** | Optimize performance by caching calculations |
| **Watchers** | Run functions when data changes |
| **`v-show` vs `v-if`** | `v-show` toggles visibility, `v-if` removes elements |
| **Slots** | Pass dynamic content into components |
| **Event Emitting** | Child components can trigger events in parents |
| **Key Modifiers** | Simplify keyboard interactions |
| **Transitions** | Add animations easily |
| **Filters (Vue 2)** | Format data within templates |
| **API Fetching** | Load data on component mount |
| **Custom Directives** | Extend Vue’s built-in behavior |

## References

- [Vue.js Official Docs](https://vuejs.org/guide/introduction.html)
- [Vue Directives](https://vuejs.org/guide/essentials/template-syntax.html#directives)
- [Vue Transition Effects](https://vuejs.org/guide/built-ins/transition.html)
- [Event Handling](https://vuejs.org/guide/essentials/event-handling.html)
- [Lifecycle Hooks](https://vuejs.org/guide/essentials/lifecycle.html)

---
-->

## 21. Dynamic Components with `:is` Attribute

Easily switch between components dynamically.

```vue
<template>
  <component :is="currentComponent"></component>
  <button @click="toggleComponent">Toggle Component</button>
</template>

<script>
import ComponentA from './ComponentA.vue';
import ComponentB from './ComponentB.vue';

export default {
  data() {
    return { currentComponent: 'ComponentA' };
  },
  components: { ComponentA, ComponentB },
  methods: {
    toggleComponent() {
      this.currentComponent = this.currentComponent === 'ComponentA' ? 'ComponentB' : 'ComponentA';
    }
  }
};
</script>
```

## 22. Using `Pinia` for State Management (Vuex Alternative)

Pinia is a modern and lightweight **state management solution** for Vue.

```javascript
// store.js
import { defineStore } from 'pinia';

export const useCounterStore = defineStore('counter', {
  state: () => ({ count: 0 }),
  actions: {
    increment() {
      this.count++;
    }
  }
});
```

## 23. Using Vue Teleport to Render Outside the Root Component

Teleport allows rendering **outside** of the Vue root node.

```vue
<template>
  <teleport to="body">
    <div class="modal">This is a modal rendered outside the app root</div>
  </teleport>
</template>
```

## 24. Creating a Debounced Search Input

Prevent excessive API calls by **debouncing** user input.

```vue
<template>
  <input v-model="searchQuery" @input="debouncedSearch" />
</template>

<script>
import { ref } from 'vue';
import { debounce } from 'lodash';

export default {
  setup() {
    const searchQuery = ref('');
    const debouncedSearch = debounce(() => {
      console.log(`Searching for: ${searchQuery.value}`);
    }, 500);

    return { searchQuery, debouncedSearch };
  }
};
</script>
```

## 25. Using Vue Composables for Reusable Logic

Extract **reusable logic** into a **composable** function.

```javascript
// useMouse.js
import { ref, onMounted, onUnmounted } from 'vue';

export function useMouse() {
  const x = ref(0);
  const y = ref(0);

  function updateMouse(event) {
    x.value = event.clientX;
    y.value = event.clientY;
  }

  onMounted(() => window.addEventListener('mousemove', updateMouse));
  onUnmounted(() => window.removeEventListener('mousemove', updateMouse));

  return { x, y };
}
```

## 26. Server-Side Rendering (SSR) with Nuxt.js

Improve **SEO and performance** with **SSR**.

```javascript
// pages/index.vue
<template>
  <h1>{{ message }}</h1>
</template>

<script>
export default {
  async asyncData() {
    return { message: 'Hello from SSR!' };
  }
};
</script>
```

## 27. Lazy Loading Components for Performance Optimization

Reduce initial bundle size by **lazy-loading** components.

```javascript
import { defineAsyncComponent } from 'vue';

export default {
  components: {
    LazyComponent: defineAsyncComponent(() => import('./LazyComponent.vue'))
  }
};
```

## 28. Using `nextTick` to Wait for DOM Updates

Ensure Vue updates the DOM before running logic.

```javascript
import { nextTick, ref } from 'vue';

export default {
  setup() {
    const count = ref(0);

    async function increment() {
      count.value++;
      await nextTick();
      console.log('DOM updated!', count.value);
    }

    return { count, increment };
  }
};
```

## 29. Using Vue’s Built-in `provide/inject` for Dependency Injection

Share state **without prop drilling**.

```javascript
// Parent.vue
<template>
  <Child />
</template>

<script>
import { provide, ref } from 'vue';
import Child from './Child.vue';

export default {
  setup() {
    const sharedData = ref('Hello from parent');
    provide('sharedData', sharedData);
  },
  components: { Child }
};
</script>
```

```javascript
// Child.vue
<template>
  <p>{{ sharedData }}</p>
</template>

<script>
import { inject } from 'vue';

export default {
  setup() {
    const sharedData = inject('sharedData');
    return { sharedData };
  }
};
</script>
```

## 30. Using Render Functions for Dynamic UI Generation

Vue allows you to **dynamically generate templates** with render functions.

```javascript
export default {
  render() {
    return h('button', {
      onClick: () => alert('Clicked!')
    }, 'Click Me');
  }
};
```

***

<!-- 
## Key Ideas

| Key Concept | Explanation |
|-------------|------------|
| **Dynamic Components** | Switch components dynamically using `:is` |
| **Pinia State Management** | Vuex alternative for easier state handling |
| **Vue Teleport** | Render content outside the main app structure |
| **Debounced Search** | Optimize API calls with debounce |
| **Composable Functions** | Reuse logic across components |
| **SSR with Nuxt.js** | Improve SEO with Server-Side Rendering |
| **Lazy Loading Components** | Reduce initial load times with dynamic imports |
| **`nextTick`** | Ensure Vue updates the DOM before running logic |
| **Provide/Inject API** | Share state without prop drilling |
| **Render Functions** | Programmatically create UI elements |

## References

- [Vue.js Official Docs](https://vuejs.org/guide/introduction.html)
- [Pinia Docs](https://pinia.vuejs.org/)
- [Nuxt.js](https://nuxt.com/)
- [Vue’s Provide/Inject API](https://vuejs.org/guide/components/provide-inject.html)

---

-->

## 31. Creating a Custom Directive for Lazy Loading Images

Improve performance by lazy-loading images only when they are visible.

```javascript
// directives/lazyLoad.js
export default {
  beforeMount(el, binding) {
    const loadImage = () => {
      el.src = binding.value;
    };
    const observer = new IntersectionObserver(([entry]) => {
      if (entry.isIntersecting) {
        loadImage();
        observer.unobserve(el);
      }
    });
    observer.observe(el);
  }
};
```

```vue
<!-- Usage -->
<template>
  <img v-lazy-load="imageSrc" alt="Lazy loaded image" />
</template>

<script>
import LazyLoadDirective from '@/directives/lazyLoad';

export default {
  directives: { lazyLoad: LazyLoadDirective },
  data() {
    return { imageSrc: 'https://example.com/image.jpg' };
  }
};
</script>
```

## 32. Using Web Workers in Vue for Heavy Computation

Offload **heavy computations** to a web worker to keep the UI responsive.

```javascript
// worker.js
self.onmessage = function (e) {
  const result = e.data.num * 2;
  self.postMessage(result);
};
```

```vue
<template>
  <div>
    <button @click="runWorker">Compute</button>
    <p>Result: {{ result }}</p>
  </div>
</template>

<script>
export default {
  data() {
    return { result: null };
  },
  methods: {
    runWorker() {
      const worker = new Worker(new URL('@/worker.js', import.meta.url));
      worker.postMessage({ num: 10 });
      worker.onmessage = (e) => {
        this.result = e.data;
      };
    }
  }
};
</script>
```

## 33. Dynamic Form Handling with Vue

Generate and handle dynamic forms efficiently.

```vue
<template>
  <form @submit.prevent="submitForm">
    <div v-for="(field, index) in formFields" :key="index">
      <label :for="field.name">{{ field.label }}</label>
      <input v-model="formData[field.name]" :type="field.type" />
    </div>
    <button type="submit">Submit</button>
  </form>
</template>

<script>
export default {
  data() {
    return {
      formFields: [
        { name: 'username', label: 'Username', type: 'text' },
        { name: 'email', label: 'Email', type: 'email' }
      ],
      formData: {}
    };
  },
  methods: {
    submitForm() {
      console.log(this.formData);
    }
  }
};
</script>
```

## 34. Using Async Components for Faster Load Times

Dynamically import Vue components to improve **performance**.

```javascript
import { defineAsyncComponent } from 'vue';

export default {
  components: {
    HeavyComponent: defineAsyncComponent(() => import('@/components/HeavyComponent.vue'))
  }
};
```

## 35. Implementing Route Authentication Guards

Secure routes using Vue Router guards.

```javascript
// router.js
router.beforeEach((to, from, next) => {
  const isAuthenticated = localStorage.getItem('auth');
  if (to.meta.requiresAuth && !isAuthenticated) {
    next('/login');
  } else {
    next();
  }
});
```

## 36. Creating a Throttled Event Listener

Improve **performance** by throttling event listeners.

```javascript
import { throttle } from 'lodash';

export default {
  methods: {
    handleScroll: throttle(() => {
      console.log('Scrolled!');
    }, 200)
  },
  mounted() {
    window.addEventListener('scroll', this.handleScroll);
  },
  beforeUnmount() {
    window.removeEventListener('scroll', this.handleScroll);
  }
};
```

## 37. Encrypting and Decrypting Data in Vue.js

Use **AES encryption** to secure user data.

```javascript
import CryptoJS from 'crypto-js';

const secretKey = 'my-secret-key';

export function encrypt(data) {
  return CryptoJS.AES.encrypt(data, secretKey).toString();
}

export function decrypt(ciphertext) {
  return CryptoJS.AES.decrypt(ciphertext, secretKey).toString(CryptoJS.enc.Utf8);
}
```

## 38. Using Vue to Detect Dark Mode Preferences

Automatically detect and switch themes based on system preferences.

```javascript
export default {
  data() {
    return { isDarkMode: window.matchMedia('(prefers-color-scheme: dark)').matches };
  },
  watch: {
    isDarkMode(newValue) {
      document.body.classList.toggle('dark-mode', newValue);
    }
  }
};
```

## 39. Using Vue with GraphQL

Fetch data using **GraphQL** with Vue.

```javascript
import { useQuery } from '@vue/apollo-composable';
import gql from 'graphql-tag';

const GET_USERS = gql`{
  users {
    id
    name
  }
}`;

export default {
  setup() {
    const { result, loading } = useQuery(GET_USERS);
    return { result, loading };
  }
};
```

## 40. Implementing File Upload in Vue

Handle **file uploads** in Vue components.

```vue
<template>
  <input type="file" @change="uploadFile" />
</template>

<script>
export default {
  methods: {
    uploadFile(event) {
      const file = event.target.files[0];
      const formData = new FormData();
      formData.append('file', file);
      fetch('/upload', { method: 'POST', body: formData });
    }
  }
};
</script>
```

***

## References

* [Vue.js Official Docs](https://vuejs.org/guide/introduction.html)
* [Vue Router](https://router.vuejs.org/)
* [Vuex](https://vuex.vuejs.org/)
* [Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
* [Teleport API](https://vuejs.org/guide/built-ins/teleport.html)
