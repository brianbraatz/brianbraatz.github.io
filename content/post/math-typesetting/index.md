---
title: Math Typesetting
description: Math typesetting using KaTeX
date: 2023-08-24 00:00:00+0000
math: true
lastmod: 2025-01-27T11:00:51.222Z
---
Stack has built-in support for math typesetting using [KaTeX](https://katex.org/).

**It's not enabled by default side-wide,** but you can enable it for individual posts by adding `math: true` to the front matter. Or you can enable it side-wide by adding `math = true` to the `params.article` section in `config.toml`.

## Inline math

This is an inline mathematical expression: $\varphi = \dfrac{1+\sqrt5}{2}= 1.6180339887…$

```markdown
$\varphi = \dfrac{1+\sqrt5}{2}= 1.6180339887…$
```

## Block math

<div>
$$
    \varphi = 1+\frac{1} {1+\frac{1} {1+\frac{1} {1+\cdots} } } 
$$
</div>

```markdown
$$
    \varphi = 1+\frac{1} {1+\frac{1} {1+\frac{1} {1+\cdots} } } 
$$
```

<div>
$$
    f(x) = \int_{-\infty}^\infty\hat f(\xi)\,e^{2 \pi i \xi x}\,d\xi
$$
</div>

```markdown
$$
    f(x) = \int_{-\infty}^\infty\hat f(\xi)\,e^{2 \pi i \xi x}\,d\xi
$$
```
