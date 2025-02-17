---
title: Latext Trocls
description: todo
slug: Latext-tricks
date: 2019-11-11
image: post/Articles/IMAGES/ansible.png
categories:
  - DevOps
  - Infrastructure as Code-IAC
  - Cloud
  - Ansible
  - CI\CD
tags: 
draft: true
weight: 219
lastmod: 2025-02-17T12:52:25.143Z
---
in tex i want a box in the middle of the page. I want the box to take 30% of the width of the page, and inside the box I want a heading that is bold and has a line below it and the heading is centered. Bellow that I want a heriarchial bullet list which is left justified

tex\
Copy\
Edit\
\documentclass{article}\
\usepackage\[a4paper,margin=1in]{geometry}\
\usepackage{tcolorbox}

\begin{document}

\begin{center}\
\begin{tcolorbox}\[width=0.3\textwidth, colframe=black, colback=white, boxrule=1pt]\
\begin{center}\
\textbf{\Large My Heading} \ \hrule\
\end{center}\
\vspace{0.5em}\
\begin{itemize}\
\item Level 1\
\begin{itemize}\
\item Level 2\
\begin{itemize}\
\item Level 3\
\end{itemize}\
\end{itemize}\
\end{itemize}\
\end{tcolorbox}\
\end{center}

\end{document}\
Breakdown:\
The tcolorbox package creates the bordered box.\
width=0.3\textwidth makes the box take 30% of the page width.\
\textbf{\Large My Heading} creates a bold, large heading.\
\hrule draws a line below the heading.\
itemize is used for hierarchical bullet points.\
The whole thing is centered using \begin{center} ... \end{center}.\
This should render your box nicely in the middle of the page with a structured bullet list inside. 🚀

***

![](/post/Articles/_new8/Latex%20Tricks/Pasted%20image%2020250211152327.png)
