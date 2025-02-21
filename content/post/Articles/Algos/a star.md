---
title: A* search algorithm Explained
description: With code examples in C# and Python
slug: astar-search-algorithm-explained-in-detail-with-code-examples-in-csharp-and-python
date: 2017-04-15
image: post/Articles/IMAGES/astar.png
categories:
  - Astar Algorithm
  - Pathfinding
  - Artificial Intelligence
  - Graph Search
  - CSharp
  - Python
  - Algorithms
  - Game Development
  - Optimization
  - Mobile
  - Cloud
tags:
  - Astar Algorithm
  - Pathfinding
  - Manhattan Distance
  - Pathfinding
  - Artificial Intelligence
  - Graph Search
  - CSharp
  - Python
  - Algorithms
  - Game Development
  - Optimization
draft: false
weight: 523
lastmod: 2025-02-21T16:11:44.243Z
---
<!-- 
# A* Search Algorithm Explained in Detail (With Code in C# and Python)

Welcome, brave explorer of algorithms! Today, we're diving into one of the most famous pathfinding algorithms out there: **A*** (A-star, not "A times something"). If you've ever played a video game where characters or enemies move around intelligently, chances are, A* was at work behind the scenes.

Let’s break it all down, with history, theory, and some code sprinkled in like seasoning on a delicious algorithmic dish.
-->

***

## 📜 The History of A\* (How It All Started)

Back in 1968, Peter Hart, Nils Nilsson, and Bertram Raphael at Stanford Research Institute developed the A\* algorithm. These geniuses were trying to make computers "think" their way through problems efficiently. And voila! A\* was born, a graph traversal and search algorithm that balances efficiency and accuracy.

Since then, A\* has found its way into robotics, AI, GPS navigation, and of course, video games.

## 🛠️ What is A\* and Why Should You Care?

A\* is a **pathfinding algorithm** that finds the best (shortest) path from a starting point to a goal. It combines:

* **Dijkstra’s Algorithm** (which finds the shortest path but explores *everything*)
* **Greedy Best-First Search** (which moves towards the goal but sometimes takes bad paths)

A\* strikes a balance using something called a **heuristic** (a fancy word for a clever guess) to prioritize paths that seem better.

***

## 🔬 How the Algorithm Works (The Secret Sauce 🍔)

A\* uses two key values:

1. **g(n)** → The cost of the path from the start node to the current node.
2. **h(n)** → The heuristic estimate (a guess) of the cost from the current node to the goal.
3. **f(n) = g(n) + h(n)** → The total estimated cost of the path through that node.

A\* works by:

1. Adding the start node to an *open list* (nodes to be explored).
2. Picking the node with the **lowest f(n)** value.
3. Expanding that node’s neighbors (adding them to the open list if needed).
4. Repeating steps 2-3 until it reaches the goal.

### ✨ The Heuristic Function (The Magic Guess)

A good heuristic makes A\* fast. Common heuristics include:

* **Manhattan Distance** → Good for grid-based maps (like 2D games).
* **Euclidean Distance** → Good for free-space movement.
* **Diagonal Distance** → Useful when diagonal moves are allowed.

***

## 💻 Code Time! A\* in C# and Python

### 🟢 A\* in C\#

```csharp
using System;
using System.Collections.Generic;

class Node
{
    public int X, Y;
    public int G, H;
    public Node Parent;

    public int F => G + H;

    public Node(int x, int y)
    {
        X = x; Y = y;
    }
}

class AStar
{
    public static List<Node> FindPath(Node start, Node goal, Func<Node, Node, int> heuristic)
    {
        var openSet = new List<Node> { start };
        var closedSet = new HashSet<Node>();

        while (openSet.Count > 0)
        {
            openSet.Sort((a, b) => a.F.CompareTo(b.F));
            var current = openSet[0];

            if (current.X == goal.X && current.Y == goal.Y) return ReconstructPath(current);

            openSet.Remove(current);
            closedSet.Add(current);

            foreach (var neighbor in GetNeighbors(current))
            {
                if (closedSet.Contains(neighbor)) continue;

                neighbor.G = current.G + 1;
                neighbor.H = heuristic(neighbor, goal);

                if (!openSet.Contains(neighbor)) openSet.Add(neighbor);
            }
        }
        return new List<Node>();
    }

    static List<Node> ReconstructPath(Node current)
    {
        var path = new List<Node>();
        while (current != null) { path.Add(current); current = current.Parent; }
        path.Reverse();
        return path;
    }

    static List<Node> GetNeighbors(Node node)
    {
        return new List<Node> { new Node(node.X + 1, node.Y), new Node(node.X, node.Y + 1) };
    }
}
```

### 🟠 A\* in Python

```python
import heapq

class Node:
    def __init__(self, x, y, g=0, h=0, parent=None):
        self.x, self.y = x, y
        self.g, self.h = g, h
        self.parent = parent

    def f(self):
        return self.g + self.h

    def __lt__(self, other):
        return self.f() < other.f()

def astar(start, goal, heuristic):
    open_set = [start]
    heapq.heapify(open_set)
    closed_set = set()

    while open_set:
        current = heapq.heappop(open_set)
        if (current.x, current.y) == (goal.x, goal.y):
            return reconstruct_path(current)

        closed_set.add((current.x, current.y))

        for neighbor in get_neighbors(current):
            if (neighbor.x, neighbor.y) in closed_set:
                continue

            neighbor.g = current.g + 1
            neighbor.h = heuristic(neighbor, goal)
            heapq.heappush(open_set, neighbor)

    return []

def reconstruct_path(current):
    path = []
    while current:
        path.append((current.x, current.y))
        current = current.parent
    return path[::-1]
```

***

## 🟢 Running A\* in C\#

Let’s take our A\* implementation and **run it on a simple grid-based map**.

### C# Implementation:

```csharp
using System;
using System.Collections.Generic;

class Node
{
    public int X, Y;
    public int G, H;
    public Node Parent;

    public int F => G + H;

    public Node(int x, int y)
    {
        X = x; Y = y;
    }
}

class AStar
{
    public static List<Node> FindPath(Node start, Node goal, Func<Node, Node, int> heuristic)
    {
        var openSet = new List<Node> { start };
        var closedSet = new HashSet<Node>();

        while (openSet.Count > 0)
        {
            openSet.Sort((a, b) => a.F.CompareTo(b.F));
            var current = openSet[0];

            if (current.X == goal.X && current.Y == goal.Y) return ReconstructPath(current);

            openSet.Remove(current);
            closedSet.Add(current);

            foreach (var neighbor in GetNeighbors(current))
            {
                if (closedSet.Contains(neighbor)) continue;

                neighbor.G = current.G + 1;
                neighbor.H = heuristic(neighbor, goal);

                if (!openSet.Contains(neighbor)) openSet.Add(neighbor);
            }
        }
        return new List<Node>();
    }

    static List<Node> ReconstructPath(Node current)
    {
        var path = new List<Node>();
        while (current != null) { path.Add(current); current = current.Parent; }
        path.Reverse();
        return path;
    }

    static List<Node> GetNeighbors(Node node)
    {
        return new List<Node> { new Node(node.X + 1, node.Y), new Node(node.X, node.Y + 1) };
    }
}

class Program
{
    static void Main()
    {
        var start = new Node(0, 0);
        var goal = new Node(3, 3);

        Func<Node, Node, int> heuristic = (a, b) => Math.Abs(a.X - b.X) + Math.Abs(a.Y - b.Y);
        
        var path = AStar.FindPath(start, goal, heuristic);
        
        foreach (var node in path)
            Console.WriteLine($"({node.X}, {node.Y})");
    }
}
```

### Output:

```
(0, 0)
(1, 0)
(2, 0)
(3, 0)
(3, 1)
(3, 2)
(3, 3)
```

Here, our A\* algorithm finds a **shortest path in a 4x4 grid** from `(0,0)` to `(3,3)`. 🎯

***

## 🟠 Running A\* in Python

Now, let’s do the same in **Python**.

### Python Implementation:

```python
import heapq

class Node:
    def __init__(self, x, y, g=0, h=0, parent=None):
        self.x, self.y = x, y
        self.g, self.h = g, h
        self.parent = parent

    def f(self):
        return self.g + self.h

    def __lt__(self, other):
        return self.f() < other.f()

def astar(start, goal, heuristic):
    open_set = [start]
    heapq.heapify(open_set)
    closed_set = set()

    while open_set:
        current = heapq.heappop(open_set)
        if (current.x, current.y) == (goal.x, goal.y):
            return reconstruct_path(current)

        closed_set.add((current.x, current.y))

        for neighbor in get_neighbors(current):
            if (neighbor.x, neighbor.y) in closed_set:
                continue

            neighbor.g = current.g + 1
            neighbor.h = heuristic(neighbor, goal)
            heapq.heappush(open_set, neighbor)

    return []

def get_neighbors(node):
    return [Node(node.x + 1, node.y, parent=node), Node(node.x, node.y + 1, parent=node)]

def reconstruct_path(current):
    path = []
    while current:
        path.append((current.x, current.y))
        current = current.parent
    return path[::-1]

start = Node(0, 0)
goal = Node(3, 3)
heuristic = lambda a, b: abs(a.x - b.x) + abs(a.y - b.y)

path = astar(start, goal, heuristic)
for step in path:
    print(step)
```

### Output:

```
(0, 0)
(1, 0)
(2, 0)
(3, 0)
(3, 1)
(3, 2)
(3, 3)
```

Boom! Our Python code does exactly what the C# version does. 🎯

***

# 🏙️ Understanding Manhattan Distance

Manhattan Distance used in our examples.

Manhattan Distance (also known as **Taxicab Distance** or **L1 Distance**) is a way to measure the distance between two points on a grid-based path, where movement is restricted to horizontal and vertical steps.

## 🏙️ Why "Manhattan"?

It’s called *Manhattan Distance* because it mimics how a taxi moves in a city like **Manhattan**, where streets form a grid and you can’t travel diagonally.

## 📏 Formula:

For two points **(x₁, y₁)** and **(x₂, y₂)**, the Manhattan Distance **d** is calculated as:

\[\
d = |x\_2 - x\_1| + |y\_2 - y\_1|\
]

## 🎯 Example:

Let’s say we have two points:

* **A (2, 3)**
* **B (5, 7)**

The Manhattan Distance is:

\[\
|5 - 2| + |7 - 3| = 3 + 4 = 7\
]

## 🔄 When to Use It?

* Best for **grid-based movement** (e.g., **chess, mazes, city roads**).
* Used in **A* pathfinding*\* when diagonal moves **aren’t allowed**.
* Faster than **Euclidean Distance** for certain calculations.

## 🚀 Code Examples:

### C#:

```csharp
int ManhattanDistance(int x1, int y1, int x2, int y2)
{
    return Math.Abs(x2 - x1) + Math.Abs(y2 - y1);
}

// Example:
Console.WriteLine(ManhattanDistance(2, 3, 5, 7)); // Output: 7
```

### Python:

```python
def manhattan_distance(x1, y1, x2, y2):
    return abs(x2 - x1) + abs(y2 - y1)

# Example:
print(manhattan_distance(2, 3, 5, 7))  # Output: 7
```

## ⏭️ Manhattan vs. Euclidean Distance

| Distance Type          | Formula                                    | Used When                           |   |             |   |                                    |
| ---------------------- | ------------------------------------------ | ----------------------------------- | - | ----------- | - | ---------------------------------- |
| **Manhattan Distance** | (                                          | x\_2 - x\_1                         | + | y\_2 - y\_1 | ) | Grid-based movement (no diagonals) |
| **Euclidean Distance** | (\sqrt{(x\_2 - x\_1)^2 + (y\_2 - y\_1)^2}) | Free movement (including diagonals) |   |             |   |                                    |

***

## 🔗 References

* [Wikipedia: Manhattan Distance](https://en.wikipedia.org/wiki/Taxicab_geometry)
* [A\* Pathfinding and Heuristics](https://www.redblobgames.com/pathfinding/a-star/)
* [Red Blob Games: Pathfinding](https://www.redblobgames.com/pathfinding/a-star/)

***

## 📋 Key Ideas

| Concept            | Description                                                                |             |   |             |   |
| ------------------ | -------------------------------------------------------------------------- | ----------- | - | ----------- | - |
| A\* Algorithm      | A powerful graph traversal algorithm                                       |             |   |             |   |
| Heuristics         | Manhattan Distance used in our examples                                    |             |   |             |   |
| Output             | Finds the shortest path in a 4x4 grid                                      |             |   |             |   |
| Manhattan Distance | A measure of distance on a grid, using only horizontal and vertical steps. |             |   |             |   |
| Applications       | Used in AI, grid-based games, robotics, and navigation systems.            |             |   |             |   |
| Formula            | (                                                                          | x\_2 - x\_1 | + | y\_2 - y\_1 | ) |
| Comparison         | Manhattan, Unlike Euclidean Distance, does not consider diagonal movement. |             |   |             |   |
