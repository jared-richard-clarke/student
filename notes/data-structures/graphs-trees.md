# Graphs and Trees

## Graph

A graph is a collection of two sets `V` and `E` where `V` is a finite non-empty
set of vertices and `E` is a finite non-empty set of edges.

- Vertices are nodes in the graph.
- Two adjacent vertices are joined by edges.
- Any graph is denoted as `G = {V, E}`.

```text
        1 --- 2
        |    /|\
edge -> |   / | \ 5 <- vertex (node)
        |  /  | /
        | /   |/
        3 --- 4
```

## Tree

A tree is a special type of graph that is connected and acyclic. There is a
unique path between any two vertices and there is a single vertex called the root
that is used as the starting point for traversing the tree.

Trees can be binary or non-binary. In a binary tree, each node has at most two
children, while in a non-binary tree, each node can have any number of children.

```text
          1
       /     \ <---- edge
      /       \
     2         3 <-- node
   /  \       /  \
  /    \     /    \
 4      5   6      7
```

|                | graph                                         | tree                                                     |
| -------------- | --------------------------------------------- | -------------------------------------------------------- |
| structure      | collection of vertices/nodes and edges        | collection of nodes and edges                            |
| cycle          | cycles, loops, connected, disconnected        | connected, acyclic, root node                            |
| edges          | unlimited edges per node                      | if `n` nodes, then `n-1` edges                           |
| edge types     | directed or undirected                        | directed                                                 |
| root           | no                                            | yes                                                      |
| traversal      | breadth-first or depth-first                  | in-order, pre-order, or post-order                       |
| node relations | unlimited connections, no strict parent-child | except root, each has parent and zero or more children   |
