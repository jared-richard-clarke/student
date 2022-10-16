# Recursive Evaluation

> "It is called an Abstract Syntax Tree, and it represents the structure of the
> program based on the input entered by the user. At the leaves of this tree are
> numbers and operators - the actual data to be processed. At the branches are 
> the rules used to produce this part of the tree - the information on how to
> traverse and evaluate it."
>
> — Build Your Own LISP by Daniel Holden

```c
typedef struct mpc_ast_t {
  char* tag; // <------------------- List of rules that parsed item
  char* contents; // <-------------- Contents of item
  mpc_state_t state; // <----------- State of parser when parsing item
  int children_num; // <------------ Number of child nodes
  struct mpc_ast_t** children; // <- Array of child nodes (double pointer type)
} mpc_ast_t;
```

- Use `->` instead of `.` to access the field of a pointer struct.

## Recursion

Using recursion to count the number of nodes in an AST

```c
int number_of_nodes(mpc_ast_t* t) {
  if (t->children_num == 0) { return 1; }
  if (t->children_num >= 1) {
    int total = 1;
    for (int i = 0; i < t->children_num; i++) {
      total = total + number_of_nodes(t->children[i]);
    }
    return total;
  }
  return 0;
}
```

> "Like most things, recursive functions almost always end up following a similar
> pattern. First a base case is defined. This is the case that ends the recursion,
> such as `t->children_num == 0` in our previous example. After this the recursive
> case is defined, such as `t->children_num >= 1` in our previous example, which 
> breaks down a computation into smaller parts, and calls itself recursively to
> compute those parts, before combining them together."

> "When we evaluate our tree, just like when counting the nodes, we'll need to 
> accumulate the result."

```c
long eval(mpc_ast_t* t) {

  /* If tagged as number return it directly. */
  if (strstr(t->tag, "number")) {
    return atoi(t->contents);
  }

  /* The operator is always second child. */
  char* op = t->children[1]->contents;

  /* We store the third child in `x` */
  long x = eval(t->children[2]);

  /* Iterate the remaining children and combining. */
  int i = 3;
  while (strstr(t->children[i]->tag, "expr")) {
    x = eval_op(x, op, eval(t->children[i]));
    i++;
  }

  return x;
}
```

```c
/* Use operator string to see which operation to perform */
long eval_op(long x, char* op, long y) {
  if (strcmp(op, "+") == 0) { return x + y; }
  if (strcmp(op, "-") == 0) { return x - y; }
  if (strcmp(op, "*") == 0) { return x * y; }
  if (strcmp(op, "/") == 0) { return x / y; }
  return 0;
}
```
