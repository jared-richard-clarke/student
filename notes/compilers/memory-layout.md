# Memory Layout

In principle, a CPU can use memory arbitrarily. Code and data can be scattered and
intermixed. A CPU can modify the memory of its code while it is running. It is
convention to lay memory out into logical segments — each segment a sequential address
range dedicated to a particular purpose.

## Convential Layout

```
| code | data | heap | ... | <- stack |
```

- code: the machine code of the program.
- data: the global data of the program.
- heap: the memory managed at runtime. *break* is historically the top of the heap.
- stack: records the current execution state and local variables.

Typically, the heap grows up, from lower to higher addresses, and the stack grows down,
from higher to lower addresses. In embedded systems and microcontrollers, the heap and
stack, if managed improperly, can crash into each other.

## The Heap

The OS does not control the internal organization of the heap, except to limit its
total size. Runtime software or a standard library manages the heap instead.

In a C program, the functions `malloc` and `free` allocate and release memory on the heap.
The simplest implementation of `malloc` and `free` is to treat the entire heap as one large
linked list. Each entry in the list records the state of the region, the size of the region,
and has pointers to the previous and next regions.

```c
struct chunk {
    enum { FREE, USED } state;
    int size;
    struct chunk *next;
    struct chunk *prev;
    char data[0];
};
```

## The Stack

Used to record the current state of the running program. Most CPUs have a specialized register
— the **stack pointer** — which stores the address where the next item will be pushed or popped.
Pushing an item on the stack moves the pointer to a lower number address, while popping an item
causes the pointer to move to a higher number address.

Each function call occupies a range of memory in the stack known as the **stack frame**.
When a function is called, a stack frame is pushed. When the function returns, a stack frame
is popped. Another specialized register known as the **frame pointer** or **base pointer**
indicates the beginning of the current frame.

```
--- main(f(g())) --->

main | main parameters   |
     | old frame pointer |
     | local variables   |
     | return address    |
     | ----------------- |
f    | f parameters      |
     | old frame pointer |
     | local variables   |
     | return address    |
     | ----------------- |
g    | g parameters      | <- frame pointer
     | old frame pointer |
     | local variables   | <- stack pointer
     V                   V

```

### Calling Conventions

```
=== function ===

f(10, 20)

=== stack ===

PUSH $20
PUSH $10
CALL f

| 2nd argument (20) |
| 1st argument (10) |
| return address    |
| old frame pointer | <- frame pointer
| local variables   | <- stack pointer
V                   V

=== register ===

MOVE $10 -> %R10
MOVE $20 -> %R11
CALL f

| return address    |
| old frame pointer | <- frame pointer
| 1st argument (10) |
| 2nd argument (20) |
| local variables   | <- stack pointer
V                   V
```
