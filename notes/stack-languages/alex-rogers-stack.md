# Stack-Based Virtual Machine

Originally implemented by Alex Rogers in his paper **Stack: A stack-based virtual machine for physical devices**

## Core Instructions

The instructions manipulate two stacks: the operand stack `S`, and the return address stack,
`R`. Stack sizes are denoted by `#S` and `#R` respectively.

All core instructions are encoded as a single byte with the exception of PUSH which comes in two
variants, opcodes `0x18` and `0x19` which are followed by either one or two bytes representing an 8-bit
or a 16-bit little-endian two’s complement number. The internal representation of the address and
operand stack are 32-bit integers, and ll arithmetic operations saturate, rather than overflow.

| Instruction     | Effect   | Description                             | Requires    |
| --------------- | -------- | --------------------------------------- | ----------- |
| `0x00` ADD, `+` | a b -- x | Pops `a` and `b` and pushes `a + b`     | `#S` >= `2` |
| `0x01` SUB, `-` | a b -- x | Pops `a` and `b` and pushes `a - b`     | `#S` >= `2` |
| `0x02` MUL, `*` | a b -- x | Pops `a` and `b` and pushes `a * b`     | `#S` >= `2` |
| `0x03` DIV, `/` | a b -- x | Pops `a` and `b` and pushes `a / b`     | `#S` >= `2`, `b` > `0` |
| `0x04` MOD      | a b -- x | Pops `a` and `b` and pushes `a mod b`   | `#S` >= `2`, `b` > `0` |
| `0x05` INC      | a -- x   | Pops `a` and pushes `a + 1`             | `#S` >= 1   |
| `0x06` DEC      | a -- x   | Pops `a` and pushes `a - 1`             | `#S` >= 1   |
| `0x07` MAX      | a b -- x | Pops `a` and `b` and pushes `max(a, b)` | `#S` >= `2` |
| `0x08` MIN      | a b -- x | Pops `a` and `b` and pushes `min(a, b)` | `#S` >= `2` |
| `0x09` LT, <    | a b -- x | Pops `a` and `b` and pushes `1` if `a < b`, else `0` | `#S` >= `2` |
| `0x0A` LE, <=   | a b -- x | Pops `a` and `b` and pushes `1` if `a <= b`, else `0` | `#S` >= `2` |
| `0x0B` EQ, =    | a b -- x | Pops `a` and `b` and pushes `1` if `a = b`, else `0` | `#S` >= `2` |
| `0x0C` GE, >=   | a b -- x | Pops `a` and `b` and pushes `1` if `a >= b`, else `0` | `#S` >= `2` |
| `0x0D` GT, >    | a b -- x | Pops `a` and `b` and pushes `1` if `a > b`, else `0` | `#S` >= `2` |
| `0x0E` DROP     | a --     | Pops `a` and discards it                | `#S` >= `1` |
| `0x0F` DUP      | a -- a a | Pops `a` and pushes two copies          | `#S` >= `1` |
| `0x10` NDUP     | bc2 -- bcb | Pops N and pushes copy of value at depth N | N > `0`, `#S` > N |
| `0x11` SWAP     | a b -- b a | Pops `a` and `b` and pushes `b` and `a` | `#S` >= `2` |
| `0x12` ROT      | a b c -- b c a | Rotates top 3 values (bottom to top) | `#S` >= `3` |
| `0x13` NROT     | abcd4 -- bcda | Pops N and rotates top N values | N > `0` `#S` > N |
| `0x14` TUCK     | a b c -- c a b | Rotates top 3 values (top to bottom) | `#S` >= `3` |
| `0x15` NTUCK    | abcd4 -- dabc | Pops N and tucks top N values | N > `0` `#S` > N |
| `0x16` SIZE     | -- x     | Pushes `#S`                             | --          |
| `0x17` NRND     | n -- x   | Pops N and pushes random number between `0` and N - `1` | `#S` >= `1`, N > `1` |
| `0x18` PUSH     | -- x     | Takes the 8-bit two's complement encoded in instruction and pushes this onto the stack | -- |
| `0x19` PUSH     | -- x     | Takes the 16-bit two's complement little endian encoded in instruction and pushes this onto the stack | -- |
| `0x1A` FETCH    | a -- x   | Pops address and pushes back 16-bit two's complement little endian found there | `#S` >= `1`, `0` <= `a` < `#P` - `1` |
| `0x1B` CALL     | a --     | Pushes the current program counter onto the return address stack, pops the destination from the operand stack and jumps to that address | `#S` >= `1`, `0` <= `a` < `#P` - `1` |
| `0x1C` RET      | --       | Pops address from return stack and jumps there | -- |
| `0x1D` JMP      | a --     | Pops destination from operand stack and jumps to that address | `#S` >= `1`, `0` <= `a` < `#P` - `1` |
| `0x1E` CJMP     | a b --   | Pops destination `b` and jumps to that addres if `a != 0` | `#S` >= `2`, `0` <= `b` < `#P` - `1` |
| `0x1F` WAIT     | d --     | Pops `d` and waits `d` milliseconds | `0` <= `d` <= `32767` |
| `0x20` HALT     | --       | Halts program execution                 | --          |

## Procedural Music

Plays the Fibonacci sequence in modulo 7, which repeats every 16 notes. The frequencies of the 7 notes
of the scale are stored in the data area. Includes optional instructions not described here.

```
.data
    B4 C5 D5 E5 F5 F#5 G5
.code
    33 6 1
loop:
    dup rot + 7 mod
    dup colour
    dup play call
    rot dec dup 4 ntuck
    0 > loop cjmp
    halt
play:
    2 * data + fetch
    200 beep
    50 wait
    ret
```

## Fibonacci Sequences

### Iterative

```
    12 fibonacci call
    halt
fibonacci:
    dup 1 > isGreaterThanOne cjmp
    ret
isGreaterThanOne:
   0 1
loop:
    dup tuck +
    rot 1 - dup 4 ntuck
    1 > loop cjmp
    rot drop swap drop
    ret
```

### Recursive

```
    12 fibonacci call
    halt
fibonacci:
    dup 1 > isGreaterThanOne cjmp
    ret
isGreaterThanOne:
    dup
    1 - fibonacci call
    swap
    2 - fibonacci call
    +
    ret
```
