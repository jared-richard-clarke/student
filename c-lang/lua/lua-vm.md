# Lua's Virtual Machine

Original text from **The Implementation of Lua 5.0**

There are 35 instructions in Lua's virtual machine. Most instructions correspond
directly to language constructs, such as arithmetic, table creation and indexing,
function and method calls, setting variables and getting values. There are also jump
instructions for implementing control flow.

## Notational Conventions

- `R(X)`: `X`th register.
- `K(X)`: `X`th constant.
- `RK(X)`: either `R(X)` or `K(X-k)`, where `R(X)` is for values of `X` smaller
   than `k`.
- `G[X]`: field `X` in a table of globals.
- `U[X]`: the `X`th upvalue.
- `Bx` and `sBx`: unsigned and signed 18-bit value.

## Op Code Instruction Set

```text
MOVE      A B      R(A) := R(B)
LOADK     A Bx     R(A) := K(Bx)
LOADBOOL  A B C    R(A) := (Bool)B; if (C) PC++
LOADNIL   A B      R(A) := ... := R(B) := nil
GETUPVAL  A B      R(A) := U[B]
GETGLOBAL A Bx     R(A) := G[K(Bx)]
GETTABLE  A B C    R(A) := R(B)[RK(C)]
SETGLOBAL A Bx     G[K(Bx)] := R(A)
SETUPVAL  A B      U[B] := R(A)
SETTABLE  A B C    R(A)[RK(B)] := RK(C)
NEWTABLE  A B C    R(A) := {} (size = B,C)
SELF      A B C    R(A+1) := R(B); R(A) := R(B)[RK(C)]
ADD       A B C    R(A) := RK(B) + RK(C)
SUB       A B C    R(A) := RK(B) - RK(C)
MUL       A B C    R(A) := RK(B) * RK(C)
DIV       A B C    R(A) := RK(B) / RK(C)
POW       A B C    R(A) := RK(B) ^ RK(C)
UNM       A B      R(A) := -R(B)
NOT       A B      R(A) := not R(B)
CONCAT    A B C    R(A) := R(B) .. ... .. R(C)
JMP       sBx      PC += sBx
EQ        A B C    if ((RK(B) == RK(C)) ~= A) then PC++
LT        A B C    if ((RK(B) < RK(C)) ~= A) then PC++
LE        A B C    if ((RK(B) <= RK(C)) ~= A) then PC++
TEST      A B C    if (R(B) <=> C) then R(A) := R(B) else PC++
CALL      A B C    R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
TAILCALL  A B C    return R(A)(R(A+1), ... ,R(A+B-1))
RETURN    A B      return R(A), ... ,R(A+B-2)
FORLOOP   A sBx    R(A)+=R(A+2); if R(A) <?= R(A+1) then PC+= sBx
TFORLOOP  A C      R(A+2), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));
TFORPREP  A sBx    if type(R(A)) == table then R(A+1) := R(A), R(A):=next;
SETLIST   A Bx     R(A)[Bx-Bx%FPF+i] := R(A+i), 1 <= i <= Bx%FPF+1
SETLISTO  A Bx
CLOSE     A        close stack variables up to R(A)
CLOSURE   A Bx     R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
```

## Instruction Layout

Lua's virtual machine instructions take 32 bits divided into 3 or 4 fields.
The 6-bit `OP` field identifies the instruction. 8-bit field `A` is always
present. Fields `B` and `C` are 9 bits each. They can be combined into an
18-bit field: `Bx` (unsigned) and `sBx` (signed).

Most instructions use a three-address format, where `A` points to the register that
will hold the result and `B` and `C` point to the operands, which can be either a
register or a constant.

```text
| OP | A | B | C |
|----|---|-------|
| OP | A | Bx    |
|----|---|-------|
| OP | A | sBx   |
```

## Lua Function to Bytecode

### Function

```lua
function max(a, b)
    local m = a
    if b > a then
        m = b
    end
    return m
end
```

### Bytecode

```text
1 MOVE      2 0 0    ; R(2) = R(0)
2 LT        0 0 1    ; R(0) < R(1) ?
3 JMP       1        ; to 5 (4+1)
4 MOVE      2 1 0    ; R(2) = R(1)
5 RETURN    2 2 0    ; return R(2)
6 RETURN    0 1 0    ; return
```

## Lua Data Types

```c
// tagged union (t, v)
// where t = integer tag identifying type
//       v = union of C types implementing Lua types
typedef struct {
    int t;
    Value v;
} TObject;

typedef union {
    GCObject *gc; // strings, tables, functions (referenced and garbage-collected data)
    void *p;      // light user data
    lua_Number n; // numbers (Double by default)
    int b;        // booleans
} Value;
```
