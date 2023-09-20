# Lua REPL

An interactive interpreter for Lua implemented in C.

Orginal example authored by Roberto Ierusalimschy, Luiz Henrique de Figueiredo, Waldemar Celes Filho.

```c
#include <stdio.h>
#include "lua.h"                // lua header file
#include "lualib.h"             // extra libraries (optional)

int main (int argc, char *argv[]) {
    char line[BUFSIZ];
    iolib_open();               // opens I/O library (optional)
    strlib_open();              // opens string lib (optional)
    mathlib_open();             // opens math lib (optional)
    while (gets(line) != 0) {
        lua_dostring(line);
    }
}
```
