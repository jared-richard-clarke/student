# Lua REPL

## Interpreter

An interactive interpreter for Lua implemented in C.

Original example authored by Roberto Ierusalimschy, Luiz Henrique de Figueiredo, Waldemar Celes Filho.

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

## Command Line

A simplified version of the Lua command line program.

```c
#include <stdio.h>

#include "luaxlib.h"
#include "lualib.h"

int main (void) {
    char line[256];
    lua_State *L = luaL_newstate();                    // create a new state
    luaL_openlibs(L);                                  // open the standard libraries

    // reads lines and executes them
    while (fgets(line, sizeof(line), stdin) != NULL) {
        luaL_loadstring(L, line);                      // compile line to a function
        lua_pcall(L, 0, 0, 0);                         // call the function
    }

    lua_close(L);
    return 0;
}
```
