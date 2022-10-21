# Equivalent Modules

## Module

```julia
module Mod

# ...

end
```

## Bare Module

Automatically imports only `Core`.

```julia
baremodule Mod

using Base

eval(x) = Core.eval(Mod, x)
include(p) = Base.include(Mod, p)

# ...

end
```
