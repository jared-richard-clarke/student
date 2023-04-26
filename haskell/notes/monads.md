# Monads in Haskell

```haskell
class  Monad m  where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a

        -- Minimal complete definition:
        --      (>>=), return
    m >> k  =  m >>= \_ -> k
```

A monad in Haskell is defined by three elements:
1. A type constructor `m`.
2. A function `return`.
3. A combinator `(>>=)`, which is pronounced "bind".

> In short, a monad is a way to structure computations in terms of values and sequences 
> of computations using typed values...For those coming from languages where the semicolon
> is a statement separator in imperative control flow, the metaphor of "programmable semicolon"
> has helped many understand the advantages of monads. The monad determines how combined
> computations form a new computation and frees the programmer from having to code the
> combination manually each time it is required. Think of monads as "statically typed filters"
> (in the Unix sense of "pipes and filters") and you may be halfway there.
>
> — **All About Monads** - The Haskell Wiki

## Why Monads?

> 1. Modularity - They allow computations to be composed from simpler computations and separate 
> the combination strategy from the actual computations being performed.
> 
> 2. Flexibility - They allow functional programs to be much more adaptable than equivalent programs 
> written without monads. This is because the monad distills the computational strategy into a single 
> place instead of requiring it be distributed throughout the entire program.
> 
> 3. Isolation - They can be used to create imperative-style computational structures which remain 
> safely isolated from the main body of the functional program. This is useful for incorporating 
> side-effects (such as I/O) and state (which violates referential transparency) into 
> a pure functional language like Haskell.
>
> — **All About Monads** - The Haskell Wiki

## The Three Monad Laws

The monad laws are not so much laws as generally-agreed-upon rules for predictable
behaviors among different monad implementations.

```haskell
m >>= return     =  m                        -- right unit
return x >>= f   =  f x                      -- left unit

(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  -- associativity
```

**Right and Left Unit** ensure `return` is a neutral element in that it doesn't perform any computation.

**Associativity** ensures `>>=` cares only about the order of computations, not their nesting.

## Categorical Definition

Category Theory treats monads as functors with two additional combinators.

```haskell
fmap   :: (a -> b) -> M a -> M b  -- functor

return :: a -> M a
join   :: M (M a) -> M a          -- similar to concat for lists, where concat :: [[a]] -> [a]

-- bind can be defined as ...
m >>= g = join (fmap g m)

-- likewise ...
fmap f x = x >>= (return . f)
join x   = x >>= id
```

## A Catalog of Standard Monads

### Identity

```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return a           = Identity a   -- i.e. return = id
    (Identity x) >>= f = f x          -- i.e. x >>= f = f x
```

### Maybe

```haskell
data Maybe a = Nothing | Just a

instance Monad Maybe where
    return         = Just
    Nothing  >>= f = Nothing
    (Just x) >>= f = f x

instance MonadFail Maybe where
    fail _         = Nothing

instance MonadPlus Maybe where
    mzero             = Nothing
    Nothing `mplus` x = x
    x `mplus` _       = x
```

### Error

```haskell
-- Error class
class Error a where
    noMsg :: a
    strMsg :: String -> a

class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a
 
 -- Either instance
instance MonadError e (Either e) where
    throwError = Left
    (Left e) `catchError` handler = handler e
    a        `catchError` _       = a
```

### List

```haskell
instance Monad [] where
    m >>= f  = concatMap f m
    return x = [x]
    fail s   = []

instance MonadPlus [] where
    mzero = []
    mplus = (++)
```

### IO

The definition of the IO monad is platform specific. Its purpose is to isolate side effects
from referentially transparent code.

### State

A State monad threads a state parameter through a sequence of bound functions so that the 
same state value is never used twice, giving the illusion of in-place update.

```haskell
newtype State s a = State { runState :: (s -> (a,s)) }

instance Monad (State s) where
    return a        = State $ \s -> (a,s)
    (State x) >>= f = State $ \s -> let (v,s') = x s in runState (f v) s'
    
class MonadState m s | m -> s where
    get :: m s
    put :: s -> m ()

instance MonadState (State s) s where
    get   = State $ \s -> (s,s)
    put s = State $ \_ -> ((),s)
```

### Reader

The `ask` function retrieves the environment and the `local` function executes a 
computation in a modified environment. The `asks` function is a convenience function 
that retrieves a function of the current environment, and is typically used with a 
selector or lookup function. 

```haskell
newtype Reader e a = Reader { runReader :: (e -> a) }

instance Monad (Reader e) where
    return a         = Reader $ \e -> a
    (Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e

class MonadReader e m | m -> e where
    ask   :: m e
    local :: (e -> e) -> m a -> m a

instance MonadReader e (Reader e) where
    ask       = Reader id
    local f c = Reader $ \e -> runReader c (f e)

asks :: (MonadReader e m) => (e -> a) -> m a
asks selector = ask >>= return . selector
```

## Defining Functors, Applicatives, and Monads

Functor and Applicative must be defined for an instances of Monad.
You can implement types moving down the class hierarchy from
Functor to Applicative to Monad.

```haskell
instance Functor M where
    fmap = -- etc.

instance Applicative M where
    pure = -- etc.
    (<*>) = -- etc.

instance Monad M where
    (>>=) = -- etc.
```

Coversely, Functor and Applicative can be derived from Monad.

```haskell
instance Monad M where
    return = -- etc.
    (>>=) = -- etc.

instance Applicative M where
    pure = return
    (<*>) = ap

instance Functor M where
    fmap = liftM
```
