# State Management Using Monads

**Learn You a Haskell for Great Good** by Miran Lipovača

## State Management: Manual

```haskell
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManual :: Stack -> (Int, Stack)
stackManual stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2

-- stackManual [5, 8, 2, 1] -> (5, [8, 2, 1])
```

## State Management: Monad

```haskell
type Stack = [Int]

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a:xs)

stackMonad :: State Stack Int
stackMonad = do
    push 3
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

-- runState stackStuff [9, 0, 2, 1, 0] -> ((), [8, 3, 0, 2, 1, 0])
```
