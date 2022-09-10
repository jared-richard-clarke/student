# Combining Function Signatures

## Bind

```hs
(>>=) :: m a -> (a -> m b) -> m b
```

## Return

```hs
return :: a -> m a
```

## Map

```hs
fmap :: (a -> b) -> f a -> f b
```

## Apply

```hs
apply :: f (a -> b) -> f a -> f b
```

## Lift2

```hs
lift2 :: f (a -> b -> c) -> f a -> f b -> f c
```
