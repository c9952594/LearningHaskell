## Functor

```haskell
class Functor f where
   fmap :: (a -> b) -> f a -> f b
```

```haskell
data Maybe a = Nothing | Just a
instance Functor Maybe where
   fmap _ Nothing = Nothing
   fmap g (Just x) = Just (g x)

fmap (+1) Nothing
Nothing

fmap (*2) Just 3
Just 6

fmap not (Just False)
Just True
```

```haskell
data Tree a =
   Leaf a |
   Node (Tree a) (Tree a)
instance Functor Tree where
   fmap g Leaf a = Leaf (g a)
   fmap g (Node l r) = Node (fmap g l) (fmap g r)

fmap length (Leaf "abc")
Leaf 3

fmap even (Node (Leaf 1) (Leaf 2))
Node (Leaf False) (Leaf True)
```

```haskell
instance Functor [] where
   fmap g [] = []
   fmap g (x:xs) = (g x) : fmap g xs

fmap (*2) [1,2,3]
[2,4,6]
```

```haskell
instance Functor IO where
   fmap g iox = do {
                   x <- iox
                   return (g x)
                }

fmap show (return True)
"True"
```

### Laws

```haskell
fmap id = id
fmap (g . h) = fmap g . fmap h
```
