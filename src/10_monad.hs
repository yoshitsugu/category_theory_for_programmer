module Program ((Program..), Program.Monad, (>=>)) where

(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \a ->
  let b = f a
   in g b

(>=>) :: (Program.Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \a -> (f a) Program.>>= g

class Functor m => Monad m where
  join :: m (m a) -> m a
  return :: a -> m a

(>>=) :: (Program.Monad m) => m a -> (a -> m b) -> m b
ma >>= f = join $ fmap f ma

instance Program.Monad [] where
  join [[a]] = [a]
  return a = [a]

-- class Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

-- instance Program.Monad [] where
--   [a] >>= g = g a
--   return a = [a]