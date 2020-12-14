{-# LANGUAGE InstanceSigs #-}

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

newtype Reader r a = Reader {getName :: r -> a}

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (f . g)

first3 :: (a, a, a) -> a
first3 (a, _, _) = a

count :: Reader (String, String, String) Int
count = fmap (length :: String -> Int) (Reader first3)

team = [("Taro", "A", "Suzuki"), ("Hanako", "B", "Sato")]