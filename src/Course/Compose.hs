{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) a2b (Compose fga) = Compose $ (a2b <$>) <$> fga

  -- Applicative.Apply
  -- (<*>) :: forall a b . Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- (Compose _a) <*> (Compose _b) = Compose $ _c
  -- _a :: f (g (a -> b))
  -- _b :: f (g a)
  -- _c :: f (g b)

  -- To get _c, we are going to want a function on the left and f (g a) on the right:
  -- f ((g a) -> ?) <*> f (g a)

  -- To get ((g a) -> ?) into f, we can <$> into f (g (a -> b)):
  -- (g (a -> b) -> ?) <$> f (g (a -> b)

  -- Need g (a -> b) <*> g a => g b
  -- To get `g (a -> b)` or `g a`:
  -- - (\g (a -> b) -> ) <$> f (g (a -> b))
  -- - (\g a -> ) <$> f (g a)

  -- <$> :: (a -> b) -> (f a -> f b)
  -- <*> :: g (a -> b) -> (g a -> g b)
  -- ((<*>) <$>) :: f (g (a -> b)) -> f (g a -> g b)

  -- Simplest
  -- (Compose fga2b) <*> (Compose fga) = Compose $ ((\ga2b -> (\ga -> ga2b <*> ga)) <$> fga2b) <*> fga
  
  -- With broken out function
  -- (Compose fga2b) <*> (Compose fga) = Compose $ run ((<*>) <$> fga2b)
  --   where 
  --     run :: f (g a -> g b) -> f (g b)
  --     run fga2gb = fga2gb <*> fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: forall a b . Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fga2b) <*> (Compose fga) = Compose $ ((<*>) <$> fga2b) <*> fga



instance (Monad f, Monad g) => Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =  error "Not possible"
