{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) fn (Compose f) = Compose $ (\g -> fn <$> g) <$> f

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a = Compose $ pure (pure a)

-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: forall a b. Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose ffn) (Compose fa) = Compose $ x <*> fa
    where
      x :: f(g a -> g b)
      x = (<*>) <$> ffn

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) _ _ = undefined
