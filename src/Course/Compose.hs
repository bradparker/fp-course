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
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  (<$>) f (Compose fga) = Compose $ (f <$>) <$> fga

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  -- Implement the pure function for an Applicative instance for Compose
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  -- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  --(<*>) (Compose fgab) (Compose fga) = Compose $ (<*>) <$> fgab <*> fga
  (<*>) (Compose fgab) (Compose fga) = Compose $ lift2 (<*>) fgab fga

-- Tip: think about apply on the inner applicative `g`
-- (<*>) on g :: Applicative g => g (a -> b) -> g a -> g b
-- then
-- (<*>) <$> fgab :: f (g a -> g b)
-- which we can then use with apply on the outer applicative `f`
-- (<*>) on f :: f (g a -> g b) -> f (g a) -> f (g b)
-- i.e. (<*>) <$> fgab <*> fga
-- Also, this is lift2 :)
-- lift2 :: (g (a -> b) -> g a -> g b) -> f (g (a -> b)) -> f (g a) -> f (g b)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "You can't compose Monads."
