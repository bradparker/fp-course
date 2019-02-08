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
  a2b <$> Compose fga = Compose $ (a2b <$>) <$> fga

  -- Applicative.Apply
  -- (<*>) :: forall a b . Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- (Compose _a) <*> (Compose _b) = Compose $ _c

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: forall a b . Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose fga2b <*> Compose fga = Compose $ (<*>) <$> fga2b <*> fga

instance (Monad f, Monad g) => Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) = error "Not possible"
