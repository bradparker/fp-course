{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  aToB <$> (Compose fga) =
    Compose $ (aToB <$>) <$> fga

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose fgaToB) <*> (Compose fga) =
    Compose $ (<*>) <$> fgaToB <*> fga

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
