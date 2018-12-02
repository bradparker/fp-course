{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

newtype Compose f g a =
  Compose { getCompose :: f (g a) }

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f = Compose . (f <$$>) . getCompose
    where
      (<$$>) = (<$>) . (<$>)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure = Compose . pure . pure
  (<*>) (Compose fgab) (Compose fga) = Compose $ (<*>) <$> fgab <*> fga

