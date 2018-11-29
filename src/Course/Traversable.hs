{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse aToFb (ExactlyOne a) =
    ExactlyOne <$> aToFb a

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse f =
    optional ((Full <$>) . f) (pure Empty)

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA =
  traverse id

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
  traverse ::
       (Traversable f, Traversable g, Applicative h)
    => (a -> h b)
    -> Compose f g a
    -> h (Compose f g b)
  traverse aToHb (Compose fga) =
    Compose <$> traverse (traverse aToHb) fga

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
  aToB <$> (Product fa ga) =
    Product (aToB <$> fa) (aToB <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
  traverse ::
       (Traversable f, Traversable g, Applicative h)
    => (a -> h b)
    -> Product f g a
    -> h (Product f g b)
  traverse aToHb (Product fa ga) =
    Product <$> traverse aToHb fa <*> traverse aToHb ga

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
  aToFb <$> (InL fa) = InL $ aToFb <$> fa
  aToFb <$> (InR ga) = InR $ aToFb <$> ga

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
  traverse aToHb (InL fa) = InL <$> traverse aToHb fa
  traverse aToHb (InR ga) = InR <$> traverse aToHb ga
