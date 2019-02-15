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

-- | All instances of the `Traversable` type-class must satisfy three laws. These
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
  traverse g = (pure <$>) . g . runExactlyOne

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse g (Full a) = pure <$> g a

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
sequenceA = traverse id

instance (Traversable g, Traversable h) =>
  Traversable (Compose g h) where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Compose g h a
    -> f (Compose g h b)
  traverse g (Compose a) = Compose <$> (traverse . traverse) g a
  -- traverse g = (Compose <$>) . sequenceA . (sequenceA <$>) . getCompose . (g <$>)
  -- traverse g a = Compose <$> sequenceA (sequenceA <$> getCompose (g <$> a))

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
  (<$>) f (Product a b) = Product (f <$> a) (f <$> b)

instance (Traversable g, Traversable h) =>
  Traversable (Product g h) where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Product g h a
    -> f (Product g h b)
  traverse g (Product fa fa') = Product <$> traverse g fa <*> traverse g fa'

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
    (<$>) f (InL fa) = InL $ f <$> fa
    (<$>) f (InR fa) = InR $ f <$> fa

instance (Traversable g, Traversable h) =>
  Traversable (Coproduct g h) where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Coproduct g h a
    -> f (Coproduct g h b)
  traverse g (InL fa) = InL <$> traverse g fa
  traverse g (InR fa) = InR <$> traverse g fa
