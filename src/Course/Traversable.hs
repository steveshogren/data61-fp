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
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse :: Applicative f => (a -> f b) -> ExactlyOne a -> f (ExactlyOne b)
  traverse a_fb (ExactlyOne a) = pure <$> a_fb a

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse a_fb (Full a) = pure <$> a_fb a

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
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA tfa = traverse (\fa -> fa) tfa

-- Thoughts
  -- the previous instances were able to "unpack" the types using
  -- simple pattern matching
  -- as evidenced by the lack of a Monad instance, we cannot do that here
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Functor h) => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse a_hb (Compose fga) =
    error "unsure how to complete"
    -- (a_hb (<$>)) <$> fga

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) :: (a -> b) -> (Product f g a) -> (Product f g b)
  (<$>) a_b (Product fa ga) =
    Product (a_b <$> fa) (a_b <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse :: (Functor h) => (a -> h b) -> Product f g a -> h (Product f g b)
  traverse _ _ =
    error ""
  -- traverse a_hb (Product fa ga) =
  --   Product (a_hb <$> fa) (a_hb <$> ga)

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) :: (a -> b) -> (Coproduct f g a) -> (Coproduct f g b)
  (<$>) a_b (InL fa) = InL (a_b <$> fa)
  (<$>) a_b (InR ga) = InR (a_b <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse =
    error "todo: Course.Traversable traverse#instance (Coproduct f g)"
