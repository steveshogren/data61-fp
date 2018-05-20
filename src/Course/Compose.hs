{-# LANGUAGE InstanceSigs #-}
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
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) :: (a -> b) -> (Compose f g a) -> (Compose f g b)
  (<$>) a_b (Compose f_c_a) = Compose ((a_b <$>) <$> f_c_a)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure :: a -> (Compose f g a)
  pure a = Compose (pure $ pure $ a)
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: (Compose f g (a -> b)) -> (Compose f g a) -> (Compose f g b)
  (<*>) (Compose fga_b) (Compose fga_a) =
    Compose (pure (<*>) <*> fga_b <*> fga_a)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) :: (a -> Compose f g b) -> (Compose f g a) -> (Compose f g b)
  (=<<) a_mcb (Compose m_c_a) = error "can't 'unpack' the compose value to pass to a_mcb"
