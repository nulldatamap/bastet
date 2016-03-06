{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Annotation (Fix(..), Ann(..)) where

data Fix f = Fix (f (Fix f))

instance (Show (f (Fix f))) => Show (Fix f) where
  show (Fix f) = show f

data Ann i f a =
  Ann { annotated  :: f a
      , annotation :: i }
  deriving Functor

