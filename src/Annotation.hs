{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Annotation (Fix(..), Ann(..), unfix) where

data Fix f = Fix (f (Fix f))

unfix (Fix x) = x

instance (Show (f (Fix f))) => Show (Fix f) where
  show (Fix f) = show f

data Ann i f a =
  Ann { annotated  :: f a
      , annotation :: i }
  deriving Functor

