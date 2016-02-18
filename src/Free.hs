{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Free (Free(..), liftF) where

data Free f a = Free (f (Free f a)) | Pure a
  deriving Functor

instance (Show (f (Free f a)), Show a) => Show (Free f a) where
  show (Pure x) = show x
  show (Free f) = show f

liftF :: Functor f => f a -> Free f a
liftF action = Free (fmap Pure action)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure a  <*> Pure b  = Pure $ a b
  Pure a  <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b       = Free $ (<*> b) <$> ma
  
instance Functor f => Monad (Free f) where
  return       = Pure
  Free a >>= f = Free (fmap (>>= f) a)
  Pure a >>= f = f a

