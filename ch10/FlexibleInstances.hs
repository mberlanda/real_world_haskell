-- file: ch10/FlexibleInstances.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-} 

module FlexibleInstances where
  instance Functor (Either Int) where
      fmap _ (Left n) = Left n
      fmap f (Right r) = Right (f r)