module Optics.Polysemy.Writer
  ( glistening
  , glistenings
  ) where

import Optics
  ( Optic'
  , ViewResult
  , ViewableOptic
  )
import qualified Optics

import Polysemy
import Polysemy.Writer

glistening :: (ViewableOptic k r, Member (Writer s) effs) => Optic' k is s r -> Sem effs a -> Sem effs (ViewResult k r, a)
glistening o m = do
  (s, a) <- listen m
  return (Optics.gview o s, a)
{-# INLINE glistening #-}

glistenings :: (ViewableOptic k r, Member (Writer s) effs) => Optic' k is s a -> (a -> r) -> Sem effs b -> Sem effs (ViewResult k r, b) 
glistenings o f m = do
  (s, b) <- listen m
  return (Optics.gviews o f s, b)
{-# INLINE glistenings #-}
