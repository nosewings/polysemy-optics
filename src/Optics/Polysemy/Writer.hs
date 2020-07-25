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

glistening :: (ViewableOptic k r, Member (Writer s) effs) => Optic' k is s r -> Sem effs a -> Sem effs (a, ViewResult k r)
glistening o m = do
  (s, a) <- listen m
  return (a, Optics.gview o s)
{-# INLINE glistening #-}

glistenings :: (ViewableOptic k r, Member (Writer s) effs) => Optic' k is s a -> (a -> r) -> Sem effs b -> Sem effs (b, ViewResult k r)
glistenings o f m = do
  (s, b) <- listen m
  return (b, Optics.gviews o f s)
{-# INLINE glistenings #-}
