module Optics.Polysemy.Reader
  ( gview
  , gviews
  ) where

import Optics
  ( Optic'
  , ViewResult
  , ViewableOptic
  )
import qualified Optics

import Polysemy
import Polysemy.Reader
import Polysemy.ConstraintAbsorber.MonadReader

gview :: (ViewableOptic k r, Member (Reader s) effs) => Optic' k is s r -> Sem effs (ViewResult k r)
gview o = absorbReader (Optics.gview o)
{-# INLINE gview #-}

gviews :: (ViewableOptic k r, Member (Reader s) effs) => Optic' k is s a -> (a -> r) -> Sem effs (ViewResult k r)
gviews o f = absorbReader (Optics.gviews o f)
{-# INLINE gviews #-}
