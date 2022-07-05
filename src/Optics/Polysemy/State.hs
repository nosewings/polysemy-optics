{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Optics.Polysemy.State
  ( modifying
  , modifying'
  , assign
  , assign'
  , use
  , preuse
  , (.=)
  , (?=)
  , (%=)
  , (%%=)
  , (<.=)
  , (<?=)
  , (<%=)
  , (<<.=)
  , (<<?=)
  , (<<%=)
  , guse
  , guses
  , zoom
  , zoomMaybe
  ) where

import Data.Maybe
  ( fromMaybe
  )

import Optics
  ( A_Getter
  , A_Lens
  , A_Setter
  , An_AffineFold
  , An_AffineTraversal
  , Is
  , Optic
  , Optic'
  , ViewResult
  , ViewableOptic
  , castOptic
  )
import qualified Optics
import qualified Optics.State
import Optics.State.Operators
  ( PermeableOptic
  )
import qualified Optics.State.Operators

import Polysemy
import Polysemy.State
import Polysemy.ConstraintAbsorber.MonadState

modifying :: (Is k A_Setter, Member (State s) effs) => Optic k is s s a b -> (a -> b) -> Sem effs ()
modifying o f = absorbState (Optics.State.modifying o f)
{-# INLINE modifying #-}

modifying' :: (Is k A_Setter, Member (State s) effs) => Optic k is s s a b -> (a -> b) -> Sem effs ()
modifying' o f = absorbState (Optics.State.modifying o f)
{-# INLINE modifying' #-}

assign :: (Is k A_Setter, Member (State s) effs) => Optic k is s s a b -> b -> Sem effs ()
assign o b = absorbState (Optics.State.assign o b)
{-# INLINE assign #-}

assign' :: (Is k A_Setter, Member (State s) effs) => Optic k is s s a b -> b -> Sem effs ()
assign' o b = absorbState (Optics.State.assign' o b)
{-# INLINE assign' #-}

use :: (Is k A_Getter, Member (State s) effs) => Optic' k is s a -> Sem effs a
use o = absorbState (Optics.State.use o)
{-# INLINE use #-}

preuse :: (Is k An_AffineFold, Member (State s) effs) => Optic' k is s a -> Sem effs (Maybe a)
preuse o = absorbState (Optics.State.preuse o)
{-# INLINE preuse #-}

infix 4 .=
(.=) :: (Is k A_Setter, Member (State s) effs) => Optic k is s s a b -> b -> Sem effs ()
o .= b = absorbState ((Optics.State.Operators..=) o b)
{-# INLINE (.=) #-}

infix 4 ?=
(?=) :: (Is k A_Setter, Member (State s) effs) => Optic k is s s (Maybe a) (Maybe b) -> b -> Sem effs ()
o ?= b = absorbState ((Optics.State.Operators.?=) o b)
{-# INLINE (?=) #-}

infix 4 %=
(%=) :: (Is k A_Setter, Member (State s) effs) => Optic k is s s a b -> (a -> b) -> Sem effs ()
o %= f = absorbState ((Optics.State.Operators.%=) o f)
{-# INLINE (%=) #-}

infix 4 %%=
(%%=) :: (PermeableOptic k r, Member (State s) effs) => Optic k is s s a b -> (a -> (r, b)) -> Sem effs (ViewResult k r)
o %%= f = absorbState ((Optics.State.Operators.%%=) o f)
{-# INLINE (%%=) #-}

infix 4 <.=
(<.=) :: (PermeableOptic k b, Member (State s) effs) => Optic k is s s a b -> b -> Sem effs (ViewResult k b)
o <.= b = absorbState ((Optics.State.Operators.<.=) o b)
{-# INLINE (<.=) #-}

infix 4 <?=
(<?=) :: (PermeableOptic k (Maybe b), Member (State s) effs) => Optic k is s s (Maybe a) (Maybe b) -> b -> Sem effs (ViewResult k (Maybe b))
o <?= b = absorbState ((Optics.State.Operators.<?=) o b)
{-# INLINE (<?=) #-}

infix 4 <%=
(<%=) :: (PermeableOptic k b, Member (State s) effs) => Optic k is s s a b -> (a -> b) -> Sem effs (ViewResult k b)
o <%= f = absorbState ((Optics.State.Operators.<%=) o f)
{-# INLINE (<%=) #-}

infix 4 <<.=
(<<.=) :: (PermeableOptic k a, Member (State s) effs) => Optic k is s s a b -> b -> Sem effs (ViewResult k a)
o <<.= b = absorbState ((Optics.State.Operators.<<.=) o b)
{-# INLINE (<<.=) #-}

infix 4 <<?=
(<<?=) :: (PermeableOptic k (Maybe a), Member (State s) effs) => Optic k is s s (Maybe a) (Maybe b) -> b -> Sem effs (ViewResult k (Maybe a))
o <<?= b = absorbState ((Optics.State.Operators.<<?=) o b)
{-# INLINE (<<?=) #-}

infix 4 <<%=
(<<%=) :: (PermeableOptic k a, Member (State s) effs) => Optic k is s s a b -> (a -> b) -> Sem effs (ViewResult k a)
o <<%= f = absorbState ((Optics.State.Operators.<<%=) o f)
{-# INLINE (<<%=) #-}

guse :: (ViewableOptic k a, Member (State s) effs) => Optic' k is s a -> Sem effs (ViewResult k a)
guse o = absorbState (Optics.guse o)
{-# INLINE guse #-}

guses :: (ViewableOptic k r, Member (State s) effs) => Optic' k is s a -> (a -> r) -> Sem effs (ViewResult k r)
guses o f = absorbState (Optics.guses o f)
{-# INLINE guses #-}

zoom :: (Is k A_Lens, Member (State s) effs) => Optic' k is s a -> Sem (State a ': effs) c -> Sem effs c
zoom o = interpret \case
  Get   -> use o'
  Put a -> assign o' a
  where o' = castOptic @A_Lens o
{-# INLINE zoom #-}

zoomMaybe :: (Is k An_AffineTraversal, Member (State s) effs) => Optic' k is s a -> Sem (State a ': effs) c -> Sem effs (Maybe c)
zoomMaybe o m = preuse o' >>= traverse \a ->
  interpret \case
      Get    -> fromMaybe a <$> preuse o'
      Put a' -> assign o' a'
  m
  where o' = castOptic @An_AffineTraversal o
{-# INLINE zoomMaybe #-}
