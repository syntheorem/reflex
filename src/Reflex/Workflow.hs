{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:
--   Reflex.Workflow
-- Description:
--   Provides a convenient way to describe a series of interrelated widgets that
--   can send data to, invoke, and replace one another. Useful for modeling user interface
--   "workflows."
module Reflex.Workflow (
    Workflow (..)
  , workflow
  , workflowView
  , mapWorkflow
  , mapWorkflowCheap
  ) where

import Control.Arrow ((***))
import Control.Lens (makePrisms, preview)
import Control.Monad (ap, (<=<), (>=>))
import Control.Monad.Fix (MonadFix)
import Data.Functor.Bind
import Data.Maybe (fromMaybe)
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PostBuild.Class

--------------------------------------------------------------------------------
-- Workflow
--------------------------------------------------------------------------------
-- | A widget in a workflow
-- When the 'Event' returned by a 'Workflow' fires, the current 'Workflow' is replaced by the one inside the firing 'Event'. A series of 'Workflow's must share the same return type.
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

-- | Runs a 'Workflow' and returns the initial value together with an 'Event' that fires whenever one 'Workflow' is replaced by another.
runWorkflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (a, Event t a)
runWorkflow w0 = mdo
  ((a, e0), eResult) <- runWithReplace (unWorkflow w0) (fmap unWorkflow eReplace)
  eReplace <- switchHold e0 $ fmap snd eResult
  return (a, fmap fst eResult)

-- | Similar to 'runWorkflow' but combines the result into a 'Dynamic'.
workflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow = uncurry holdDyn <=< runWorkflow

-- | Similar to 'runWorkflow', but also puts the initial value in the 'Event'.
workflowView :: (Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w = do
  postBuildEv <- getPostBuild
  (initialValue, replaceEv) <- runWorkflow w
  pure $ leftmost [initialValue <$ postBuildEv, replaceEv]

-- | Map a function over a 'Workflow', possibly changing the return type.
mapWorkflow :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow f (Workflow x) = Workflow (fmap (f *** fmap (mapWorkflow f)) x)

-- | Map a "cheap" function over a 'Workflow'. Refer to the documentation for 'pushCheap' for more information and performance considerations.
mapWorkflowCheap :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflowCheap f (Workflow x) = Workflow (fmap (f *** fmapCheap (mapWorkflowCheap f)) x)

--------------------------------------------------------------------------------
-- Wizard
--------------------------------------------------------------------------------
newtype Wizard t m a = Wizard { unWizard :: m (WizardInternal t m a) } deriving Functor
data WizardInternal t m a
  = WizardInternal_Terminal a
  | WizardInternal_Update (Event t a)
  | WizardInternal_Replace (Event t (Wizard t m a))
  deriving Functor
makePrisms ''WizardInternal

step :: Functor m => m (Event t a) -> Wizard t m a
step = Wizard . fmap WizardInternal_Update

runWizard :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Wizard t m a -> m (Event t a)
runWizard w = mdo
  let getReplace = fromMaybe never . preview _WizardInternal_Replace
      getUpdate = fromMaybe never . preview _WizardInternal_Update
  (wint0, wintEv) <- runWithReplace (unWizard w) $ leftmost [unWizard <$> replace, pure . WizardInternal_Terminal <$> updates]
  replace <- switchHold (getReplace wint0) (getReplace <$> wintEv)
  updates <- switchHold (getUpdate wint0) (getUpdate <$> wintEv)
  pb <- getPostBuild
  let terminal0 = maybe never (<$ pb) $ preview _WizardInternal_Terminal wint0
      terminal = fmapMaybe (preview _WizardInternal_Terminal) wintEv
  pure $ leftmost [terminal0, terminal]

instance (Monad m, Reflex t) => Apply (Wizard t m) where
  (<.>) = ap

instance (Monad m, Reflex t) => Applicative (Wizard t m) where
  pure = Wizard . pure . WizardInternal_Terminal
  (<*>) = (<.>)

instance (Monad m, Reflex t) => Bind (Wizard t m) where
  join ww = Wizard $ unWizard ww >>= \case
    WizardInternal_Terminal (Wizard w) -> w
    WizardInternal_Update ev -> pure $ WizardInternal_Replace ev
    WizardInternal_Replace ev -> pure $ WizardInternal_Replace $ ffor ev join

instance (Monad m, Reflex t) => Monad (Wizard t m) where
  (>>=) = (>>-)

--------------------------------------------------------------------------------
-- Stack
--------------------------------------------------------------------------------
newtype Stack t m a = Stack { unStack :: m (a, Event t a) } deriving Functor

frame :: m (a, Event t a) -> Stack t m a
frame = Stack

stackHold :: MonadHold t m => Stack t m a -> m (Dynamic t a)
stackHold = unStack >=> uncurry holdDyn

stackView :: PostBuild t m => Stack t m a -> m (Event t a)
stackView = unStack >=> \(a, ev) -> do
  pb <- getPostBuild
  pure $ leftmost [a <$ pb, ev]

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Apply (Stack t m) where
  (<.>) = ap

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Applicative (Stack t m) where
  pure = Stack . pure . (, never)
  (<*>) = (<.>)

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Bind (Stack t m) where
  join ss = frame $ do
    (s0, sEv) <- unStack ss
    ((a0,ev0), ev) <- runWithReplace (unStack s0) $ unStack <$> sEv
    e <- switchHold never $ fmap snd ev
    pure (a0, leftmost [ev0, fmap fst ev, e])

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Monad (Stack t m) where
  (>>=) = (>>-)
