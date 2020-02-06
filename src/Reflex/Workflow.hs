{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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
  , runWorkflow

  , Wizard (..)
  , step
  , runWizard

  , Stack (..)
  , frame
  , stackHold
  , stackView

  , replay
  ) where

import Control.Arrow ((***))
import Control.Lens (makePrisms, preview)
import Control.Monad (ap, (<=<), (>=>))
import Control.Monad.Cont (ContT(..), MonadCont, callCC)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.Trans (lift)
import Data.Functor.Bind
import Data.Maybe (fromMaybe)
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PostBuild.Class

import Unsafe.Coerce

replay :: MonadCont m => m (m a)
replay = callCC $ return . fix

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

-- Core
newtype W t m a = W { unW :: m (WInternal t m a) } deriving Functor
data WInternal t m a
  = WInternal_Terminal a
  | WInternal_Update (Event t a)
  | WInternal_Replace (Event t (W t m a))
  deriving Functor
makePrisms ''WInternal

stepW :: Functor m => m (Event t a) -> W t m a
stepW = W . fmap WInternal_Update

runW :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => W t m a -> m (Event t a)
runW w = mdo
  let getReplace = fromMaybe never . preview _WInternal_Replace
      getUpdate = fromMaybe never . preview _WInternal_Update
  (wint0, wintEv) <- runWithReplace (unW w) $ leftmost [unW <$> replace, pure . WInternal_Terminal <$> updates]
  replace <- switchHold (getReplace wint0) (getReplace <$> wintEv)
  updates <- switchHold (getUpdate wint0) (getUpdate <$> wintEv)
  pb <- getPostBuild
  let terminal0 = maybe never (<$ pb) $ preview _WInternal_Terminal wint0
      terminal = fmapMaybe (preview _WInternal_Terminal) wintEv
  pure $ leftmost [terminal0, terminal]

instance (Monad m, Reflex t) => Apply (W t m) where
  (<.>) = ap

instance (Monad m, Reflex t) => Applicative (W t m) where
  pure = W . pure . WInternal_Terminal
  (<*>) = (<.>)

instance (Monad m, Reflex t) => Bind (W t m) where
  join ww = W $ unW ww >>= \case
    WInternal_Terminal (W w) -> w
    WInternal_Update ev -> pure $ WInternal_Replace ev
    WInternal_Replace ev -> pure $ WInternal_Replace $ ffor ev join

instance (Monad m, Reflex t) => Monad (W t m) where
  (>>=) = (>>-)


-- Enhanced
newtype Wizard t m a = Wizard { unWizard :: forall r. ContT r (W t m) a } deriving Functor

instance Apply (Wizard t m) where
  (<.>) = ap

instance Applicative (Wizard t m) where
  pure a = Wizard $ pure a
  (<*>) = (<*>)

instance Bind (Wizard t m) where
  join ww = Wizard $ join $ fmap unWizard $ unWizard ww

instance Monad (Wizard t m) where
  (>>=) = (>>-)

instance MonadCont (Wizard t m) where
  callCC f = Wizard $ callCC $ \g ->
    unWizard $ f $ \a -> Wizard $ unsafeCoerce $ g a -- TODO: figure out the rank shenanigans needed to prove this

step :: (Reflex t, Monad m) => m (Event t a) -> Wizard t m a
step a = Wizard $ lift $ stepW a

runWizard :: (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Wizard t m a -> m (Event t a)
runWizard = runW . flip runContT pure . unWizard
--------------------------------------------------------------------------------
-- Stack
--------------------------------------------------------------------------------

-- Core
newtype S t m a = S { unS :: m (a, Event t a) } deriving Functor

frameS :: m (a, Event t a) -> S t m a
frameS = S

sHold :: MonadHold t m => S t m a -> m (Dynamic t a)
sHold = unS >=> uncurry holdDyn

sView :: PostBuild t m => S t m a -> m (Event t a)
sView = unS >=> \(a, ev) -> do
  pb <- getPostBuild
  pure $ leftmost [a <$ pb, ev]

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Apply (S t m) where
  (<.>) = ap

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Applicative (S t m) where
  pure = S . pure . (, never)
  (<*>) = (<.>)

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Bind (S t m) where
  join ss = frameS $ do
    (s0, sEv) <- unS ss
    ((a0,ev0), ev) <- runWithReplace (unS s0) $ unS <$> sEv
    e <- switchHold never $ fmap snd ev
    pure (a0, leftmost [ev0, fmap fst ev, e])

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Monad (S t m) where
  (>>=) = (>>-)

-- Enhanced
newtype Stack t m a = Stack { unStack :: forall r. ContT r (S t m) a } deriving Functor

frame :: (Adjustable t m, MonadHold t m, PostBuild t m) => m (a, Event t a) -> Stack t m a
frame a = Stack $ lift $ frameS a

stackHold :: (Adjustable t m, MonadHold t m, PostBuild t m) => Stack t m a -> m (Dynamic t a)
stackHold = sHold . flip runContT pure . unStack

stackView :: (Adjustable t m, MonadHold t m, PostBuild t m) => Stack t m a -> m (Event t a)
stackView = sView . flip runContT pure . unStack

instance Apply (Stack t m) where
  (<.>) = ap

instance Applicative (Stack t m) where
  pure a = Stack $ pure a
  (<*>) = (<*>)

instance Bind (Stack t m) where
  join ww = Stack $ join $ fmap unStack $ unStack ww

instance Monad (Stack t m) where
  (>>=) = (>>-)

instance MonadCont (Stack t m) where
  callCC f = Stack $ callCC $ \g ->
    unStack $ f $ \a -> Stack $ unsafeCoerce $ g a  -- TODO: figure out the rank shenanigans needed to prove this
