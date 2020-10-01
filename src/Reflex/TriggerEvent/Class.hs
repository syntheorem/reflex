-- | This module defines 'TriggerEvent', which describes actions that may create
-- new 'Event's that can be triggered from 'IO'.
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.TriggerEvent.Class
  ( TriggerEvent (..)
  ) where

import Reflex.Class

import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Maybe (MaybeT)

--TODO: Shouldn't have IO hard-coded
-- | 'TriggerEvent' represents actions that can create 'Event's that can be
-- triggered by 'IO' actions.
class Monad m => TriggerEvent t m | m -> t where
  -- | Create a triggerable 'Event'.  Whenever the resulting function is called,
  -- the resulting 'Event' will fire at some point in the future.  Note that
  -- this may not be synchronous.
  newTriggerEvent :: m (Event t a, a -> IO ())
  default newTriggerEvent :: (m ~ f m', MonadTrans f, TriggerEvent t m') => m (Event t a, a -> IO ())
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEvent #-}

  -- | Like 'newTriggerEvent', but the callback itself takes another callback,
  -- to be invoked once the requested 'Event' occurrence has finished firing.
  -- This allows synchronous operation.
  newTriggerEventWithOnComplete :: m (Event t a, a -> IO () -> IO ()) --TODO: This and newTriggerEvent should be unified somehow
  default newTriggerEventWithOnComplete :: (m ~ f m', MonadTrans f, TriggerEvent t m') => m (Event t a, a -> IO () -> IO ())
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  {-# INLINABLE newTriggerEventWithOnComplete #-}

  -- | Like 'newTriggerEventWithOnComplete', but with setup and teardown.  This
  -- relatively complex type signature allows any external listeners to be
  -- subscribed lazily and then removed whenever the returned 'Event' is no
  -- longer being listened to.  Note that the setup/teardown may happen multiple
  -- times, and there is no guarantee that the teardown will be executed
  -- promptly, or even at all, in the case of program termination.
  newEventWithLazyTriggerWithOnComplete :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)
  default newEventWithLazyTriggerWithOnComplete :: (m ~ f m', MonadTrans f, TriggerEvent t m') => ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}

instance TriggerEvent t m => TriggerEvent t (ReaderT r m)
instance TriggerEvent t m => TriggerEvent t (StateT s m)
instance TriggerEvent t m => TriggerEvent t (Strict.StateT s m)
instance TriggerEvent t m => TriggerEvent t (MaybeT m)
