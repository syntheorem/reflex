-- | This module defines the 'EventWriter' class.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.EventWriter.Class
  ( EventWriter (..)
  ) where

import Control.Monad.Reader (ReaderT, MonadTrans, lift)
import Data.Semigroup (Semigroup)

import Reflex.Class (Event)


-- | 'EventWriter' efficiently collects 'Event' values using 'tellEvent'
-- and combines them via 'Semigroup' to provide an 'Event' result.
class (Monad m, Semigroup w) => EventWriter t w m | m -> t w where
  tellEvent :: Event t w -> m ()
  default tellEvent :: (m ~ f m', MonadTrans f, EventWriter t w m') => Event t w -> m ()
  tellEvent = lift . tellEvent
  {-# INLINABLE tellEvent #-}

instance EventWriter t w m => EventWriter t w (ReaderT r m)
