-- | This module defines the 'DynamicWriter' class.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.DynamicWriter.Class
  ( MonadDynamicWriter
  , DynamicWriter(..)
  ) where

import Control.Monad.Reader (ReaderT, MonadTrans, lift)
import Reflex.Class (Dynamic)

{-# DEPRECATED MonadDynamicWriter "Use 'DynamicWriter' instead" #-}
-- | Type synonym for 'DynamicWriter'
type MonadDynamicWriter = DynamicWriter

-- | 'MonadDynamicWriter' efficiently collects 'Dynamic' values using 'tellDyn'
-- and combines them monoidally to provide a 'Dynamic' result.
class (Monad m, Monoid w) => DynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()
  default tellDyn :: (m ~ f m', MonadTrans f, DynamicWriter t w m') => Dynamic t w -> m ()
  tellDyn = lift . tellDyn
  {-# INLINABLE tellDyn #-}

instance DynamicWriter t w m => DynamicWriter t w (ReaderT r m)
