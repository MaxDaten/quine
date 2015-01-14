{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.StateVar
  ( HasGetter(get)
  , GettableStateVar
  , HasSetter(($=))
  , SettableStateVar(SettableStateVar)
  , ($=!)
  , StateVar(StateVar)
  , mapStateVar
  , HasUpdate(($~), ($~!))
  , set
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Functor
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.IORef
import Data.Typeable
import Data.Void
import Foreign.Ptr
import Foreign.Storable

infixr 2 $=, $=!, $~, $~!

class HasSetter t a m | t m -> a where
  ($=) :: t -> a -> m ()

instance (MonadIO m, Storable a) => HasSetter (Ptr a) a m where
  p $= a = liftIO $ poke p a
  {-# INLINE ($=) #-}

instance MonadIO m => HasSetter (IORef a) a m where
  p $= a = liftIO $ writeIORef p a
  {-# INLINE ($=) #-}

instance MonadIO m => HasSetter (TVar a) a m where
  p $= a = liftIO $ atomically $ writeTVar p a

($=!) :: (HasSetter t a m, Monad m) => t -> a -> m ()
p $=! a = (p $=) $! a
{-# INLINE ($=!) #-}

newtype SettableStateVar a = SettableStateVar (a -> IO ()) deriving Typeable

instance Contravariant SettableStateVar where
  contramap f (SettableStateVar k) = SettableStateVar (k . f)
  {-# INLINE contramap #-}

instance Divisible SettableStateVar where
  divide k (SettableStateVar l) (SettableStateVar r) = SettableStateVar $ \ a -> case k a of
    (b, c) -> l b >> r c
  conquer = SettableStateVar $ \_ -> return ()

instance Decidable SettableStateVar where
  lose k = SettableStateVar (absurd . k)
  choose k (SettableStateVar l) (SettableStateVar r) = SettableStateVar $ \ a -> case k a of
    Left b -> l b
    Right c -> r c

instance MonadIO m => HasSetter (SettableStateVar a) a m where
  SettableStateVar f $= a = liftIO (f a)
  {-# INLINE ($=) #-}

type GettableStateVar = IO

class HasGetter t m a | t m -> a where
  get :: t -> m a

instance MonadIO m => HasGetter (TVar a) m a where
  get = liftIO . atomically . readTVar

instance MonadIO m => HasGetter (IO a) m a where
  get = liftIO

instance (Storable a, MonadIO m) => HasGetter (Ptr a) m a where
  get = liftIO . peek

instance MonadIO m => HasGetter (IORef a) m a where
  get = liftIO . readIORef

data StateVar a = StateVar (IO a) (a -> IO ()) deriving Typeable

instance MonadIO m => HasGetter (StateVar a) m a where
  get (StateVar g _) = liftIO g

instance MonadIO m => HasSetter (StateVar a) a m where
  StateVar _ s $= a = liftIO $ s a

mapStateVar :: (b -> a) -> (a -> b) -> StateVar a -> StateVar b
mapStateVar ba ab (StateVar ga sa) = StateVar (ab <$> ga) (sa . ba)
{-# INLINE mapStateVar #-}

class Monad m => HasUpdate t a m | t m -> a where
  ($~) :: t -> (a -> a) -> m ()
  default ($~) :: (HasGetter t m a, HasSetter t a m) => t -> (a -> a) -> m ()
  r $~ f = do
    a <- get r
    r $= f a
  ($~!) :: t -> (a -> a) -> m ()
  default ($~!) :: (HasGetter t m a, HasSetter t a m) => t -> (a -> a) -> m ()
  r $~! f = do
    a <- get r
    r $=! f a

instance MonadIO m => HasUpdate (StateVar a) a m
instance (Storable a, MonadIO m) => HasUpdate (Ptr a) a m
instance MonadIO m => HasUpdate (IORef a) a m where
  r $~ f  = liftIO $ atomicModifyIORef r $ \a -> (f a,())
  r $~! f = liftIO $ atomicModifyIORef' r $ \a -> (f a,())

-- | Non-infix version of '($=)'
set :: HasSetter t a m => t -> a -> m ()
set = ($=)
