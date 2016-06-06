{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module HL.DelayT where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Logic.Class
import GHC.Base

data DelayT' m a =
   Done a
   | Delay (DelayT m a)

newtype DelayT m a = DelayT (m (DelayT' m a))

runDelayT (DelayT x) = x

instance Monad m => Functor (DelayT m) where
    fmap = liftM

instance Monad m => Applicative (DelayT m) where
    pure x  = DelayT $ return $ Done x
    (<*>) = ap

instance Monad m => Monad (DelayT m) where
   (DelayT x) >>= k = DelayT $ do
      x' <- x
      case x' of
         Done a -> runDelayT $ k a
         Delay m -> return $ Delay $ m >>= k

instance MonadTrans DelayT where
   lift x = DelayT $ do x <- x
                        return $ Done x

instance MonadState s m => MonadState s (DelayT m) where
   get = lift get
   put x = lift (put x)

instance (GHC.Base.Alternative m, Monad m) => GHC.Base.Alternative (DelayT m) where
   empty = DelayT $ empty
   (DelayT x) <|> y = undefined {- DelayT $ do x <- x
                                  case x of
                                     Done x -> undefine -}

instance MonadPlus m => MonadPlus (DelayT m) where


instance MonadLogic m => MonadLogic (DelayT m) where
