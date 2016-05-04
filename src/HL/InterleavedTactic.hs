{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HL.Compile where

import GHC.Base (Alternative,(<|>),empty)
import Control.Monad (MonadPlus)
import Control.Monad.Logic.Class

data StreamT' m a =
    Done
  | Yield a (StreamT m a)
  | Delay (StreamT m a)
newtype StreamT m a = StreamT (m (StreamT' m a))

unStreamT (StreamT x) = x

instance Functor m => Functor (StreamT' m) where
    fmap f Done = Done
    fmap f (Yield x xs) = Yield (f x) $ fmap f xs
    fmap f (Delay xs) = Delay $ fmap f xs

instance Functor m => Functor (StreamT m) where
    fmap f (StreamT x) = StreamT $ fmap (fmap f) x

joinStreamT' :: Applicative m => StreamT' m (a -> b) -> StreamT' m a -> StreamT' m b
joinStreamT' Done _ = Done
joinStreamT' _ Done = Done
joinStreamT' (Yield f fs) (Yield x xs) = Yield (f x) $ joinStreamT fs xs
joinStreamT' (Delay x) (Delay y) = Delay $ x <*> y
joinStreamT' (Yield f fs) (Delay (StreamT xs)) = Delay $ StreamT $ fmap (joinRight f fs) xs
    where
      joinRight f fs Done = Done
      joinRight f fs (Yield x xs) = Yield (f x) (joinStreamT fs xs)
      joinRight f fs (Delay (StreamT xs)) = Delay $ StreamT $ fmap (joinRight f fs) xs

joinStreamT' (Delay (StreamT fs)) (Yield x xs) = Delay $ StreamT $ fmap (joinLeft x xs) fs
    where
      joinLeft x xs Done = Done
      joinLeft x xs (Yield f fs) = Yield (f x) (joinStreamT fs xs)
      joinLeft x xs (Delay (StreamT fs)) = Delay $ StreamT $ fmap (joinLeft x xs) fs

joinStreamT :: Applicative m => StreamT m (a -> b) -> StreamT m a -> StreamT m b
joinStreamT (StreamT x) (StreamT y) = StreamT $ fmap joinStreamT' x <*> y

instance Applicative m => Applicative (StreamT' m) where
    pure x = Yield x $ StreamT $ pure Done

    x <*> y = joinStreamT' x y

instance Applicative m => Applicative (StreamT m) where
    pure x = StreamT $ pure $ pure x

    x <*> y = joinStreamT x y

appendStreamT' :: Monad m => StreamT' m a -> StreamT m a -> m (StreamT' m a)
appendStreamT' Done ys = unStreamT ys
appendStreamT' (Yield x xs) ys =
    return $ Yield x $ StreamT $ do xs' <- unStreamT xs
                                    appendStreamT' xs' ys
appendStreamT' (Delay xs) ys =
    return $ Delay $ StreamT $ do xs' <- unStreamT xs
                                  appendStreamT' xs' ys

flatMapStreamT' :: Monad m => StreamT' m a -> (a -> StreamT m b) -> m (StreamT' m b)
flatMapStreamT' Done _ = return Done
flatMapStreamT' (Yield x xs) ys =
    do x' <- unStreamT $ ys x
       appendStreamT' x' $ StreamT $ do xs' <- unStreamT xs
                                        flatMapStreamT' xs' ys
flatMapStreamT' (Delay xs) ys =
    return $ Delay $ StreamT $ do xs' <- unStreamT xs
                                  flatMapStreamT' xs' ys

instance Monad m => Monad (StreamT m) where
    return x = pure x

    StreamT c >>= k = StreamT $ do x <- c
                                   flatMapStreamT' x k

interleaveStreamT' :: Monad m => StreamT' m a -> StreamT m a -> m (StreamT' m a)
interleaveStreamT' Done ys = unStreamT ys
interleaveStreamT' (Yield x xs) ys =
    return $ Yield x $ StreamT $ do ys' <- unStreamT ys
                                    interleaveStreamT' ys' xs
interleaveStreamT' (Delay xs) ys =
    return $ Delay $ StreamT $ do ys' <- unStreamT ys
                                  interleaveStreamT' ys' xs

instance Monad m => GHC.Base.Alternative (StreamT m) where
    empty = StreamT $ pure $ Done

    StreamT x <|> y = StreamT $ do x' <- x
                                   interleaveStreamT' x' y

instance MonadPlus m => MonadPlus (StreamT m) where

splitStreamT' :: Monad m => StreamT' m a -> StreamT' m (Maybe (a, StreamT m a))
splitStreamT' Done = Yield Nothing $ StreamT $ return Done
splitStreamT' (Yield x y) = Yield (Just (x, StreamT $ return $ Delay y)) $ StreamT $ return Done
splitStreamT' (Delay y) = Delay $ StreamT $ do y' <- unStreamT y
                                               return $ splitStreamT' y'

instance MonadLogic m => MonadLogic (StreamT m) where
    msplit (StreamT x) = StreamT $ do x' <- x
                                      return $ splitStreamT' x'
