{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module HL.InterleavedTactic where

import GHC.Base (Alternative,(<|>),empty)
import Control.Monad (MonadPlus)
import Control.Monad.Logic.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Debug.Trace (trace)

data Stream a =
    Done
  | Yield a (Stream a)
  | Delay (Stream a)
newtype StreamS s a = StreamS (s -> Stream (a,s))

unStreamS (StreamS x) = x

instance Functor m => Functor (Stream m) where
    fmap f Done = Done
    fmap f (Yield x xs) = Yield (f x) $ fmap f xs
    fmap f (Delay xs) = Delay $ fmap f xs

instance Functor m => Functor (StreamS s m) where
    fmap f (StreamS x) = StreamS $ fmap (fmap f) x

{-
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
-}

instance Applicative m => Applicative (StreamT' m) where
    pure x = Yield (pure x) $ StreamT $ pure Done

    x <*> y = undefined {- joinStreamT' x y -}

instance Applicative m => Applicative (StreamT m) where
    pure x = StreamT $ pure $ pure x

    x <*> y = undefined {- $ joinStreamT x y -}

{-
appendStreamT' :: Monad m => StreamT' m a -> StreamT m a -> m (StreamT' m a)
appendStreamT' Done ys = unStreamT ys
appendStreamT' (Yield x xs) ys =
    return $ Yield x $ StreamT $ do xs' <- unStreamT xs
                                    appendStreamT' xs' ys
appendStreamT' (Delay xs) ys =
    return $ Delay $ StreamT $ do xs' <- unStreamT xs
                                  appendStreamT' xs' ys
-}

flatMapStreamT' :: Monad m => StreamT' m a -> (a -> StreamT m b) -> m (StreamT' m b)
flatMapStreamT' Done _ = return Done
flatMapStreamT' (Yield x xs) ys =
    do x <- x
       x' <- unStreamT $ ys x
       interleaveStreamT' x' $ StreamT $ do xs' <- unStreamT xs
                                            flatMapStreamT' xs' ys
flatMapStreamT' (Delay xs) ys =
    return $ Delay $ StreamT $ do xs' <- unStreamT xs
                                  flatMapStreamT' xs' ys

instance Monad m => Monad (StreamT m) where
    return = pure

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

instance Monad m => MonadPlus (StreamT m) where

splitStreamT' :: Monad m => StreamT' m a -> StreamT' m (Maybe (a, StreamT m a))
splitStreamT' Done = Yield (return Nothing) $ StreamT $ return Done
splitStreamT' (Yield x y) = Yield (do x <- x
                                      return $ Just (x, StreamT $ return $ Delay y)) $ StreamT $ return Done
splitStreamT' (Delay y) = Delay $ StreamT $ do y' <- unStreamT y
                                               return $ splitStreamT' y'

instance Monad m => MonadLogic (StreamT m) where
    msplit (StreamT x) = StreamT $ do x' <- x
                                      return $ splitStreamT' x'

observeAllStreamT :: Monad m => StreamT m a -> m [a]
observeAllStreamT x = do x <- unStreamT x
                         case x of
                            Done -> return []
                            Yield v k ->
                               fmap (\x -> v : x) $ observeAllStreamT k
                            Delay k -> observeAllStreamT k

observeManyStreamT :: Monad m => Int -> StreamT m a -> m [a]
observeManyStreamT i x | i <= 0 = return []
observeManyStreamT i x = do x <- unStreamT x
                            case x of
                               Done -> trace "Done" $ return []
                               Yield v k ->
                                  trace "Yield" $ fmap (\x -> v : x) $ observeManyStreamT (i - 1) k
                               Delay k -> trace "Delay" $ observeManyStreamT i k

delayT :: Monad m => StreamT m a -> StreamT m a
delayT s = StreamT $ return $ Delay $ s

   {-
do xs <- observeAllStreamT x
                            return $ take i xs
-}

instance MonadTrans StreamT where
    lift x = StreamT $ do x <- x
                          return $ Yield x $ StreamT $ return $ Done

instance MonadState s m => MonadState s (StreamT m) where
    get = lift get
    put x = lift $ put x
