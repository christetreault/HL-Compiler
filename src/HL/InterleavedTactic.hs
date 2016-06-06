{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module HL.InterleavedTactic where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import GHC.Base (Alternative,(<|>),empty)
import Control.Monad (MonadPlus)
import Control.Monad.Logic.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Debug.Trace (trace)

data Stream a =
    Done
  | Yield (Maybe a) (Stream a)
  | Delay (Stream a)
newtype StreamS s a = StreamS (s -> Stream (a,s))

unStreamS (StreamS x) = x

instance Functor Stream where
    fmap f Done = Done
    fmap f (Yield x xs) = Yield (fmap f x) $ fmap f xs
    fmap f (Delay xs) = Delay $ fmap f xs

instance Functor (StreamS s) where
    fmap f (StreamS x) = StreamS $ \ s -> fmap (\(a,b) -> (f a, b)) $ x s

instance Applicative (StreamS s) where
    pure x = StreamS $ \ s -> Yield (Just (x,s)) Done
    (<*>) = ap

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream Done ys = ys
interleaveStream (Yield Nothing xs) ys =
    Yield Nothing $ interleaveStream' ys xs
interleaveStream (Yield x xs) ys =
    Yield x $ interleaveStream ys xs
interleaveStream (Delay xs) ys =
    Delay $ interleaveStream ys xs

interleaveStream' :: Stream a -> Stream a -> Stream a
interleaveStream' Done ys = ys
interleaveStream' (Yield Nothing xs) ys =
    Delay $ interleaveStream' ys xs
interleaveStream' (Yield x xs) ys =
    Yield x $ interleaveStream' ys xs
interleaveStream' (Delay xs) ys =
    Delay $ interleaveStream' ys xs

flatMapStream :: Stream (a,s) -> (a -> StreamS s b) -> Stream (b,s)
flatMapStream Done _ = Done
flatMapStream (Yield Nothing xs) ys =
    flatMapStream' xs ys
flatMapStream (Yield (Just (x,s)) xs) ys =
    interleaveStream (unStreamS (ys x) s)
                   $ flatMapStream xs ys
flatMapStream (Delay xs) ys = Delay $ flatMapStream xs ys

flatMapStream' :: Stream (a,s) -> (a -> StreamS s b) -> Stream (b,s)
flatMapStream' Done _ = Done
flatMapStream' (Yield Nothing xs) ys =
    Delay $ flatMapStream' xs ys
flatMapStream' (Yield (Just (x,s)) xs) ys =
    interleaveStream' (unStreamS (ys x) s)
                   $ flatMapStream' xs ys
flatMapStream' (Delay xs) ys = Delay $ flatMapStream' xs ys


instance Monad (StreamS s) where
    StreamS c >>= k = StreamS $ \ s -> flatMapStream (c s) k

instance GHC.Base.Alternative (StreamS s) where
    empty = StreamS $ \ _ -> Yield Nothing Done

    StreamS x <|> y = StreamS $ \ s -> interleaveStream (x s) (unStreamS y s)

instance MonadPlus (StreamS s) where

splitStreamS :: Stream (a,s) -> s -> Stream (Maybe (a, StreamS s a),s)
splitStreamS Done sx = Yield (Just (Nothing,sx)) Done
splitStreamS (Yield (Just (x,s)) y) _ =
    Yield (Just (Just (x, StreamS $ \ _ -> y),s)) Done
splitStreamS (Yield Nothing y) s = Delay $ splitStreamS y s
splitStreamS (Delay y) s = Delay $ splitStreamS y s

instance MonadLogic (StreamS s) where
    msplit (StreamS x) = StreamS $ \ s -> splitStreamS (x s) s
    interleave m1 m2 = StreamS $ \ s ->
                       unStreamS m1 s `interleaveStream` unStreamS m2 s

instance MonadState s (StreamS s) where
    get = StreamS $ \ s -> Yield (Just (s,s)) Done
    put s = StreamS $ \ _ -> Yield (Just ((),s)) Done

observeAllStreamS :: StreamS s a -> s -> [(a,s)]
observeAllStreamS x s = observeAll $ unStreamS x s
    where
      observeAll Done = []
      observeAll (Yield (Just x) xs) = x : observeAll xs
      observeAll (Yield Nothing xs) = observeAll xs
      observeAll (Delay xs) = observeAll xs

observeManyStreamS :: Int -> StreamS s a -> s -> [(a,s)]
observeManyStreamS i x s = observeMany i $ unStreamS x s
    where
      observeMany i Done = []
      observeMany i _ | i <= 0 = []
      observeMany i (Delay xs) = observeMany i xs
      observeMany i (Yield (Just x) xs) = x : observeMany (i - 1) xs
      observeMany i (Yield Nothing xs) = observeMany i xs

delayT :: StreamS s a -> StreamS s a
delayT x = StreamS $ \ s -> Delay $ unStreamS x s
