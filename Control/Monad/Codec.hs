{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | The Codec monad provides functions for encoding and decoding
-- complex data structures with unique integer numbers.  In the
-- simplest case the entire sturecture can be transformed to unique
-- atom (see 'example1' below).  When it is not sufficient to encode
-- the input object with one codec, more complex codec structure can
-- be used (see 'example2' below).  In order to simplify codec
-- manipulations the library relies on a 'lens' package  which provides
-- lenses for typical data types.
--
-- Example:
--
-- > example1 = evalCodec empty $ do
-- >     let xs = "abcabd"
-- >     ys <- mapM (encode id) xs
-- >     zs <- mapM (decode id) ys
-- >     return $ zip zs ys
--
-- >>> example1
-- >>> [('a',0),('b',1),('c',2),('a',0),('b',1),('d',3)]
--
-- > example2 = evalCodec (empty, empty) $ do
-- >     let xs = zip "abcabd" [1, 34342, 5435, 34342, 124, 1]
-- >     ys <- forM xs $ \(x, y) ->
-- >         (,) <$> encode _1 x <*> encode _2 y
-- >     zs <- forM ys $ \(i, j) -> 
-- >         (,) <$> decode _1 i <*> decode _2 j
-- >     return (zs, ys)
--
-- >>> fst example2
-- >>> [('a',1),('b',34342),('c',5435),('a',34342),('b',124),('d',1)]
-- >>> snd example2
-- >>> [(0,0),(1,1),(2,2),(0,1),(1,3),(3,0)]

module Control.Monad.Codec
( Codec ()
, AtomCodec (..)
, empty
, AtomLens
, maybeEncode
, encode
, maybeDecode
, decode

, runCodec
, evalCodec
, execCodec
) where

import Control.Monad (forM)

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Lens (Simple, Lens, view, set, _1, _2)
import qualified Control.Monad.State as S
import qualified Data.Map as M
import qualified Data.IntMap as I

newtype Codec c a = Codec
    { unCodec :: S.State c a }
    deriving (Functor, Applicative, Monad)

{-# INLINE getCodec #-}
getCodec :: Codec c c
getCodec = Codec S.get

{-# INLINE setCodec #-}
setCodec :: c -> Codec c ()
setCodec codec = Codec (S.put codec)

data AtomCodec a = AtomCodec
    { to    :: !(M.Map a Int)
    , from  :: !(I.IntMap a) }

-- | Empty atom codec.
empty :: AtomCodec a
empty = AtomCodec M.empty I.empty

updateMap :: Ord a => M.Map a Int -> a -> M.Map a Int
updateMap mp x =
  case M.lookup x mp of
    Just k  -> mp
    Nothing -> M.insert x n mp
  where
    !n = M.size mp

type AtomLens c a = Simple Lens c (AtomCodec a)

maybeEncode :: Ord a => AtomLens c a -> a -> Codec c (Maybe Int)
maybeEncode lens x = 
    M.lookup x . to . view lens <$> getCodec

encode :: Ord a => AtomLens c a -> a -> Codec c Int
encode lens x = do
    codec <- getCodec
    let atomCodec = view lens codec
        m' = updateMap (to atomCodec) x
        y  = m' M.! x
        r' = I.insert y x (from atomCodec)
        codec' = set lens (AtomCodec m' r') codec
    setCodec codec'
    return y

-- | Version of encode which doesn't update the return componenent
-- of the atom codec.  It is useful when we know that particular
-- value (e.g. value of a condition observation) won't be decoded
-- afterwards so there is no need to store it and waste memory.
encode' :: Ord a => AtomLens c a -> a -> Codec c Int
encode' lens x = do
    codec <- getCodec
    let atomCodec = view lens codec
        m' = updateMap (to atomCodec) x
        y  = m' M.! x
        codec' = set lens (atomCodec { to = m' }) codec
    setCodec codec'
    return y

maybeDecode :: Ord a => AtomLens c a -> Int -> Codec c (Maybe a)
maybeDecode lens i = 
    I.lookup i . from . view lens <$> getCodec

decode :: Ord a => AtomLens c a -> Int -> Codec c a
decode lens i = maybeDecode lens i >>= \mx -> case mx of
    Just x  -> return x
    Nothing -> error $ "decode: no " ++ show i ++ " key"

runCodec :: c -> Codec c a -> (a, c)
runCodec codec (Codec state) = S.runState state codec

evalCodec :: c -> Codec c a -> a
evalCodec codec (Codec state) = S.evalState state codec

execCodec :: c -> Codec c a -> c
execCodec codec (Codec state) = S.execState state codec

-- example1 = evalCodec empty $ do
--     let xs = "abcabd"
--     ys <- mapM (encode id) xs
--     zs <- mapM (decode id) ys
--     return $ zip zs ys
-- 
-- example2 = evalCodec (empty, empty) $ do
--     let xs = zip "abcabd" [1, 34342, 5435, 34342, 124, 1]
--     ys <- forM xs $ \(x, y) ->
--         (,) <$> encode _1 x <*> encode _2 y
--     zs <- forM ys $ \(i, j) -> 
--         (,) <$> decode _1 i <*> decode _2 j
--     return (zs, ys)
