{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Data.ByteString.Read.Fractional
    ( -- * functions
      fractional
    , double

     -- * raw functions
    , fractional10
    , fractional'
    ) where

import GHC.TypeLits.Compat
import Data.Proxy.Compat

import Data.ByteString.Read.Class as C

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings
-- >>> import qualified Data.ByteString as S
-- >>> import qualified Data.ByteString.Lazy as L

integral :: forall proxy n r s. (Radix n, ReadFractional r, Ord (Fraction r), Num (Fraction r), Source s)
         => proxy n -> s -> (Fraction r, Int, Int, s)
integral pn = loop 0 0 0
  where
    pr :: Proxy r
    pr = Proxy

    loop !i !d !ad !s
        | C.null s                         = (i, d, ad, s)
        | not (isDigit pn (C.head s))      = (i, d, ad, s)
        | maybe False (i >=) (maxValue pr) = loop i d (ad + 1) (C.tail s)
        | otherwise                        = loop
            (i * fromIntegral (natVal pn) + (fromIntegral $ unsafeToDigit pn (C.head s) :: Fraction r))
            (d+1) ad (C.tail s)
{-# INLINABLE integral #-}

toFractional :: (Radix b, ReadFractional r, Fractional r)
             => proxy b -> Fraction r -> Fraction r -> Int -> Int -> r
toFractional p q r du d = fromFraction q * radix ^ du + fromFraction r / radix ^ d
  where
    radix = fromIntegral (natVal p)
{-# INLINABLE toFractional #-}

-- | convert bytestring into unsigned fractional using radix.
--
-- this function can parse
--
-- * fractional(0.1, 12224.3543)
--
-- >>> fractional' (Proxy :: Proxy 36) "12z" :: Maybe (Double, S.ByteString)
-- Just (1403.0,"")
-- >>> fractional' (Proxy :: Proxy 2) "1012" :: Maybe (Double, L.ByteString)
-- Just (5.0,"2")
-- >>> fractional' (Proxy :: Proxy 10) "a12" :: Maybe (Double, S.ByteString)
-- Nothing
fractional' :: (Radix b, ReadFractional r, Source s) => proxy b -> s -> Maybe (r, s)
fractional' pn s = case integral pn s of
    (_, 0, _,   _) -> Nothing
    (q, _, d, s1)
        | C.null s1        -> Just (fromFraction q * fromIntegral (natVal pn) ^ d, C.empty)
        | C.head s1 /= dot -> Just (fromFraction q, s1)
        | otherwise -> case integral pn (C.tail s1) of
            (_, 0,  _, _)  -> Just (fromFraction q, s1)
            (r, d', _, s2) -> Just (toFractional pn q r d d', s2)
  where
    dot = 46
{-# INLINABLE fractional' #-}

exponential :: forall s. Source s => s -> (Int, s)
exponential s0
    | C.null s0       = (0, s0)
    | isE (C.head s0) = sign (C.tail s0)
    | otherwise       = (0, s0)
  where
    isE w = w == 101 || w == 69

    minus = 45
    plus  = 43

    sign s1
        | C.null s1          = (0, s0)
        | C.head s1 == plus  = expPart $ C.tail s1
        | C.head s1 == minus = let (e, s) = expPart $ C.tail s1 in (-e, s)
        | otherwise          = expPart s1

    expPart s2 = case integral (Proxy :: Proxy 10) s2 :: (Fraction Double, Int, Int, s) of
        (_, 0, _, _) -> (0, s0)
        (e, _, _, s) -> (fromFraction e, s)
{-# INLINABLE exponential #-}

setExpPart :: Fractional f => Int -> f -> f
setExpPart e f
    | e >= 0    = f * 10 ^ e
    | otherwise = f / 10 ^ abs e
{-# SPECIALIZE setExpPart :: Int -> Double -> Double #-}
{-# SPECIALIZE setExpPart :: Int -> Float -> Float #-}
{-# INLINABLE setExpPart #-}

-- | convert bytestring into unsigned fractional using radix.
--
-- this function can parse
--
-- * fractional(0.1, 12224.3543)
-- * exponential (e1, E+2, e-123) (optional)
--
-- >>> fractional10 "12.5" :: Maybe (Double, S.ByteString)
-- Just (12.5,"")
-- >>> fractional10 "124.1e12" :: Maybe (Double, L.ByteString)
-- Just (1.241e14,"")
-- >>> fractional10 "12.5e-3" :: Maybe (Double, S.ByteString)
-- Just (1.25e-2,"")
-- >>> fractional10 "3.11e+3" :: Maybe (Double, L.ByteString)
-- Just (3110.0,"")
fractional10 :: (ReadFractional r, Source s) => s -> Maybe (r, s)
fractional10 s = fractional' (Proxy :: Proxy 10) s >>= \(f, s') ->
    let (e, s'') = exponential s'
    in Just (setExpPart e f, s'')
{-# INLINABLE fractional10 #-}

-- | convert bytestring into unsigned fractional using radix.
--
-- this function can parse
--
-- * oct/hexa-decimal (0o,0O,0x,0X) (optional)
-- * fractional(0.1, 12224.3543)
-- * exponential (e1, E+2, e-123) (10-radixed only, optional)
--
-- >>> fractional "12.4" :: Maybe (Double, S.ByteString)
-- Just (12.4,"")
-- >>> fractional "1.23e12" :: Maybe (Double, L.ByteString)
-- Just (1.23e12,"")
-- >>> fractional "0o0.4" :: Maybe (Double, S.ByteString)
-- Just (0.5,"")
-- >>> fractional "0x3f.12" :: Maybe (Double, L.ByteString)
-- Just (63.0703125,"")
fractional :: (ReadFractional r, Source s) => s -> Maybe (r, s)
fractional s0
    | C.null s0         = Nothing
    | C.head s0 == zero = radix $ C.tail s0
    | otherwise         = fractional10 s0
  where
    zero  = 48
    isX w = w == 120 || w == 88
    isO w = w == 111 || w == 79

    radix s1
        | C.null s1       = Just (0, C.empty)
        | isX (C.head s1) = fractional' (Proxy :: Proxy 16) (C.tail s1)
        | isO (C.head s1) = fractional' (Proxy :: Proxy 8)  (C.tail s1)
        | otherwise       = fractional10 s0
{-# INLINABLE fractional #-}

-- | @
-- double = fractional
-- @
double :: Source s => s -> Maybe (Double, s)
double = fractional 
