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

import Data.ByteString.Unsafe
import Data.ByteString(ByteString)
import qualified Data.ByteString as S

import GHC.TypeLits.Compat
import Data.Proxy.Compat

import Data.ByteString.Read.Class

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings

integral :: forall proxy n r. (Radix n, ReadFractional r, Ord (Fraction r), Num (Fraction r))
         => proxy n -> ByteString -> (Fraction r, Int, Int, ByteString)
integral pn = loop 0 0 0
  where
    pr :: Proxy r
    pr = Proxy

    loop !i !d !ad !s
        | S.null s                         = (i, d, ad, s)
        | not (isDigit pn (unsafeHead s))  = (i, d, ad, s)
        | maybe False (i >=) (maxValue pr) = loop i d (ad + 1) (unsafeTail s)
        | otherwise                        = loop
            (i * fromIntegral (natVal pn) + (fromIntegral $ unsafeToDigit pn (unsafeHead s) :: Fraction r))
            (d+1) ad (unsafeTail s)
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
-- >>> fractional' (Proxy :: Proxy 36) "12z" :: Maybe (Double, ByteString)
-- Just (1403.0,"")
-- >>> fractional' (Proxy :: Proxy 2) "1012" :: Maybe (Double, ByteString)
-- Just (5.0,"2")
-- >>> fractional' (Proxy :: Proxy 10) "a12" :: Maybe (Double, ByteString)
-- Nothing
fractional' :: (Radix b, ReadFractional r) => proxy b -> ByteString -> Maybe (r, ByteString)
fractional' pn s = case integral pn s of
    (_, 0, _,   _) -> Nothing
    (q, _, d, "") -> Just (fromFraction q * fromIntegral (natVal pn) ^ d, "")
    (q, _, d, s1)
        | unsafeHead s1 /= dot -> Just (fromFraction q, s1)
        | otherwise -> case integral pn (unsafeTail s1) of
            (_, 0,  _, _)  -> Just (fromFraction q, s1)
            (r, d', _, s2) -> Just (toFractional pn q r d d', s2)
  where
    dot = 46
{-# INLINABLE fractional' #-}

exponential :: ByteString -> (Int, ByteString)
exponential s0
    | S.null s0           = (0, s0)
    | isE (unsafeHead s0) = sign (unsafeTail s0)
    | otherwise           = (0, s0)
  where
    isE w = w == 101 || w == 69

    minus = 45
    plus  = 43

    sign s1
        | S.null s1              = (0, s0)
        | unsafeHead s1 == plus  = expPart $ unsafeTail s1
        | unsafeHead s1 == minus = let (e, s) = expPart $ unsafeTail s1 in (-e, s)
        | otherwise              = expPart s1

    expPart s2 = case integral (Proxy :: Proxy 10) s2 :: (Fraction Double, Int, Int, ByteString) of
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
-- >>> fractional10 "12.5" :: Maybe (Double, ByteString)
-- Just (12.5,"")
-- >>> fractional10 "124.1e12" :: Maybe (Double, ByteString)
-- Just (1.241e14,"")
-- >>> fractional10 "12.5e-3" :: Maybe (Double, ByteString)
-- Just (1.25e-2,"")
-- >>> fractional10 "3.11e+3" :: Maybe (Double, ByteString)
-- Just (3110.0,"")
fractional10 :: ReadFractional r => ByteString -> Maybe (r, ByteString)
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
-- >>> fractional "12.4" :: Maybe (Double, ByteString)
-- Just (12.4,"")
-- >>> fractional "1.23e12" :: Maybe (Double, ByteString)
-- Just (1.23e12,"")
-- >>> fractional "0o0.4" :: Maybe (Double, ByteString)
-- Just (0.5,"")
-- >>> fractional "0x3f.12" :: Maybe (Double, ByteString)
-- Just (63.0703125,"")
fractional :: ReadFractional r => ByteString -> Maybe (r, ByteString)
fractional s0
    | S.null s0             = Nothing
    | unsafeHead s0 == zero = radix $ unsafeTail s0
    | otherwise             = fractional10 s0
  where
    zero  = 48
    isX w = w == 120 || w == 88
    isO w = w == 111 || w == 79

    radix s1
        | S.null s1           = Just (0, "")
        | isX (unsafeHead s1) = fractional' (Proxy :: Proxy 16) (unsafeTail s1)
        | isO (unsafeHead s1) = fractional' (Proxy :: Proxy 8)  (unsafeTail s1)
        | otherwise           = fractional10 s0
{-# INLINABLE fractional #-}

-- | @
-- double = fractional
-- @
double :: ByteString -> Maybe (Double, ByteString)
double = fractional 
