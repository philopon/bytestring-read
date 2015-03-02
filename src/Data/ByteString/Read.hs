{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Data.ByteString.Read
    ( -- * functions
      floating
    , double
    , signed

    -- * classes
    , EffectiveDigit(..)
    , Base(..)

     -- * raw functions
    , floating10
    , floating'
    ) where

import Control.Arrow(first)
import Control.Applicative((<$>))

import Data.ByteString.Unsafe
import Data.ByteString(ByteString)
import qualified Data.ByteString as S
import Data.Word

import GHC.TypeLits.Compat
import Data.Proxy.Compat

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings

minus :: Word8
minus = 45
{-# INLINE minus #-}

plus :: Word8
plus = 43
{-# INLINE plus #-}


class (Fractional a, Num (Fraction a), Ord (Fraction a)) => EffectiveDigit a where
    -- | data type to store fractional part of floating
    data Fraction a

    -- | maximum value of fractional part.
    --
    -- Nothing if arbitrary-precision.
    -- 
    -- @
    -- Just $ fromIntegral (floatRadix t) ^ floatDigits t
    -- @
    maxValue :: proxy a -> Maybe (Fraction a)

    -- | lifted fromIntegral
    fromFraction :: Num b => Fraction a -> b

instance EffectiveDigit Float where
    newtype Fraction Float = FractionFloat Word32
        deriving(Eq, Ord, Num)

    maxValue _ = let t = 0 :: Float in Just $ fromIntegral (floatRadix t) ^ floatDigits t
    fromFraction (FractionFloat a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE fromFraction #-}

instance EffectiveDigit Double where
    newtype Fraction Double = FractionDouble Word64
        deriving(Eq, Ord, Num)

    maxValue _ = let t = 0 :: Double in Just $ fromIntegral (floatRadix t) ^ floatDigits t
    fromFraction (FractionDouble a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE fromFraction #-}

instance EffectiveDigit Rational where
    newtype Fraction Rational = WordRational Integer
        deriving(Eq, Ord, Num)

    maxValue _ = Nothing
    fromFraction (WordRational a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE fromFraction #-}

class KnownNat n => Base n where
    -- | check input Word8 is digit charactor or not.
    isDigit :: proxy n -> Word8 -> Bool

    -- | convert digit charactor to number.
    -- undefined behaviour when give non-digit charactor.
    unsafeToDigit :: proxy n -> Word8 -> Word8

#define defineBaseUnder10(BASE, MAX)\
instance Base BASE where;\
    {-# INLINE isDigit #-};\
    {-# INLINE unsafeToDigit #-};\
    isDigit _ = \w -> 48 <= w && w <= MAX;\
    unsafeToDigit _ w = w - 48

defineBaseUnder10( 2, 49)
defineBaseUnder10( 3, 50)
defineBaseUnder10( 4, 51)
defineBaseUnder10( 5, 52)
defineBaseUnder10( 6, 53)
defineBaseUnder10( 7, 54)
defineBaseUnder10( 8, 55)
defineBaseUnder10( 9, 56)
defineBaseUnder10(10, 57)

#define defineBaseOver10(BASE, MAXu, MAXl)\
instance Base BASE where;\
    {-# INLINE isDigit #-};\
    {-# INLINE unsafeToDigit #-};\
    isDigit _ = \w -> 48 <= w && w <= 57 || 65 <= w && w <= MAXu || 97 <= w && w <= MAXl;\
    unsafeToDigit _ w = if\
        | 48 <= w && w <= 57 -> fromIntegral w - 48;\
        | 65 <= w && w <= 90 -> fromIntegral w - 55;\
        | otherwise          -> fromIntegral w - 87

defineBaseOver10(11, 65, 97)
defineBaseOver10(12, 66, 98)
defineBaseOver10(13, 67, 99)
defineBaseOver10(14, 68, 100)
defineBaseOver10(15, 69, 101)
defineBaseOver10(16, 70, 102)
defineBaseOver10(17, 71, 103)
defineBaseOver10(18, 72, 104)
defineBaseOver10(19, 73, 105)
defineBaseOver10(20, 74, 106)
defineBaseOver10(21, 75, 107)
defineBaseOver10(22, 76, 108)
defineBaseOver10(23, 77, 109)
defineBaseOver10(24, 78, 110)
defineBaseOver10(25, 79, 111)
defineBaseOver10(26, 80, 112)
defineBaseOver10(27, 81, 113)
defineBaseOver10(28, 82, 114)
defineBaseOver10(29, 83, 115)
defineBaseOver10(30, 84, 116)
defineBaseOver10(31, 85, 117)
defineBaseOver10(32, 86, 118)
defineBaseOver10(33, 87, 119)
defineBaseOver10(34, 88, 120)
defineBaseOver10(35, 89, 121)
defineBaseOver10(36, 90, 122)


integral :: forall proxy n r. (Base n, EffectiveDigit r, Ord (Fraction r), Num (Fraction r))
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

toFractional :: (Base b, EffectiveDigit r, Fractional r)
             => proxy b -> Fraction r -> Fraction r -> Int -> Int -> r
toFractional p q r du d = fromFraction q * base ^ du + fromFraction r / base ^ d
  where
    base = fromIntegral (natVal p)
{-# INLINABLE toFractional #-}

-- | convert bytestring into unsigned floating using radix.
--
-- this function can parse
--
-- * floating(0.1, 12224.3543)
--
-- >>> floating' (Proxy :: Proxy 36) "12z"
-- Just (1403.0,"")
-- >>> floating' (Proxy :: Proxy 2) "1012"
-- Just (5.0,"2")
-- >>> floating' (Proxy :: Proxy 10) "a12"
-- Nothing
floating' :: (Base b, EffectiveDigit r) => proxy b -> ByteString -> Maybe (r, ByteString)
floating' pn s = case integral pn s of
    (_, 0, _,   _) -> Nothing
    (q, _, d, "") -> Just (fromFraction q * fromIntegral (natVal pn) ^ d, "")
    (q, _, d, s1)
        | unsafeHead s1 /= dot -> Just (fromFraction q, s1)
        | otherwise -> case integral pn (unsafeTail s1) of
            (_, 0,  _, _)  -> Just (fromFraction q, s1)
            (r, d', _, s2) -> Just (toFractional pn q r d d', s2)
  where
    dot = 46
{-# INLINABLE floating' #-}

exponential :: forall proxy r. (EffectiveDigit r, Ord (Fraction r), Num (Fraction r))
            => proxy r -> ByteString -> (Int, ByteString)
exponential _ s0
    | S.null s0           = (0, s0)
    | isE (unsafeHead s0) = sign (unsafeTail s0)
    | otherwise           = (0, s0)
  where
    isE w = w == 101 || w == 69

    sign s1
        | S.null s1              = (0, s0)
        | unsafeHead s1 == plus  = expPart $ unsafeTail s1
        | unsafeHead s1 == minus = let (e, s) = expPart $ unsafeTail s1 in (-e, s)
        | otherwise              = expPart s1

    expPart s2 = case integral (Proxy :: Proxy 10) s2 :: (Fraction r, Int, Int, ByteString) of
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

-- | convert bytestring into unsigned floating using radix.
--
-- this function can parse
--
-- * floating(0.1, 12224.3543)
-- * exponential (e1, E+2, e-123) (optional)
--
-- >>> floating10 "12.5"
-- Just (12.5,"")
-- >>> floating10 "124.1e12"
-- Just (1.241e14,"")
-- >>> floating10 "12.5e-3"
-- Just (1.25e-2,"")
-- >>> floating10 "3.11e+3"
-- Just (3110.0,"")
floating10 :: forall r. EffectiveDigit r => ByteString -> Maybe (r, ByteString)
floating10 s = floating' (Proxy :: Proxy 10) s >>= \(f, s') ->
    let (e, s'') = exponential (Proxy :: Proxy r) s'
    in Just (setExpPart e f, s'')
{-# INLINABLE floating10 #-}

-- | convert bytestring into unsigned floating using radix.
--
-- this function can parse
--
-- * oct/hexa-decimal (0o,0O,0x,0X) (optional)
-- * floating(0.1, 12224.3543)
-- * exponential (e1, E+2, e-123) (10-radixed only, optional)
--
-- >>> floating "12.4"
-- Just (12.4,"")
-- >>> floating "1.23e12"
-- Just (1.23e12,"")
-- >>> floating "0o0.4"
-- Just (0.5,"")
-- >>> floating "0x3f.12"
-- Just (63.0703125,"")
floating :: EffectiveDigit r => ByteString -> Maybe (r, ByteString)
floating s0
    | S.null s0             = Nothing
    | unsafeHead s0 == zero = base $ unsafeTail s0
    | otherwise             = floating10 s0
  where
    zero  = 48
    isX w = w == 120 || w == 88
    isO w = w == 111 || w == 79

    base s1
        | S.null s1           = Just (0, "")
        | isX (unsafeHead s1) = floating' (Proxy :: Proxy 16) (unsafeTail s1)
        | isO (unsafeHead s1) = floating' (Proxy :: Proxy 8)  (unsafeTail s1)
        | otherwise           = floating10 s0
{-# INLINABLE floating #-}

-- | @
-- double = floating
-- @
double :: ByteString -> Maybe (Double, ByteString)
double = floating

-- | convert unsigned parser to signed parser.
--
-- this function can parse
--
-- * sign (+, -) (optional)
--
-- >>> signed double "12.4"
-- Just (12.4,"")
-- >>> signed double "-3.21e3"
-- Just (-3210.0,"")
-- >>> signed double "+0x1f.4"
-- Just (31.25,"")
signed :: Num r => (ByteString -> Maybe (r, ByteString)) -> ByteString -> Maybe (r, ByteString)
signed f s
    | S.null s = Nothing
    | unsafeHead s == minus = first negate <$> f (unsafeTail s)
    | unsafeHead s == plus  = f (unsafeTail s)
    | otherwise = f s
