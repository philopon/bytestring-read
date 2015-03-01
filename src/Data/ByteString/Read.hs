{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Data.ByteString.Read
    ( -- * classes
      EffectiveDigit(..)
    , Base(..)
      -- * types
    , ReadFloating
     
     -- * functions
    , floating
    , floating10
    , floating'
    , signed
    ) where

import Control.Arrow(first)
import Control.Applicative((<$>))

import Data.ByteString(ByteString)
import qualified Data.ByteString as S
import Data.Word
import GHC.TypeLits
import Data.Proxy
import Data.ByteString.Unsafe

minus :: Word8
minus = 45
{-# INLINE minus #-}

plus :: Word8
plus = 43
{-# INLINE plus #-}


class EffectiveDigit a where
    data FractionWord a

    maxValue :: proxy a -> FractionWord a

    unFractionWord :: FractionWord a -> a
    fractionWordAsInt :: FractionWord a -> Int

instance EffectiveDigit Float where
    newtype FractionWord Float = WordFloat Word32
        deriving(Show, Eq, Ord, Num)

    maxValue _ = let t = 0 :: Float in fromIntegral (floatRadix t) ^ floatDigits t

    unFractionWord (WordFloat a) = fromIntegral a
    fractionWordAsInt (WordFloat a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE unFractionWord #-}
    {-# INLINE fractionWordAsInt #-}

instance EffectiveDigit Double where
    newtype FractionWord Double = WordDouble Word64
        deriving(Show, Eq, Ord, Num)

    maxValue _ = let t = 0 :: Double in fromIntegral (floatRadix t) ^ floatDigits t

    unFractionWord (WordDouble a) = fromIntegral a
    fractionWordAsInt (WordDouble a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE unFractionWord #-}
    {-# INLINE fractionWordAsInt #-}

class KnownNat n => Base n where
    isDigit :: proxy n -> Word8 -> Bool
    unsafeToDigit :: proxy n -> Word8 -> Word8

instance Base 8 where
    isDigit _ = \w -> 48 <= w && w <= 55
    unsafeToDigit _ w = w - 48
    {-# INLINE isDigit #-}
    {-# INLINE unsafeToDigit #-}

instance Base 10 where
    isDigit _ = \w -> 48 <= w && w <= 57
    unsafeToDigit _ w = w - 48
    {-# INLINE isDigit #-}
    {-# INLINE unsafeToDigit #-}

instance Base 16 where
    isDigit _ = \w -> 48 <= w && w <= 57 || 65 <= w && w <= 70 || 97 <= w && w <= 102
    unsafeToDigit _ w
        | 48 <= w && w <= 57 = fromIntegral w - 48
        | 65 <= w && w <= 70 = fromIntegral w - 55
        | otherwise          = fromIntegral w - 87
    {-# INLINE isDigit #-}
    {-# INLINE unsafeToDigit #-}

type ReadFloating r = (EffectiveDigit r, Ord (FractionWord r), Num (FractionWord r), Fractional r)

integral :: forall proxy n r. (Base n, EffectiveDigit r, Ord (FractionWord r), Num (FractionWord r))
         => proxy n -> ByteString -> (FractionWord r, Int, Int, ByteString)
integral pn = loop 0 0 0
  where
    pr :: Proxy r
    pr = Proxy

    loop !i !d !ad !s
        | S.null s                        = (i, d, ad, s)
        | not (isDigit pn (unsafeHead s)) = (i, d, ad, s)
        | i >= maxValue pr                = loop i d (ad + 1) (unsafeTail s)
        | otherwise                       = loop
            (i * fromIntegral (natVal pn) + (fromIntegral $ unsafeToDigit pn (unsafeHead s) :: FractionWord r))
            (d+1) (ad + 1) (unsafeTail s)
{-# INLINABLE integral #-}

toFractional :: (Base b, EffectiveDigit r, Fractional r)
             => proxy b -> FractionWord r -> FractionWord r -> Int -> r
toFractional p q r d = unFractionWord q + unFractionWord r / exp_
  where
    exp_ = fromIntegral (natVal p) ^ d
{-# INLINABLE toFractional #-}

floating' :: (Base b, ReadFloating r) => proxy b -> ByteString -> Maybe (r, ByteString)
floating' pn s = case integral pn s of
    (_, 0, _,   _) -> Nothing
    (q, d, ad, "") -> Just (unFractionWord q * fromIntegral (natVal pn) ^ (ad - d), "")
    (q, _, _,  s1)
        | unsafeHead s1 /= dot -> Just (unFractionWord q, s1)
        | otherwise -> case integral pn (unsafeTail s1) of
            (_, 0, _, _)  -> Just (unFractionWord q, s1)
            (r, d, _, s2) -> Just (toFractional pn q r d, s2)
  where
    dot = 46
{-# INLINABLE floating' #-}

exponential :: forall proxy r. (EffectiveDigit r, Ord (FractionWord r), Num (FractionWord r))
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

    expPart s2 = case integral (Proxy :: Proxy 10) s2 :: (FractionWord r, Int, Int, ByteString) of
        (_, 0, _, _) -> (0, s0)
        (e, _, _, s) -> (fractionWordAsInt e, s)
{-# INLINABLE exponential #-}

setExpPart :: Fractional f => Int -> f -> f
setExpPart e f
    | e >= 0    = f * 10 ^ e
    | otherwise = f / 10 ^ abs e
{-# SPECIALIZE setExpPart :: Int -> Double -> Double #-}
{-# SPECIALIZE setExpPart :: Int -> Float -> Float #-}
{-# INLINABLE setExpPart #-}

floating10 :: forall r. ReadFloating r => ByteString -> Maybe (r, ByteString)
floating10 s = floating' (Proxy :: Proxy 10) s >>= \(f, s') ->
    let (e, s'') = exponential p s'
    in Just (setExpPart e f, s'')
  where
    p :: Proxy r
    p = Proxy
{-# INLINABLE floating10 #-}

floating :: ReadFloating r => ByteString -> Maybe (r, ByteString)
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

signed :: Num r => (ByteString -> Maybe (r, ByteString)) -> ByteString -> Maybe (r, ByteString)
signed f s
    | S.null s = Nothing
    | unsafeHead s == minus = first negate <$> f (unsafeTail s)
    | unsafeHead s == plus  = f (unsafeTail s)
    | otherwise = f s
