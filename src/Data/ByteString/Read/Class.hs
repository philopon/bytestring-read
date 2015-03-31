{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Data.ByteString.Read.Class
    ( ReadFractional(..)
    , Radix(..)
    , Source(..)
    ) where

import Data.Word
import GHC.TypeLits.Compat
import qualified Data.ByteString as S
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as L

class (Fractional a, Num (Fraction a), Ord (Fraction a)) => ReadFractional a where
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

instance ReadFractional Float where
    newtype Fraction Float = FractionFloat Word32
        deriving(Eq, Ord, Num)

    maxValue _ = let t = 0 :: Float in Just $ fromIntegral (floatRadix t) ^ floatDigits t
    fromFraction (FractionFloat a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE fromFraction #-}

instance ReadFractional Double where
    newtype Fraction Double = FractionDouble Word64
        deriving(Eq, Ord, Num)

    maxValue _ = let t = 0 :: Double in Just $ fromIntegral (floatRadix t) ^ floatDigits t
    fromFraction (FractionDouble a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE fromFraction #-}

instance ReadFractional Rational where
    newtype Fraction Rational = WordRational Integer
        deriving(Eq, Ord, Num)

    maxValue _ = Nothing
    fromFraction (WordRational a) = fromIntegral a

    {-# INLINE maxValue #-}
    {-# INLINE fromFraction #-}

class KnownNat n => Radix n where
    -- | check input Word8 is digit charactor or not.
    isDigit :: proxy n -> Word8 -> Bool

    -- | convert digit charactor to number.
    -- undefined behaviour when give non-digit charactor.
    unsafeToDigit :: proxy n -> Word8 -> Word8

#define defineRadixUnder10(RADIX, MAX)\
instance Radix RADIX where;\
    {-# INLINE isDigit #-};\
    {-# INLINE unsafeToDigit #-};\
    isDigit _ = \w -> 48 <= w && w <= MAX;\
    unsafeToDigit _ w = w - 48

defineRadixUnder10( 2, 49)
defineRadixUnder10( 3, 50)
defineRadixUnder10( 4, 51)
defineRadixUnder10( 5, 52)
defineRadixUnder10( 6, 53)
defineRadixUnder10( 7, 54)
defineRadixUnder10( 8, 55)
defineRadixUnder10( 9, 56)
defineRadixUnder10(10, 57)

#define defineRadixOver10(RADIX, MAXu, MAXl)\
instance Radix RADIX where;\
    {-# INLINE isDigit #-};\
    {-# INLINE unsafeToDigit #-};\
    isDigit _ = \w -> 48 <= w && w <= 57 || 65 <= w && w <= MAXu || 97 <= w && w <= MAXl;\
    unsafeToDigit _ w = if 48 <= w && w <= 57;\
                        then fromIntegral w - 48;\
                        else if 65 <= w && w <= 90;\
                             then fromIntegral w - 55;\
                             else fromIntegral w - 87

defineRadixOver10(11, 65, 97)
defineRadixOver10(12, 66, 98)
defineRadixOver10(13, 67, 99)
defineRadixOver10(14, 68, 100)
defineRadixOver10(15, 69, 101)
defineRadixOver10(16, 70, 102)
defineRadixOver10(17, 71, 103)
defineRadixOver10(18, 72, 104)
defineRadixOver10(19, 73, 105)
defineRadixOver10(20, 74, 106)
defineRadixOver10(21, 75, 107)
defineRadixOver10(22, 76, 108)
defineRadixOver10(23, 77, 109)
defineRadixOver10(24, 78, 110)
defineRadixOver10(25, 79, 111)
defineRadixOver10(26, 80, 112)
defineRadixOver10(27, 81, 113)
defineRadixOver10(28, 82, 114)
defineRadixOver10(29, 83, 115)
defineRadixOver10(30, 84, 116)
defineRadixOver10(31, 85, 117)
defineRadixOver10(32, 86, 118)
defineRadixOver10(33, 87, 119)
defineRadixOver10(34, 88, 120)
defineRadixOver10(35, 89, 121)
defineRadixOver10(36, 90, 122)

class Source a where
    null  :: a -> Bool
    empty :: a
    head  :: a -> Word8
    tail  :: a -> a

instance Source S.ByteString where
    null  = S.null
    empty = S.empty
    head  = unsafeHead
    tail  = unsafeTail
    {-# INLINE null #-}
    {-# INLINE head #-}
    {-# INLINE tail #-}

instance Source L.ByteString where
    null  = L.null
    empty = L.empty
    head  = L.head
    tail  = L.tail
    {-# INLINE null #-}
    {-# INLINE head #-}
    {-# INLINE tail #-}
