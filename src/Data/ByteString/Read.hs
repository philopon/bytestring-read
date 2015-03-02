{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Data.ByteString.Read
    ( -- * fractional
      ReadFractional
    , fractional
    , double
      -- * common
    , signed

      -- * DEPRECATED
    , floating
    , EffectiveDigit
    , Base
    , floating10
    , floating'
    ) where

import Control.Applicative
import Control.Arrow

import Data.ByteString(ByteString)
import Data.ByteString.Unsafe
import qualified Data.ByteString as S

import Data.ByteString.Read.Class
import Data.ByteString.Read.Fractional

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings

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
  where
    minus = 45
    plus  = 43

{-# DEPRECATED EffectiveDigit "use ReadFractional" #-}
type EffectiveDigit = ReadFractional

{-# DEPRECATED floating "use fractional" #-}
floating :: EffectiveDigit r => ByteString -> Maybe (r, ByteString)
floating = fractional

{-# DEPRECATED Base "use Radix" #-}
type Base = Radix

{-# DEPRECATED floating10 "use fractional10" #-}
floating10 :: forall r. EffectiveDigit r => ByteString -> Maybe (r, ByteString) 
floating10 = fractional10

{-# DEPRECATED floating' "use floating'" #-}
floating' :: (Base b, EffectiveDigit r) => proxy b -> ByteString -> Maybe (r, ByteString) 
floating' = fractional'
